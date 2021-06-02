(defpackage :cl-read-mzml
  (:use :common-lisp)
  (:export
   :open-file
   :collect-spectra
   :collect-data
   :filter-spectra
   :centroid-data
   :save-spectrum-to-file))

(in-package :cl-read-mzml)

(declaim (optimize (speed 3) (debug 1) (safety 1)))

;;; open file, read/unread chars

(defvar *file*)

(defun open-file (file)
  (setf *file* (open file :direction :input)))

(defun next-char ()
  (handler-case (read-char *file* nil 'eof)))

(defun undo-char (ch)
  (unread-char ch *file*))
  
(defun peek-next ()
  (peek-char t *file* nil 'eof))

;;; tokenize characters

(defstruct token
  (pos nil :type (or null fixnum))
  (type nil :type (or null symbol))
  (value nil :type (or null string)))

(defun make-adj-string ()
  (make-array 0
	      :element-type 'character
	      :adjustable t
	      :fill-pointer 0))

(defun get-token ()
  (let ((ch (next-char))
	(pk (peek-next)))
    (cond ((and (eq ch #\<) (or (eq pk #\?)(eq pk #\!)))
	   (read-comment))
	  ((and (eq ch #\<) (not (eq pk #\/)))
	   (read-open))
	  ((and (eq ch #\<) (eq pk #\/))
	   (read-close))
	  ((and (eq ch #\/) (eq pk #\>))
	   (make-token :type 'self-close :value ""))
	  ((eq ch #\space)
	   (read-attr-name))
	  ((eq ch #\")
	   (read-attr-value))
	  ((and (eq ch #\>) (not (eq pk 'eof)))
	   (read-text))
	  ((or (and (eq ch #\>) (eq pk 'eof)) (eq ch 'eof))
	   (make-token :type 'eof :value "eof"))
	  (t (make-token :type 'unknown :value ch)))))

(defun read-comment ()
  (do ((pos (file-position *file*))
       (ch (next-char)
	   (next-char))
       (value (make-adj-string)))
      ((eq ch #\>)
       (undo-char ch)
       (make-token :pos pos :type 'comment :value value))
    (vector-push-extend ch value)))

(defun read-open ()
  (do ((pos (file-position *file*))
       (ch (next-char)
	   (next-char))
       (value (make-adj-string)))
      ((or (eq ch #\space) (eq ch #\>))
       (undo-char ch)
       (make-token :pos pos :type 'open :value value))
    (vector-push-extend ch value)))

(defun read-close ()
  (do ((pos (file-position *file*))
       (ch (next-char)
	   (next-char))
       (value (make-adj-string)))
      ((or (eq ch #\space) (eq ch #\>))
       (undo-char ch)
       (make-token :pos pos :type 'close :value value))
    (when (not (eq ch #\/)) (vector-push-extend ch value))))

(defun read-attr-name ()
  (do ((pos (file-position *file*))
       (ch (next-char)
	   (next-char))
       (value (make-adj-string)))
      ((eq ch #\=)
       (make-token :pos pos :type 'attr-name :value value))
    (vector-push-extend ch value)))

(defun read-attr-value ()
  (do ((pos (file-position *file*))
       (ch (next-char)
	   (next-char))
       (value (make-adj-string)))
      ((eq ch #\")
       (make-token :pos pos :type 'attr-value :value value))
    (vector-push-extend ch value)))

(defun read-text ()
  (do ((pos (file-position *file*))
       (ch (next-char)
	   (next-char))
       (value (make-adj-string)))
      ((eq ch #\<)
       (undo-char ch)
       (make-token :pos pos :type 'text :value value))
    (vector-push-extend ch value)))

;;; token stream

(defstruct token-stream
  (token nil :type (or null token))
  (buffer nil :type (or null token)))

(defvar *ts* (make-token-stream :buffer nil :token nil))

(defun next-token ()
  (let ((tok (token-stream-token *ts*))
	(buf (token-stream-buffer *ts*)))
    (cond (buf
	   (setf (token-stream-buffer *ts*) nil)
	   buf)
	  ((and tok (not buf))
	   (setf (token-stream-token *ts*) nil)
	   tok)
	  (t
	   (setf (token-stream-token *ts*) (get-token))
	   (next-token)))))

(defun peek-token ()
  (let ((tok (token-stream-token *ts*))
	(buf (token-stream-buffer *ts*)))
    (cond (buf
	   buf)
	  ((and tok (not buf))
	   tok)
	  (t
	   (setf (token-stream-token *ts*) (get-token))
	   (peek-token)))))

(defun skip-token ()
  (let ((tok (token-stream-token *ts*))
	(buf (token-stream-buffer *ts*)))
    (cond (buf (setf (token-stream-buffer *ts*) nil))
	  ((and tok (not buf))
	   (setf (token-stream-token *ts*) nil))
	  (t (setf (token-stream-token *ts*) (get-token))
	     (skip-token)))))

(defun undo-token (tok)
  (setf (token-stream-buffer *ts*) tok))

;;; parse tokens

(defstruct spectrum
  (index 0 :type fixnum)
  (time 0.0 :type float)
  (mslevel 1 :type fixnum)
  (centroid nil :type boolean)
  (zlib nil :type boolean)
  (data-size 32 :type fixnum)
  (data nil :type list))

(defun collect-spectra ()
  (do ((tk (next-token)
	   (next-token))
       (spectra (make-array 0
			    :element-type 'spectrum
			    :adjustable t
			    :fill-pointer 0)))
      ((eofp tk) spectra)
    (cond ((new-spectrum-p tk)
	   (add-spectrum spectra)))))

(defun add-spectrum (spectra)
  (do ((tk (next-token)
	   (next-token))
       (sp (make-spectrum)))
      ((end-of-spectrum-p tk)
       (vector-push-extend sp spectra))
    (cond ((indexp tk)
	   (setf (spectrum-index sp) (parse-integer (token-value (next-token)))))
	  ((timep tk)
	   (skip-token)
	   (setf (spectrum-time sp) (parse-float:parse-float (token-value (next-token)))))
	  ((mslevelp tk)
	   (skip-token)
	   (setf (spectrum-mslevel sp) (parse-integer (token-value (next-token)))))
	  ((centroidedp tk)
	   (setf (spectrum-centroid sp) t))
	  ((data-size-p tk)
	   (setf (spectrum-data-size sp) (parse-integer (token-value tk) :junk-allowed t)))
	  ((zlibp tk)
	   (setf (spectrum-zlib sp) t))
	  ((datap tk)
	   (push (token-pos (next-token)) (spectrum-data sp)))
	  (t "error"))))

(defun datap (tk)
  (and (eq (token-type tk) 'open)
       (string= (token-value tk) "binary")))

(defun data-size-p (tk)
  (and (eq (token-type tk) 'attr-value)
       (or (string= (token-value tk) "64-bit float")
	   (string= (token-value tk) "32-bit float"))))
	    
(defun zlibp (tk)
  (and (eq (token-type tk) 'attr-value)
       (string= (token-value tk) "zlib compression")))

(defun mslevelp (tk)
  (and (eq (token-type tk) 'attr-value)
       (string= (token-value tk) "ms level")))

(defun centroidedp (tk)
  (and (eq (token-type tk) 'attr-value)
       (string= (token-value tk) "centroid spectrum")))

(defun timep (tk)
  (and (eq (token-type tk) 'attr-value)
       (string= (token-value tk) "scan start time")))

(defun indexp (tk)
  (and (eq (token-type tk) 'attr-name)
       (string= (token-value tk) "index")))

(defun end-of-spectrum-p (tk)
  (and (eq (token-type tk) 'close)
       (string= (token-value tk) "spectrum")))

(defun new-spectrum-p (tk)
  (and (eq (token-type tk) 'open)
       (string= (token-value tk) "spectrum")))

(defun eofp (tk)
  (eq (token-type tk) 'eof))

;;; select individual spectra for analysis

(defun get-spectrum (spectra index)
  (aref spectra index))

(defvar *mutex* (sb-thread:make-mutex :name "file access"))

(defun get-mz (spectrum)
  (let* ((pos (cadr (spectrum-data spectrum)))
	 (data (sb-thread:with-mutex (*mutex*)
		 (file-position *file* pos)
		 (token-value (read-text)))))
    (get-data spectrum data)))

(defun get-intensity (spectrum)
  (let* ((pos (car (spectrum-data spectrum)))
	 (data (sb-thread:with-mutex (*mutex*)
		 (file-position *file* pos)
		 (token-value (read-text)))))
    (get-data spectrum data)))
    
(defun get-data (sp data)
  (let ((float-size-bytes (/ (spectrum-data-size sp) 8))
	(compressed (spectrum-zlib sp))
	(byte-array)
	(int-array))
    (progn
      (with-input-from-string (in data)
	(if compressed
	    (setf byte-array (zlib:uncompress (s-base64:decode-base64-bytes in)))
	    (setf byte-array (s-base64:decode-base64-bytes in))))
      (setf int-array (bytes->int byte-array float-size-bytes))
      (int->float int-array float-size-bytes))))

(defun bytes->int (octets-array float-size-bytes)
  (let ((len-array (length octets-array))
	(octet-to-int (if (= 4 float-size-bytes)
			  #'cl-intbytes:octets->uint32
			  #'cl-intbytes:octets->uint64)))
    (do ((pos 0 (incf pos float-size-bytes))
	 (out (make-array 0
			  :adjustable t
			  :element-type (if (= float-size-bytes 4)
					    '(unsigned-byte 32) '(unsigned-byte 64))
			  :fill-pointer 0)))
	((= pos len-array) out)
      (declare (type fixnum len-array pos))
      (vector-push-extend (funcall octet-to-int octets-array pos) out))))

(defun int->float (int-array float-size-bytes)
  (let ((len-array (length int-array))
	(int-to-float (if (= 4 float-size-bytes)
				 #'ieee-floats:decode-float32
				 #'ieee-floats:decode-float64)))
    (do ((pos 0 (incf pos))
	 (out (make-array len-array :element-type (if (= 4 float-size-bytes)
						      'single-float 'double-float))))
	((eq pos len-array) out)
      (declare (type fixnum len-array pos))
      (setf (aref out pos) (funcall int-to-float (aref int-array pos))))))

;;; reset package variables
(defun reset-mzml ()
  (progn
    (close *file*)
    (setf *ts* (make-token-stream :buffer nil :token nil))))

;;; filter spectra
(defun filter-spectra (spectra &key mslevel start end)
  (declare (type (or null fixnum) mslevel start end))
  (let ((mslevel (if (null mslevel) 1 mslevel))
	(start (if (null start) 0 start))
	(end (if (null end) (1- (length spectra)) end)))
    (remove-if-not #'(lambda (sp)
		       (and (= (spectrum-mslevel sp) mslevel)
			    (>= (spectrum-index sp) start)
			    (<= (spectrum-index sp) end)))
		   spectra)))

(defparameter *core-count* (cl-cpus:get-number-of-processors))

(setf lparallel:*kernel* (lparallel:make-kernel *core-count*))

(defun collect-data (spectra)
  (lparallel:pmap 'vector
       #'(lambda (sp)
	   (let ((out (make-array 0
				  :adjustable t
				  :fill-pointer 0)))
	     (progn
	       (vector-push-extend (get-mz sp) out)
	       (vector-push-extend (get-intensity sp) out))
	     out)) :parts *core-count* spectra ))

;;; centroiding profile spectra
(defun profile->centroid (profile)
  (assert (and (= (length profile) 2)
	       (= (length (aref profile 0))
		  (length (aref profile 1)))))
  (let ((mz (aref profile 0))
	(it (aref profile 1))
	(mzout (make-array 0
			   :adjustable t
			   :element-type 'double-float
			   :fill-pointer 0))
	(itout (make-array 0
			   :adjustable t
			   :element-type 'double-float
			   :fill-pointer 0))
	(out (make-array 0
			 :adjustable t
			 :element-type '(simple-array double-float (*))
			 :fill-pointer 0)))
    (do ((i 1 (incf i))
	 (count-up 0)
	 (count-down 0)
	 (peak nil))
	((> i (1- (length it)))
	 (progn
	   (vector-push-extend mzout out)
	   (vector-push-extend itout out)
	   out))
      (cond ((> (aref it i) (aref it (1- i)))
	     (when (and (>= count-up 2) (>= count-down 2) peak)
	       (vector-push-extend (aref mz peak) mzout)
	       (vector-push-extend (aref it peak) itout)
	       (setf count-up 0)
	       (setf count-down 0)
	       (setf peak nil))
	     (incf count-up))
	    ((< (aref it i) (aref it (1- i)))
	     (when (>= count-up 2)
	       (setf peak (1- i)))
	     (incf count-down))))))

(defun centroid-data (profile-spectra)
  (lparallel:pmap 'vector #'profile->centroid :parts *core-count* profile-spectra))

(defun save-spectrum-to-file (spectrum filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (dotimes (i (length (aref spectrum 0)))
      (format out "~6$~t~6$~%" (aref (aref spectrum 0) i) (aref (aref spectrum 1) i)))))
