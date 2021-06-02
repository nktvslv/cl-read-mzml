(defpackage #:cl-read-mzml
  (:use :common-lisp :asdf))

(defsystem cl-read-mzml
  :name "cl-read-mzml"
  :version "0.0.1"
  :maintainer "Nikita Vasilyev"
  :author "Nikita Vasilyev"
  :licence "GPL-3"
  :depends-on ("s-base64" "zlib" "cl-intbytes" "ieee-floats" "parse-float" "lparallel" "cl-cpus")
  :serial t
  :components ((:file "cl-read-mzml")))
