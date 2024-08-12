;;;; cl-useq.asd

(asdf:defsystem #:cl-useq
  :description "A common lisp interface to uSEQ."
  :author "Elliott Johnson <elliott@elliottjohnson.net>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("libserialport")
  :components ((:file "package")
               (:file "cl-useq")))
