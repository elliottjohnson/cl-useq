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

(asdf:defsystem #:cl-useq-upgrade
  :description "A common lisp package to upgrade uSEQ to the latest firmware."
  :author "Elliott Johnson <elliott@elliottjohnson.net>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("drakma" "yason" "cl-useq" "flexi-streams" "cl-ppcre" "cl-fad")
  :components
  ((:module "upgrade"
	    :components ((:file "package")
			 (:file "cl-useq-upgrade")))))
