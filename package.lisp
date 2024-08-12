;;;; package.lisp

(defpackage #:cl-useq
  (:use #:cl #:libserialport)
  (:export #:open-useq
	   #:*useq*
	   #:send-to-useq
	   #:read-from-useq
	   #:cl-useq-repl))
