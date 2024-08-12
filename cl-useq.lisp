;;;; cl-useq.lisp

(in-package #:cl-useq)

(defvar *default-useq-device* "/dev/ttyACM0"
  "The default device to form a serial connection with to uSEQ.")
(defvar *default-useq-baud* 115200
  "The default baud rate to use to connect to uSEQ.")
(defvar *default-useq-encoding* :ascii
  "The default character encoding to use when sending and receiving data from uSEQ.")
(defvar *default-useq-line-end* :lf
  "The default character to send at the end of a line.  Typically its \n, which is :lf.")
(defvar *default-useq-timeout* 1000
  "The default duration to block for reads or writes.  I'd like to move to a non-blocking model for this.")

(defvar *default-useq-echo* t
  "A variable that indicates if we pass the useq echo responses back or is silence golden?")
(defvar *useq-serial-response-prefix* (format nil "~A " #\Us)
  "The prefix for all useq return messages over serial.")
(defvar *useq-max-read-trys* 5
  "Try to read from uSEQ this many times.  We are seeing some failures after a while.")

(defun find-serial-port (serial-port-name)
  "A function to find a serial port by name."
  (gethash serial-port-name *serial-port-hash*))

(defvar *useq* nil
  "A variable to hold the current uSEQ serial port connection.")

(defun open-useq (device-string &rest keys &key (baud *default-useq-baud*))
  "Ensures uSEQ is opened using a device string uSEQ-DEVICE."
  (setf *useq*
	(or (find-serial-port device-string)
	    (apply #'open-serial-port device-string :baud baud keys))))

(defun send-to-useq (statement &key (serial-port *useq*)
				 (timeout *default-useq-timeout*)
				 (encoding *default-useq-encoding*)
				 (line-end *default-useq-line-end*))
  "Wites statement to uSEQ.  Statement can be a string, unsigned byte 8 vector, a number 0-255, 
or a character."
  (serial-write-data serial-port
		     statement
		     :blocking t
		     :timeout timeout
		     :encoding encoding
		     :line-end line-end))

(defun close-useq (&optional (serial-port *useq*))
  "Closes the connection to uSEQ."
  (shutdown-serial-port serial-port)
  (when (eql serial-port *useq*)
    (setf *useq* nil)))

(defun read-from-useq (&key (serial-port *useq*)
			 (timeout *default-useq-timeout*)
			 (encoding *default-useq-encoding*))
  "Reads a line from useq.  Todo nonblocking, handle midi and other data IO"
  (when (serial-input-waiting serial-port)
    (let ((response (serial-read-line serial-port
				      :blocking t
				      :timeout timeout
				      :encoding encoding)))
      ;;(format t "~%DBC> ~S" response)
      (cond (;; Occasionally we just get empty responses.
	     (string= response "") nil)
	    (;; Sometimes the responses are just the prefix: ASCII code 31
	     ;;    (#\Us) and a space.
	     (string= response *useq-serial-response-prefix*) nil)
	    (;; The issued command is echoed back with the prefix and
	     ;;    sometimes the response too?!?
	     (string= (subseq response 0 2) *useq-serial-response-prefix*)
	     (subseq response 2))
	    (;; Sometimes just a space is given before the response.
	     (char= #\Space (char response 0)) (subseq response 1))
	    ;; Catch any errors that might be out there.
	    (t (error "Unknown response from uSEQ."))))))

(defun cl-useq-repl (&optional (echo *default-useq-echo*))
  (serial-flush-buffer *useq*)
  (loop
    (progn (format t "~%uSEQ    < ")
	   (let ((input (read-line)))
	     (unless (string= "" input)
	       (send-to-useq input))
	     ;; Firstly we need to loop over the input, because
	     ;;    I'm not getting reliable behaviour from uSEQ
	     ;;    At the top level we loop twice because we always get
	     ;;       the issued command echoed and then the response.
	     (dotimes (i 2)
	       ;; For the inner loop, we go to *USEQ-MAX-READ-TRYS*
	       (do ((count 0 (incf count))
		    (output (read-from-useq)(read-from-useq)))
		   ((or output
			(= count *useq-max-read-trys*))
		    (when (or echo (= i 2))
		      (format echo "~%uSEQ ~D ~D> ~S" i count output)))
		 (format t ".")))
	     (finish-output)))))



