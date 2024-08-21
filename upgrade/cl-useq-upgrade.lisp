(in-package :cl-useq-upgrade)

(defvar *useq-release-url*
  "https://api.github.com/repos/Emute-Lab-Instruments/uSEQ/releases"
  "A url used to query the latest useq releases.")

(defun fetch-emute-labs-version (&optional (url *useq-release-url*))
  (multiple-value-bind (body
			status-code
			headers
			uri
			stream
			must-close
			reason-phrase)
      (http-request url)
    (declare (ignore headers uri))
    (unwind-protect
	 (when (and (= 200 status-code)
		    (string= "OK" reason-phrase))
	   body)
      (when must-close
	(close stream)))))

(defun parse-emute-latest-firmware (http-body)
  (let ((forms (parse (octets-to-string http-body :external-format :utf8))))
    (gethash "tag_name" (car forms))))

(defun useq-latest-firmware-info (&optional (url *useq-release-url*))
  (parse-emute-latest-firmware (fetch-emute-labs-version url)))

(defun parse-firmware-version (firmware-string)
  (register-groups-bind (hw version major minor patch)
      ("uSEQ_([^_]+)_(\\d+).(\\d+).(\\d+)_(\\d{8})" firmware-string)
    (list hw version major minor patch)))

(defun firmware> (fw1-string fw2-string &optional exact-match)
  "Returns true if fw1 is greater than fw2.  If exact match is true
all parameters will be considered."
  (destructuring-bind (v1 major1 minor1 patch1 rel1)
      (parse-firmware-version fw1-string)
    (destructuring-bind (v2 major2 minor2 patch2 rel2)
	(parse-firmware-version fw2-string)
      (and (if exact-match (string> v1 v2) t)
	   (>= (parse-integer major1) (parse-integer major2))
	   (>= (parse-integer minor1) (parse-integer minor2))
	   (> (parse-integer patch1) (parse-integer patch2))
	   (if exact-match
	       (> (parse-integer rel1) (parse-integer rel2))
	       t)))))

(defun check-for-useq-upgrade (&optional (url *useq-release-url*)
				 (useq *useq*))
  (let ((hw (useq-report-firmware-info useq))
	(net (useq-latest-firmware-info url)))
    (when (firmware> net hw)
      (format t
	      "~%uSEQ firmware update ~A is current available for download."
	      net)
      t)))

