(in-package :cl-useq-upgrade)

(defvar *useq-release-url*
  "https://api.github.com/repos/Emute-Lab-Instruments/uSEQ/releases"
  "A url used to query the latest useq releases.")

(defun fetch-emute-labs-releases (&optional (url *useq-release-url*))
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

(defun parse-github-http-json (http-body)
  (parse (octets-to-string http-body :external-format :utf8)))

(defun useq-latest-firmware-version-string (json-table)
  (gethash "tag_name" json-table))

(defun useq-latest-firmware-download-url (json-table)
  (gethash "browser_download_url"
	   (car (gethash "assets" json-table))))

(defun parse-firmware-version (firmware-string)
  (register-groups-bind (hw version major minor patch)
      ("uSEQ_([^_]+)_(\\d+).(\\d+).(\\d+)_(\\d{8})" firmware-string)
    (list hw version major minor patch)))

(defun firmware> (fw1-string fw2-string &optional exact-match)
  "Returns true if fw1 is greater than fw2.  If EXACT-MATCH is true
all parameters will be considered."
  (destructuring-bind (v1 major1 minor1 patch1 rel1)
      (parse-firmware-version fw1-string)
    (destructuring-bind (v2 major2 minor2 patch2 rel2)
	(parse-firmware-version fw2-string)
      (flet ((string-int-apply (fn s1 s2)
	       (funcall fn (parse-integer s1) (parse-integer s2))))
	(and (if exact-match (string>= v1 v2) t)
	    (string-int-apply #'>= major1 major2)
	    (string-int-apply #'>= minor1 minor2)
	    (string-int-apply #'> patch1 patch2)
	    (if exact-match
		(string-int-apply #'> rel1 rel2)
		t))))))

(defvar *default-useq-firmware-upgrade-filename* nil
  "A default filename to use when saving a new firmware.")

(defun useq-upgrade-check (&optional (url *useq-release-url*) (useq *useq*))
  "A function to query the hardware and a URL for information about the
current firmware version of the uSEQ module.  If a newer version is
available, it will prompt the user to proceed with the upgrade and provide
a link to step by step instructions by the project maintainers."
  (let* ((json (car (parse-github-http-json (fetch-emute-labs-releases url))))
	 (net-version (useq-latest-firmware-version-string json))
	 (hw-version (useq-report-firmware-info useq)))
    (when (firmware> net-version hw-version)
      (%prompt-for-upgrade net-version
			   hw-version
			   (useq-latest-firmware-download-url json)
			   *default-useq-firmware-upgrade-filename*))))

(defun *default-useq-upgrade-file-prompt-hook* (net-version
						hw-version
						dl-url &optional file)
  "A default method to inform the user that there is a new version of the
firmware, display the URL of the new version, and ask if they wish to
download it.  Finally a link to the upgrade instructions is provided."
  (flet ((final-statement ()
	   (format *query-io* "~&~%Please see https://emutelabinstruments.co.uk/useqinfo/useq-update/~%for further upgrade instructions~%~%")))
    
    (if (y-or-n-p
	 "~&~%uSEQ firmware version: ~S~%uSEQ firmware available: ~S~%~%Download url: ~A ?"
	 hw-version
	 net-version
	 dl-url)
	(let ((file-name
		(if (and (not file)
			 (y-or-n-p
			  "~&~%Do you want to use temporary storage?"))
		    (with-output-to-temporary-file (stream
						    :element-type
						    '(unsigned-byte 8))
		      (%save-useq-firmware-to-stream dl-url stream))
		    (%save-useq-firmware-to-prompted-filename file dl-url))))
	  (format *query-io* "~&~%Saved as file: ~A" file-name)
	  (final-statement)
	  file-name)
	(final-statement))))

(defun %save-useq-firmware-to-prompted-filename (file dl-url)
  "Prompts the user for a filename to save the firmware to then
calls a function to query the DL-URL and save the file to the
user provided filename"
  (let ((file-name file))
    (unless file-name
      (format t "~&~%Please provide a filename: ")
      (setf file-name (read-line)))
    (with-open-file (stream
		     file-name
		     :direction :output
		     :element-type '(unsigned-byte 8))
      (%save-useq-firmware-to-stream dl-url stream))
    file-name))

(defun %save-useq-firmware-to-stream (url file-stream)
  "Queries firmware download URL and writes the file contents to stream."
  (multiple-value-bind (http-stream status-code headers)
      (http-request url :want-stream t :force-binary t)
    (when (/= 200 status-code)
      (error "Failed to fetch ~A" url))
    (loop for i from 0 to (parse-integer (cdr (assoc :content-length headers)))
	  for byte = (read-byte http-stream nil nil)
	  when byte do (write-byte byte file-stream)
	    finally (close http-stream))))

(defvar *useq-upgrade-file-prompt-hook* nil
  "A variable that can be defined to act as a hook into prompting a cl-useq
user where to save a firmware file.

The uSEQ Upgrade file prompt hook should be a function that accepts 4 inputs:
  NETWORK-VERSION-STRING
  HARDWARE-VERSION-STRING
  DOWNLOAD-URL
  &optional LOCAL-FILE

The return value of this function is not of consequence.")

(defun %prompt-for-upgrade (net-version hw-version dl-url &optional file)
  "An internal function to call the file prompt hook, if one exists.
If it does not exist, a call to *default-useq-upgrade-file-prompt-hook*
is made, which does a very basic prompt."
  (if (functionp *useq-upgrade-file-prompt-hook*)
      (funcall #'*useq-upgrade-file-prompt-hook*
	       net-version
	       hw-version
	       dl-url
	       file)
      (*default-useq-upgrade-file-prompt-hook* net-version
					       hw-version
					       dl-url
					       file)))
