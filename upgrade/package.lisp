(defpackage :cl-useq-upgrade
  (:use :cl :cl-user :cl-useq :drakma :flexi-streams :yason :cl-ppcre :cl-fad)
  (:export
   :useq-upgrade-check
   :download-latest-useq-firmware
   :*useq-release-url*
   :*default-useq-firmware-upgrade-filename*
   :*useq-upgrade-file-prompt-hook*))

