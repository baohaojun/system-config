;;; emms-info-exiftool.el --- info-method for EMMS using exiftool  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Author: Yoni Rabkin (yrk@gnu.org)
;; Keywords: multimedia

;; EMMS is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING..  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; (setq emms-info-functions '(emms-info-exiftool))

;; To use this you would need to have exiftool installed on your
;; system.


;;; Code:

(require 'emms-info)
(require 'json)


(defgroup emms-info-exiftool nil
  "Options for EMMS."
  :group 'emms-info)

(defvar emms-info-exiftool-field-map
  '((info-album        . Album)
    (info-artist       . Artist)
    (info-title        . Title)
    (info-tracknumber  . TrackNumber)
    (info-composer     . Composer)
    (info-year         . Year)
    (info-discnumber   . Discnumber)
    (info-genre        . Genre)
    (info-note         . Comment)
    (info-playing-time . Duration)
    (info-albumartist  . Albumartist))
  "Mapping for exiftool output.")


;; should only be called inside a buffer containing the json output of
;; exiftool
(defun emms-info-exiftool-time ()
  "Convert from exiftool-time to seconds."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "duration.+\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)"
			   (point-max) t)
	(let ((hours   (string-to-number (match-string-no-properties 1)))
	      (minutes (string-to-number (match-string-no-properties 2)))
	      (seconds (string-to-number (match-string-no-properties 3))))
	  (+ (* hours 60 60)
	     (* minutes 60)
	     seconds))
      0)))

(defun emms-info-exiftool (track)
  "Set TRACK info using exiftool."
  (when (eq (emms-track-type track) 'file)
    (with-temp-buffer
      (when (zerop
	     (let ((coding-system-for-read 'utf-8))
	       (call-process "exiftool" nil '(t nil) nil
			     "-json" (emms-track-name track))))
	(goto-char (point-min))
	(condition-case nil
	    (let ((json-fields (elt (json-read) 0)))
	      (mapc
	       (lambda (field-map)
		 (let ((emms-field (car field-map))
		       (exiftool-field (cdr field-map)))
		   (let ((track-field (assoc exiftool-field json-fields)))
		     (when track-field
		       (emms-track-set
			track
			emms-field
			(cond ((eq emms-field 'info-playing-time)
			       (emms-info-exiftool-time))
			      ((memq emms-field '(info-tracknumber
						  info-year
						  info-discnumber))
                               (format "%s" (cdr track-field)))
			      (t (cdr track-field))))))))
	       emms-info-exiftool-field-map))
	  (error (message "error while reading track info")))
	track))))


(provide 'emms-info-exiftool)

;;; emms-info-exiftool.el ends here
