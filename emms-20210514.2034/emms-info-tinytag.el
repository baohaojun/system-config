;;; emms-info-tinytag.el --- Info-method for EMMS using tinytag  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Free Software Foundation, Inc.

;; Author: Fran Burstall <fran.burstall@gmail.com>
;; Keywords: multimedia

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING..  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This code has been adapted from code found in emms-info-libtag.el,
;; written by Ulrik Jensen <terryp@daimi.au.dk> and Jorgen Schäfer
;; <forcer@forcix.cx>.

;; To activate this method for getting info, use:

;; (require 'emms-info-tinytag)
;; (add-to-list 'emms-info-functions 'emms-info-tinytag)

;; Note that you should remove emms-info-mp3info and emms-info-ogginfo
;; from the emms-info-functions list if you want to avoid
;; conflicts.  For example, to set tinytag as your exclusive info
;; provider:

;; (setq emms-info-functions '(emms-info-tinytag))

;; To use this provider, you need to install the tinytag python module
;; with something like:

;; pip install tinytag

;;; Code:

(require 'emms-info)
(require 'json)  			;see below

(defgroup emms-info-tinytag nil
  "Options for EMMS."
  :group 'emms-info)

(defvar emms-info-tinytag-coding-system 'utf-8)	;is this used anywhere?

(defcustom emms-info-tinytag-python-name "python"
  "Name of python we use."
  :type '(string))

(defcustom emms-info-tinytag-known-extensions
  (regexp-opt '("mp3" "mp4" "m4a" "m4b" "ogg" "opus" "flac" "wma" "wav"))
  "Regexp of known extensions that `emms-info-tinytag' can handle.

Case is irrelevant."
  :type '(string))

(defvar emms-info-tinytag--info-fields
  '((info-album . album)
    (info-albumartist . albumartist)
    (info-artist . artist)
    (info-composer . composer)
    (info-year . year)	     
    (info-discnumber . disc)
    (info-genre . genre)
    (info-note . comment)
    (info-playing-time . duration)
    (info-title . title)
    (info-tracknumber . track))
  "An alist mapping info-* fields to tinytag fields.")

(defun emms-info-tinytag (track)
  "Set tags for TRACK using tinytag."
  (when (and (emms-track-file-p track)
	     (let ((case-fold-search t))
	       (string-match
		emms-info-tinytag-known-extensions
		(emms-track-name track))))
    (with-temp-buffer
      (when (zerop
	     (let ((coding-system-for-read 'utf-8))
	       (call-process emms-info-tinytag-python-name
			     nil '(t nil) nil
			     "-m" "tinytag" (emms-track-name track))))
	(goto-char (point-min))
	;; tinytag can output json or [ct]sv.  Sadly, in the latter
	;; case, null values are unhelpfully represented by the string
	;; "None" so we parse the json.
	(let ((track-info (json-read)))
	  (dolist (field emms-info-tinytag--info-fields)
	    (let ((name (car field))
		  (value (alist-get (cdr field) track-info)))
	      (when (and value (or (numberp value) (> (length value)
						      0)))
		(emms-track-set track
				name
				(cond ((eq name 'info-playing-time)
				       (round value))
				      ;; for m4a, disc or track is an int: issue raised upstream
				      ((and (or (eq name 'info-discnumber)
						(eq name 'info-tracknumber))
					    (numberp value))
				       (number-to-string value))
				      (t value)))))))))))

(provide 'emms-info-tinytag)

;;; emms-info-tinytag.el ends here
