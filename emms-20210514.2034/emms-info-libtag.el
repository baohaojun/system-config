;;; emms-info-libtag.el --- Info-method for EMMS using libtag  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2021  Free Software Foundation, Inc.

;; Authors: Ulrik Jensen <terryp@daimi.au.dk>
;;          Jorgen Schäfer <forcer@forcix.cx>
;; Keywords:

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code has been adapted from code found in mp3player.el, written
;; by Jean-Philippe Theberge (jphiltheberge@videotron.ca), Mario
;; Domgoergen (kanaldrache@gmx.de) and Jorgen Schäfer
;; <forcer@forcix.cx>

;; To activate this method for getting info, use something like:

;; (require 'emms-info-libtag)
;; (add-hook 'emms-info-functions 'emms-info-libtag)

;; Note that you should remove emms-info-mp3info and emms-info-ogginfo
;; from the emms-info-functions list if you want to avoid
;; conflicts. For example, to set libtag as your exclusive info
;; provider:

;; (setq emms-info-functions '(emms-info-libtag))

;; You may have to compile the program from source.
;; Make sure that you have libtag installed.
;; In the EMMS source directory do
;;
;;    make emms-print-metadata
;;
;; and copy src/emms-print-metadata to your PATH.

;; If compilation fails and libtag is installed, you may have to
;; change the line
;;
;;    #include <tag_c.h>
;;
;; to the correction location, e.g.
;;
;;    #include <taglib/tag_c.h>

;;; Code:

(require 'emms-info)

(defgroup emms-info-libtag nil
  "Options for EMMS."
  :group 'emms-info)

(defvar emms-info-libtag-coding-system 'utf-8)

(defcustom emms-info-libtag-program-name "emms-print-metadata"
  "Name of emms-info-libtag program."
  :type '(string))

(defcustom emms-info-libtag-known-extensions
  (regexp-opt '("mp3" "mp4" "m4a" "ogg" "flac" "spx" "wma"))
  "Regexp of known extensions compatible with `emms-info-libtag-program-name'.

Case is irrelevant."
  :type '(string))

(defun emms-info-libtag (track)
  (when (and (emms-track-file-p track)
             (let ((case-fold-search t))
               (string-match
                emms-info-libtag-known-extensions
                (emms-track-name track))))
    (with-temp-buffer
      (when (zerop
             (let ((coding-system-for-read 'utf-8))
               (call-process emms-info-libtag-program-name
                             nil '(t nil) nil
                             (emms-track-name track))))
        (goto-char (point-min))
        ;; Crush the trailing whitespace
        (while (re-search-forward "[[:space:]]+$" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (while (looking-at "^\\([^=\n]+\\)=\\(.*\\)$")
          (let ((name (intern-soft (match-string 1)))
                (value (match-string 2)))
            (when (> (length value)
                     0)
              (emms-track-set track
                              name
                              (if (eq name 'info-playing-time)
                                  (string-to-number value)
                                value))))
          (forward-line 1))))))

(provide 'emms-info-libtag)
;;; emms-info-libtag.el ends here
