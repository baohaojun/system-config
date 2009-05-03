;; -*- mode: emacs-lisp; template-expand-function: template-tempo-expand -*-
;;; (>>>FILE<<<) --- (>>>DESC<<<)

;; Copyright (C) (>>>YEAR<<<) Free Software Foundation, Inc.
;;
;; Author: (>>>USER_NAME<<<) <(>>>AUTHOR<<<)>
;; Maintainer: (>>>USER_NAME<<<) <(>>>AUTHOR<<<)>
;; Created: (>>>CDATE<<<)
;; Version: 0.01
;; Keywords: (>>>
(progn
  (require 'finder)
  (setq minibuffer-help-form
        (mapconcat (lambda (x) (format "%10s:  %s" (car x) (cdr x)))
                   finder-known-keywords
                   "\n"))
  nil)
(R (pi ("Keyword, C-h: " finder-known-keywords nil t) nil keyword)
   (& keyword (delete-backward-char 2))
   ", ")
(setq minibuffer-help-form nil)<<<)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; (>>>POINT<<<)

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require '(>>>FILE_SANS<<<))

;;; Code:

(eval-when-compile
  (require 'cl))

(>>>POINT<<<)

(provide '(>>>FILE_SANS<<<))
;;; (>>>FILE<<<) ends here
