;;; emms-later-do.el --- Execute Lisp code ... later -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2021  Free Software Foundation, Inc.

;; Author: Jorgen Schaefer <forcer@forcix.cx>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary

;; This file will execute lisp code ``later on''.  This way it is
;; possible to work while elisp does some longer calculations, if you
;; can convert those calculations into a sequence of function calls.

;; 2020-09-22: Name changed from later-do to emms-later-do in order to
;; avoid polluting the namespace.

;;; Code:

(defvar emms-later-do-version "0.2emms4 (2018-04-07)"
  "Version string of emms-later-do.")

(defgroup emms-later-do nil
  "*Running functions ... later!"
  :prefix "emms-later-do-"
  :group 'development)

(defcustom emms-later-do-interval 0.5
  "How many seconds to wait between running events."
  :type 'number)

(defcustom  emms-later-do-batch 20
  "How many functions to process before waiting `emms-later-do-interval'.
The functions are processed from `emms-later-do-list'.  Must be 1 or
greater.  Too high a value might make Emacs slower while the
list is being processed."
  :type 'number)

(defvar emms-later-do-list nil
  "A list of functions to be called later on.")

(defvar emms-later-do-timer nil
  "The timer that emms-later-do uses.")

(defun emms-later-do (function &rest args)
  "Apply FUNCTION to ARGS later on.  This is an unspecified
amount of time after this call, and definitely not while lisp is
still executing.  Code added using `emms-later-do' is guaranteed to be
executed in the sequence it was added."
  (setq emms-later-do-list (nconc emms-later-do-list
                              (list (cons function args))))
  (unless emms-later-do-timer
    (setq emms-later-do-timer
          (run-with-timer emms-later-do-interval nil 'emms-later-do-timer))))

(defun emms-later-do-timer ()
  "Run the next element in `emms-later-do-list', or do nothing if it's
empty."
  (if (null emms-later-do-list)
      (setq emms-later-do-timer nil)
    (let (res)
      (unwind-protect
          (dotimes (_b (min emms-later-do-batch (length emms-later-do-list)) res)
            (let ((fun (caar emms-later-do-list))
                  (args (cdar emms-later-do-list)))
              (setq emms-later-do-list (cdr emms-later-do-list))
              (setq res (apply fun args)))))
      (setq emms-later-do-timer (run-with-timer emms-later-do-interval
                                           nil
                                           'emms-later-do-timer)))))

(provide 'emms-later-do)
;;; emms-later-do.el ends here
