;;; emms-playing-time.el --- Display emms playing time on mode line  -*- lexical-binding: t; -*-

;; Copyright (C) 2005-2021  Free Software Foundation, Inc.

;; Author: William Xu <william.xwl@gmail.com>, Yoni Rabkin (yrk@gnu.org)

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Display playing time on mode line, it looks like: 01:32/04:09.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;     (require 'emms-playing-time)
;;     (emms-playing-time 1)

;; Note: `(emms-playing-time -1)' will disable emms-playing-time module
;; completely, and is not recommended. (since some other emms modules
;; may rely on it, such as `emms-lastfm.el')

;; Instead, to toggle displaying playing time on mode line, one could
;; call `emms-playing-time-enable-display' and
;; `emms-playing-time-disable-display'."

;;; Code:

(require 'cl-lib)
(require 'emms-info)
(require 'emms-player-simple)

;;; Customizations

(defgroup emms-playing-time nil
  "Playing-time module for EMMS."
  :group 'emms)

(defcustom emms-playing-time-display-short-p nil
  "Non-nil will only display elapsed time.
e.g., display 02:37 instead of 02:37/05:49."
  :type 'boolean)

(defcustom emms-playing-time-display-format " %s "
  "Format used for displaying playing time."
  :type 'string)

(defcustom emms-playing-time-style 'time
  "Style used for displaying playing time.
Valid styles are `time' (e.g., 01:30/4:20),
 `bar' (e.g., [===>  ]),
and `downtime' (e.g. -03:58)."
  :type 'symbol)


;;; Emms Playing Time

(define-obsolete-variable-alias 'emms-playing-time-display-p
  'emms-playing-time-display-mode "Apr 2021")
(defvar emms-playing-time-display-mode)

(defvar emms-playing-time 0
  "Time elapsed in current track.")

(defvar emms-playing-time-string "")

(defvar emms-playing-time-display-timer nil)

(define-obsolete-variable-alias 'emms-playing-time-p
  'emms-playing-time-mode "Apr 2021")

(defun emms-playing-time-start ()
  "Get ready for display playing time."
  (setq emms-playing-time 0)
  (unless emms-playing-time-display-timer
    (setq emms-playing-time-display-timer
	  (run-at-time t 1 #'emms-playing-time-display))))

(defun emms-playing-time-stop ()
  "Remove playing time on the mode line."
  (if (or (not emms-player-paused-p)
	  emms-player-stopped-p)
      (progn
	(setq emms-playing-time-string "")
	(force-mode-line-update)))
  (emms-cancel-timer emms-playing-time-display-timer)
  (setq emms-playing-time-display-timer nil))

(defun emms-playing-time-pause ()
  "Pause playing time."
  (if emms-player-paused-p
      (emms-playing-time-stop)
    (unless emms-playing-time-display-timer
      (setq emms-playing-time-display-timer
	    (run-at-time t 1 #'emms-playing-time-display)))))

(defun emms-playing-time-seek (sec)
  "Seek forward or backward SEC playing time."
  (setq emms-playing-time (+ emms-playing-time sec))
  (when (< emms-playing-time 0)		; back to start point
    (setq emms-playing-time 0)))

(defun emms-playing-time-set (sec)
  "Set the playing time to SEC."
  (setq emms-playing-time sec)
  (when (< emms-playing-time 0)		; back to start point
    (setq emms-playing-time 0)))

(defun emms-playing-time (arg)
  (declare (obsolete emms-playing-time-mode "Apr 2021"))
  (emms-playing-time-mode (if (and arg (> arg 0)) 1 -1)))


(define-minor-mode emms-playing-time-mode
  "Turn on emms playing time if ARG is positive, off otherwise.

Note: `(emms-playing-time -1)' will disable emms-playing-time
module completely, and is not recommended. (since some other emms
modules may rely on it, such as `emms-lastfm.el')

Instead, to toggle displaying playing time on mode line, one
could call `emms-playing-time-enable-display' and
`emms-playing-time-disable-display'."
  :global t
  (if emms-playing-time-mode
      (progn
	;; FIXME: Maybe we shouldn't set this here, and instead the users
        ;; should call `emms-playing-time-display-mode' if that's what
        ;; they want.
	(setq emms-playing-time-display-mode t)
	(emms-playing-time-mode-line)
	(add-hook 'emms-player-started-hook       #'emms-playing-time-start)
	(add-hook 'emms-player-stopped-hook       #'emms-playing-time-stop)
	(add-hook 'emms-player-finished-hook      #'emms-playing-time-stop)
	(add-hook 'emms-player-paused-hook        #'emms-playing-time-pause)
	(add-hook 'emms-player-seeked-functions   #'emms-playing-time-seek)
	(add-hook 'emms-player-time-set-functions #'emms-playing-time-set))
    (setq emms-playing-time-display-mode nil)
    (emms-playing-time-stop)
    (emms-playing-time-restore-mode-line)
    (remove-hook 'emms-player-started-hook       #'emms-playing-time-start)
    (remove-hook 'emms-player-stopped-hook       #'emms-playing-time-stop)
    (remove-hook 'emms-player-finished-hook      #'emms-playing-time-stop)
    (remove-hook 'emms-player-paused-hook        #'emms-playing-time-pause)
    (remove-hook 'emms-player-seeked-functions   #'emms-playing-time-seek)
    (remove-hook 'emms-player-time-set-functions #'emms-playing-time-set)))

;;;###autoload
(define-minor-mode emms-playing-time-display-mode
  "Minor mode to display playing time on mode line."
  :global t
  ;; When disabling the mode, don't disable `emms-playing-time-display-mode'
  ;; since that may be used by other packages.
  (if emms-playing-time-display-mode
      (emms-playing-time-display-mode 1)))

;;;###autoload
(defun emms-playing-time-enable-display ()
  "Display playing time on mode line."
  (declare (obsolete emms-playing-time-display-mode "Apr 2021"))
  (interactive)
  (setq emms-playing-time-display-mode t))

;;;###autoload
(defun emms-playing-time-disable-display ()
  "Remove playing time from mode line."
  (declare (obsolete emms-playing-time-display-mode "Apr 2021"))
  (interactive)
  (setq emms-playing-time-display-mode nil))

(defun emms-playing-time-display ()
  "Display playing time on the mode line."
  (setq emms-playing-time (round (1+ emms-playing-time)))
  (setq emms-playing-time-string
        (if (null emms-playing-time-display-mode)
            ""
          (let* ((min (/ emms-playing-time 60))
                 (sec (% emms-playing-time 60))
                 (total-playing-time
                  (or (emms-track-get
                       (emms-playlist-current-selected-track)
                       'info-playing-time)
                      0))
                 (total-min-only (/ total-playing-time 60))
                 (total-sec-only (% total-playing-time 60))
                 (string
	          (cl-case emms-playing-time-style
	            ((downtime)         ; `downtime' style
	             (emms-replace-regexp-in-string
                      " " "0"
                      (if (or emms-playing-time-display-short-p
                              ;; unable to get total playing-time
                              (eq total-playing-time 0))
                          (format "%2d:%2d" min sec)
                        (format "-%2d:%2d"
			        (/ (- total-playing-time emms-playing-time) 60)
			        (% (- total-playing-time sec) 60)))))
		    ((bar)              ; `bar' style
		     (if (zerop total-playing-time)
		         "[==>........]"
                       (let (;; percent based on 10
                             (percent (/ (* emms-playing-time 10)
                                         total-playing-time)))
                         (concat "["
                                 (make-string percent ?=)
                                 ">"
                                 (make-string (- 10 percent) ?\s)
                                 "]"))))
                    (t                  ; `time' style
                     (emms-replace-regexp-in-string
                      " " "0"
                      (if (or emms-playing-time-display-short-p
                              ;; unable to get total playing-time
                              (eq total-playing-time 0))
                          (format "%2d:%2d" min sec)
                        (format "%2d:%2d/%2s:%2s"
                                min sec total-min-only total-sec-only)))))))
            (format emms-playing-time-display-format string))))
  (force-mode-line-update))

(defun emms-playing-time-mode-line ()
  "Add playing time to the mode line."
  (or global-mode-string (setq global-mode-string '("")))
  (unless (member 'emms-playing-time-string
		  global-mode-string)
    (setq global-mode-string
	  (append global-mode-string
		  '(emms-playing-time-string)))))

(defun emms-playing-time-restore-mode-line ()
  "Restore the mode line."
  (setq global-mode-string
	(remove 'emms-playing-time-string global-mode-string))
  (force-mode-line-update))

(provide 'emms-playing-time)

;;; emms-playing-time.el ends here
