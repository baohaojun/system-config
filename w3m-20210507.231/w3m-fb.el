;;; w3m-fb.el --- frame-local buffers support for emacs-w3m

;;; Copyright (C) 2005, 2006, 2018, 2019 Matthew P. Hodges

;; Author: Matthew P. Hodges <MPHodges@member.fsf.org>
;; Version: $Id$

;; w3m-fb.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; w3m-fb.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; This mode handles the DEDICATED-FRAMES and TABBED-DEDICATED-FRAMES
;; display options, set in variable `w3m-display-mode' as 'frames or
;; 'tabbed-frames.
;;
;; With this mode switched on, W3M buffers are associated with the
;; frame on which they were created. For TABBED-DEDICATED-FRAMES, only
;; tabs for the current frame's W3M buffers are shown; other affected
;; commands are w3m-next-buffer w3m-previous-buffer, w3m-select-buffer
;; and w3m-quit.
;;
;; Switch the mode on programmatically with:
;;
;;     (w3m-fb-mode 1)
;;
;; or toggle interactively with M-x w3m-fb-mode RET.

;;; Code:

(defconst w3m-fb-version "1.0.0"
  "Version number of this package.")

(declare-function w3m-delete-buffer "w3m" (&optional force))
(declare-function w3m-list-buffers "w3m-util" (&optional nosort))
(declare-function w3m-next-buffer "w3m" (arg &optional buffer))
(defvar w3m-pop-up-frames)

(defvar w3m-fb-list-buffers-frame nil
  "Frame to list buffers for in `w3m-list-buffers'.
Bind this if the buffers associated with a frame other than the
selected frame are required.")

;; Customizable variables

(defgroup w3m-fb nil
  "Frame local buffers for emacs-w3m."
  :group 'w3m)

(defcustom w3m-fb-delete-frame-kill-buffers t
  "If non-nil, kill W3M buffers after deleting frames."
  :group 'w3m-fb
  :type 'boolean
  :set (lambda (sym val)
	 (set sym val)
	 (when (boundp 'w3m-fb-mode)
	   (if w3m-fb-mode
	       (add-hook 'delete-frame-functions
			 'w3m-fb-delete-frame-buffers)
	     (remove-hook 'delete-frame-functions
			  'w3m-fb-delete-frame-buffers)))))

;; Internal variables

(defvar w3m-fb-buffer-list nil
  "List of w3m buffers associated with the selected frame.")

(defvar w3m-fb-inhibit-buffer-selection nil
  "Non-nil to inhibit selecting a suitable w3m buffer.")

;; Internal functions

(defun w3m-fb-delete-frame-buffers (&optional frame)
  "Delete W3M buffers associated with frame FRAME."
  (let* ((w3m-fb-list-buffers-frame frame)
	 (buffers (w3m-list-buffers))
	 ;; Now bind w3m-fb-mode to nil so that w3m-delete-buffer
	 ;; doesn't call w3m-quit when there are w3m buffers belonging
	 ;; to other frames.
	 (w3m-fb-mode nil)
	 (w3m-fb-inhibit-buffer-selection t))
    (dolist (b buffers)
      (kill-buffer b))))

;; Could use set-frame-parameter here, but it isn't portable
(defun w3m-fb-set-frame-parameter (frame parameter value)
  "Set for frame FRAME parameter PARAMETER to VALUE."
  (modify-frame-parameters frame (list (cons parameter value))))

(defun w3m-fb-add ()
  "Add current buffer to `w3m-fb-buffer-list'."
  (let ((val (frame-parameter nil 'w3m-fb-buffer-list)))
    (w3m-fb-set-frame-parameter
     nil 'w3m-fb-buffer-list (nconc val (list (current-buffer))))))

(defun w3m-fb-remove ()
  "Remove current buffer from `w3m-fb-buffer-list'.
Applies to all frames."
  (when (eq major-mode 'w3m-mode)
    (let (val)
      (dolist (f (frame-list))
	(setq val (frame-parameter f 'w3m-fb-buffer-list))
	(w3m-fb-set-frame-parameter
	 f 'w3m-fb-buffer-list (delq (current-buffer) val))))))

(defun w3m-fb-associate ()
  "Associate all `w3m-mode' buffers with a frame."
  (let (buffers done rest)
    ;; Buffers displayed in windows
    (dolist (f (frame-list))
      (setq buffers nil)
      (dolist (w (window-list f nil (frame-selected-window f)))
	(when (with-current-buffer (window-buffer w)
		(eq major-mode 'w3m-mode))
	  (setq buffers (nconc buffers (list (window-buffer w))))
	  (setq done (nconc done (list (window-buffer w))))))
      (w3m-fb-set-frame-parameter
       f 'w3m-fb-buffer-list buffers))
    ;; Buffers not displayed in windows; add to selected frame
    (let ((w3m-fb-mode nil))
      (setq rest (w3m-list-buffers)))
    (dolist (b done)
      (setq rest (delq b rest)))
    (when rest
      (w3m-fb-set-frame-parameter
       nil 'w3m-fb-buffer-list
       (nconc (frame-parameter nil 'w3m-fb-buffer-list) rest)))))

(defun w3m-fb-dissociate ()
  "Disassociate `w3m-mode' buffers from frames."
  (dolist (f (frame-list))
    (w3m-fb-set-frame-parameter f 'w3m-fb-buffer-list nil)))

(defun w3m-fb-select-buffer ()
  "Select an appropriate W3M buffer to display."
  ;; If there are w3m buffers belonging to this frame, ensure one is
  ;; selected; if not make sure that we're not displaying a w3m
  ;; buffer
  (cond
   ;; Select w3m buffer belonging to frame, if one is available
   ((w3m-list-buffers)
    (unless (memq (current-buffer) (w3m-list-buffers))
      (w3m-next-buffer -1)))
   (t
    ;; If no w3m buffers belong to frame, don't display any w3m buffer
    (while (eq major-mode 'w3m-mode)
      ;; (assert (eq (current-buffer) (window-buffer (selected-window))))
      (bury-buffer)))))

;; Minor mode setup

;;;###autoload
(define-minor-mode w3m-fb-mode
  "Toggle W3M Frame Buffer mode.
This allows frame-local lists of buffers (tabs)."
  :init-value nil
  :group 'w3m-fb
  :global t
  (if (and w3m-fb-mode
	   (if w3m-pop-up-frames
	       (prog1
		   (setq w3m-fb-mode nil)
		 (message "\
W3M Frame Buffer mode not activated (Hint: `M-x w3m-display-mode').")
		 (sit-for 2))
	     t))
      (progn
	(add-hook 'w3m-mode-hook 'w3m-fb-add)
	(add-hook 'kill-buffer-hook 'w3m-fb-remove)
	(when w3m-fb-delete-frame-kill-buffers
	  (add-hook 'delete-frame-functions 'w3m-fb-delete-frame-buffers))
	(w3m-fb-associate))
    (remove-hook 'w3m-mode-hook 'w3m-fb-add)
    (remove-hook 'kill-buffer-hook 'w3m-fb-remove)
    (remove-hook 'delete-frame-functions 'w3m-fb-delete-frame-buffers)
    (w3m-fb-dissociate)))

(provide 'w3m-fb)

;;; w3m-fb.el ends here
