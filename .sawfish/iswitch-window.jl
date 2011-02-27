;; iswitch-window.jl 1.4 -- incremental search for windows
;; Time-stamp: <03/05/28 18:35:49 karlheg>
;;
;; Copyright (C) 2000 Topi Paavola <tjp@iki.fi>
;; Modifications and enhancements by Jens-Ulrik Petersen
;; <jens-ulrik.petersen@nokia.com>
;; And
;; Karl M. Hegbloom <hegbloom@pdx.edu>
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; Incremental window search, similar to iswitchb.el in emacs.
;;
;; Default keys (change in iswitch-get-window; ideally we should have an
;; iswitch-keymap):
;;
;; C-s,A-s,TAB    find next matching window
;; C-r,A-r,M-TAB  find previous matching window
;; C-g,ESC        quit iswitch
;; C-u            clear input buffer
;; backspace      delete previous character
;; C-z,A-z        iconify window
;; C-h,A-h        shade window
;; RET            select window
;; 
;; Other keys insert themselves. A-s (or H-s, depending on your
;; modifiers) would be a good key to bind iswitch-window to; for example,
;;
;;   (bind-keys global-keymap "A-s" 'iswitch-window)
;;
;; The iswitch-window-with function also does something to the current
;; window. For example,
;;
;;   (bind-keys global-keymap "A-s" '(iswitch-window-with
;;                                    (lambda (w) (iconify-window w))))
;;
;; would iconify the currently focused window and focus the newly
;; selected window.
;;
;; This file is available at
;;   http://aton.co.jyu.fi/~tjp/sawfish/iswitch-window.jl
;; and can also be found in the sawfish software map:
;;   http://adraken.themes.org/map.php
;;
;; 1.4:
;; * Allow setting font and colors - karlheg
;; * Add a filter regexp to remove some windows from the list - karlheg
;; 1.3:
;; * Updated URL.
;; * Added iswitch-show-window-class variable.
;; * Fixed problem with undefined classes, thanks to Matt Tucker.
;; 1.2:
;; * Added patches, A-z, A-h, reverse cycling (A-r) and
;;   iswitch-window-with from Jens.
;, * TAB and M-TAB now do the same as A-s and A-r.
;;

(provide 'iswitch-window)

(require 'window-order)

;; Use the following to customize the look of the menu.

(defvar iswitch-font default-font
  "Font for iswitch-window popup")

(defvar iswitch-background-color "white"
  "Background color for iswitch-window popup")

(defvar iswitch-foreground-color "black"
  "Foreground color for iswitch-window popup")

(defvar iswitch-show-window-class nil
  "Display WM_CLASS of windows in the iswitch menu.")

(defvar iswitch-show-window-machine t
  "Display WM_CLIENT_MACHINE of windows in the iswitch menu.")

(defvar iswitch-filter-out-window-name-regexp-list
  '("^\.gnome-desktop$"
    "^gmc$"
    "^panel$"))

; event callback for iswitch-get-window
(defun iswitch-read-event ()
  (throw 'iswitch-read (event-name (current-event))))

; (iswitch-rotate-from 2 '(1 2 3 4)) -> (2 3 4 1)
(defun iswitch-rotate-from (elem lst)
  (if elem
      (append (memq elem lst) (reverse (cdr (memq elem (reverse lst)))))
    lst))

; find next window matching input string
(defun iswitch-rotate-to-next-match (input wlist &optional previous)
  (setq wlist
        (iswitch-rotate-from
         (catch 'iswitch-found
           (mapc (lambda (w)
                   (when (string-match input (window-name w) 0 t)
                     (throw 'iswitch-found w)))
                 (if previous
                     (reverse (cdr wlist))
                   (cdr wlist))))
         wlist)))

(defun iswitch-update-match (input wlist)
  (filter (lambda (w) (string-match input (window-name w) 0 t))
		 wlist))

(setq iswitch-visibility-alist
      '((unobscured	    . "+")
	(partially-obscured . "%")
	(fully-obscured	    . "@")))

(setq iswitch-viewport-direction-alist
      '(((1  . 0)  . ">")
	((-1 . 0)  . "<")
	((0  . 1)  . "v")
	((0  . -1) . "^")
	((1  . 1)  . ".")
	((-1 . 1)  . ",")
	((-1 . -1) . "`")
	((1  . -1) . "'")))

(defun sign (num)
  (cond ((> num 0)
	 1)
	((zerop num)
	 0)
	((< num 0)
	 -1)))

(defun iswitch-viewport-direction (wv cv)
  (let ((wvx (car wv))
	(wvy (cdr wv))
	(cvx (car cv))
	(cvy (cdr cv)))
    (cdr (assoc (cons (sign (- wvx cvx)) (sign (- wvy cvy)))
		iswitch-viewport-direction-alist))))

(defun iswitch-window-line (w)
    (concat
     (cond ((window-get w 'iconified)
	    "i")
	   ((window-get w 'sticky)
	    "=")
	   ((not (window-in-workspace-p w current-workspace))
	    "X")
	   ((window-outside-viewport-p w)
	    (iswitch-viewport-direction (window-viewport w) (screen-viewport)))
	   ((window-get w 'shaded)
	    "s")
	   ((cdr (assoc (window-visibility w)
			iswitch-visibility-alist))))
     " "
     (window-name w)
     (let ((class-name (get-x-text-property w 'WM_CLASS)))
       (if (and iswitch-show-window-class class-name)
           (concat "  <" 
                   (aref class-name 1)
                   ">")))
     (let ((machine-name (get-x-text-property w 'WM_CLIENT_MACHINE)))
       (if (and iswitch-show-window-machine machine-name)
	   (concat " ["
		   (aref machine-name 0)
		   "]")))
     "\n"))

; format user input and window list for display-message
(defun iswitch-display-format (input wlist)
  (concat "  " input "_\n\n"
          (apply concat (mapcar iswitch-window-line wlist))))

(defun iswitch-filter-wlist-func (window)
  (let loop ((fow-nrl iswitch-filter-out-window-name-regexp-list))
       (cond
	 ((null fow-nrl) t)
	 ((string-match (car fow-nrl) (window-name window) 0 nil) nil)
	 (t (loop (cdr fow-nrl))))))

(defun iswitch-get-window ()
  "Let user pick a window with incremental search and return that window."
  (when (grab-keyboard)
    (unwind-protect
        (let* ((override-keymap '(keymap))
	      (input "")
	      (key "")
	      (init-wlist (filter iswitch-filter-wlist-func (window-order nil t t)))
	      (focused-window (car init-wlist))
	      wlist)
	  (setq init-wlist (append (cdr init-wlist) (list focused-window))
		wlist init-wlist)
          (add-hook 'unbound-key-hook iswitch-read-event)
          (catch 'exit-iswitch
            (while t
              (display-message (iswitch-display-format input wlist)
                               `((font . ,iswitch-font)
                                 (background . ,iswitch-background-color)
                                 (foreground . ,iswitch-foreground-color)))
              (setq key
                    (catch 'iswitch-read
                      (recursive-edit)))
              (cond ((or (equal key "C-g")
			 (equal key "A-g")
                         (equal key "ESC"))
                     (throw 'exit-iswitch nil))
                    ((or (equal key "C-u")
			 (equal key "A-u"))
		     (setq input ""
			   wlist (iswitch-update-match input init-wlist)))
                    ((equal key "BS")
                     (when (> (length input) 0)
			 (setq input (substring input 0 (1- (length input)))))
		     (setq wlist (iswitch-update-match input init-wlist)))
		    ((or (equal key "C-s")
			 (equal key "A-s")
                         (equal key "TAB"))
		     (setq wlist (iswitch-rotate-to-next-match input wlist)))
		    ((or (equal key "C-r")
			 (equal key "A-r")
                         (equal key "M-TAB"))
		     (setq wlist (iswitch-rotate-to-next-match input wlist t)))
		    ((equal key "SPC")
		     (setq input (concat input " ")
			   wlist (iswitch-update-match input init-wlist)))
		    ((equal key "RET")
		     (throw 'exit-iswitch (car wlist)))
		    ((or (equal key "C-z")
                         (equal key "A-z"))
		     (let ((w (car wlist)))
		       (if (window-get w 'iconified)
			   (uniconify-window w)
			 (iconify-window w))))
		    ((or (equal key "C-h")
                         (equal key "A-h"))
		     (toggle-window-shaded (car wlist)))
		    ((= 1 (length key))
		     (setq input (concat input key)
			   wlist (iswitch-update-match input wlist)))))))
      (remove-hook 'unbound-key-hook iswitch-read-event)
      (display-message nil)
      (ungrab-keyboard))))

(defun iswitch-window ()
  "Pick a window by incremental search and select it."
  (interactive)
  (display-window (iswitch-get-window)))

(defun iswitch-window-with (act)
  "Pick a window by incremental search, select it and ACT on previous."
  (interactive)
  (let ((old (car (window-order)))
	(new (iswitch-get-window)))
    (when new
      (unless (or (eq new old)
		  (window-get old 'sticky)
		  (window-outside-viewport-p new))
	(act old))
      (display-window new))))

(defun iswitch-swap-top-two ()
  "Flip between the top two windows in the iswitch-window list."
  (interactive)
  (display-window (cadr (filter iswitch-filter-wlist-func (window-order nil t t)))))
