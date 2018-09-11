;;; xclip.el --- use xclip to copy&paste             -*- lexical-binding: t; -*-

;; Copyright (C) 2007, 2012, 2013, 2017  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: convenience, tools
;; Created: 2007-12-30
;; Version: 1.4

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows Emacs to copy to and paste from the X clipboard
;; when running in xterm.  It uses the external command-line tool xclip
;; found on http://xclip.sourceforge.net.
;;
;; To use: (xclip-mode 1)

;;; Code:

(defcustom xclip-program "xclip"
  "Name of the xclip program."
  :type 'string
  :group 'killing)

(defcustom xclip-select-enable-clipboard t
  "Non-nil means cutting and pasting uses the clipboard.
This is in addition to, but in preference to, the primary selection."
  :type 'boolean
  :group 'killing)

(defcustom xclip-use-pbcopy&paste (and xclip-select-enable-clipboard
                                       (eq system-type 'darwin)
                                       (executable-find "pbcopy")
                                       t)
  "Non-nil means using pbcopy and pbpaste instead of xclip.
If non-nil `xclip-program' is ignored."
  :type 'boolean
  :group 'killing)

(defvar xclip-last-selected-text-clipboard nil
  "The value of the CLIPBOARD X selection from xclip.")

(defvar xclip-last-selected-text-primary nil
  "The value of the PRIMARY X selection from xclip.")

;;;; Core functions.

(defun xclip-set-selection (type data)
  "TYPE is a symbol: primary, secondary and clipboard.

See also `x-set-selection'."
  (let* ((process-connection-type nil)
         (proc (cond
                ((and xclip-use-pbcopy&paste (memq type '(clipboard CLIPBOARD)))
                 (start-file-process "pbcopy" nil "pbcopy"))
                ((getenv "DISPLAY")
                 (start-file-process "xclip" nil xclip-program
                                     "-selection" (symbol-name type))))))
    (when proc
      (process-send-string proc data)
      (process-send-eof proc))
    data))

(defun xclip-get-selection (type)
  "TYPE is a symbol: primary, secondary and clipboard."
  (with-output-to-string
    (cond
     ((and xclip-use-pbcopy&paste (memq type '(clipboard CLIPBOARD)))
      (process-file "pbpaste" nil standard-output nil))
     ((getenv "DISPLAY")
      (process-file xclip-program nil standard-output nil
                    "-o" "-selection" (symbol-name type))))))

;;;###autoload
(define-minor-mode xclip-mode
  "Minor mode to use the `xclip' program to copy&paste."
  :global t
  (remove-hook 'terminal-init-xterm-hook #'xclip--setup)
  (when xclip-mode
    (unless (or xclip-use-pbcopy&paste
		(executable-find xclip-program))
      (setq xclip-mode nil)
      (signal 'file-error (list "Searching for program"
				xclip-program "no such file")))
    (when (< emacs-major-version 25)
      ;; NOTE: See `tty-run-terminal-initialization' and term/README
      (add-hook 'terminal-init-xterm-hook #'xclip--setup))))

;;;; Glue code for Emacs < 25

(defun xclip-select-text (text)
  "See `x-select-text'."
  (xclip-set-selection 'primary text)
  (setq xclip-last-selected-text-primary text)
  (when xclip-select-enable-clipboard
    (xclip-set-selection 'clipboard text)
    (setq xclip-last-selected-text-clipboard text)))

(defun xclip-selection-value ()
  "See `x-selection-value'."
  (let ((clip-text (when xclip-select-enable-clipboard
                     (xclip-get-selection 'CLIPBOARD))))
    (setq clip-text
          (cond                         ; Check clipboard selection.
           ((or (not clip-text) (string= clip-text ""))
            (setq xclip-last-selected-text-clipboard nil))
           ((eq clip-text xclip-last-selected-text-clipboard)
            nil)
           ((string= clip-text xclip-last-selected-text-clipboard)
            ;; Record the newer string so subsequent calls can use the
            ;; `eq' test.
            (setq xclip-last-selected-text-clipboard clip-text)
            nil)
           (t (setq xclip-last-selected-text-clipboard clip-text))))
    (or clip-text
        (when (and (not xclip-use-pbcopy&paste) (getenv "DISPLAY"))
          (let ((primary-text (with-output-to-string
                                (process-file xclip-program nil
                                              standard-output nil "-o"))))
            (setq primary-text
                  (cond                 ; Check primary selection.
                   ((or (not primary-text) (string= primary-text ""))
                    (setq xclip-last-selected-text-primary nil))
                   ((eq primary-text xclip-last-selected-text-primary)
                    nil)
                   ((string= primary-text xclip-last-selected-text-primary)
                    ;; Record the newer string so subsequent calls can
                    ;; use the `eq' test.
                    (setq xclip-last-selected-text-primary primary-text)
                    nil)
                   (t (setq xclip-last-selected-text-primary primary-text))))
            primary-text)))))

(defun xclip--setup ()
  (setq interprogram-cut-function #'xclip-select-text)
  (setq interprogram-paste-function #'xclip-selection-value))


;;;; Glue code for Emacs ≥ 25

(eval-when-compile
  (defmacro xclip--if-macro-fboundp (name then &rest else)
    ;; FIXME: copy&pasted from AUCTeX's tex.el.
    "Execute THEN if macro NAME is bound and ELSE otherwise.
Essentially,

  (xclip--if-macro-fboundp name then else...)

is equivalent to

  (if (fboundp 'name) then else...)

but takes care of byte-compilation issues where the byte-code for
the latter could signal an error if it has been compiled with
emacs 24.1 and is then later run by emacs 24.5."
    (declare (indent 2) (debug (symbolp form &rest form)))
    (if (fboundp name)           ;If macro exists at compile-time, just use it.
        then
      `(if (fboundp ',name)             ;Else, check if it exists at run-time.
	   (eval ',then)                ;If it does, then run the then code.
         ,@else))))                     ;Otherwise, run the else code.

(xclip--if-macro-fboundp cl-defmethod
 (progn
   ;; FIXME: implement the methods for gui-backend-selection-owner-p
   ;; and gui-backend-selection-exists-p.  Not sure about pbcopy, but at least
   ;; with xcopy, gui-backend-selection-owner-p should just require us
   ;; to use "-silent" and keep track of the liveness of the subprocess.

   (cl-defmethod gui-backend-get-selection (selection-symbol _target-type
                                            &context (window-system nil))
     (if (not xclip-mode)
         (cl-call-next-method)
       (xclip-get-selection selection-symbol)))

   (cl-defmethod gui-backend-set-selection (selection-symbol value
                                            &context (window-system nil))
     (if (not xclip-mode)
         (cl-call-next-method)
       (xclip-set-selection selection-symbol value)
       nil))

   ;; BIG UGLY HACK!
   ;; xterm.el has a defmethod to use some poorly supported escape sequences
   ;; (code named OSC 52) for clipboard interaction, and enables it by default.
   ;; Problem is, that its defmethod takes precedence over our defmethod,
   ;; so we need to disable it in order to be called.
   (cl-defmethod gui-backend-set-selection :extra "xclip-override"
     (selection-symbol value &context (window-system nil)
                       ((terminal-parameter nil 'xterm--set-selection) (eql t)))
     ;; Disable this method which doesn't work anyway in 99% of the cases!
     (setf (terminal-parameter nil 'xterm--set-selection) nil)
     ;; Try again!
     (gui-backend-set-selection selection-symbol value))))

;;;; ChangeLog:

;; 2017-11-08  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* xclip/xclip.el: Use gui-backend-*-selection in Emacs≥25
;; 
;; 	(xclip-set-selection) <pbcopy>: Don't set the clipboard when primary is
;; 	requested.
;; 	(xclip-get-selection): New function extracted from
;; 	xclip-selection-value.
;; 	(turn-on-xclip): Rename to xclip--setup.
;; 	(xclip-mode): Don't use xclip--setup in Emacs≥25. Disable the mode is
;; 	xclip-program is not found.
;; 	(xclip--if-macro-fboundp): New macro.
;; 	(gui-backend-get-selection, gui-backend-set-selection): New methods.
;; 
;; 2013-09-06  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	* xclip.el: Fix last change
;; 
;; 2013-09-06  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	* xclip.el: Use pbcopy and pbpaste if available
;; 
;; 	(xclip-use-pbcopy&paste): New variable.
;; 	(xclip-set-selection, xclip-selection-value, xclip-mode): Use it.
;; 
;; 2013-09-05  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	* xclip.el: Some cleanups and fix copyright years.
;; 
;; 	(xclip-program, xclip-select-enable-clipboard): Use defcustom.
;; 	(xclip-select-text): Cleanup.
;; 	(turn-off-xclip): Remove.
;; 	(xclip-mode): Check xclip-program here.
;; 
;; 2012-02-05  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* xclip.el: Better follow conventions. Fix up copyright notice.
;; 	(xclip-program): Make it work in the usual way.
;; 	(xclip-set-selection, xclip-selection-value): Obey xclip-program.
;; 	(turn-on-xclip, turn-off-xclip): Don't autoload, not interactive.
;; 	(xclip-mode): New minor mode to avoid enabling it unconditionally.
;; 
;; 2012-02-05  Leo Liu  <sdl.web@gmail.com>
;; 
;; 	Add xclip.el.
;; 


(provide 'xclip)
;;; xclip.el ends here
