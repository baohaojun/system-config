;;; -*- coding: utf-8 -*-
;;; eim.el --- Emacs Input method

;; Copyright 2006 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: eim.el,v 1.5 2007/01/14 01:50:15 ywb Exp $
;; Keywords:
;; X-URL: not distributed yet

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

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/eim")
;; (autoload 'eim-use-package "eim" "Another emacs input method")

;; (register-input-method
;;  "eim-wb" "euc-cn" 'eim-use-package
;;  "五笔" "汉字五笔输入法" "wb.txt")
;; (register-input-method
;;  "eim-py" "euc-cn" 'eim-use-package
;;  "拼音" "汉字拼音输入法" "py.txt")

;;; Code:

(provide 'eim)
(eval-when-compile
  (require 'cl))
(require 'help-mode)

(defvar eim-version "2.4")


(defgroup eim nil
  "eim: emacs input method"
  :group 'leim)
(defvar eim-page-length 7 "每页显示的词条数目")

(defface eim-string-face '((t (:underline t)))
  "Face to show current string"
  :group 'eim)

(defvar eim-current-key "" "已经输入的代码")
(defvar eim-current-str "" "当前选择的词条")
(defvar eim-current-cands "" "所有可选的词条。
比如：\"菹(aie) 伜(wvfh) 伹(wegg) 倅(wywf) 傶(wdht%20wdhy) 卆(vfj) 哫(kkhy) 啐(kyw) 嘁(kdht%20kdhy) 岨(megg)\"
词条与词条之间用空格分开，如果候选词内出现空格的话会用%20代替。
")

(defvar eim-current-index 1 "当前激活的候选词序号，如果按下上屏键的话，对应的候选词就会上屏。")

(defvar eim-hint-str "" "提示帮助")
(defvar eim-translating nil "记录是否在转换状态")
(defvar eim-overlay nil "显示当前选择词条的overlay")

(defvar eim-use-tooltip t)
(defvar eim-tooltip-timeout 15)

(defvar eim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'eim-im-key-event)
    map)
  "Keymap")

(defvar eim-local-variable-list
  '(eim-current-str
    eim-guidance-str
    eim-translating
    input-method-function
    inactivate-current-input-method-function
    eim-overlay)
  "A list of buffer local variable")

(dolist (var eim-local-variable-list)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(defsubst eim-delete-region ()
  "Delete the text in the current translation region of E+."
  (if (overlay-start eim-overlay)
      (delete-region (overlay-start eim-overlay)
                     (overlay-end eim-overlay))))

;;;_ , eim-use-package
(defun eim-use-package (package-name &optional word-file active-func)
  (interactive)
  (mapc 'kill-local-variable eim-local-variable-list)
  (mapc 'make-local-variable eim-local-variable-list)

  (eim-install-variable)
  (setq input-method-function 'eim-input-method)
  (setq inactivate-current-input-method-function 'eim-inactivate)
  ;; If we are in minibuffer, turn off the current input method
  ;; before exiting.
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'eim-exit-from-minibuffer))
  (if (functionp (eim-active-function))
      (funcall (eim-active-function))))

(defun eim-inactivate ()
  (interactive)
  (mapc 'kill-local-variable eim-local-variable-list))

(defun eim-terminate-translation ()
  "Terminate the translation of the current key."
  (setq eim-translating nil)
  (eim-delete-region)
  (setq eim-current-choices nil)
  (setq eim-guidance-str "")
  (when eim-use-tooltip
    (x-hide-tip)))

(defun eim-translate (char)
  (if (functionp eim-translate-function)
      (funcall eim-translate-function char)
    (char-to-string char)))

;;;_ , Core function of input method (stole from quail)
(defun eim-exit-from-minibuffer ()
  (inactivate-input-method)
  (if (<= (minibuffer-depth) 1)
      (remove-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer)))

(defun eim-setup-overlays ()
  (let ((pos (point)))
    (if (overlayp eim-overlay)
        (move-overlay eim-overlay pos pos)
      (setq eim-overlay (make-overlay pos pos))
      (if input-method-highlight-flag
          (overlay-put eim-overlay 'face 'eim-string-face)))))

(defun eim-delete-overlays ()
  (if (and (overlayp eim-overlay) (overlay-start eim-overlay))
      (delete-overlay eim-overlay)))

(defun eim-show ()
  (unless enable-multibyte-characters
    (setq eim-current-key nil
          eim-current-str nil)
    (error "Can't input characters in current unibyte buffer"))
  (eim-delete-region)
  (insert eim-current-str)
  (move-overlay eim-overlay (overlay-start eim-overlay) (point))
  ;; Then, show the guidance.
  (when (and (not input-method-use-echo-area)
             (null unread-command-events)
             (null unread-post-input-method-events))
    (if (eq (selected-window) (minibuffer-window))
        ;; Show the guidance in the next line of the currrent
        ;; minibuffer.
        (eim-minibuffer-message
         (format "  [%s]\n%s"
                 current-input-method-title eim-guidance-str))
      ;; Show the guidance in echo area without logging.
      (let ((message-log-max nil))
        (if eim-use-tooltip
            (let ((pos (string-match ": " eim-guidance-str)))
              (if pos
                  (setq eim-guidance-str
                        (concat (substring eim-guidance-str 0 pos)
                                "\n"
                                (make-string (/ (- (string-width eim-guidance-str) pos) 2) (decode-char 'ucs #x2501))
                                "\n"
                                (substring eim-guidance-str (+ pos 2)))))
              (eim-show-tooltip eim-guidance-str))
          (message "%s" eim-guidance-str))))))

(defun eim-make-guidance-frame ()
  "Make a new one-line frame for Quail guidance."
  (let* ((fparam (frame-parameters))
         (top (cdr (assq 'top fparam)))
         (border (cdr (assq 'border-width fparam)))
         (internal-border (cdr (assq 'internal-border-width fparam)))
         (newtop (- top
                    (frame-char-height) (* internal-border 2) (* border 2))))
    (if (< newtop 0)
        (setq newtop (+ top (frame-pixel-height) internal-border border)))
    (make-frame (append '((user-position . t) (height . 1)
                          (minibuffer)
                          (menu-bar-lines . 0) (tool-bar-lines . 0))
                        (cons (cons 'top newtop) fparam)))))

(defun eim-minibuffer-message (string)
  (message nil)
  (let ((point-max (point-max))
        (inhibit-quit t))
    (save-excursion
      (goto-char point-max)
      (insert string))
    (sit-for 1000000)
    (delete-region point-max (point-max))
    (when quit-flag
      (setq quit-flag nil
            unread-command-events '(7)))))

(defun eim-input-method (key)
  (if (or buffer-read-only
          overriding-terminal-local-map
          overriding-local-map)
      (list key)
    ;; (message "call with key: %c" key)
    (eim-setup-overlays)
    (let ((modified-p (buffer-modified-p))
          (buffer-undo-list t)
          (inhibit-modification-hooks t))
      (unwind-protect
          (let ((input-string (eim-start-translation key)))
            ;;   (message "input-string: %s" input-string)
            (setq eim-guidance-str "")
            (when (and (stringp input-string)
                       (> (length input-string) 0))
              (if input-method-exit-on-first-char
                  (list (aref input-string 0))
                (eim-input-string-to-events input-string))))
        (eim-delete-overlays)
        (set-buffer-modified-p modified-p)
        ;; Run this hook only when the current input method doesn't
        ;; require conversion. When conversion is required, the
        ;; conversion function should run this hook at a proper
        ;; timing.
        (run-hooks 'input-method-after-insert-chunk-hook)))))

(defun eim-start-translation (key)
  "Start translation of the typed character KEY by the current Quail package.
Return the input string."
  ;; Check the possibility of translating KEY.
  ;; If KEY is nil, we can anyway start translation.
  (if (or (integerp key) (null key))
      ;; OK, we can start translation.
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (overriding-terminal-local-map (eim-mode-map))
             (generated-events nil)
             (input-method-function nil)
             (modified-p (buffer-modified-p))
             last-command-event last-command this-command)
        (setq eim-current-str ""
              eim-current-key ""
              eim-translating t)
        (if key
            (setq unread-command-events
                  (cons key unread-command-events)))
        (while eim-translating
          (set-buffer-modified-p modified-p)
          (let* ((prompt (if input-method-use-echo-area
                             (format "%s%s %s"
                                     (or input-method-previous-message "")
                                     eim-current-key
                                     eim-guidance-str)))
                 (keyseq (read-key-sequence prompt nil nil t))
                 (cmd (lookup-key (eim-mode-map) keyseq)))
            ;;             (message "key: %s, cmd:%s\nlcmd: %s, lcmdv: %s, tcmd: %s"
            ;;                      key cmd last-command last-command-event this-command)
            (if (if key
                    (commandp cmd)
                  (eq cmd 'eim-self-insert-command))
                (progn
                  ;; (message "keyseq: %s" keyseq)
                  (setq last-command-event (aref keyseq (1- (length keyseq)))
                        last-command this-command
                        this-command cmd)
                  (setq key t)
                  (condition-case err
                      (call-interactively cmd)
                    (error (message "%s" (cdr err)) (beep))))
              ;; KEYSEQ is not defined in the translation keymap.
              ;; Let's return the event(s) to the caller.
              (setq unread-command-events
                    (string-to-list (this-single-command-raw-keys)))
              ;; (message "unread-command-events: %s" unread-command-events)
              (eim-terminate-translation))))
        ;;    (1message "return: %s" eim-current-str)
        eim-current-str)
    ;; Since KEY doesn't start any translation, just return it.
    ;; But translate KEY if necessary.
    (char-to-string key)))

;;; borrow from completion-ui
(defun eim-frame-posn-at-point (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window."
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (let ((x-y (posn-x-y (posn-at-point position window)))
        (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car  edges))
          (+ (cdr x-y) (cadr edges)))))

(defface eim-tooltip-face '((((class color)) :inherit tooltip))
  "face to display items"
  :group 'eim)

(defun eim-show-tooltip (text)
  "Show tooltip text near cursor."
  (let ((pos (eim-frame-posn-at-point))
        (fg (face-attribute 'eim-tooltip-face :foreground nil 'tooltip))
        (bg (face-attribute 'eim-tooltip-face :background nil 'tooltip))
        (params tooltip-frame-parameters)
        ;; seem the top position should add 65 pixel to make
        ;; the text display under the baseline of cursor
        (top-adjust 65)
        (frame-height (frame-pixel-height))
        (frame-width (frame-pixel-width))
        (lines (split-string text "\n"))
        width height left top)
    (setq width (* (frame-char-width) (apply 'max (mapcar 'string-width lines)))
          height (* (frame-char-height) (length lines)))
    (setq left (frame-parameter nil 'left)
          top (frame-parameter nil 'top))
    ;; if the cursor is at near the right frame fringe or at bottom
    ;; of the bottom fringe, move the frame to
    ;; -frame-width or -frame-height from right or bottom
    (if (< (- frame-width (car pos)) width)
        (setq left (+ left (max 0 (- frame-width width))))
      (setq left (+ left (car pos))))
    (if (< (- frame-height (cdr pos)) (+ height top-adjust))
        (setq top (+ top (max 0 (- frame-height height))))
      (setq top (+ top (cdr pos))))
    (setq top (+ top top-adjust))
    (when (stringp fg)
      (setq params (append params `((foreground-color . ,fg)
                                    (border-color . ,fg)))))
    (when (stringp bg)
      (setq params (append params `((background-color . ,bg)))))
    (setq params (append params `((left . ,left) (top . ,top))))
    (x-show-tip (propertize text 'face 'eim-tooltip-face)
                nil params eim-tooltip-timeout)))

