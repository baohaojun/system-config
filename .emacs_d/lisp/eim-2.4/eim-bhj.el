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

;;; Code:

(provide 'eim)
(eval-when-compile
  (require 'cl))
(require 'help-mode)

(defvar eim-version "2.4")
(defvar eim-server-answer "" "anser from server")


(defgroup eim nil
  "eim: emacs input method"
  :group 'leim)
(defvar eim-page-length 7 "每页显示的词条数目")

(defface eim-string-face '((t (:underline t)))
  "Face to show current string"
  :group 'eim)

(defvar eim-comp-str "" "当前选择的词条")
(defvar eim-cands-str "" "所有可选的词条。
比如：\"菹(aie) 伜(wvfh) 伹(wegg) 倅(wywf) 傶(wdht%20wdhy) 卆(vfj) 哫(kkhy) 啐(kyw) 嘁(kdht%20kdhy) 岨(megg)\"
词条与词条之间用空格分开，如果候选词内出现空格的话会用%20代替。")
(defvar eim-commit-str "" "上屏字符串")
(defvar eim-beep? "" "出错吡一声")
(defvar eim-active? "" "在emacs-ime里没有用的变量，只有在notepad等有用")
(defvar eim-cand-index 1 "当前激活的候选词序号，如果按下上屏键的话，对应的候选词就会上屏。")
(defvar eim-hint-str "" "提示帮助")

(defvar eim-translating nil "记录是否在转换状态")
(defvar eim-answer-ready nil "答案是否已经到达")
(defvar eim-overlay nil "显示当前选择词条的overlay")

(defvar eim-use-tooltip t)
(defvar eim-tooltip-timeout 15)

(defvar eim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'eim-im-key-event)
    map)
  "Keymap")

(defvar eim-local-variable-list
  '(eim-translating
    input-method-function
    inactivate-current-input-method-function)
  "A list of buffer local variable")

(defvar eim-ime-connection nil "connection to the ime server, normally localhost:12345")

(dolist (var eim-local-variable-list)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(defsubst eim-delete-region ()
  "Delete the text in the current translation region of E+."
  (if (overlay-start eim-overlay)
      (delete-region (overlay-start eim-overlay)
                     (overlay-end eim-overlay))))

(defun eim-network-filter (proc data)
  (setq eim-server-answer (concat eim-server-answer data))
  (if (string-match "^end:\n" eim-server-answer)
      (progn 
        (setq answer (substring eim-server-answer 0 (match-beginning 0))
              eim-server-answer (substring eim-server-answer 0 (match-end 0)))
        (eim-got-ime-answer answer))))


(defun eim-got-ime-answer (answer)
  (message "%s" answer)
  (let ((answer (split-string answer "\n" t)))
    (setq eim-comp-str "" eim-cands-str "" eim-cand-index "" eim-beep? "" eim-commit-str "" eim-hint-str "" eim-active? "")
    (while answer
      (cond
       ((string-match "^comp: " (car answer))
        (setq eim-comp-str (substring (car answer) (match-end 0))))
       ((string-match "^cands: " (car answer))
        (setq eim-cands-str (substring (car answer) (match-end 0))))
       ((string-match "^commit: " (car answer))
        (setq eim-commit-str (substring (car answer) (match-end 0))))
       ((string-match "^hint: " (car answer))
        (setq eim-hint-str (substring (car answer) (match-end 0))))
       ((string-match "^beep: " (car answer))
        (setq eim-beep? (substring (car answer) (match-end 0))))
       ((string-match "^cand_index: " (car answer))
        (setq eim-cand-index (substring (car answer) (match-end 0))))
       ((string-match "^active: " (car answer))
        (setq eim-active? (substring (car answer) (match-end 0))))
       (t
        (message "unknown answer from ime server: %s" (car answer))))
      (setq answer (cdr answer)))
    (setq eim-answer-ready t)))
    
        
        
    

(defun eim-network-sentinel (proc event)
  '(do nothing))

(defun eim-connect-to-server ()
  (unless eim-ime-connection
    (setq eim-ime-connection 
          (make-network-process :name "ime-server"
                                :host "localhost"
                                :service 12345
                                :buffer "*ime-server*"
                                :filter 'eim-network-filter
                                :sentinel 'eim-network-sentinel))))



(defun eim-use-package (package-name &optional word-file active-func)
  (interactive)
  (mapc 'kill-local-variable eim-local-variable-list)
  (mapc 'make-local-variable eim-local-variable-list)
  (eim-connect-to-server)

  (setq input-method-function 'eim-input-method)
  (setq inactivate-current-input-method-function 'eim-inactivate)
  ;; If we are in minibuffer, turn off the current input method
  ;; before exiting.
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'eim-exit-from-minibuffer)))

(defun eim-inactivate ()
  (interactive)
  (mapc 'kill-local-variable eim-local-variable-list))

(defun eim-terminate-translation ()
  "Terminate the translation of the current key."
  (setq eim-translating nil)
  (eim-delete-region)
  (setq eim-current-choices nil)
  (setq eim-hint-str "")
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
      (remove-hook 'minibuffer-exit-hook 'eim-exit-from-minibuffer)))

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
          eim-comp-str nil)
    (error "Can't input characters in current unibyte buffer"))
  (eim-delete-region)
  (when (length eim-commit-str)
    (insert eim-commit-str)
    (move-overlay eim-overlay (point) (point)))

  (when (length eim-comp-str)
    (insert eim-comp-str)
    (move-overlay eim-overlay (overlay-start eim-overlay) (point))))


  ;; ;; Then, show the guidance.
  ;; (when (and (not input-method-use-echo-area)
  ;;            (null unread-command-events)
  ;;            (null unread-post-input-method-events))
  ;;   (if (eq (selected-window) (minibuffer-window))
  ;;       ;; Show the guidance in the next line of the currrent
  ;;       ;; minibuffer.
  ;;       (eim-minibuffer-message
  ;;        (format "  [%s]\n%s"
  ;;                current-input-method-title eim-guidance-str))
  ;;     ;; Show the guidance in echo area without logging.
  ;;     (let ((message-log-max nil))
  ;;       (if eim-use-tooltip
  ;;           (let ((pos (string-match ": " eim-guidance-str)))
  ;;             (if pos
  ;;                 (setq eim-guidance-str
  ;;                       (concat (substring eim-guidance-str 0 pos)
  ;;                               "\n"
  ;;                               (make-string (/ (- (string-width eim-guidance-str) pos) 2) (decode-char 'ucs #x2501))
  ;;                               "\n"
  ;;                               (substring eim-guidance-str (+ pos 2)))))
  ;;             (eim-show-tooltip eim-guidance-str))
  ;;         (message "%s" eim-guidance-str))))))

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
          (eim-start-translation key)
        (eim-delete-overlays)
        (set-buffer-modified-p modified-p)
        ;; Run this hook only when the current input method doesn't
        ;; require conversion. When conversion is required, the
        ;; conversion function should run this hook at a proper
        ;; timing.
        (run-hooks 'input-method-after-insert-chunk-hook)))))

(defun eim-key-modifier (event)
  (let ((mod (event-modifiers event))
        (mod-str ""))
    (while mod
      (case (car mod)
        ('control
         (setq mod-str (format "%s C" mod-str)))
        ('shift
         (setq mod-str (format "%s S" mod-str)))
        (t
         (setq mod-str (format "%s A" mod-str))))
      (setq mod (cdr mod)))
    mod-str))

(defun eim-key-base (event)
  (let ((base (event-basic-type event))
        (base-str ""))
    (cond
     ((symbolp base)
      (symbol-name base))
     ((equal base 32)
      "space")
     ((numberp base)
      (string base))
     (t
      "unknown"))))      

(defun eim-start-translation (key)
  "Start translation of the typed character KEY by the current Quail package.
Return the input string."
  ;; Check the possibility of translating KEY.
  ;; If KEY is nil, we can anyway start translation.
  (if (or (integerp key) (null key))
      ;; OK, we can start translation.
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (overriding-terminal-local-map eim-mode-map)
             (generated-events nil)
             (input-method-function nil)
             (modified-p (buffer-modified-p))
             last-command-event last-command this-command)
        (setq eim-translating t)

        (while eim-translating
          (set-buffer-modified-p modified-p)
          (let* ((prompt (if input-method-use-echo-area
                             (format "%s"
                                     (or input-method-previous-message ""))))
                 (keyseq (if key (prog1 (vector key) (setq key nil))
                           (read-key-sequence-vector prompt)))
                 (keyed-str (format "keyed %s %s\n" 
                                    (eim-key-modifier (aref keyseq 0))
                                    (eim-key-base (aref keyseq 0)))))
            (message "%s" eim-server-answer)
            (setq eim-answer-ready nil eim-server-answer "")
            (process-send-string eim-ime-connection keyed-str)
            (sit-for 10)
                        
            (if (not (length eim-comp-str))
                (setq eim-translating nil)))))))

(register-input-method
 "sdim" "euc-cn" 'eim-use-package "影舞笔")

(setq default-input-method "sdim")
(toggle-input-method)
