;;; -*- coding: utf-8 -*-
;;; sdim.el --- Shadow Dance Input method

;; Copyright 2010 Bao Haojun
;;
;; Author: baohaojun@gmail.com

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

(provide 'sdim)
(defvar sdim-server-answer "" "anser from server")
(defvar sdim-ime-debug nil "debug mode, transaction is saved in buffer")


(defface sdim-string-face '((t (:underline t)))
  "Face to show current string")

(defvar sdim-comp-str "" "当前选择的词条")
(defvar sdim-cands-str "" "所有可选的词条。
比如：\"菹(aie) 伜(wvfh) 伹(wegg) 倅(wywf) 傶(wdht%20wdhy) 卆(vfj) 哫(kkhy) 啐(kyw) 嘁(kdht%20kdhy) 岨(megg)\"
词条与词条之间用空格分开，如果候选词内出现空格的话会用%20代替。")
(defvar sdim-commit-str "" "上屏字符串")
(defvar sdim-beep? "" "出错吡一声")
(defvar sdim-active? "" "在emacs-ime里没有用的变量，只有在notepad等有用")
(defvar sdim-cand-index 1 "当前激活的候选词序号，如果按下上屏键的话，对应的候选词就会上屏。")
(defvar sdim-hint-str "" "提示帮助")

(defvar sdim-translating nil "记录是否在转换状态")
(defvar sdim-answer-ready nil "答案是否已经到达")
(defvar sdim-overlay nil "显示当前选择词条的overlay")

(defun sdim-im-key-event ()
  (interactive))
(defvar sdim-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'sdim-im-key-event)
    map)
  "Keymap")

(defvar sdim-local-variable-list
  '(sdim-translating
    input-method-function
    inactivate-current-input-method-function)
  "A list of buffer local variable for the SDIM input method")

(defvar sdim-ime-connection nil "connection to the ime server, normally localhost:31415")

(dolist (var sdim-local-variable-list)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

(defsubst sdim-delete-region ()
  "Delete the text in the current translation region of E+."
  (if (overlay-start sdim-overlay)
      (let ((buffer-undo-list t))
        (delete-region (overlay-start sdim-overlay)
                       (overlay-end sdim-overlay)))))

(defun sdim-network-filter (proc data)
  (when sdim-ime-debug
    (with-current-buffer (process-buffer proc)
      (insert data)))
  (setq sdim-server-answer (concat sdim-server-answer data))
  (if (string-match "^end:\n" sdim-server-answer)
      (progn
        (setq answer (substring sdim-server-answer 0 (match-beginning 0))
              sdim-server-answer (substring sdim-server-answer 0 (match-end 0)))
        (sdim-got-ime-answer answer))))


(defun sdim-minor-mode-got-key ()
  (interactive)
  (sdim-input-method last-command-event))

(defvar sdim-minor-mode-map
  (let* ((tmp-map (make-sparse-keymap))
        (min-key 32)
        (max-key 126)
        (key min-key))
    (while (<= key max-key)
      (define-key tmp-map (make-vector 1 key) 'sdim-minor-mode-got-key)
      (setq key (1+ key)))
    tmp-map))
(defvar sdim-minor-mode-hook nil
  "Hook to run upon entry into sdim minor mode.")

(define-minor-mode sdim-minor-mode
  "Toggle sdim-minor mode.
With no argument, the mode is toggled on/off.
Non-zero argument turns mode on.
Zero argument turns mode off.

Commands:
\\{sdim-minor-entry-mode-map}

Entry to this mode calls the value of `sdim-minor-mode-hook'."

  :init-value nil
  :lighter " sdim"
  :group 'sdim-minor
  :keymap sdim-minor-mode-map

  (if sdim-minor-mode
      (run-mode-hooks 'sdim-minor-mode-hook)))
(defun sdim--formalize-str (s)
  (let ((replaces '(("%0a" . "\n")
                    ("%20" . " ")
                    ("%25" . "%")))
        from to)
    (while replaces
      (setq from (caar replaces)
            to (cdar replaces)
            replaces (cdr replaces)
            s (replace-regexp-in-string from to s)))
    s))
(defun sdim-got-ime-answer (answer)
  (let ((answer (split-string answer "\n" t)))
    (setq sdim-comp-str "" sdim-cands-str "" sdim-cand-index "0" sdim-beep? "" sdim-commit-str "" sdim-hint-str "" sdim-active? "")
    (while answer
      (cond
       ((string-match "^comp: " (car answer))
        (setq sdim-comp-str (substring (car answer) (match-end 0))))
       ((string-match "^cands: " (car answer))
        (setq sdim-cands-str (substring (car answer) (match-end 0))))
       ((string-match "^commit: " (car answer))
        (setq sdim-commit-str (sdim--formalize-str (substring (car answer) (match-end 0)))))
       ((string-match "^hint: " (car answer))
        (setq sdim-hint-str (substring (car answer) (match-end 0))))
       ((string-match "^beep: " (car answer))
        (setq sdim-beep? (substring (car answer) (match-end 0)))
        (beep))
       ((string-match "^cand_index: " (car answer))
        (setq sdim-cand-index (substring (car answer) (match-end 0))))
       ((string-match "^active: " (car answer))
        (setq sdim-active? (substring (car answer) (match-end 0))))
       (t
        (message "unknown answer from ime server: %s" (car answer))))
      (setq answer (cdr answer)))
    (setq sdim-answer-ready t)
    (sdim-show)
    (when (string-equal "" sdim-comp-str)
      (setq sdim-translating nil))))




(defun sdim-network-sentinel (proc event)
  '(do nothing))

(defun sdim-connect-to-server ()
  (unless sdim-ime-connection
    (setq sdim-ime-connection
          (make-network-process :name "ime-server"
                                :host "localhost"
                                :service 31415
                                :buffer "*ime-server*"
                                :filter 'sdim-network-filter
                                :sentinel 'sdim-network-sentinel))))



(defun sdim-use-package (package-name &optional word-file active-func)
  (interactive)
  (mapc 'kill-local-variable sdim-local-variable-list)
  (mapc 'make-local-variable sdim-local-variable-list)
  (sdim-connect-to-server)

  (sdim-minor-mode 1)
  (setq input-method-function 'sdim-input-method)
  (setq inactivate-current-input-method-function 'sdim-inactivate)
  ;; If we are in minibuffer, turn off the current input method
  ;; before exiting.
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'sdim-exit-from-minibuffer)))

(defun sdim-inactivate ()
  (interactive)
  (sdim-minor-mode 0)
  (mapc 'kill-local-variable sdim-local-variable-list))

;;;_ , Core function of input method (stole from quail)
(defun sdim-exit-from-minibuffer ()
  (inactivate-input-method)
  (if (<= (minibuffer-depth) 1)
      (remove-hook 'minibuffer-exit-hook 'sdim-exit-from-minibuffer)))

(defun sdim-setup-overlays ()
  (let ((pos (point)))
    (if (overlayp sdim-overlay)
        (move-overlay sdim-overlay pos pos)
      (setq sdim-overlay (make-overlay pos pos))
      (if input-method-highlight-flag
          (overlay-put sdim-overlay 'face 'sdim-string-face)))))

(defun sdim-delete-overlays ()
  (if (and (overlayp sdim-overlay) (overlay-start sdim-overlay))
      (delete-overlay sdim-overlay)))

(defun sdim-minibuffer-message (string)
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

(defun sdim-show ()
  (unless enable-multibyte-characters
    (setq sdim-current-key nil
          sdim-comp-str nil)
    (error "Can't input characters in current unibyte buffer"))
  (sdim-delete-region)
  (when (not (string-equal "" sdim-commit-str))
    (setq sdim-modified-p t)
    (let ((inhibit-modification-hooks nil)
          (repeat 1))
      (if current-prefix-arg
          (setq repeat (if (numberp current-prefix-arg)
                           current-prefix-arg
                         (car current-prefix-arg))
                current-prefix-arg nil
                prefix-arg nil))

      (while (> repeat 0)
        (if (eq major-mode 'term-mode)
            (term-send-raw-string sdim-commit-str)
          (insert sdim-commit-str))
        (setq repeat (1- repeat)))
      (if auto-fill-function
          (apply normal-auto-fill-function ())))
    (move-overlay sdim-overlay (point) (point)))

  (if (not (string-equal "" sdim-cands-str))
      (let ((cands-list (mapcar #'sdim--formalize-str
                                 (split-string sdim-cands-str " ")))
             (cand-index (% (read sdim-cand-index) 10)))
        (setq sdim-comp-str (substring (nth cand-index cands-list) 0))
        (let ((i 0))
          (setq sdim-cands-str "")
          (mapc (lambda (str)
                  (when (eq i cand-index)
                    (put-text-property 0 (length str) 'face '(:background "green2") str))
                  (put-text-property 0 (length str) 'face '(:height .8) str)
                  (setq i (1+ i)
                        prefix (string (+ ?0 (% i 10)) ?.)
                        sdim-cands-str (concat sdim-cands-str
                                               (if (eq i 0)
                                                   ""
                                                 " ")
                                               prefix
                                               str))) cands-list))))




  (let ((buffer-undo-list t))
    (insert sdim-comp-str)
    (move-overlay sdim-overlay (overlay-start sdim-overlay) (point)))
  (when (and (not input-method-use-echo-area)
             (null unread-command-events)
             (null unread-post-input-method-events))
    (if (eq (selected-window) (minibuffer-window))
        ;; Show the guidance in the next line of the currrent
        ;; minibuffer.
        (sdim-minibuffer-message
         (format "  [%s]\n%s"
                 current-input-method-title sdim-cands-str))
      ;; Show the guidance in echo area without logging.
      (let ((message-log-max nil))
        (message "%s" sdim-cands-str)))))


(defvar sdim-modified-p nil)
(defun sdim-input-method (key)
  (if (or buffer-read-only
          overriding-terminal-local-map
          overriding-local-map)
      (progn
        (message "key not translated: %s" key)
        (list key))
    (sdim-setup-overlays)
    (let ((inhibit-modification-hooks t)
          (unwind-indicator nil))
      (setq sdim-modified-p (buffer-modified-p))
      (unwind-protect
          (progn
            (sdim-start-translation key)
            nil) ; must make sure the sdim-input-method got translated
                 ; events, in this case, we don't have any generated
                 ; events, the buffer is already modified by us using
                 ; insert directly.
        (unless unwind-indicator
          (message "sdim translation failed")
          (unless (process-live-p sdim-ime-connection)
            (setq sdim-ime-connection nil)
            (kill-buffer "*ime-server*")))
        (sdim-delete-overlays)
        (set-buffer-modified-p sdim-modified-p)
        ;; Run this hook only when the current input method doesn't
        ;; require conversion. When conversion is required, the
        ;; conversion function should run this hook at a proper
        ;; timing.
        (run-hooks 'input-method-after-insert-chunk-hook)))))

(defun sdim-key-modifier (event)
  (if (and (numberp event)
           (<= event ?Z)
           (>= event ?A))
      ""
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
      mod-str)))

(defun sdim-key-base (event)
  (if (and (numberp event)
           (<= event ?Z)
           (>= event ?A))
      (string event)
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
        "unknown")))))

(defun sdim-start-translation (key)
  "Start translation of the typed character KEY."
  ;; Check the possibility of translating KEY.
  ;; If KEY is nil, we can anyway start translation.
  (if (or (integerp key) (null key))
      ;; OK, we can start translation.
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (overriding-terminal-local-map sdim-mode-map)
             (input-method-function nil)
             last-command-event last-command this-command)

        (setq sdim-translating t)
        (while sdim-translating

          (let* ((prompt  (if input-method-use-echo-area
                              (format "%s %s"
                                      (or input-method-previous-message "")
                                      sdim-cands-str)))
                 (keyseq (if key
                             (prog1 (vector key) (setq key nil))
                           (read-key-sequence-vector prompt)))
                 (keyed-str (format "keyed %s %s\n"
                                    (sdim-key-modifier (aref keyseq 0))
                                    (sdim-key-base (aref keyseq 0)))))
            (setq sdim-answer-ready nil sdim-server-answer "")
            (process-send-string sdim-ime-connection keyed-str)
            (while (not sdim-answer-ready)
              (accept-process-output sdim-ime-connection nil nil 1))
            (setq unwind-indicator t))))))


;; (add-hook 'isearch-mode-hook (lambda () (when sdim-minor-mode (sdim-minor-mode 0))))
;; (add-hook 'isearch-mode-end-hook (lambda () (when (eq input-method-function 'sdim-input-method)
;;                                           (sdim-minor-mode 1))))
