(register-input-method
 "sdim" "euc-cn" 'sdim-use-package "影舞笔")

(defun sdim-use-package (&rest args)
  (interactive)
  (mapc 'kill-local-variable sdim-local-variable-list)
  (mapc 'make-local-variable sdim-local-variable-list)

  (setq input-method-function 'sdim-input-method)
  (setq inactivate-current-input-method-function 'sdim-inactivate)

  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'sdim-exit-from-minibuffer))
  (run-hooks 'sdim-active-hook))

(defvar eim-overlay nil "显示当前选择词条的 overlay")

(defface eim-string-face '((t (:underline t)))
  "Face to show current string")

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

(defun sdim-input-method (key)
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
