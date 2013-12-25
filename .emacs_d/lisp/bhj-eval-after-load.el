
(eval-after-load "grep"
  '(progn
     (grep-compute-defaults)
     (load "bhj-grep")))

(eval-after-load "bhj-defines"
  '(progn
     (add-hook 'before-save-hook 'cleanup-buffer-safe)
     (add-hook 'before-save-hook 'confirm-risky-remote-edit)))

(eval-after-load 'helm-buffers
  '(defun helm-buffers-sort-transformer (candidates _source)
     candidates))

(eval-after-load 'c-mode '(c-set-style 'k&r))

(eval-after-load 'js '(define-key js-mode-map [(meta .)] 'grep-gtags))

(eval-after-load 'cc-vars '(require 'guess-offset))

(eval-after-load 'rainbow-mode
  '(setq rainbow-hexadecimal-colors-font-lock-keywords
         (append rainbow-hexadecimal-colors-font-lock-keywords
                 '(("#[0-9a-fA-F]\\{2\\}\\([0-9a-fA-F]\\{6\\}\\)<"
                   (1 (rainbow-colorize-hexadecimal-without-sharp)))))))

(eval-after-load 'org-mode
  '(progn
     (require 'org-jira)
     (condition-case nil
         (load "jira-users")
       (error nil))
     (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
     (org-babel-do-load-languages
      (quote org-babel-load-languages)
      (quote ((emacs-lisp . t)
              (dot . t)
              (ditaa . t)
              (R . t)
              (python . t)
              (ruby . t)
              (gnuplot . t)
              (clojure . t)
              (sh . t)
              (ledger . t)
              (org . t)
              (plantuml . t)
              (latex . t))))
     (require 'org-md)
     (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
     (add-hook 'org-mode-hook (lambda () (org-jira-mode t)))))

(eval-after-load "diff-mode"
  '(define-key diff-mode-map (kbd "M-g") (lookup-key global-map (kbd "M-g"))))

(eval-after-load 'gnus
  '(progn
     (require 'message)
     (require 'bbdb-gnus)
     (require 'bbdb)
     (load "bbdb-com" t)
     (add-hook 'gnus-summary-mode-hook
               (lambda ()
                 (let ((keymap (make-keymap)))
                   (define-key keymap "m" 'bhj-view-mail-external)
                   (define-key keymap "a" 'bhj-nnmaildir-search-aliman)
                   (define-key keymap "f" 'bhj-nnmaildir-find-file)
                   (define-key gnus-article-mode-map "v" keymap)
                   (define-key gnus-summary-mode-map "v" keymap))))
     (add-hook 'message-send-hook 'bhj-set-reply)
     (setq gnus-parameters
           (nconc
            ;; Some charsets are just examples!
            '(("\\bcn\\.bbs\\..*" ;; Chinese
               (mm-coding-system-priorities
                '(iso-8859-1 gbk utf-8))))
            gnus-parameters))
     (bbdb-initialize 'gnus 'message)
     (bbdb-insinuate-message)
     (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)))

(eval-after-load 'browse-kill-ring
  '(setq browse-kill-ring-quit-action
         (lambda ()
            (mapc
             (lambda (w)
               (when (eq browse-kill-ring-original-buffer (window-buffer w))
                 (set-window-point w
                  (with-current-buffer browse-kill-ring-original-buffer
                    (point)))))
             (window-list))
            (bury-buffer)
            (unless (= (count-windows) 1)
              (delete-window)))))

(eval-after-load 'douban-music-mode
  '(progn
     (add-hook 'douban-song-before-info-hook #'bhj-douban-start)))

(eval-after-load 'java-mode
  '(define-key java-mode-map (kbd "M-s d") 'bhj-open-android-doc-on-java-buffer))

(eval-after-load "ediff-init" '(add-hook 'ediff-quit-hook (lambda () (shell-command "find-or-exec emacs"))))

(eval-after-load 'yasnippet
  '(progn
    (define-key yas-minor-mode-map [(tab)]     nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-M-i") 'yas-expand)))

(eval-after-load 'bbdb-com
  '(autoload 'bbdb-complete-mail "bhj-defines"))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-s") nil)
     (define-key paredit-mode-map (kbd "M-S") nil)
     (define-key paredit-mode-map (kbd "M-s x") 'paredit-splice-sexp)
     (define-key paredit-mode-map (kbd "M-s X") 'paredit-split-sexp)))

(eval-after-load 'message
  '(progn
     (add-hook 'message-mode-hook 'turn-on-orgtbl)
     (add-hook 'message-mode-hook 'turn-on-orgstruct++)))

(eval-after-load 'paredit-everywhere
  '(progn
     (define-key paredit-everywhere-mode-map (kbd "M-s") nil)
     (define-key paredit-everywhere-mode-map (kbd "M-S") nil)
     (define-key paredit-everywhere-mode-map (kbd "M-s x") 'paredit-splice-sexp)
     (define-key paredit-everywhere-mode-map (kbd "M-s X") 'paredit-split-sexp)))

(eval-after-load 'edebug
  '(progn
     (define-key emacs-lisp-mode-map "\C-x\C-a" (make-sparse-keymap))))

;; (defun eliminate-dup-invalid-buffers ()
;;   (let* ((new-buffer (current-buffer))
;;          (new-file-abs-name (buffer-file-name))
;;          (new-file-rela-name (file-name-nondirectory (buffer-file-name)))
;;          (new-buffer-name (buffer-name))
;;          (buffer-list (buffer-list)))
;;     (mapc
;;      (lambda (abuf)
;;        (unless (eq abuf new-buffer)
;;          (let ((abuf-file-abs-name (buffer-file-name abuf)))
;;            (when (or (and (not abuf-file-abs-name)
;;                           (string= (replace-regexp-in-string "<[0-9]+>$" "" (buffer-name abuf))
;;                                    (replace-regexp-in-string "<[0-9]+>$" "" new-buffer-name)))
;;                      (and
;;                       abuf-file-abs-name
;;                       (string= (file-name-nondirectory abuf-file-abs-name) new-file-rela-name)
;;                       (not (file-exists-p abuf-file-abs-name))))
;;              (with-current-buffer abuf
;;                (kill-buffer abuf))))))
;;      buffer-list)))

;; (add-hook 'find-file-hook 'eliminate-dup-invalid-buffers)
(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 4 tab-width 4)))
