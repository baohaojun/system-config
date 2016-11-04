
(eval-after-load "grep"
  '(progn
     (grep-compute-defaults)
     (load "bhj-grep")))

(eval-after-load 'helm-buffers
  '(defun helm-buffers-sort-transformer (candidates _source)
     candidates))

(eval-after-load 'c-mode '(c-set-style 'k&r))

(eval-after-load 'js '(define-key js-mode-map [(meta .)] 'grep-beatags))

(eval-after-load 'cc-vars '(require 'guess-offset))

(eval-after-load 'rainbow-mode
  '(setq rainbow-hexadecimal-colors-font-lock-keywords
         (append rainbow-hexadecimal-colors-font-lock-keywords
                 '(("#[0-9a-fA-F]\\{2\\}\\([0-9a-fA-F]\\{6\\}\\)<"
                    (1 (rainbow-colorize-hexadecimal-without-sharp)))))))

(eval-after-load 'org
  '(progn
     (defadvice org-make-link-regexps (after bhj/org-link-tweak activate)
       (setq org-plain-link-re (replace-regexp-in-string "\\[^ \t\n()<>\\]" "[^] \t\n()<>]" org-plain-link-re)))
     (org-defkey org-mode-map [?\e (shift return)] 'org-insert-todo-heading)
     (require 'org-jira)
     (require 'bhj-defines)
     (require 'org-crypt)
     (org-crypt-use-before-save-magic)
     (setq org-tags-exclude-from-inheritance (quote ("crypt")))
     (setq org-crypt-key nil)
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
     (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
     (add-hook 'org-mode-hook (lambda ()
                                (visual-line-mode 1)
                                (setq imenu-create-index-function #'ajoke--create-index-function)
                                (define-key org-mode-map [(control c) (v)] 'bhj-todo-from-mail-view-mail)
                                (define-key org-mode-map [(control c) (c) (c)] 'bhj-todo-copy-id)
                                (define-key org-mode-map [(control c) (s)] (lambda () (interactive) (shell-command-to-string "ask-to-sync-org >/dev/null 2>&1&")))
                                (define-key org-agenda-mode-map [(control c) (s)] (lambda () (interactive) (shell-command-to-string "ask-to-sync-org >/dev/null 2>&1&")))
                                (define-key org-mode-map [(control c) (meta j) (d)] 'bhj-jwords-done)
                                (define-key org-mode-map [(control c) (meta j) (D)] 'bhj-jwords-undone)
                                (define-key org-mode-map [(control c) (meta j) (s)] 'bhj-jwords-someday)
                                (org-jira-mode t)))))

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

(eval-after-load 'session
  '(defadvice session-save-session (before session-save-session-remove-helm-historys activate)
     (dolist (helm-history-var (apropos-internal "\\`helm-.*-history\\'" 'boundp))
       (makunbound helm-history-var))))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (make-local-variable
                                   (quote ajoke-symbol-chars))
                                  (setq ajoke-symbol-chars "-A-Za-z0-9_")))

(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))

(eval-after-load 'auto-complete
  '(progn
     (define-key ac-completing-map (kbd "C-n") 'ac-next)
     (define-key ac-completing-map (kbd "C-p") 'ac-previous)
     (require 'ac-helm)
     (global-set-key (kbd "C-.") 'ac-complete-with-helm)
     (define-key ac-complete-mode-map (kbd "C-.") 'ac-complete-with-helm)))

(defun org-html-fontify-code-compout (orig-fun &rest args)
  "Make compilation output htmlized."
  (if (or (string= (cadr args) "compout")
          (string= (cadr args) "grepout"))
      (flet ((generate-new-buffer (name)
                                  (when (string= name " *temp*")
                                    (setq name "temp-ox"))
                                  (get-buffer-create (generate-new-buffer-name name))))
        (message "Working with %s" (cadr args))
        (apply orig-fun args))
    (apply orig-fun args)))

(eval-after-load 'org
  '(org-add-link-type "smb" nil #'org-smb-link-export))

(eval-after-load 'ox-html
  '(advice-add 'org-html-fontify-code :around #'org-html-fontify-code-compout))

(defun bhj-hack-helm-s-return ()
  (interactive)
  (bhj-hack-helm-s-return-helper))

(defun bhj-hack-helm-s-return-helper ()
    (message "hello world: %s" (buffer-substring-no-properties (point-min) (point-max))))

(eval-after-load 'helm
  '(define-key helm-map [(shift return)] 'bhj-hack-helm-s-return))

(eval-after-load 'bbdb-com
  '(defun bbdb-complete-mail (&optional start-pos)
     (bhj-bbdb-complete-mail start-pos)))

(eval-after-load 'elisp-slime-nav
  '(define-key elisp-slime-nav-mode-map (kbd "M-.") 'grep-beatags))

(add-hook 'python-mode-hook
          (lambda ()
            (setq imenu-create-index-function #'ajoke--create-index-function))
          t)

(add-hook 'csharp-mode-hook
          (lambda ()
            (defun csharp-imenu-create-index ()
              (ajoke--create-index-function))
            (setq imenu-create-index-function #'ajoke--create-index-function))
          t)

(add-hook 'scala-mode-hook
          (lambda ()
            (setq imenu-create-index-function #'ajoke--create-index-function)))
