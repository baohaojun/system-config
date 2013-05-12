(eval-after-load 'ox-latex
  '(setq org-latex-default-packages-alist
         (cons '("" "CJK" nil)
               (mapcar (lambda (entry)
                         (when (and (listp entry)
                                    (stringp (car entry))
                                    (stringp (cadr entry))
                                    (string= "" (car entry))
                                    (string= "hyperref" (cadr entry)))
                           (setq entry (cons "CJKbookmarks" (cdr entry))))
                         entry)
                       org-latex-default-packages-alist))))

(eval-after-load 'package
  '(progn
     (add-to-list 'package-archives
                  '("marmalade" . "http://marmalade-repo.org/packages/"))
     (package-initialize)))

(eval-after-load "grep"
  '(progn
     (grep-compute-defaults)
     (load "bhj-grep")))

(eval-after-load "bhj-defines"
  '(progn
     (add-hook 'before-save-hook 'cleanup-buffer-safe)
     (add-hook 'before-save-hook 'confirm-risky-remote-edit)))

(eval-after-load 'c-mode '(c-set-style 'k&r))

(eval-after-load 'js '(define-key js-mode-map [(meta .)] 'grep-gtags))

(eval-after-load 'cc-vars '(require 'guess-offset))

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
     (require 'bbdb-gnus)
     (require 'bbdb)
     (require 'bbdb-hooks)
     (load "bbdb-com" t)
     (add-hook 'gnus-summary-mode-hook
               (lambda ()
                 (define-key gnus-summary-mode-map "v" 'bhj-view-mail-external)))
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
     (add-hook 'douban-song-info-complete-hook
               (lambda ()
                 (shell-command-to-string "douban info >/dev/null 2>&1 &")))))

(eval-after-load 'java-mode
  '(define-key java-mode-map (kbd "M-s d") 'bhj-open-android-doc-on-java-buffer))

(eval-after-load "ediff-init" '(add-hook 'ediff-quit-hook (lambda () (shell-command "find-or-exec emacs"))))

;; (eval-after-load 'yasnippet
;;   '(progn
;;      (message "hello yas/reload-all")))

(add-hook 'emacs-startup-hook (lambda () (call-interactively 'yas-global-mode)))

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
