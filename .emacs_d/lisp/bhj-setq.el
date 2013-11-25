(setq twittering-initial-timeline-spec-string `(":home@sina"))
(setq describe-char-unicodedata-file "~/system-config/.emacs_d/UnicodeData.txt")
(setq-default c-basic-offset 4)
(setq bbdb-file "~/src/github/private-config/.bbdb")
(setq auto-mode-alist (append '((".*/kernel.*\\.[ch]$" . linux-c-mode)
                                ("logcat\\.log.*" . fundamental-mode)
                                (".*/Mlog/*" . fundamental-mode)
                                (".*\\.cpp$" . linux-c++-mode)
                                (".*\\.aidl$" . java-mode)
                                (".*\\.mm?$" . objc-mode)
                                ("Kbuild*" . makefile-gmake-mode)
                                (".*/java/.*\\.wiki$" . org-mode)
                                ("makefile*" . makefile-gmake-mode))
                                  auto-mode-alist))
(setq frame-title-format "emacs@%b")
(setq-default transient-mark-mode t)
(setq org2blog/wp-blog-alist
      '(("wordpress"
         :url "http://baohaojun.wordpress.com/xmlrpc.php"
         :username "baohaojun")))
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-remote-path '(tramp-own-remote-path tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
(setq auto-mode-alist (append '(("\\.cs\\'" . poor-mans-csharp-mode))
                              auto-mode-alist))
(setq weblogger-entry-mode-hook
      (list
       (lambda ()
         (make-local-variable 'fill-nobreak-predicate)
         (add-hook 'fill-nobreak-predicate 'markdown-nobreak-p))))

(setq weblogger-pre-struct-hook
      (list
       (lambda ()
         (interactive)
         (message-goto-body)
         (shell-command-on-region
          (point) (point-max) "markdown" nil t nil nil)
         (message-goto-body)
         (save-excursion
           (let ((body (buffer-substring-no-properties (point) (point-max))))
             (find-file "~/markdown.html")
             (kill-region (point-min) (point-max))
             (insert body)
             (save-buffer)
             (kill-buffer)
             (shell-command "of ~/markdown.html" nil nil)
             (yes-or-no-p "Is the preview ok?"))))))

(setq weblogger-post-struct-hook
      (list
       (lambda ()
         (interactive)
         (run-hooks 'weblogger-start-edit-entry-hook)
         (set-buffer-modified-p t))))

(setq weblogger-start-edit-entry-hook
      (list
       (lambda ()
         (interactive)
         (message-goto-body)
         (shell-command-on-region
          (point) (point-max) "unmarkdown" nil t nil nil)
         (let ((paragraph-start (concat paragraph-start "\\|    \\|\t")))
           (message-goto-body)
           (fill-region (point) (point-max)))
         (set-buffer-modified-p nil))))

(setq weblogger-post-setup-headers-hook
      (list
       (lambda ()
         (interactive)
         (add-hook 'fill-nobreak-predicate 'markdown-nobreak-p)
         (set-buffer-modified-p nil))))

(setq waw-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "\C-m" 'waw-ret-key)
        (define-key map [(return)] 'waw-ret-key)
        (define-key map [(meta p)] 'previous-error-no-select)
        (define-key map [(meta n)] 'next-error-no-select)
        map))

(setq java-bt-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "\C-m" 'java-bt-ret-key)
        (define-key map [(return)] 'java-bt-ret-key)
        (define-key map [(meta p)] 'previous-error-no-select)
        (define-key map [(meta n)] 'next-error-no-select)
        map))

(setq org-agenda-files `("~/doc/projects"))

(setq org-log-done 'note)

(setq bbdb-north-american-phone-numbers nil)

(setq bbdb-auto-notes-alist
      (quote (("To"
               ("w3o" . "w3o")
               ("plug" . "plug")
               ("linux" . "linux")
               ("emacs-commit" . "emacs commit")
               ("emacs" . "emacs")
               ("pinoyjug" . "pinoyjug")
               ("digitalfilipino" . "digitalfilipino")
               ("sacha" . "personal mail"))
              ("From"
               ("admu" company "Ateneo de Manila University")
               ("Organization" (".*" company 0 nil))
               ))))

(setq bbdb-auto-notes-ignore
      (quote
       (("Organization" . "^Gatewayed from\\\\|^Source only"))))

(setq bbdb-auto-notes-ignore-all nil)

(setq bbdb-check-zip-codes-p nil)

(setq bbdb-ignore-some-messages-alist
      '(
        ("From" . "susanpan@marvell.com")
        ("From" . "chenli@marvell.com")
        ("From" . "linkedin.com")
        ("From" . "bear.eee168.com")
        ("To" . "linux-kernel@vger.kernel.org\\|public.gmane.org")
        ("Cc" . "linux-kernel@vger.kernel.org\\|public.gmane.org")
        ))

(setq bbdb-notice-hook (quote (bbdb-auto-notes-hook)))

(setq bbdb/news-auto-create-p nil
      bbdb/mail-auto-create-p nil)

(setq bbdb/gnus-update-records-mode '(my-bbdb/gnus-update-records-mode))

(setq org-publish-project-alist
      '(("notes"
         :base-directory "~/notes/"
         :publishing-directory "~/notes_html"
         :section-numbers t
         :table-of-contents t
         :style "<link rel=\"stylesheet\"
                     href=\"../other/mystyle.css\"
                     type=\"text/css\"/>")

        ("org"
         :base-directory "~/doc/projects"
         :publishing-directory "~/public_html"
         :section-numbers nil
         :table-of-contents nil)))



(setq org-src-fontify-natively t)

(setq org-todo-keywords
      '((sequence "TODO(!)" "|" "DONE(@)" "CANCELED(@)" "LATER(!)")))

(setq org-plantuml-jar-path "~/bin/plantuml.jar")

(setq org-babel-results-keyword "results")

(setq org-confirm-babel-evaluate nil)

(setq gnus-posting-styles
      `(
        (".*"
         ("From" "Ask <ask@ask.com>")
         (signature "All the best\n\n Bao Haojun"))
        (".*ali\\..*"
         ("From" ,(format "%s <%s>"
                          (shell-command-to-string "cat ~/.config/about_me/花名")
                          (shell-command-to-string "cat ~/.config/about_me/mail"))))
        (".*gmail.*"
         ("From" "Bao Haojun <baohaojun@gmail.com>"))))

(setq eclim-auto-save t)
(setq ring-bell-function (lambda ()))
