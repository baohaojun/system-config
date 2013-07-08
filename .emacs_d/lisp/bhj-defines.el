;;;###autoload
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (set-buffer-file-coding-system 'utf-8)
  (unless (string-match "message-mode\\|org-mode\\|text-mode" (symbol-name major-mode))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (and (search-forward-regexp "\t\\|[ \t]$" nil t)
                   bhj-force-cleanup-buffer)
          (untabify (point-min) (point-max))
          (delete-trailing-whitespace))))))

;;;###autoload
(defun fix-latex-cjk ()
  "move the cjk env outmost with the document env"
  (interactive)
  (when (eq major-mode 'latex-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((move-around-lines '("\\AtBeginDvi{\\special{pdf:tounicode UTF8-UCS2}}"
                                 "\\begin{CJK"
                                 "\\CJKtilde")))

        (mapc (lambda (str)
               (when (search-forward str nil t)
                 (goto-char (line-beginning-position))
                 (kill-line 1)
                 (search-backward "\\begin{document}")
                 (goto-char (line-end-position))
                 (forward-char)
                 (yank)))
             (reverse move-around-lines)))
      (let ((move-around-lines '("\\end{CJK")))
        (mapc (lambda (str)
               (when (search-forward str nil t)
                 (goto-char (line-beginning-position))
                 (kill-line 1)
                 (search-forward "\\end{document}")
                 (goto-char (line-beginning-position))
                 (yank)
                 (let ((use-cjk "\\usepackage{CJKutf8}"))
                   (unless (save-excursion
                             (goto-char (point-min))
                             (search-forward use-cjk nil t))
                     (search-backward "\\documentclass")
                     (goto-char (line-end-position))
                     (insert (concat "\n" use-cjk))))))
             (reverse move-around-lines))))))

;;;###autoload
(defun confirm-risky-remote-edit ()
  (let ((filename (buffer-file-name)))
    (when (and (file-remote-p filename) (string-match "/system-config/" filename))
      (yes-or-no-p "Are you sure it's alright to save this remote file when you have a local copy?"))))



;;;###autoload
(defun bhj-c-beginning-of-defun (&optional arg)
  (interactive "^p")
  (progn
    (unless mark-active
      (push-mark))
    (ctags-beginning-of-defun arg)))

;;;###autoload
(defun bhj-c-show-current-func ()
  (interactive)
  (save-excursion
    (bhj-c-beginning-of-defun)
    (message "%s" (current-line-string))))

(defun current-regexp (re &optional func)
  (save-excursion
    (let (start end)
      (while (not (looking-at re))
        (backward-char))
      (while (looking-at re)
        (backward-char))
      (forward-char)
      (setq start (point))
      (search-forward-regexp re)
      (setq end (point))
      (funcall (or func 'buffer-substring-no-properties) start end))))

;;;###autoload
(defun java-resolve (id)
  (interactive
   (list (or (and transient-mark-mode mark-active
                  (/= (point) (mark))
                  (buffer-substring-no-properties (point) (mark)))
             (current-regexp "[.a-z0-9]+"))))
  (shell-command (format "java-get-imports.pl %s -r %s"
                         (shell-quote-argument (buffer-file-name))
                         (shell-quote-argument id))))

;;;###autoload
(defun java-complete-method (id)
  (interactive
   (list (or (and transient-mark-mode mark-active
                  (/= (point) (mark))
                  (buffer-substring-no-properties (point) (mark)))
             (current-regexp "[.a-z0-9_]+"))))

  (let (method (remove ""))
    (save-excursion
      (let* ((resolve (shell-command-to-string (format "java-get-imports.pl %s -r %s|tr -d '\\n'"
                                                       (shell-quote-argument (buffer-file-name))
                                                       (shell-quote-argument id))))
             (comp (split-string resolve "\\."))
             (comp-last (car (last comp)))
             (class (cond
                     ((string= comp-last "")
                      (setq remove ".")
                      (mapconcat 'identity (butlast comp) "."))
                     ((let ((case-fold-search nil))
                              (string-match "^[a-z]" comp-last))
                      (setq remove (concat "." comp-last))
                      (mapconcat 'identity (butlast comp) "."))
                     (t resolve)))
             (hierarchy (shell-command-to-string (format "java-get-hierarchy.pl %s -v|grep '('|perl -npe 's/^\\s+//'|sort -u" class)))
             (methods (split-string hierarchy "\n")))
        (setq method (completing-read "Which method to call? " methods nil t))))
    (goto-char (current-regexp "[.a-z0-9_]+" (lambda (start end) end)))
    (when (not (string-equal remove ""))
      (delete-region (- (point) (length remove)) (point)))
    (insert ".")
    (insert (replace-regexp-in-string ".*\\s \\(.*(.*)\\){" "\\1" method))))

;;;###autoload
(defun bhj-c-end-of-defun (&optional arg)
  (interactive "^p")
  (progn
    (unless mark-active
      (push-mark))
    (ctags-beginning-of-defun (- arg))))

;;;###autoload
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "k&r")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

;;;###autoload
(defun linux-c++-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c++-mode)
  (c-set-style "k&r")
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

;;;###autoload
(defun weekrep ()
  (interactive)
  (call-process "wr" nil t nil "-6"))

;;;###autoload
(defun wiki-local-bhj ()
  (interactive)
  (let
      ((search-string (current-word)))
    (progn
      (setq search-string
            (read-string (format "search local wiki with [%s]: " search-string) nil nil search-string))
      (call-process "bash" nil nil nil "local-wiki.sh" search-string)
      )))

;;;###autoload
(defun bhj-clt-insert-file-name ()
  (interactive)
  (let ((prev-buffer (other-buffer (current-buffer) t)))

    (insert
     (if (buffer-file-name prev-buffer)
         (replace-regexp-in-string ".*/" "" (buffer-file-name prev-buffer))
       (buffer-name prev-buffer)))))

;;;###autoload
(defun bhj-insert-pwdw ()
  (interactive)
  (insert "'")
  (call-process "cygpath" nil t nil "-alw" default-directory)
  (backward-delete-char 1)
  (insert "'"))

;;;###autoload
(defun bhj-insert-pwdu ()
  (interactive)
  (insert "'")
  (insert
   (replace-regexp-in-string
    "^/.?scp:.*?@.*?:" ""
    (expand-file-name default-directory)))
  (insert "'"))


(defun nodup-ring-insert (ring obj)
  (unless (and (not (ring-empty-p ring))
               (equal (ring-ref ring 0) obj))
    (ring-insert ring obj)))



(defun set-gtags-start-file ()
  (let ((file (my-buffer-file-name (current-buffer))))
    (if (file-remote-p file)
        (let ((process-environment tramp-remote-process-environment))
          (setenv "GTAGS_START_FILE" (file-remote-p file 'localname))
          (setq tramp-remote-process-environment process-environment))
      (setenv "GTAGS_START_FILE" file))))

;;;###autoload
(defun c-get-includes ()
  (interactive)
  ;; when use call this function, 1) assume we will have some headers
  ;; to include, 2) assume we need insert them at a special position
  ;; marked with /**** start of bhj auto includes ****/ and /**** end
  ;; of bhj auto includes ****/.

  (let (start-include-mark-line
        end-include-mark-line
        mark-line-found
        (start-mark "/**** start of bhj auto includes ****/")
        (end-mark "/**** end of bhj auto includes ****/")
        (includes (split-string
                   (shell-command-to-string
                    (concat "c-get-includes "
                            (shell-quote-argument (buffer-file-name))))
                   "\n" t)))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward start-mark nil t)
          (setq start-include-mark-line (line-number-at-pos)
                end-include-mark-line (save-excursion
                                        (search-forward end-mark)
                                        (line-number-at-pos)))
        (goto-char (point-min))
        (insert start-mark "\n" end-mark "\n\n")
        (setq start-include-mark-line 1
              end-include-mark-line 2))
      (goto-line (1+ start-include-mark-line))
      (goto-char (point-at-bol))
      (mapc (lambda (head)
              (insert head "\n"))
            includes)
      (setq end-include-mark-line (save-excursion
                                    (search-forward end-mark)
                                    (line-number-at-pos)))
      (shell-command-on-region
       (save-excursion
         (goto-line (1+ start-include-mark-line))
         (point-at-bol))
       (save-excursion
         (goto-line end-include-mark-line)
         (point-at-bol))
       "sort -u"
       nil
       t))))

;;;###autoload
(defun bhj-indent-region-as-prev-line()
  (interactive)
  (when mark-active
    (let* ((begin (region-beginning))
          (end (region-end))
          (last-line-indent
           (save-excursion
             (goto-char begin)
             (previous-line)
             (back-to-indentation)
             (current-column))))
      (replace-regexp "^" (make-string last-line-indent ? ) nil begin end))))

;;;###autoload
(defun java-get-hierarchy ()
  (interactive)
  (set-gtags-start-file)
  (let ((class-name (get-the-tag-around-me 'class-name-from-tag-line 0))
        (method-name (or (and transient-mark-mode mark-active
                              (/= (point) (mark))
                              (buffer-substring-no-properties (point) (mark)))
                         (get-the-tag-around-me 'tag-name-from-tag-line 0)))
        (compilation-buffer-name-function (lambda (_ign) "*java-get-hierarchy*")))
    (compile (format "java-get-hierarchy.pl %s %s"
                     class-name
                     (if current-prefix-arg
                         "-v"
                       (concat "-m " method-name))))))

;;;###autoload
(defun java-get-override ()
  (interactive)
  (set-gtags-start-file)
  (let (method)
    (save-excursion
      (let* ((class-name (get-the-tag-around-me 'class-name-from-tag-line 0))
             (hierarchy (shell-command-to-string (format "java-get-hierarchy.pl %s -v|grep '('|perl -npe 's/^\\s+//'|sort -u" class-name)))
             (methods (split-string hierarchy "\n")))
        (setq method (completing-read "Which method to override? " methods nil t))))
    (insert "@Override\n")
    (insert (replace-regexp-in-string  "\\(,\\|)\\)" "\\1 " method))))



;;;###autoload
(defun bhj-occur ()
  (interactive)
  (with-syntax-table (let ((new-table (make-syntax-table (syntax-table))))
                       (modify-syntax-entry ?_ "w" new-table)
                       new-table)
    (let
        ((regexp (or bhj-occur-regexp
                     (if mark-active
                         (buffer-substring-no-properties (region-beginning)
                                                         (region-end))
                       (current-word)))))
      (progn
        (nodup-ring-insert cscope-marker-ring (point-marker))
        (when (or (equal regexp "")
                  (not regexp))
          (setq regexp
                (buffer-substring-no-properties
                 (save-excursion
                   (back-to-indentation)
                   (point))
                 (line-end-position))))

        (unless bhj-occur-regexp
          (setq regexp (concat
                        (if (string-match "^\\w" regexp)
                            "\\b"
                          "")
                        (replace-regexp-in-string "\\([][^$*?\\\\.+]\\)" "\\\\\\1" regexp)
                        (if (string-match "\\w$" regexp)
                            "\\b"
                          ""))))

        (setq regexp
              (read-shell-command "List lines matching regexp: " regexp))
        (if (eq major-mode 'antlr-mode)
            (let ((occur-excluded-properties t))
              (occur regexp))
          (occur regexp))))))

;;;###autoload
(defun bhj-occur-make-errors ()
  (interactive)
  (let ((bhj-occur-regexp "\\*\\*\\*.*stop\\|syntax error\\|invalid argument\\|no such \\|circular.*dropped\\|no rule to\\|failed\\|[0-9]elapsed \\|cannot find symbol\\|error [0-9]\\|because of errors\\|[0-9] error\\b\\|error:\\|undefined reference to"))
    (call-interactively 'bhj-occur)))

;;;###autoload
(defun bhj-occur-merge-conflicts ()
  (interactive)
  (let ((bhj-occur-regexp "<<<<<<\\|>>>>>>\\|======"))
    (call-interactively 'bhj-occur)))

;;;###autoload
(defun bhj-isearch-from-bod (&optional col-indent)
  (interactive "p")
  (with-syntax-table (let ((new-table (make-syntax-table (syntax-table))))
                       (modify-syntax-entry ?_ "w" new-table)
                       new-table)
    (let ((word (current-word)))
      (nodup-ring-insert cscope-marker-ring (point-marker))
      (bhj-c-beginning-of-defun)

      (unless (string-equal (car regexp-search-ring) (concat "\\b" word "\\b"))
        (add-to-history
         'regexp-search-ring
         (concat "\\b" word "\\b")
         regexp-search-ring-max))
      (let ((not-match t))
        (while not-match
          (search-forward-regexp (concat "\\b" word "\\b"))
          (when (string-equal word (current-word))
            (setq not-match nil)))))))

;;;###autoload
(defun bhj-w3m-scroll-up-or-next-url ()
  (interactive)
  (if (pos-visible-in-window-p (point-max))
      (save-excursion
        (end-of-buffer)
        (search-backward-regexp "下一\\|下章\\|后一\\|还看了")
        (if (w3m-url-valid (w3m-anchor))
            (call-interactively 'w3m-view-this-url)
          (call-interactively 'w3m-next-anchor)
          (call-interactively 'w3m-view-this-url)))
    (call-interactively 'w3m-scroll-up-or-next-url)))

;;;###autoload
(defun bhj-w3m-scroll-down-or-previous-url ()
  (interactive)
  (if (pos-visible-in-window-p (point-min))
      (save-excursion
        (end-of-buffer)
         (search-backward-regexp "上一\\|上章")
         (call-interactively 'w3m-view-this-url))
    (call-interactively 'w3m-scroll-down-or-previous-url)))

;;;###autoload
(defun bhj-mimedown ()
  (interactive)
  (if (not mark-active)
      (message "mark not active\n")
    (save-excursion
      (let* ((start (min (point) (mark)))
             (end (max (point) (mark)))
             (orig-txt (buffer-substring-no-properties start end)))
        (shell-command-on-region start end "markdown" nil t)
        (insert "<#multipart type=alternative>\n")
        (insert orig-txt)
        (insert "<#part type=text/html>\n<html>\n<head>\n<title> HTML version of email</title>\n</head>\n<body>")
        (exchange-point-and-mark)
        (insert "\n</body>\n</html>\n<#/multipart>\n")))))

(defvar bhj-gmail-host "smtp.gmail.com")
;;;###autoload
(defun bhj-set-reply ()
  (interactive)
  (save-excursion
    (let ((receivers
           (concat
            (save-restriction (message-narrow-to-headers)
                              (message-fetch-field "to"))
            ", "
            (save-restriction (message-narrow-to-headers)
                              (message-fetch-field "cc"))
            ", "
            (save-restriction (message-narrow-to-headers)
                              (message-fetch-field "bcc"))))
          (all-marvell t)
          (start-pos 0))

      (when (save-excursion
              (save-restriction
                (message-narrow-to-headers)
                (message-fetch-field "Newsgroups")))
        (setq all-marvell nil))

      (while (and all-marvell (string-match "@" receivers start-pos))
        (setq start-pos (match-end 0))
        (unless (equal (string-match
                    "@marvell.com"
                    receivers
                    (1- start-pos))
                   (1- start-pos))
          (setq all-marvell nil)))

      (when all-marvell
        (save-excursion
          (message-goto-from)
          (message-beginning-of-line)
          (kill-line)
          (insert "\"Bao Haojun\" <hjbao@marvell.com>")))

      (save-excursion
        (message-goto-from)
        (message-beginning-of-line)
        (when (save-excursion
                (search-forward-regexp "@ask.com" (line-end-position) t))
          (kill-line)
          (insert (completing-read "use account? " '("hjbao@marvell.com" "baohaojun@gmail.com") nil t "baohaojun@gmail.com")))
        (message-goto-from)
        (message-beginning-of-line)
        (cond ((save-excursion (search-forward-regexp "@marvell.com" (line-end-position) t))
               (kill-line)
               (insert "\"Bao Haojun\" <hjbao@marvell.com>")
               (setq smtpmail-auth-credentials
                     '(("localhost"
                        2025
                        "hjbao@marvell.com"
                        nil))
                     message-send-mail-function 'smtpmail-send-it
                     smtpmail-stream-type nil
                     user-mail-address "hjbao@marvell.com"
                     smtpmail-default-smtp-server "localhost"
                     smtpmail-smtp-server "localhost"
                     smtpmail-smtp-service 2025))

              ((save-excursion (search-forward-regexp "@gmail.com" (line-end-position) t))
               (kill-line)
               (insert "\"Bao Haojun\" <baohaojun@gmail.com>")
               (setq smtpmail-auth-credentials
                     '((bhj-gmail-host
                        465
                        "baohaojun@gmail.com"
                        nil))
                     message-send-mail-function 'smtpmail-send-it
                     smtpmail-stream-type 'ssl
                     user-mail-address "baohaojun@gmail.com"
                     smtpmail-default-smtp-server bhj-gmail-host
                     smtpmail-smtp-server bhj-gmail-host
                     smtpmail-smtp-service 465))
              (t
               (error "don't know send as whom")))))))

;;;###autoload
(defun bhj-jdk-help (jdk-word)
  "start jdk help"
  (interactive
   (progn
     (let ((default (current-word)))
       (list (read-string "Search JDK help on: "
                          default
                          'jdk-help-history)))))

  ;; Setting process-setup-function makes exit-message-function work
  (call-process "/bin/bash" nil nil nil "jdkhelp.sh" jdk-word)
  (w3m-goto-url "file:///d/knowledge/jdk-6u18-docs/1.html"))

(defun devenv-cmd (&rest args)
  "Send a command-line to a running VS.NET process.  'devenv' comes from devenv.exe"
  (apply 'call-process "DevEnvCommand" nil nil nil args))

;;;###autoload
(defun switch-to-devenv ()
  "Jump to VS.NET, at the same file & line as in emacs"
  (interactive)
  (save-some-buffers)
  (let ((val1
           (devenv-cmd "File.OpenFile" (buffer-file-name (current-buffer))))
        (val2
           (devenv-cmd "Edit.GoTo" (int-to-string (line-number-at-pos)))))
    (cond ((zerop (+ val1 val2))
              ;(iconify-frame)  ;; what I really want here is to raise the VS.NET window
                 t)
            ((or (= val1 1) (= val2 1))
                (error "command failed"))  ;; hm, how do I get the output of the command?
              (t
                  (error "couldn't run DevEnvCommand")))))

;;;###autoload
(defun devenv-toggle-breakpoint ()
  "Toggle a breakpoint at the current line"
  (interactive)
  (switch-to-devenv)
  (devenv-cmd "Debug.ToggleBreakpoint"))

;;;###autoload
(defun devenv-debug ()
  "Run the debugger in VS.NET"
  (interactive)
  (devenv-cmd "Debug.Start"))

(defun poor-mans-csharp-mode ()
  (csharp-mode)
  (setq mode-name "C#")
  (set-variable 'tab-width 8)
  (set-variable 'indent-tabs-mode t)
  (set-variable 'c-basic-offset 8)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'case-label 0)
)

;;;###autoload
(defun random-theme()
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme (let ((theme (nth (random (length (custom-available-themes))) (custom-available-themes))))
                (message "loaded theme: %s" theme)
                theme)))

;;;###autoload
(defun try-all-themes()
  (interactive)
  (dolist (theme (custom-available-themes))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (message "will enable %s" theme)
    (load-theme theme)
    (recursive-edit)))

;;;###autoload
(defun try-all-color-themes()
  (interactive)
  (dolist (theme color-themes)
    (recursive-edit)
    (message "will use %s" (car theme)
    (funcall (car theme)))))

(defun markdown-nobreak-p ()
  "Returns nil if it is ok for fill-paragraph to insert a line
  break at point"
  ;; are we inside in square brackets
  (or (looking-back "\\[[^]]*")
      (save-excursion
        (beginning-of-line)
        (looking-at "    \\|\t"))))

(defun cscope-search-directory-hierarchy (directory)
  "Look for a cscope database in the directory hierarchy.
Starting from DIRECTORY, look upwards for a cscope database."
  (let* ((saved-directory directory)
         (remote-prefix (or (file-remote-p directory) ""))
         (directory (or (file-remote-p directory 'localname) directory))
         this-directory database-dir)




    (catch 'done
      (if (file-regular-p saved-directory)
          (throw 'done saved-directory))
      (setq directory (concat (getenv "HOME") "/.cache/for-code-reading/" (cscope-canonicalize-directory directory))
            this-directory directory)
      (while this-directory
        (when (or (file-exists-p (concat remote-prefix this-directory cscope-database-file))
                  (file-exists-p (concat remote-prefix this-directory cscope-index-file)))
          (progn
            (setq database-dir (substring
                                this-directory
                                (length
                                 (concat (getenv "HOME") "/.cache/for-code-reading/"))))
            (throw 'done (concat "" database-dir))))
        (when (string-match "^\\(/\\|[A-Za-z]:[\\/]\\)$" this-directory)
            (throw 'done (concat "" (expand-file-name "~/.gtags-dir/"))))
        (setq this-directory (file-name-as-directory
                              (file-name-directory
                               (directory-file-name this-directory))))
        ))
    ))

;;;###autoload
(defun cscope-pop-mark ()
  "Pop back to where cscope was last invoked."
  (interactive)

  ;; This function is based on pop-tag-mark, which can be found in
  ;; lisp/progmodes/etags.el.

  (if (ring-empty-p cscope-marker-ring)
      (error "There are no marked buffers in the cscope-marker-ring yet"))
  (let* ( (marker (ring-remove cscope-marker-ring 0))
          (old-buffer (current-buffer))
          (marker-buffer (marker-buffer marker))
          marker-window
          (marker-point (marker-position marker))
          (cscope-buffer (get-buffer cscope-output-buffer-name)) )
    (when (and (not (ring-empty-p cscope-marker-ring))
               (equal marker (ring-ref cscope-marker-ring 0)))
      (ring-remove cscope-marker-ring 0))
    (nodup-ring-insert cscope-marker-ring-poped (point-marker))

    ;; After the following both cscope-marker-ring and cscope-marker will be
    ;; in the state they were immediately after the last search.  This way if
    ;; the user now makes a selection in the previously generated *cscope*
    ;; buffer things will behave the same way as if that selection had been
    ;; made immediately after the last search.
    (setq cscope-marker marker)

    (if marker-buffer
        (if (eq old-buffer cscope-buffer)
            (progn ;; In the *cscope* buffer.
              (set-buffer marker-buffer)
              (setq marker-window (display-buffer marker-buffer))
              (set-window-point marker-window marker-point)
              (select-window marker-window))
          (switch-to-buffer marker-buffer))
      (error "The marked buffer has been deleted"))
    (goto-char marker-point)
    (set-buffer old-buffer)))

;;;###autoload
(defun cscope-pop-mark-back ()
  "Pop back to where cscope was last invoked."
  (interactive)

  ;; This function is based on pop-tag-mark, which can be found in
  ;; lisp/progmodes/etags.el.

  (if (ring-empty-p cscope-marker-ring-poped)
      (error "There are no marked buffers in the cscope-marker-ring-poped yet"))
  (let* ( (marker (ring-remove cscope-marker-ring-poped 0))
          (old-buffer (current-buffer))
          (marker-buffer (marker-buffer marker))
          marker-window
          (marker-point (marker-position marker))
          (cscope-buffer (get-buffer cscope-output-buffer-name)) )
    (nodup-ring-insert cscope-marker-ring (point-marker))

    ;; After the following both cscope-marker-ring-poped and cscope-marker will be
    ;; in the state they were immediately after the last search.  This way if
    ;; the user now makes a selection in the previously generated *cscope*
    ;; buffer things will behave the same way as if that selection had been
    ;; made immediately after the last search.
    (setq cscope-marker marker)

    (if marker-buffer
        (if (eq old-buffer cscope-buffer)
            (progn ;; In the *cscope* buffer.
              (set-buffer marker-buffer)
              (setq marker-window (display-buffer marker-buffer))
              (set-window-point marker-window marker-point)
              (select-window marker-window))
          (switch-to-buffer marker-buffer))
      (error "The marked buffer has been deleted"))
    (goto-char marker-point)
    (set-buffer old-buffer)))

;;;###autoload
(defun where-are-we ()
  (interactive)
  (save-excursion
    (end-of-line)
    (shell-command-on-region
     1 (point)
     (concat "where-are-we "
             (shell-quote-argument (or (buffer-file-name) (buffer-name)))
             (format " %s" tab-width))))
  (pop-to-buffer "*Shell Command Output*")
  (end-of-buffer)
  (insert "\n")
  (beginning-of-buffer)
  (forward-line)
  (waw-mode))

;;;###autoload
(defun tag-this-file (&optional output-buf)
  (interactive)
  (save-excursion
    (save-window-excursion
      (save-restriction
        (widen)
        (shell-command-on-region
         (point-min)
         (point-max)
         (let ((mode-name-minus-mode
                (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
           (concat "ctags-stdin --language-force="
                   (shell-quote-argument
                    (or (cdr (assoc mode-name-minus-mode emacs-mode-ctags-lang-map))
                        mode-name-minus-mode))
                   " -xu "
                   (cdr (assoc mode-name-minus-mode emacs-mode-ctags-tag-filter))))
         output-buf)))))


(defun code-line-number-from-tag-line (line)
  (goto-line line)
  (let ((subs (split-string (current-line-string))))
    (string-to-number
     (if (string-equal (car subs) "operator")
         (cadddr subs) ;operator +=      function    183 /home...
       (caddr subs))))) ;region_iterator  struct      189 /home...

(defun code-def-from-tag-line (line)
  (goto-line line)
  (let ((subs (split-string (current-line-string))))
    (car subs)))

(defun code-indentation-from-tag-line (line)
  (let ((code-line (code-line-number-from-tag-line line)))
    (with-current-buffer old-buffer
      (save-excursion
        (goto-line code-line)
        (back-to-indentation)
        (current-column)))))

;;;###autoload
(defun ctags-get-fully-qualified-name ()
  (interactive)
  (save-excursion
    (let (deactivate-mark) ;;see the help of save-excursion
      (tag-this-file (get-buffer-create "*ctags-beginning-of-defun*"))
      (let ((old-buffer (current-buffer))
            (old-code-line (line-number-at-pos))
            (last-code-line (line-number-at-pos (buffer-end 1)))
            (last-def-line 1)
            (fully-qualified-name "")
            current-code-line-indent)
        (with-current-buffer "*ctags-beginning-of-defun*"
          (goto-char (point-max))
          (insert ( concat "hello function "
                           (number-to-string last-code-line)
                           "hello world"))
          (while (< old-code-line (code-line-number-from-tag-line (line-number-at-pos)))
            (previous-line))
          (setq fully-qualified-name (code-def-from-tag-line (line-number-at-pos))
                current-code-line-indent (code-indentation-from-tag-line (line-number-at-pos)))
          (while (and (> current-code-line-indent 0)
                      (> (line-number-at-pos) 1))
            (previous-line)
            (if (< (code-indentation-from-tag-line (line-number-at-pos)) current-code-line-indent)
                (setq fully-qualified-name (concat (code-def-from-tag-line (line-number-at-pos))
                                                   "."
                                                   fully-qualified-name)
                      current-code-line-indent (code-indentation-from-tag-line (line-number-at-pos)))))
          (message "%s" fully-qualified-name)
          (with-temp-buffer
            (insert-string fully-qualified-name)
            (copy-region-as-kill (point-min) (point-max))))))))

;;;###autoload
(defun ctags-beginning-of-defun (&optional arg)
  (interactive "^p")
  (goto-line
   (get-the-tag-around-me 'code-line-number-from-tag-line arg)))

(defun tag-name-from-tag-line (line)
  (goto-line line)
  (car (split-string (current-line-string))))

(defun completing-read-one? (prompt collection &rest args)
  (if (= (length (delete-dups collection)) 1)
      (car collection)
    (apply 'completing-read prompt collection args)))

(defun class-name-from-tag-line (line)
  (goto-line line)
  (let ((limit (line-end-position))
        classes)
    (goto-char (point-min))
    (while (search-forward-regexp "class\\|interface" limit t)
      (let* ((line (current-line-string))
             (fields (split-string line))
             (name (car fields))
             (type (cadr fields)))
        (cond
         ((or (string-equal type "class")
              (string-equal type "interface"))
          (setq classes (cons line classes))))))
    (car (split-string (completing-read-one? "Which class/interface to hierarchy? " (delete-dups (nreverse classes)) nil t)))))

;;;###autoload
(defun android-get-help ()
  (interactive)
  (shell-command (format "%s %s" "android-get-help" (my-buffer-file-name-local))))

;;;###autoload
(defun get-the-tag-around-me (get-attr-func &optional arg)
  (interactive)
  "GET-ATTR-FUNC is a function to specify what attribute of the tag to return,
for e.g., the line number the tag is on, or the name of the tag.

ARG means found the (ARG - 1)th tag to find."
  (save-excursion
     (let (deactivate-mark) ;;see the help of save-excursion
       (tag-this-file (get-buffer-create "*ctags-beginning-of-defun*"))
       (let ((old-buffer (current-buffer))
             (old-code-line (line-number-at-pos))
             (last-code-line (line-number-at-pos (buffer-end 1)))
             (last-def-line 1))
         (with-current-buffer "*ctags-beginning-of-defun*"
           (goto-char (point-max))
           (insert (concat "hello function "
                           (number-to-string last-code-line)
                           " hello world"))
           (let* ((min 1)
                  (max (line-number-at-pos (buffer-end 1)))
                  (mid (/ (+ min max) 2))
                  (mid-code-line (code-line-number-from-tag-line mid))
                  (mid+1-codeline (code-line-number-from-tag-line (1+ mid))))
             (while (and
                     (not (and
                           (< mid-code-line old-code-line)
                           (>= mid+1-codeline old-code-line)))
                     (< min max))
               (if (>= mid-code-line old-code-line)
                   (setq max (1- mid))
                 (setq min (1+ mid)))
               (setq mid (/ (+ min max) 2)
                     mid-code-line (code-line-number-from-tag-line mid)
                     mid+1-codeline (code-line-number-from-tag-line (1+ mid))))
             (funcall get-attr-func (- mid -1
                                       (if (and (numberp arg)
                                                (= arg 0)
                                                (not (= (code-line-number-from-tag-line (1+ mid)) old-code-line)))
                                           1
                                         (or arg 1))))))))))

;;;###autoload
(defun visit-code-reading (&optional arg)
  (interactive "p")
  (let ((from-waw nil))
    (when (equal (buffer-name (current-buffer))
               "*Shell Command Output*")
      (setq from-waw t))

    (if (= arg 1)
        (find-file code-reading-file)
      (call-interactively 'find-file)
      (setq code-reading-file (buffer-file-name)))
    (when from-waw
      (goto-char (point-max))
      (insert "\n****************\n\n\n")
      (insert-buffer "*Shell Command Output*")
      (forward-line -2))
      (waw-mode)))

(defun waw-find-match (n search message)
  (if (not n) (setq n 1))
  (while (> n 0)
    (or (funcall search)
        (error message))
    (setq n (1- n))))

(defun java-bt-find-match (n search message)
  (if (not n) (setq n 1))
    (while (> n 0)
      (or (funcall search)
          (error message))
      (setq n (1- n))))

(defun java-bt-search-prev ()
  (beginning-of-line)
  (search-backward-regexp "(.*:[0-9]+)$"))

(defun java-bt-search-next ()
  (end-of-line)
  (search-forward-regexp "(.*:[0-9]+)$"))

(defun waw-search-prev ()
  (beginning-of-line)
  (search-backward-regexp "^    ")
  (let ((this-line-str (current-line-string)))
    (cond ((string-match ":[0-9]+:" this-line-str)
           (search-backward "    =>"))
          ((string-match "^\\s *\\.\\.\\.$" this-line-str)
           (forward-line -1))
          (t))))

(defun waw-search-next ()
  (end-of-line)
  (search-forward-regexp "^    ")
  (let ((this-line-str (current-line-string)))
    (cond ((string-match ":[0-9]+:" this-line-str)
           (forward-line))
          ((string-match "^\\s *\\.\\.\\.$" this-line-str)
           (forward-line))
          (t))))

(defun cscope-next-error-bhj (&optional argp reset)
  (pop-to-buffer "*cscope*")
  (if (> argp 0)
      (cscope-next-symbol)
    (cscope-prev-symbol))
  (pop-to-buffer "*cscope*")
  (other-window 1))

;;;###autoload
(defun waw-next-error (&optional argp reset)
  (interactive "p")
  (with-current-buffer
      (if (next-error-buffer-p (current-buffer))
          (current-buffer)
        (next-error-find-buffer nil nil
                                (lambda()
                                  (eq major-mode 'waw-mode))))

    (goto-char (cond (reset (point-min))
                     ((< argp 0) (line-beginning-position))
                     ((> argp 0) (line-end-position))
                     ((point))))
    (waw-find-match
     (abs argp)
     (if (> argp 0)
         #'waw-search-next
       #'waw-search-prev)
     "No more matches")

    (forward-line) ;;this is because the following was written
                   ;;originally as prev-error :-)

    (catch 'done
      (let ((start-line-number (line-number-at-pos))
            (start-line-str (current-line-string))
            new-line-number target-file target-line
            error-line-number error-line-str
            msg mk end-mk)

        (save-excursion
          (end-of-line) ;; prepare for search-backward-regexp
          (search-backward-regexp ":[0-9]+:")
          (setq error-line-str (current-line-string)
                error-line-number (line-number-at-pos))
          (string-match "^\\s *\\(.*?\\):\\([0-9]+\\):" error-line-str)
          (setq target-file (match-string 1 error-line-str)
                target-line (match-string 2 error-line-str)))

        (when (equal start-line-number error-line-number)
          (search-forward "=>")
          (forward-line))

        (when (equal start-line-number (1+ error-line-number))
          (search-backward-regexp "=>")
          (forward-line)
          (waw-next-error -1)
          (throw 'done nil))

        (setq new-line-number (line-number-at-pos))
        (forward-line -1)

        (while (> new-line-number error-line-number)
          (if (string-match "^\\s *\\.\\.\\.$" (current-line-string))
              (progn
                (setq new-line-number (1- new-line-number))
                (forward-line -1))
            (back-to-indentation)
            (let ((search-str (buffer-substring-no-properties (point) (line-end-position))))
              (if (string-match "=>  \\s *\\(.*\\)" search-str)
                  (setq search-str (match-string 1 search-str)))
              (setq msg (point-marker))
              (save-excursion
                (with-current-buffer (find-file-noselect target-file)
                  (goto-line (read target-line))
                  (end-of-line)
                  (search-backward search-str)
                  (back-to-indentation)
                  (setq mk (point-marker) end-mk (line-end-position))))
              (compilation-goto-locus msg mk end-mk))
            (throw 'done nil)))))))

;;;###autoload
(defun waw-ret-key ()
  (interactive)
  (let ((start-line-str (current-line-string)))
    (if (string-match "^    .*:[0-9]+:" start-line-str)
        (progn
          (search-forward-regexp "^    =>")
          (next-error 0))
      (if (string-match "^    " start-line-str)
          (progn
            (next-error 0))
        (insert "\n")))))

;;;###autoload
(defun waw-mode ()
  "Major mode for output from \\[where-are-we]."
  (interactive)
  (kill-all-local-variables)
  (use-local-map waw-mode-map)
  (setq major-mode 'waw-mode)
  (setq mode-name "Where-are-we")
  (setq next-error-function 'waw-next-error)
  (run-mode-hooks 'waw-mode-hook))

;;;###autoload
(defun java-bt-ret-key ()
  (interactive)
  (let ((start-line-str (current-line-string)))
    (if (string-match "(.*:[0-9]+)" start-line-str)
        (next-error 0))))

;;;###autoload
(defun java-bt-next-error (&optional argp reset)
  (interactive "p")
  (with-current-buffer
      (if (next-error-buffer-p (current-buffer))
          (current-buffer)
        (next-error-find-buffer nil nil
                                (lambda()
                                  (eq major-mode 'java-bt-mode))))

    (message "point is at %d" (point))
    (goto-char (cond (reset (point-min))
                     ((< argp 0) (line-beginning-position))
                     ((> argp 0) (line-end-position))
                     ((point))))
    (java-bt-find-match
     (abs argp)
     (if (> argp 0)
         #'java-bt-search-next
       #'java-bt-search-prev)
     "No more matches")
    (message "point is at %d" (point))

    (catch 'done
      (let ((start-line-number (line-number-at-pos))
            (start-line-str (current-line-string))
            new-line-number target-file target-line
            error-line-number error-line-str grep-output temp-buffer
            msg mk end-mk)
          (save-excursion
            (end-of-line)
            (search-backward "(")
            (search-backward ".")
            (setq msg (point-marker))
            (end-of-line)
            (setq grep-output (cdr (assoc-string start-line-str java-bt-tag-alist)))
            (unless grep-output
              (setq grep-output (progn
                                  (shell-command (concat "java-trace-grep " (shell-quote-argument (current-line-string))))
                                  (with-current-buffer "*Shell Command Output*"
                                    (buffer-substring-no-properties (point-min) (point-max)))))
              (setq java-bt-tag-alist (cons (cons start-line-str grep-output) java-bt-tag-alist))))

        (when (string-match "^\\(.*\\):\\([0-9]+\\):" grep-output)
          (setq target-file (concat (file-remote-p (or (buffer-file-name (current-buffer)) default-directory)) (match-string 1 grep-output))
                target-line (match-string 2 grep-output))
          (save-excursion
            (with-current-buffer (find-file-noselect target-file)
              (goto-line (read target-line))
              (beginning-of-line)
              (setq mk (point-marker) end-mk (line-end-position)))))

        (compilation-goto-locus msg mk end-mk))

      (throw 'done nil))))

;;;###autoload
(defun java-bt-mode ()
  "Major mode for output from java back trace."
  (interactive)
  (kill-all-local-variables)
  (use-local-map java-bt-mode-map)
  (make-local-variable 'java-bt-tag-alist)
  (setq major-mode 'java-bt-mode-map)
  (setq mode-name "java-bt")
  (setq next-error-function 'java-bt-next-error)
  (run-mode-hooks 'java-bt-mode-hook))

;;;###autoload
(defun indent-same-space-as-prev-line (n-prev &optional from-bol)
  (interactive "p")
  (when from-bol
    (goto-char (line-beginning-position)))
  (let ((start-point (point))
        (end-point (point)))
    (let* ((col-start-indent (current-column))
           (compare-to-prev-lines (if (> n-prev 0)
                                      t
                                    nil))
           (n-prev (if (> n-prev 0)
                       n-prev
                     (- n-prev)))
           (col-end-indent (save-excursion
                             (or (looking-at "\\S ")
                                 (when (search-forward-regexp "\\S " (line-end-position) t)
                                   (backward-char)
                                   t)
                                 (goto-char (line-end-position)))
                             (setq end-point (point))
                             (current-column)))
           (col-indent-to (save-excursion
                            (while (> n-prev 0)
                              (forward-line (if compare-to-prev-lines -1 1))
                              (goto-char (if compare-to-prev-lines (line-end-position) (line-beginning-position)))
                              (apply (if compare-to-prev-lines 'search-backward-regexp 'search-forward-regexp) (list "\\S "))
                              (setq n-prev (1- n-prev)))
                            (move-to-column col-start-indent)
                            (when (and (looking-at "\\S ")
                                       (not (and
                                             (looking-back "\t")
                                             (> (current-column) col-start-indent))))
                              (search-forward-regexp "\\s "))
                            (search-forward-regexp "\\S " (line-end-position))
                            (current-column))))
      (unless (equal start-point end-point)
        (delete-region start-point end-point))
      (insert (make-string (- col-indent-to col-start-indent 1) ? )))))

;;;###autoload
(defun back-to-indent-same-space-as-prev-line (n-prev)
  (interactive "p")
  (if (looking-back "\\S ")
      (progn
        (untabify (line-beginning-position) (point))
        (let* ((old-pos (point))
               (old-col (current-column))
               (pat-start (save-excursion
                            (search-backward-regexp "\\(^\\|\\s \\)\\S ")
                            (unless (looking-at "^")
                              (forward-char))
                            (point)))
               (pat (buffer-substring-no-properties old-pos pat-start))
               (col-back-to (save-excursion
                              (goto-char pat-start)
                              (search-backward pat)
                              (current-column)))
               (pos-back-to (- old-pos (- old-col col-back-to))))
          (untabify (line-beginning-position) old-pos)
          (if (< pos-back-to old-pos)
              (delete-region pos-back-to old-pos)
            (delete-region pat-start old-pos)
            (insert (make-string (- pos-back-to pat-start) ?\ )))))
    (indent-same-space-as-prev-line n-prev t)))

;;;###autoload
(defun save-all-buffers-no-check-modified ()
  (interactive)
  (flet ((verify-visited-file-modtime (&rest args) t)
         (ask-user-about-supersession-threat (&rest args) t))
    (mapcar (lambda (x)
              (when (and (buffer-file-name x)
                         (file-exists-p (buffer-file-name x)))
                (with-current-buffer x
                  (unless buffer-read-only
                    (set-buffer-modified-p t)
                    (basic-save-buffer)))))
            (buffer-list))))

;;;###autoload
(defun revert-all-buffers ()
  (interactive)
  (mapcar (lambda (x)
            (when (buffer-file-name x)
              (with-current-buffer x
                (if (file-exists-p (buffer-file-name x))
                    (revert-buffer t t)
                  (kill-buffer)))))
          (buffer-list)))

(defun my-buffer-file-name (buf)
  (with-current-buffer buf
    (if (eq major-mode 'dired-mode)
        (replace-regexp-in-string "/*$" "" default-directory)
      (or (buffer-file-name buf)
          ""))))

(defun my-buffer-file-name-local (&optional buf)
  (let ((name (my-buffer-file-name (or buf (current-buffer)))))
    (or (file-remote-p name 'localname)
        name)))

;;;###autoload
(defun switch-buffer-same-filename (&optional reverse)
  (interactive)
  (let* ((buf-list (if reverse
                       (nreverse (buffer-list))
                     (buffer-list)))
         (current-filepath (my-buffer-file-name (current-buffer)))
         (current-filename (file-name-nondirectory current-filepath))
         checking-filename
         checking-filepath
         buf-switched)
    (while buf-list
      (setq checking-filepath (my-buffer-file-name (car buf-list))
            checking-filename (file-name-nondirectory checking-filepath))
      (unless (eq (current-buffer) (car buf-list))
        (when (string-equal checking-filename current-filename)
            (progn
              (unless reverse
                (bury-buffer (current-buffer)))
              (switch-to-buffer (car buf-list))
              (message "switched to `%s'" (my-buffer-file-name (current-buffer)))
              (setq buf-list nil
                    buf-switched t))))
      (setq buf-list (cdr buf-list)))
    (unless buf-switched
      (message "You have no other buffer named `%s'" current-filename))))

;;;###autoload
(defun switch-buffer-same-filename-rev ()
  (interactive)
  (switch-buffer-same-filename t))

(defvar remote-sudo-prefix "/scp:root@localhost:"
  "The prefix for visiting a file's remote counterpart or with sudo permission")

;;;###autoload
(defun sudoedit ()
  (interactive)
  (find-alternate-file
   (if (file-remote-p (buffer-file-name))
       (replace-regexp-in-string ":.*?@" ":root@" (buffer-file-name))
     (concat remote-sudo-prefix (buffer-file-name)))))

;;;###autoload
(defun localedit ()
  (interactive)
  (find-alternate-file (replace-regexp-in-string "^/scp:.*?:" "" (buffer-file-name))))

;;;###autoload
(defun gnus-gmail-search-subject ()
  (interactive)
  (shell-command (concat
                  "search-gmail "
                  (shell-quote-argument (case major-mode
                                          ('gnus-summary-mode
                                           (gnus-summary-article-subject))
                                          ('gnus-article-mode
                                           (save-excursion
                                             (beginning-of-buffer)
                                             (search-forward-regexp "^Subject: ")
                                             (buffer-substring-no-properties (point) (line-end-position))))))
                  "&")))

;;;###autoload
(defun bhj-view-mail-external ()
  "open the current maildir file in kmail"
  (interactive)
  ;(defun nnmaildir-request-article (num-msgid &optional gname server to-buffer)
  (let ((article_id gnus-current-article))
    (with-temp-buffer
      (nnmaildir-request-article
       article_id
       gnus-newsgroup-name
       (replace-regexp-in-string ".*:" "" (gnus-group-server gnus-newsgroup-name))
       (current-buffer))))
  (let ((default-directory "/"))
    (shell-command (concat "kmail-view " (shell-quote-argument nnmaildir-article-file-name)))))

;;;###autoload
(defun my-bbdb/gnus-update-records-mode ()
  (progn
    ;(message "hello bbdb/gnus-update-records-mode: %s %s %s" (buffer-name gnus-article-current-summary) (buffer-name) bbdb/news-auto-create-p)
    (if (and (boundp 'auto-create-p) (null auto-create-p))
        (if (and (boundp 'gnus-article-current-summary)
                 (string-match "^\\*Summary nntp" (buffer-name gnus-article-current-summary)))
            'annotating
          'searching)
      'annotating)))

;;;###autoload
(defun my-bbdb-canonicalize (addr)
  (if (stringp addr)
      (replace-regexp-in-string "@adsnexus.com\\|@eee168.com" "@eee168.com" addr)
    addr))

(defun bbdb-canonicalize-address (net)
  (my-bbdb-canonicalize net))

(defun all-completions-skeleton
  (STRING COLLECTION &optional PREDICATE)
  (let* ((fragments (mapcar 'regexp-quote (split-string STRING))))
    (while fragments

      (setq COLLECTION (mapcan
                        (lambda (x)
                          (let ((tmp (if (symbolp x)
                                         (format "%s" (symbol-value x))
                                       (format "%s" x))))

                                        ; (if (string-match "haojun.bao@ray" tmp)
                                        ;   (message "'%s' : '%s'" x tmp))

                            (if (string-match (car fragments) tmp)
                                (progn ;(message "(string-match \"%s\" \"%s\") x is %s\n" (car fragments) x oldx)
                                  (if (cdr fragments)
                                      (list x)
                                    (and (when (functionp PREDICATE)
                                           (funcall PREDICATE x))
                                         (list x))))
                              nil)))
                        COLLECTION)
            fragments (cdr fragments))
      ;; (message "COLLECTION all- is %s\n" COLLECTION)
      )
    COLLECTION))

(defun try-completion-skeleton (STRING COLLECTION &optional PREDICATE)
  (let* ((fragments (mapcar 'regexp-quote (split-string STRING)))
         (final (car (reverse (split-string STRING))))
         res)
    (while fragments
      (setq COLLECTION (mapcan
                        (lambda (x)
                          (let ((tmp
                                 (if (symbolp x)
                                     (format "%s" (symbol-value x))
                                   (format "%s" x))))
                            (if (string-match (car fragments) tmp)
                                (list x)
                              nil)))
                        COLLECTION)
            fragments (cdr fragments))
      )
    ;; (message "COLLECTION is %s\n" COLLECTION)
    (if (and COLLECTION (not (cdr COLLECTION)))
        t
      nil)))

(defun string-skeleton-match (target source)
  (let* ((fragments (mapcar 'regexp-quote (split-string target)))
         (res t))
    (while fragments
      (unless (string-match (car fragments) source)
        (setq res nil
              fragments nil))
      (setq fragments (cdr fragments)))
    res))

;;;###autoload
(defun bbdb-complete-name (&optional start-pos)
  "Complete the user full-name or net-address before point (up to the
preceeding newline, colon, or comma, or the value of START-POS).  If
what has been typed is unique, insert an entry of the form \"User Name
<net-addr>\" (although see documentation for
bbdb-dwim-net-address-allow-redundancy).  If it is a valid completion
but not unique, a list of completions is displayed.

If the completion is done and `bbdb-complete-name-allow-cycling' is
true then cycle through the nets for the matching record.

When called with a prefix arg then display a list of all nets.

Completion behaviour can be controlled with `bbdb-completion-type'."
  (interactive)

  (let* ((end (point))
         (beg (or start-pos
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
         (orig (buffer-substring beg end))
         (typed (downcase orig))
         (pattern (bbdb-string-trim typed))
         (ht (bbdb-hashtable))
         ;; make a list of possible completion strings
         ;; (all-the-completions), and a flag to indicate if there's a
         ;; single matching record or not (only-one-p)
         (only-one-p t)
         (all-the-completions nil)
         (pred
          (lambda (sym)
            (when (bbdb-completion-predicate sym)
              (if (and only-one-p
                       all-the-completions
                       (or
                        ;; not sure about this. more than one record
                        ;; attached to the symbol? does that happen?
                        (> (length (symbol-value sym)) 1)
                        ;; this is the doozy, though. multiple syms
                        ;; which all match the same record
                        (delete t (mapcar (lambda(x)
                                            (equal (symbol-value x)
                                                   (symbol-value sym)))
                                          all-the-completions))))
                  (setq only-one-p nil))
              (if (not (memq sym all-the-completions))
                  (setq all-the-completions (cons sym all-the-completions))))))
         (completion (progn (all-completions-skeleton pattern ht pred) (try-completion-skeleton pattern ht)))
         (exact-match (eq completion t)))

    (cond
     ;; No matches found OR you're trying completion on an
     ;; already-completed record. In the latter case, we might have to
     ;; cycle through the nets for that record.

     ;; Match for a single record. If cycling is enabled then we don't
     ;; care too much about the exact-match part.
     ((and only-one-p (or exact-match bbdb-complete-name-allow-cycling))
      (let* ((sym (car all-the-completions))
             (recs (symbol-value sym))
             the-net match-recs lst primary matched)

        (while recs
          (when (bbdb-record-net (car recs))

            ;; Did we match on name?
            (let ((b-r-name (or (bbdb-record-name (car recs)) "")))
              (if t
                  (setq match-recs (cons (car recs) match-recs)
                        matched t)))

            ;; Did we match on lastname?
            (let ((b-r-name (or (bbdb-record-lfname (car recs)) "")))
              (if (string= pattern
                           (substring (downcase b-r-name) 0
                                      (min (length b-r-name)
                                           (length pattern))))
                  (setq match-recs (cons (car recs) match-recs)
                        matched t)))

            ;; Did we match on aka?
            (when (not matched)
              (setq lst (bbdb-record-aka (car recs)))
              (while lst
                (if (string= pattern (substring (downcase (car lst)) 0
                                                (min (length (downcase
                                                              (car
                                                               lst)))
                                                     (length pattern))))
                    (setq match-recs (append match-recs (list (car recs)))
                          matched t
                          lst '())
                  (setq lst (cdr lst)))))

            ;; Name didn't match name so check net matching
            (when (not matched)
              (setq lst (bbdb-record-net (car recs)))
              (setq primary t) ;; primary wins over secondary...
              (while lst
                (if (string= pattern (substring (downcase (car lst))
                                                0 (min (length
                                                        (downcase (car
                                                                   lst)))
                                                       (length pattern))))
                    (setq the-net (car lst)
                          lst     nil
                          match-recs
                          (if primary (cons (car recs) match-recs)
                            (append match-recs (list (car recs))))))
                (setq lst     (cdr lst)
                      primary nil))))

          ;; loop to next rec
          (setq recs    (cdr recs)
                matched nil))

        (unless match-recs
          (error "only exact matching record unhas net field"))

        ;; now replace the text with the expansion
        (delete-region beg end)
        (insert (bbdb-dwim-net-address (car match-recs) the-net))

        ;; if we're past fill-column, wrap at the previous comma.
        (if (and
             (bbdb-auto-fill-function)
             (>= (current-column) fill-column))
            (let ((p (point))
                  bol)
              (save-excursion
                (beginning-of-line)
                (setq bol (point))
                (goto-char p)
                (if (search-backward "," bol t)
                    (progn
                      (forward-char 1)
                      (insert "\n   "))))))

        ;; Update the *BBDB* buffer if desired.
        (if bbdb-completion-display-record
            (let ((bbdb-gag-messages t))
              (bbdb-display-records-1 match-recs t)))
        (bbdb-complete-name-cleanup)

        ;; call the exact-completion hook
        (run-hooks 'bbdb-complete-name-hooks)))

     ;; Partial match
     ;; note, we can't use the trimmed version of the pattern here or
     ;; we'll recurse infinitely on e.g. common first names
     ((and (stringp completion) (not (string= typed completion)))
      (delete-region beg end)
      (insert completion)
      (setq end (point))
      (let ((last "")
            (bbdb-complete-name-allow-cycling nil))
        (while (and (stringp completion)
                    (not (string= completion last))
                    (setq last completion
                          pattern (downcase orig)
                          completion (progn (all-completions-skeleton pattern ht pred) (try-completion-skeleton pattern ht))))
          (if (stringp completion)
              (progn (delete-region beg end)
                     (insert completion))))
        (bbdb-complete-name beg)))

     ;; Exact match, but more than one record
     (t
      (or (eq (selected-window) (minibuffer-window))
          (message "Making completion list..."))

      (let (dwim-completions
            uniq nets net name lfname akas)
        ;; Now collect all the dwim-addresses for each completion, but only
        ;; once for each record!  Add it if the net is part of the completions
        (bbdb-mapc
         (lambda (sym)
           (bbdb-mapc
            (lambda (rec)
              (when (not (member rec uniq))
                (setq uniq (cons rec uniq)
                      nets (bbdb-record-net rec)
                      name (downcase (or (bbdb-record-name rec) ""))
                      lfname (downcase (or (bbdb-record-lfname rec) ""))
                      akas (mapcar 'downcase (bbdb-record-aka rec)))
                (while nets
                  (setq net (car nets))
                  (when (string-skeleton-match pattern (format "%s %s %s" name (format "%s" akas) net))
                    (setq dwim-completions
                          (cons (bbdb-dwim-net-address rec net)
                                dwim-completions))
                    (if exact-match (setq nets nil)))
                  (setq nets (cdr nets)))))
            (symbol-value sym)))
         all-the-completions)

        ;; if, after all that, we've only got one matching record...
        (if (and dwim-completions (null (cdr dwim-completions)))
            (progn
              (delete-region beg end)
              (insert (car dwim-completions))
              (message ""))
          ;; otherwise, pop up a completions window
          (if (not (get-buffer-window "*Completions*"))
              (setq bbdb-complete-name-saved-window-config
                    (current-window-configuration)))
          (let ((arg (list (current-buffer)
                           (set-marker (make-marker) beg)
                           (set-marker (make-marker) end))))
            (with-output-to-temp-buffer "*Completions*"
              (bbdb-display-completion-list
               dwim-completions
               'bbdb-complete-clicked-name
               arg)))
          (or (eq (selected-window) (minibuffer-window))
              (message "Making completion list...done"))))))))

;;;###autoload
(defun bhj-org-tasks-closed-last-week (&optional match-string)
  "Produces an org agenda tags view list of the tasks completed
in the specified month and year. Month parameter expects a number
from 1 to 12. Year parameter expects a four digit number. Defaults
to the current month when arguments are not provided. Additional search
criteria can be provided via the optional match-string argument "
  (interactive "sShow tasks before (default: last mon): ")
  (if (or (not match-string)
          (and (stringp match-string) (string-equal match-string "")))
      (setq match-string "last mon"))
  (org-tags-view nil
                 (concat
                  (format "+CLOSED>=\"[%s]\""
                          (shell-command-to-string (concat "today '" match-string "'"))))))

;;;###autoload
(defun bhj-do-code-generation ()
  (interactive)
  (let (start-of-code end-of-code code-text start-of-text end-of-text code-transform)
    (search-backward "start code-generator")
    (forward-char (length "start code-generator"))
    (if (looking-at "\\s *\\(\"\\|(\\)")
        (setq code-transform
             (read
              (buffer-substring-no-properties (point) (line-end-position)))))
    (next-line)
    (move-beginning-of-line nil)
    (setq start-of-code (point))
    (search-forward "end code-generator")
    (previous-line)
    (move-end-of-line nil)
    (setq end-of-code (point))
    (setq code-text (buffer-substring-no-properties start-of-code end-of-code))
    (cond
     ((stringp code-transform)
      (setq code-text (replace-regexp-in-string code-transform "" code-text)))
     ((consp code-transform)
      (setq code-text (replace-regexp-in-string (car code-transform) (cadr code-transform) code-text))))

    (search-forward "start generated code")
    (next-line)
    (move-beginning-of-line nil)
    (setq start-of-text (point))
    (search-forward "end generated code")
    (previous-line)
    (move-end-of-line nil)
    (setq end-of-text (point))
    (shell-command-on-region start-of-text end-of-text code-text nil t)
    (unless (eq major-mode 'fundamental-mode)
      (indent-region (min (point) (mark))
                     (max (point) (mark))))))

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;;;###autoload
(defun dos2unix ()
  "Convert this entire buffer from MS-DOS text file format to UNIX."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r$" "" nil)
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))

(defun copy-string (str)
  "copy the string into kill-ring"
  (with-temp-buffer
    (insert str)
    (kill-region (point-min) (point-max))))

;;;###autoload
(defun insert-today ()
  (interactive)
  (insert (shell-command-to-string "today")))

;;;###autoload
(defun bhj-do-dictionry (word)
  "lookup the current word (or region) in dictionary"
  (interactive
   (list (if mark-active
             (buffer-substring-no-properties (region-beginning)
                                             (region-end))
           (current-word))))
  (w3m-goto-url (format "http://r66:34567/dict/%s" word)))

;;;###autoload
(defun bhj-open-android-doc-on-java-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^\\s *package ")
    (let ((package-name
           (buffer-substring-no-properties
            (point)
            (1- (line-end-position))))
          (doc-prefix "file:///home/bhj/bin/Linux/ext/android-sdk-linux_86/docs/reference")
          (html-name (replace-regexp-in-string
                      ".java$" ".html"
                      (replace-regexp-in-string
                       ".*/" ""
                       (buffer-file-name))))
          (default-directory (expand-file-name "~")))
      (shell-command (format "of %s/%s/%s"
                                   doc-prefix
                                   (replace-regexp-in-string
                                    "\\."
                                    "/"
                                    package-name)
                                   html-name)))))

;;;###autoload
(defun bhj-choose (args)
  (interactive)
  (insert
   (completing-read "Please enter which one to use: " args)))

;;;###autoload
(defun bhj-choose-from-output ()
  (interactive)
  (bhj-choose (split-string (shell-command-to-string (read-from-minibuffer "command to run: ")) nil t)))

;;;###autoload
(defun replace-double-quotes ()
  (interactive)
  (query-replace "``" "“" nil (point-min) (point-max))
  (query-replace "''" "”" nil (point-min) (point-max)))

(defun imenu-create-index-using-ctags ()
  "Create the index like the imenu-default-create-index-function,
using ctags-exuberant"

  (let ((source-buffer (current-buffer))
        (temp-buffer (get-buffer-create "* imenu-ctags *"))
        result-alist)
    (save-excursion
      (save-restriction
        (widen)
        (save-window-excursion
          (shell-command-on-region
           (point-min)
           (point-max)
           (concat "imenu-ctags "
                   (file-name-nondirectory (buffer-file-name source-buffer)))
           temp-buffer))
        (with-current-buffer temp-buffer
          (goto-char (point-min))
          (while (search-forward-regexp "^\\([0-9]+\\) : \\(.*\\)" nil t)
            (setq result-alist
                  (cons (cons (match-string 2)
                              (let ((marker (make-marker)))
                                (set-marker marker (string-to-number (match-string 1)) source-buffer)))
                        result-alist))))))
    (nreverse result-alist)))

;;;###autoload
(defun bhj-find-missing-file ()
  (interactive)
  (let (missing-file-name missing-file-name-save)
    (save-excursion
      (goto-char (point-min))
      (search-forward-regexp "(default \\(.*\\))")
      (setq missing-file-name (match-string 1))
      (setq missing-file-name-save missing-file-name))

    (setq missing-file-name
          (mapcar (lambda (b) (buffer-file-name b))
                  (delete-if-not
                   (lambda (b)
                     (let ((name (file-name-nondirectory (or (buffer-file-name b) ""))))
                       (string= name missing-file-name-save)))
                   (buffer-list))))
    (unless missing-file-name
      (setq missing-file-name (shell-command-to-string
                               (format "beagrep -e %s -f 2>&1|perl -ne 's/:\\d+:.*// and print'" missing-file-name-save)))
      (setq missing-file-name (delete-if-empty (split-string missing-file-name "\n"))))

    (when missing-file-name
      (if (nth 1 missing-file-name)
          (setq missing-file-name
                (skeleton-general-display-matches missing-file-name))
        (setq missing-file-name (car missing-file-name)))
      (when (and (not (file-remote-p missing-file-name))
                 (file-remote-p default-directory))
        (setq missing-file-name (concat (file-remote-p default-directory)
                                        missing-file-name)))
        (insert missing-file-name))))

(defun delete-if-empty (l)
  (delete-if
   (lambda (s) (string-equal s ""))
   l))

(defun ca-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))

;;;###autoload
(defun bhj-java-import ()
  (interactive)
  (save-excursion
    (let ((old-buffer (current-buffer))
          import-list)
      (with-temp-buffer
        (shell-command (format "java-get-imports.pl %s -v" (buffer-file-name old-buffer)) (current-buffer))
        (goto-char (point-min))
        (while (search-forward-regexp "^import" nil t)
          (save-excursion
            (if (looking-at "-multi")

                (setq import-list (cons
                                   (format "import %s;\n"
                                           (completing-read-one? "Import which? "
                                                                 (cdr
                                                                  (delete-if-empty
                                                                   (split-string (current-line-string) "\\s +")))
                                                                 nil
                                                                 t))
                                   import-list))
              (setq import-list (cons (format "%s;\n" (current-line-string)) import-list))))
          (forward-line)
          (beginning-of-line)))
      (goto-char (point-max))
      (or (search-backward-regexp "^import\\s +" nil t)
          (search-backward-regexp "^package\\s +" nil t))
      (forward-line)
      (beginning-of-line)
      (while import-list
        (insert (car import-list))
        (setq import-list (cdr import-list))))))

;;;###autoload
(defun source-code-help()
  (interactive)
  (let ((word (current-word)))
    (async-shell-command
     (if current-prefix-arg
         (format "search-google %s" (shell-quote-argument word))
       (format "source-code-help %s %s" major-mode word)))))

;;;###autoload
(defun bhj-upcase-symbol-or-region()
  (interactive)
  (if (region-active-p)
      (call-interactively 'upcase-region)
    (save-excursion
      (backward-sexp)
      (upcase-region (point)
                     (progn
                       (forward-sexp)
                       (point))))))

(define-key global-map [remap upcase-region] 'bhj-upcase-symbol-or-region)

;;;###autoload
(defun bhj-downcase-symbol-or-region()
  (interactive)
  (if (region-active-p)
      (call-interactively 'downcase-region)
    (save-excursion
      (backward-sexp)
      (downcase-region (point)
                     (progn
                       (forward-sexp)
                       (point))))))

(define-key global-map [remap downcase-region] 'bhj-downcase-symbol-or-region)

(provide 'bhj-defines)
