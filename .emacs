;; Red Hat Linux default .emacs initialization file  ; -*- mode: emacs-lisp -*-

(setq load-path
      (cons (expand-file-name "~/.emacs_d/lisp") load-path))

(when  (eq system-type 'cygwin)
  (let ((default-directory "~/tools/emacs-site-lisp/")) (load-file "~/tools/emacs-site-lisp/subdirs.el"))
  (setq load-path
	(cons "~/tools/emacs-site-lisp/" load-path))
  ;;press F2 to get MSDN help
  (global-set-key[(f2)](lambda()(interactive)(call-process "/bin/bash" nil nil nil "/q/bin/windows/ehelp" (current-word))))
  (setq locate-command "locateEmacs.sh")
)

(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (set-frame-font "Monaco-14")
  (set-face-font 'italic (font-spec :family "Courier New" :slant 'italic :weight 'normal :size 16))
  (set-face-font 'bold-italic (font-spec :family "Courier New" :slant 'italic :weight 'bold :size 16))


  (set-fontset-font (frame-parameter nil 'font)
		    'han (font-spec :family "Simsun" :size 16))
  (set-fontset-font (frame-parameter nil 'font)
		    'symbol (font-spec :family "Simsun" :size 16))
  (set-fontset-font (frame-parameter nil 'font)
		    'cjk-misc (font-spec :family "Simsun" :size 16))
  (set-fontset-font (frame-parameter nil 'font)
		    'bopomofo (font-spec :family "Simsun" :size 16)))

(add-to-list 'load-path "~/.emacs_d/weblogger")


(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(require 'w3m)

(require 'tramp)
(require 'weblogger)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(require 'ibuffer)
(require 'browse-kill-ring)
(require 'ido)
(require 'thumbs)
(require 'keydef)
(require 'grep-buffers)
(require 'htmlize)

(require 'muse-mode)     ; load authoring mode
(require 'muse-html)     ; load publishing styles I use
(require 'muse-project)
(setq muse-project-alist
      '(("Website" ("~/Pages" :default "index")
         (:base "html" :path "~/public_html"))))

(setq-default abbrev-mode t)                                                                   
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)   

(grep-compute-defaults)

(defun grep-shell-quote-argument (argument)
  "Quote ARGUMENT for passing as argument to an inferior shell."
  (if (or (eq system-type 'ms-dos)
          (and (eq system-type 'windows-nt) (w32-shell-dos-semantics)))
      ;; Quote using double quotes, but escape any existing quotes in
      ;; the argument with backslashes.
      (let ((result "")
	    (start 0)
	    end)
	(if (or (null (string-match "[^\"]" argument))
		(< (match-end 0) (length argument)))
	    (while (string-match "[\"]" argument start)
	      (setq end (match-beginning 0)
		    result (concat result (substring argument start end)
				   "\\" (substring argument end (1+ end)))
		    start (1+ end))))
	(concat "\"" result (substring argument start) "\""))
    (if (equal argument "")
        "\"\""
      ;; Quote everything except POSIX filename characters.
      ;; This should be safe enough even for really weird shells.
      (let ((result "") (start 0) end)
        (while (string-match "[^-0-9a-zA-Z_./]" argument start)
          (setq end (match-beginning 0)
                result (concat result (substring argument start end)
                               "\\" (substring argument end (1+ end)))
                start (1+ end)))
        (concat "\"" result (substring argument start) "\"")))))

(defun grep-default-command ()
  "Compute the default grep command for C-u M-x grep to offer."
  (let ((tag-default (grep-shell-quote-argument (grep-tag-default)))
	;; This a regexp to match single shell arguments.
	;; Could someone please add comments explaining it?
	(sh-arg-re "\\(\\(?:\"\\(?:[^\"]\\|\\\\\"\\)+\"\\|'[^']+'\\|[^\"' \t\n]\\)+\\)")
	(grep-default (or (car grep-history) grep-command)))
    ;; In the default command, find the arg that specifies the pattern.
    (when (or (string-match
	       (concat "[^ ]+\\s +\\(?:-[^ ]+\\s +\\)*"
		       sh-arg-re "\\(\\s +\\(\\S +\\)\\)?")
	       grep-default)
	      ;; If the string is not yet complete.
	      (string-match "\\(\\)\\'" grep-default))
      ;; Maybe we will replace the pattern with the default tag.
      ;; But first, maybe replace the file name pattern.
      
      ;; Now replace the pattern with the default tag.
      (replace-match tag-default t t grep-default 1))))

            



;;;; You need this if you decide to use gnuclient

(register-input-method
 "chinese-wubi86" "Chinese-CNS" 'quail-use-package
 "WuBi" "WuBi"
 "quail/wubi86")

(let ((x "~/.emacs_d/UnicodeData.txt"))
  (when (file-exists-p x)
    (setq describe-char-unicodedata-file x)))


(setq weblogger-entry-mode-hook '(flyspell-mode))

(global-set-key [(meta ?/)] 'hippie-expand)

(defun bhj-c-beginning-of-defun ()
  (interactive)
  (progn
    (push-mark)
    (c-beginning-of-defun)))

(defun bhj-c-end-of-defun ()
  (interactive)
  (progn
    (push-mark)
    (c-end-of-defun)))

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode) 
  (local-set-key [?\C-\M-a] 'bhj-c-beginning-of-defun)
  (local-set-key [?\C-\M-e] 'bhj-c-end-of-defun)
  (local-set-key [?\C-c ?\C-d] 'c-down-conditional)
  (c-set-style "K&R")
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4))

(defun linux-c++-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c++-mode) 
  (local-set-key [?\C-\M-a] 'bhj-c-beginning-of-defun)
  (local-set-key [?\C-\M-e] 'bhj-c-end-of-defun)
  (local-set-key [?\C-c ?\C-d] 'c-down-conditional)
  (c-set-style "K&R")
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4))

(setq auto-mode-alist (cons '(".*\\.[c]$" . linux-c-mode)
                            auto-mode-alist))
(setq auto-mode-alist (cons '(".*\\.cpp$" . linux-c++-mode)
                            auto-mode-alist))
(setq auto-mode-alist (cons '(".*\\.h$" . linux-c++-mode)
                            auto-mode-alist))

(setq frame-title-format "emacs@%b")
(auto-image-file-mode)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)





(global-set-key (kbd "C-x C-b") 'ibuffer)


(global-set-key [(control c)(k)] 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;ido.el

(ido-mode t)

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;;popup the manual page, try:)
(global-set-key[(f3)](lambda()(interactive)(woman (current-word))))
                                        ;(global-set-key[(f4)](lambda()(interactive)(menu-bar-mode)))
                                        ;(global-set-key[(f5)](lambda()(interactive)(ecb-minor-mode)))


;; thumb


(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary"
  "Enable/disable dictionary-tooltip-mode for all buffers" t)


(global-set-key [(control c)(d)] 'dictionary-lookup-definition)
(global-set-key [(control c)(s)] 'dictionary-search)
(global-set-key [(control c)(m)] 'dictionary-match-words)


(setq dictionary-tooltip-dictionary "wn")

(put 'narrow-to-region 'disabled nil)

(global-set-key ";" 'tmp_en_ch)

(defun do_en_when_ch (arg)
  "input method is active, but we want it out temporarily"
  (interactive "senter to input: \n")
  (insert arg))

(defun tmp_en_ch ()
  "tell if input method is active, get out of it for a while"
  (interactive)
  (if (equal nil current-input-method)
      (insert ";")
    (call-interactively 'do_en_when_ch)))

(prefer-coding-system 'gbk)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

(defun weekrep ()
  (interactive)
  (call-process "wr" nil t nil "-6"))

(put 'upcase-region 'disabled nil)


(fset 'grep-buffers-symbol-at-point 'current-word)
(global-set-key [(control meta o)]
                (lambda()(interactive)
                  (let 
                      ((regexp (current-word))) 
                    (progn     
                      (setq regexp 
                            (read-string (format "List lines matching regexp [%s]: " regexp) nil nil regexp))
                      (occur regexp)))))

(global-set-key [(control meta shift g)]
                (lambda()(interactive)
                  (let 
                      ((search-string (current-word))) 
                    (progn     
                      (setq search-string
                            (read-string (format "search google with [%s]: " search-string) nil nil search-string))
                      (call-process "bash" nil nil nil "googleemacs.sh" search-string)
                      ))))

(standard-display-ascii ?\221 "\`")
(standard-display-ascii ?\222 "\'")
(standard-display-ascii ?\223 "\"")
(standard-display-ascii ?\224 "\"")
(standard-display-ascii ?\227 "\-")
(standard-display-ascii ?\225 "\*")

(defun bhj-next-error ()
  (interactive)
  (next-error)
  (delete-other-windows)
  (with-current-buffer next-error-last-buffer
    (message "%s" (buffer-substring (line-beginning-position) (line-end-position)))))

(defun bhj-previous-error ()
  (interactive)
  (previous-error)
  (delete-other-windows)
  (with-current-buffer next-error-last-buffer
    (message "%s" (buffer-substring (line-beginning-position) (line-end-position)))))

(global-set-key [(meta n)] 'bhj-next-error)
(global-set-key [(meta p)] 'bhj-previous-error)



(keydef "C-S-g" (progn (setq grep-buffers-buffer-name "*grep-buffers*")(grep-buffers)))

(defun bhj-clt-insert-file-name ()
  (interactive)
  (insert (car args)))

(defun bhj-insert-pwdw ()
  (interactive)
  (insert "'")
  (call-process "cygpath" nil t nil "-alw" default-directory)
  (backward-delete-char 1)
  (insert "'"))

(defun bhj-insert-pwdu ()
  (interactive)
  (insert "'")
  (call-process "cygpath" nil t nil "-au" (expand-file-name default-directory))
  (backward-delete-char 1)
  (insert "'"))

(defcustom bhj-clt-branch "dbg_zch68_a22242_ringtone-hx11i"
  "the cleartool branch to use for mkbranch")

(defun bhj-clt-insert-branch ()
  (interactive)
  (insert bhj-clt-branch))


(define-key minibuffer-local-shell-command-map [(control meta f)] 'bhj-clt-insert-file-name)
(define-key minibuffer-local-shell-command-map [(control meta b )] 'bhj-clt-insert-branch)
(define-key minibuffer-local-shell-command-map [(control meta d )] 'bhj-insert-pwdu)
(fset 'bhj-clt-co-mkbranch
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217761 99 108 116 101 110 118 32 126 47 98 105 110 47 99 108 116 45 99 111 45 109 107 98 114 97 110 99 104 32 134217730 32 134217734 return] 0 "%d")) arg) (call-interactively 'bhj-reread-file)))



(defcustom bhj-grep-default-directory "/pscp:a22242@10.194.131.91:/"
  "the default directory in which to run grep")
(keydef "C-M-g" (progn
                  (let ((current-prefix-arg 4)
                        (default-directory (eval bhj-grep-default-directory))
                        (grep-use-null-device nil))
                    (call-interactively 'grep))))


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

(put 'scroll-left 'disabled nil)

(fset 'bhj-bhjd
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\"" 0 "%d")) arg)))
(fset 'bhj-preview-markdown
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 67108896 3 2 134217848 98 104 106 45 109 105 109 101 tab return 3 return 80 24 111 67108911 24 111] 0 "%d")) arg)))

(fset 'bhj-isearch-yank
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\371" 0 "%d")) arg)))

(defun bhj-isearch-from-bod ()
  (interactive)
  (let ((word (current-word)))
    (push-mark)
    (with-temp-buffer
      (insert word)
      (kill-ring-save (point-min) (point-max)))
    (c-beginning-of-defun)
    (call-interactively 'bhj-isearch-yank)))

(global-set-key [(shift meta s)] 'bhj-isearch-from-bod)

(add-hook 'w3m-mode-hook 
          (lambda () 
            (local-set-key [(left)] 'backward-char)
            (local-set-key [(right)] 'forward-char)
            (local-set-key [(down)] 'next-line)
            (local-set-key [(up)] 'previous-line)
            (local-set-key [(n)] 'next-line)
            (local-set-key [(p)] 'previous-line)
            ))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (list "~/tools/emacswin/info/" "/usr/local/share/info" "/usr/share/info"))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/tmp"))))
 '(bhj-clt-branch "dbg_zch68_a22242_soundmgr")
 '(bhj-grep-default-directory (quote default-directory))
 '(canlock-password "78f140821d1f56625e4e7e035f37d6d06711d112")
 '(case-fold-search t)
 '(default-input-method "chinese-wubi86")
 '(delete-old-versions t)
 '(describe-char-unidata-list (quote (name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value mirrored old-name iso-10646-comment uppercase lowercase titlecase)))
 '(dictem-server "localhost")
 '(dictionary-server "bhj2")
 '(ecb-compile-window-height nil)
 '(ecb-layout-name "left10")
 '(ecb-layout-window-sizes (quote (("top1" (0.25842696629213485 . 0.32075471698113206) (0.21910112359550563 . 0.32075471698113206) (0.5337078651685393 . 0.32075471698113206)))))
 '(ecb-new-ecb-frame nil)
 '(ecb-options-version "2.32")
 '(ecb-ping-program "false")
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-buffer-style (quote image))
 '(ecomplete-database-file-coding-system (quote utf-8))
 '(emms-player-mplayer-command-name "mplayer.exe")
 '(emms-player-mplayer-parameters (quote ("-slave" "-quiet" "-really-quiet" "-vo" "null")))
 '(font-lock-maximum-decoration 2)
 '(gdb-find-source-frame t)
 '(gdb-same-frame t)
 '(gdb-show-main t)
 '(gnus-ignored-newsgroups "")
 '(grep-use-null-device (quote auto-detect))
 '(htmlize-output-type (quote font))
 '(ido-enable-regexp t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".*\\.\\(loc\\|org\\|mkelem\\)")))
 '(indent-tabs-mode nil)
 '(ispell-program-name "aspell")
 '(keyboard-coding-system (quote cp936))
 '(message-dont-reply-to-names (quote (".*haojun.*")))
 '(message-mail-alias-type (quote ecomplete))
 '(mm-text-html-renderer (quote w3m))
 '(muse-html-charset-default "utf-8")
 '(muse-publish-date-format "%m/%e/%Y")
 '(nnmail-expiry-wait (quote never))
 '(normal-erase-is-backspace nil)
 '(preview-gs-command "c:/gs/gs8.54/bin/gswin32c.exe")
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((py-indent-offset . 4) (sh-indentation . 2) (c-font-lock-extra-types "FILE" "bool" "language" "linebuffer" "fdesc" "node" "regexp") (TeX-master . t) (indent-tab-mode . t))))
 '(save-place t nil (saveplace))
 '(senator-minor-mode-hook (quote (ignore)))
 '(session-initialize (quote (de-saveplace session places keys menus)) nil (session))
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote parenthesis))
 '(starttls-use-gnutls nil)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tooltip-mode nil)
 '(tooltip-use-echo-area t)
 '(tramp-syntax (quote ftp))
 '(tramp-verbose 0)
 '(transient-mark-mode t)
 '(w32-symlinks-handle-shortcuts t)
 '(w32-use-w32-font-dialog nil)
 '(w3m-bookmark-file "q:/.w3m_bookmark.html")
 '(weblogger-config-alist (quote (("yo2.cn" ("user" . "baohaojun@gmail.com") ("server-url" . "http://baohaojun.yo2.cn/xmlrpc.php") ("weblog" . "1")) ("bhj3" ("user" . "admin") ("server-url" . "http://bhj3/blog/xmlrpc.php") ("weblog" . "1")) ("default" ("user" . "baohaojun@gmail.com") ("server-url" . "http://baohaojun.yo2.cn/xmlrpc.php") ("weblog" . "1")))))
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/local/man")))
 '(woman-use-own-frame nil))




(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

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

;;(add-hook 'message-send-hook 'bhj-mimedown)


(defun message-display-abbrev (&optional choose)
  "Display the next possible abbrev for the text before point."
  (interactive (list t))
  (when (and (memq (char-after (point-at-bol)) '(?C ?T ?\t ? ))
	     (message-point-in-header-p)
	     (save-excursion
	       (beginning-of-line)
	       (while (and (memq (char-after) '(?\t ? ))
			   (zerop (forward-line -1))))
	       (looking-at "To:\\|Cc:")))
    (let* ((end (point))
	   (start (save-excursion
		    (and (re-search-backward "\\(,\\s +\\|^To: \\|^Cc: \\|^\t\\)" nil t)
			 (+ (point) (length (match-string 1))))))
	   (word (when start (buffer-substring start end)))
	   (match (when (and word
			     (not (zerop (length word))))
		    (ecomplete-display-matches 'mail word choose))))
      (when (and choose match)
	(delete-region start end)
	(insert match)))))


(mapc
 '(lambda (cmd-host-pair)
    (let ((cmd (car cmd-host-pair)) (x (cdr cmd-host-pair)))
      (eval
       `(defun ,(intern (format "%s-%s" cmd x)) ()
        (interactive)
        (let ((bhj-buffer ,(format "*bhj-%s-%s*" cmd x))
              (bhj-buffer-term ,(format "bhj-%s-%s" cmd x))
              (default-directory "c:/Documents and Settings/bhj/"))
          (if (get-buffer-process (get-buffer bhj-buffer))
              (switch-to-buffer bhj-buffer)
            (progn
              (if (bufferp (get-buffer bhj-buffer))
                (kill-buffer (get-buffer bhj-buffer)))
              (setenv "LOGIN_HOST_PORT" ,(format "bhj@%s:22" x))
              (ansi-term ,(format "%s" cmd) bhj-buffer-term)
              (local-set-key [(meta x)] 'execute-extended-command)
              (local-set-key [(control c) (escape)] 'term-send-raw)
              (local-set-key [(f1)] 'bhj-term-fn-key)
              (local-set-key [(f2)] 'bhj-term-fn-key)
              (local-set-key [(f3)] 'bhj-term-fn-key)
              (local-set-key [(control c) (control j)] 'bhj-modify-term-map)
              (local-set-key [(control c) (control k)] 'bhj-modify-term-map)
              (local-set-key [(escape)] 'bhj-modify-term-map))))))))
 '((slin . bhj3) 
   (slin . md1)
   (slin . haoxue)
   (slin . bhj1)
   (slin . md2)
   (slin . md3)
   (slin . bhj)
   (.mlj.ep . bhj1\#23)
   (.mlj.ep . bhj1\#2323)))

(defun bhj-term-fn-key ()
  (interactive)
  (case last-input-char
    ('f1
     (term-send-raw-string "\033[11~"))
    ('f2
     (term-send-raw-string "\033[12~"))
    ('f3
     (term-send-raw-string "\033[13~"))))

(defun bhj-term-control_ ()
  (interactive)
  (term-send-raw-string "\C-_"))

(defun bhj-modify-term-map ()
  (interactive)
  (if (term-in-char-mode)
      (progn
        (term-line-mode)
        (local-set-key [(control c) (control c)] 'bhj-modify-term-map)
        (local-set-key [(control c) (control j)] 'bhj-modify-term-map)
        (local-set-key [(control c) (control k)] 'bhj-modify-term-map)
        (local-set-key [(escape)] 'bhj-modify-term-map))
    (progn
      (term-char-mode)
      (local-set-key [(control c) (escape)] 'term-send-raw)
      (local-set-key [(control c) (control j)] 'bhj-modify-term-map)
      (local-set-key [(control c) (control k)] 'bhj-modify-term-map)
      (local-set-key [(control /)] 'bhj-term-control_)
      (local-set-key [(escape)] 'bhj-modify-term-map))))

(require 'xcscope)
(global-set-key [(control z)] 'keyboard-quit)
(global-set-key [(control x) (control z)] 'keyboard-quit)
(require 'moinmoin-mode)

(defun bhj-jdk-help (jdk-word)
  "start jdk help"
  (interactive
   (progn
     (let ((default (current-word)))
       (list (read-string "Search JDK help on: "
                          default
                          'jdk-help-history)))))

  ;; Setting process-setup-function makes exit-message-function work
  (call-process "/bin/bash" nil nil nil "/q/bin/windows/jdkhelp.sh" jdk-word)
  (w3m-goto-url "file:///cygdrive/d/knowledge/jdk-6u10-docs/1.html"))
(keydef "C-M-j" 'bhj-jdk-help)
(keydef (w3m "C-c e") (lambda()(interactive)(call-process "/bin/bash" nil nil nil "/q/bin/windows/w3m-external" w3m-current-url)))


;; Command to point VS.NET at our current file & line
(defun my-current-line ()
  "Return the current buffer line at point.  The first line is 0."
  (save-excursion
    (beginning-of-line)
    (count-lines (point-min) (point))))
(defun devenv-cmd (&rest args)
  "Send a command-line to a running VS.NET process.  'devenv' comes from devenv.exe"
  (apply 'call-process "DevEnvCommand" nil nil nil args))
(defun switch-to-devenv ()
  "Jump to VS.NET, at the same file & line as in emacs"
  (interactive)
  (save-some-buffers)
  (let ((val1
	   (devenv-cmd "File.OpenFile" (buffer-file-name (current-buffer))))
	(val2
	   (devenv-cmd "Edit.GoTo" (int-to-string (+ (my-current-line) 1)))))
    (cond ((zerop (+ val1 val2))
	      ;(iconify-frame)  ;; what I really want here is to raise the VS.NET window
	         t)
	    ((or (= val1 1) (= val2 1))
	        (error "command failed"))  ;; hm, how do I get the output of the command?
	      (t
	          (error "couldn't run DevEnvCommand")))))
(global-set-key [f3] 'switch-to-devenv)

;; Command to toggle a VS.NET breakpoint at the current line.
(defun devenv-toggle-breakpoint ()
  "Toggle a breakpoint at the current line"
  (interactive)
  (switch-to-devenv)
  (devenv-cmd "Debug.ToggleBreakpoint"))
(global-set-key [f9] 'devenv-toggle-breakpoint)

;; Run the debugger.
(defun devenv-debug ()
  "Run the debugger in VS.NET"
  (interactive)
  (devenv-cmd "Debug.Start"))
(global-set-key [f5] 'devenv-debug)

(server-start)
;(w32-register-hot-key [A-tab])
