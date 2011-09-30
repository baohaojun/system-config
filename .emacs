;; Red Hat Linux default .emacs initialization file  ; -*- mode: emacs-lisp -*-

(setq load-path
      (nconc (list (expand-file-name "~/.emacs_d/lisp")
		   (expand-file-name "~/.emacs_d/lisp/ext"))
	     load-path))

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
  (if (file-exists-p "~/.emacs-frame-font")
      (set-frame-font 
       (save-excursion
         (find-file "~/.emacs-frame-font")
         (goto-char (point-min))
         (let ((monaco-font (read (current-buffer))))
           (kill-buffer (current-buffer))
           monaco-font)))
      (set-frame-font "Monaco-10.5"))
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
(require 'weblogger)

(require 'csharp-mode)
(require 'w3m)
(require 'tramp)
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(require 'ibuffer)
(require 'browse-kill-ring)
(require 'ido)
(require 'thumbs)
(require 'keydef)
(require 'grep-buffers)
(require 'htmlize)

(setq-default abbrev-mode t)                                                                   
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)   

(grep-compute-defaults)

(defun grep-shell-quote-argument (argument)
  "Quote ARGUMENT for passing as argument to an inferior shell."
    (if (equal argument "")
        "\"\""
      ;; Quote everything except POSIX filename characters.
      ;; This should be safe enough even for really weird shells.
      (let ((result "") (start 0) end)
        (while (string-match "[].*[^$\"\\]" argument start)
          (setq end (match-beginning 0)
                result (concat result (substring argument start end)
                               (let ((char (aref argument end)))
                                 (cond
                                  ((eq ?$ char)
                                   "\\\\\\")
                                  ((eq ?\\  char)
                                   "\\\\\\")
                                  (t
                                   "\\"))) (substring argument end (1+ end)))
                start (1+ end)))
        (concat "\"" result (substring argument start) "\""))))

(defun grep-default-command ()
  "Compute the default grep command for C-u M-x grep to offer."
  (let ((tag-default (grep-shell-quote-argument (grep-tag-default)))
	;; This a regexp to match single shell arguments.
	;; Could someone please add comments explaining it?
	(sh-arg-re "\\(\\(?:\"\\(?:\\\\\"\\|[^\"]\\)+\"\\|'[^']+'\\|\\(?:\\\\.\\|[^\"' \\|><\t\n]\\)\\)+\\)")

	(grep-default (or (car grep-history) my-grep-command)))
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

(autoload 'sdim-use-package "sdim" "Shadow dance input method")
(register-input-method
 "sdim" "euc-cn" 'sdim-use-package "影舞笔")

(setq default-input-method "sdim")


(let ((x "~/.emacs_d/UnicodeData.txt"))
  (when (file-exists-p x)
    (setq describe-char-unicodedata-file x)))


(global-set-key [(meta ?/)] 'hippie-expand)

(defun bhj-c-beginning-of-defun (&optional arg)
  (interactive "^p")
  (progn
    (unless mark-active
      (push-mark))
    (ctags-beginning-of-defun arg)))

(defun bhj-c-end-of-defun (&optional arg)
  (interactive "^p")
  (progn
    (unless mark-active
      (push-mark))
    (ctags-beginning-of-defun (- arg))))

(mapcar (lambda (x) (add-hook x (lambda ()
                    (local-set-key [?\C-\M-a] 'bhj-c-beginning-of-defun)
                    (local-set-key [?\C-\M-e] 'bhj-c-end-of-defun)
                    (local-set-key [?\C-c ?\C-d] 'c-down-conditional)
                    (c-set-offset 'innamespace 0)
                    (c-set-offset 'substatement-open 0))))
        (list 'c-mode-hook 'c++-mode-hook))
        

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode) 
  (c-set-style "k&r")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

(defun linux-c++-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c++-mode) 
  (c-set-style "k&r")
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

(define-key java-mode-map [?\C-\M-a] 'bhj-c-beginning-of-defun)
(define-key java-mode-map [?\C-\M-e] 'bhj-c-end-of-defun)

(setq auto-mode-alist (cons '(".*/kernel.*\\.[ch]$" . linux-c-mode)
			    (cons '("logcat.log.*" . fundamental-mode)
				  auto-mode-alist)))
(setq auto-mode-alist (cons '(".*\\.cpp$" . linux-c++-mode)
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

(put 'narrow-to-region 'disabled nil)

                                    

(require 'js)

(setq load-path (cons "~/.emacs_d/org2blog/" load-path))
(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      '(("wordpress"
	 :url "http://baohaojun.wordpress.com/xmlrpc.php"
	 :username "baohaojun")))

(define-key js-mode-map [(meta .)] 'my-cscope-find-global-definition)
(define-key global-map [(meta control ,)] 'cscope-pop-mark)
(define-key global-map [(meta control .)] 'cscope-pop-mark-back)

(prefer-coding-system 'gbk)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

(defun weekrep ()
  (interactive)
  (call-process "wr" nil t nil "-6"))

(put 'upcase-region 'disabled nil)


(fset 'grep-buffers-symbol-at-point 'current-word)

(global-set-key [(control meta shift g)]
                (lambda()(interactive)
                  (let 
                      ((search-string (current-word))) 
                    (progn     
                      (setq search-string
                            (read-string (format "search google with [%s]: " search-string) nil nil search-string))
                      (call-process "bash" nil nil nil "googleemacs.sh" search-string)
                      ))))

(defun wiki-local-bhj ()
  (interactive)
  (let 
      ((search-string (current-word))) 
    (progn     
      (setq search-string
            (read-string (format "search local wiki with [%s]: " search-string) nil nil search-string))
      (call-process "bash" nil nil nil "local-wiki.sh" search-string)
      )))

(standard-display-ascii ?\221 "\`")
(standard-display-ascii ?\222 "\'")
(standard-display-ascii ?\223 "\"")
(standard-display-ascii ?\224 "\"")
(standard-display-ascii ?\227 "\-")
(standard-display-ascii ?\225 "\*")

(keydef "C-S-g" (progn (setq grep-buffers-buffer-name "*grep-buffers*")(grep-buffers)))

(defun bhj-clt-insert-file-name ()
  (interactive)
  (let ((prev-buffer (other-buffer (current-buffer) t)))

    (insert 
     (if (buffer-file-name prev-buffer)
         (replace-regexp-in-string ".*/" "" (buffer-file-name prev-buffer))
       (buffer-name prev-buffer)))))


(defun bhj-insert-pwdw ()
  (interactive)
  (insert "'")
  (call-process "cygpath" nil t nil "-alw" default-directory)
  (backward-delete-char 1)
  (insert "'"))

(defun bhj-insert-pwdu ()
  (interactive)
  (insert "'")
  (insert 
   (replace-regexp-in-string
    "^/.?scp:.*?@.*?:" "" 
    (expand-file-name default-directory)))
  (insert "'"))

;; old time motorola usage
;; (defcustom bhj-clt-branch "dbg_zch68_a22242_ringtone-hx11i"
;;   "the cleartool branch to use for mkbranch")

;; (defun bhj-clt-insert-branch ()
;;   (interactive)
;;   (insert bhj-clt-branch))
;; (define-key minibuffer-local-shell-command-map [(control meta b )] 'bhj-clt-insert-branch)

(define-key minibuffer-local-map [(control meta f)] 'bhj-clt-insert-file-name)

(define-key minibuffer-local-map [(control meta d )] 'bhj-insert-pwdu)

(defvar last-grep-marker nil)

(defvar cscope-marker-ring (make-ring 32)
  "Ring of markers which are locations from which cscope was invoked.")

(defvar cscope-marker-ring-poped (make-ring 32)
  "Ring of markers which are locations poped from cscope-marker-ring.")

(defun nodup-ring-insert (ring obj)
  (unless (and (not (ring-empty-p ring))
               (equal (ring-ref ring 0) obj))
    (ring-insert ring obj)))
;; (defcustom bhj-grep-default-directory "/pscp:a22242@10.194.131.91:/"
;;   "the default directory in which to run grep")
(keydef "M-g r" (progn
                  (let ((current-prefix-arg 4)
                        ;; (default-directory (eval bhj-grep-default-directory))
                        (grep-use-null-device nil))
                    (nodup-ring-insert cscope-marker-ring (point-marker))
                    (call-interactively 'grep))))

(defvar grep-find-file-history nil)

(defvar grep-rgrep-history nil)
    
(global-set-key [(meta n)] 'next-error)
(global-set-key [(meta p)] 'previous-error)

(setq my-grep-command "beagrep -e pat") ;; should not put it into custom, the custom will be read every time and so the `(let ((grep-command ..' scheme will fail

(global-set-key [(meta s) ?r] 
                (lambda ()
                  (interactive)
                  (let ((grep-history grep-rgrep-history)
                        (my-grep-command "rgrep -Hn -e pat")
                        (current-prefix-arg 4))
                    (nodup-ring-insert cscope-marker-ring (point-marker))
                    (call-interactively 'grep)
                    (setq grep-rgrep-history grep-history))))

(defvar grep-gtags-history nil)


(defun gtags-grep ()
  (interactive)
  (let ((grep-history grep-gtags-history)
	(my-grep-command "grep-gtags -e pat")
	(current-prefix-arg 4))
    (nodup-ring-insert cscope-marker-ring (point-marker))
    (let ((file (my-buffer-file-name (current-buffer))))
      (if (file-remote-p file)
	  (let ((process-environment tramp-remote-process-environment))
	    (setenv "GTAGS_START_FILE" (file-remote-p file 'localname))
	    (setq tramp-remote-process-environment process-environment))
	(setenv "GTAGS_START_FILE" file)))
    (call-interactively 'grep)
    (setq grep-gtags-history grep-history)))

(defun grep-tag-default-path ()
  (or (and transient-mark-mode mark-active
	   (/= (point) (mark))
	   (buffer-substring-no-properties (point) (mark)))
      (save-excursion
        (let* ((re "[^-a-zA-Z0-9._/]")
               (p1 (progn (search-backward-regexp re)
                          (if (looking-at "(")
                              (progn
                                (search-backward-regexp "\\." (line-beginning-position))
                                (prog1
                                    (1+ (point))
                                  (search-forward-regexp "(")))
                            (1+ (point)))))
               (p2 (progn (forward-char)
                          (search-forward-regexp re)
                          (backward-char)
                          (if (looking-at ":[0-9]+")
                              (progn
                                (forward-char)
                                (search-forward-regexp "[^0-9]")
                                (1- (point)))
                            (point)))))
          (buffer-substring-no-properties p1 p2)))))

(global-set-key [(meta s) ?p] 'grep-find-file)
(defun grep-find-file ()
  (interactive)
  (let ((grep-history grep-find-file-history)
	(my-grep-command "beagrep -f -e pat")
	(current-prefix-arg 4))
    (flet ((grep-tag-default () (grep-tag-default-path)))
      (nodup-ring-insert cscope-marker-ring (point-marker))
      (call-interactively 'grep)
      (setq grep-find-file-history grep-history))))

(global-set-key [(control meta o)] 'bhj-occur)
(global-set-key (kbd "M-g o") 'bhj-occur)

(defvar bhj-occur-regexp nil)
(defun bhj-occur ()
  (interactive)
  
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
	(setq regexp (replace-regexp-in-string "\\([][^$*?\\\\.+]\\)" "\\\\\\1" regexp)))

      (setq regexp 
	    (read-shell-command "List lines matching regexp: " regexp))
      (if (eq major-mode 'antlr-mode)
          (let ((occur-excluded-properties t))
            (occur regexp))
        (occur regexp)))))

(defun bhj-occur-make-errors ()
  (interactive)
  (let ((bhj-occur-regexp "no rule to\\|failed\\|errors\\|error:"))
    (call-interactively 'bhj-occur)))


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

(defun bhj-isearch-from-bod (&optional col-indent)
  (interactive "p")
  (let ((word (current-word)))
    (nodup-ring-insert cscope-marker-ring (point-marker))
    (bhj-c-beginning-of-defun)
    (setq regexp-search-ring (cons (concat "\\b" word "\\b") regexp-search-ring))
    (search-forward-regexp (concat "\\b" word "\\b"))))

(global-set-key [(shift meta s)] 'bhj-isearch-from-bod)

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

(defun bhj-w3m-scroll-down-or-previous-url ()
  (interactive)
  (if (pos-visible-in-window-p (point-min))
      (save-excursion
        (end-of-buffer)
         (search-backward-regexp "上一\\|上章")
         (call-interactively 'w3m-view-this-url))
    (call-interactively 'w3m-scroll-down-or-previous-url)))



(add-hook 'w3m-mode-hook 
          (lambda () 
            (local-set-key [(left)] 'backward-char)
            (local-set-key [(right)] 'forward-char)
            (local-set-key [(\ )] 'bhj-w3m-scroll-up-or-next-url)
            (local-set-key [(backspace)] 'bhj-w3m-scroll-down-or-previous-url)
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
 '(auth-sources (quote ((:source "~/.authinfo" :host t :protocol t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/tmp"))))
 '(canlock-password "78f140821d1f56625e4e7e035f37d6d06711d112")
 '(case-fold-search t)
 '(delete-old-versions t)
 '(describe-char-unidata-list (quote (name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value mirrored old-name iso-10646-comment uppercase lowercase titlecase)))
 '(dictem-server "localhost")
 '(dictionary-server "localhost")
 '(ecomplete-database-file-coding-system (quote utf-8))
 '(emacs-lisp-mode-hook (quote ((lambda nil (make-local-variable (quote cscope-symbol-chars)) (setq cscope-symbol-chars "-A-Za-z0-9_")))))
 '(font-lock-maximum-decoration 2)
 '(gdb-find-source-frame t)
 '(gdb-same-frame t)
 '(gdb-show-main t)
 '(gnus-ignored-newsgroups "")
 '(grep-use-null-device nil)
 '(htmlize-output-type (quote font))
 '(ido-enable-regexp t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".*\\.\\(loc\\|org\\|mkelem\\)")))
 '(ispell-program-name "aspell" t)
 '(jira-url "http://192.168.11.11/jira/rpc/xmlrc")
 '(keyboard-coding-system (quote cp936))
 '(lisp-mode-hook (quote ((lambda nil (make-local-variable (quote cscope-symbol-chars)) (setq cscope-symbol-chars "-A-Za-z0-9_")))))
 '(longlines-auto-wrap nil)
 '(makefile-mode-hook (quote ((lambda nil (make-local-variable (quote cscope-symbol-chars)) (setq cscope-symbol-chars "-A-Za-z0-9_")))))
 '(message-dont-reply-to-names (quote (".*haojun.*")))
 '(message-mail-alias-type nil)
 '(mm-text-html-renderer (quote w3m))
 '(nnmail-expiry-wait (quote never))
 '(normal-erase-is-backspace nil)
 '(org2blog/wp-confirm-post t)
 '(org2blog/wp-use-tags-as-categories t)
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((c-style . whitesmith) (major-mode . sh-mode) (py-indent-offset . 4) (sh-indentation . 2) (c-font-lock-extra-types "FILE" "bool" "language" "linebuffer" "fdesc" "node" "regexp") (TeX-master . t) (indent-tab-mode . t))))
 '(save-place t nil (saveplace))
 '(senator-minor-mode-hook (quote (ignore)))
 '(session-initialize (quote (de-saveplace session places keys menus)) nil (session))
 '(session-use-package t nil (session))
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote parenthesis))
 '(split-width-threshold 800)
 '(starttls-use-gnutls t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tooltip-mode nil)
 '(tooltip-use-echo-area t)
 '(tramp-remote-path (quote (tramp-own-remote-path tramp-default-remote-path "/usr/sbin" "/usr/local/bin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin")))
 '(tramp-syntax (quote ftp))
 '(tramp-verbose 0)
 '(transient-mark-mode t)
 '(w32-symlinks-handle-shortcuts t)
 '(w32-use-w32-font-dialog nil)
 '(w3m-default-display-inline-images t)
 '(weblogger-config-alist (quote (("default" "https://storage.msn.com/storageservice/MetaWeblog.rpc" "thomasbhj" "" "MyBlog") ("csdn" "http://blog.csdn.net/flowermonk/services/MetaBlogApi.aspx" "flowermonk" "" "814038"))))
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/local/man")))
 '(woman-use-own-frame nil)
 '(x-select-enable-clipboard t))




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
          (all-letou t)
          (start-pos 0))

      (while (and all-letou (string-match "@" receivers start-pos))
        (setq start-pos (match-end 0))
        (unless (equal (string-match 
                    "@adsnexus.com\\|@eee168.com\\|@rayzerlink.com\\|@hzwowpad.com" 
                    receivers 
                    (1- start-pos))
                   (1- start-pos))
          (setq all-letou nil)))

      (when all-letou
        (save-excursion
          (message-goto-from)
          (message-beginning-of-line)
          (kill-line)
          (insert "Bao Haojun at Letou <hjbao@eee168.com>"))))))

(add-hook 'message-send-hook 'bhj-set-reply)

(require 'electric-complete)

(require 'xcscope)

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
  (call-process "/bin/bash" nil nil nil "jdkhelp.sh" jdk-word)
  (w3m-goto-url "file:///d/knowledge/jdk-6u18-docs/1.html"))
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


(setenv "EMACS" "t")
(define-key weblogger-entry-mode-map "\C-c\C-k" 'ido-kill-buffer)

(defun poor-mans-csharp-mode ()
  (csharp-mode)
  (setq mode-name "C#")
  (set-variable 'tab-width 8)
  (set-variable 'indent-tabs-mode t)
  (set-variable 'c-basic-offset 8)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'case-label 0)
)

(setq auto-mode-alist (append '(("\\.cs\\'" . poor-mans-csharp-mode))
			      auto-mode-alist))
(require 'saveplace)

(require 'color-theme)
(condition-case nil
    (progn
      (color-theme-initialize)
      (require 'color-theme-library))
  (error nil))
(color-theme-arjen)
(server-start)
;(w32-register-hot-key [A-tab])

(defun markdown-nobreak-p ()
  "Returns nil if it is ok for fill-paragraph to insert a line
  break at point"
  ;; are we inside in square brackets
  (or (looking-back "\\[[^]]*")
      (save-excursion
        (beginning-of-line)
        (looking-at "    \\|\t"))))

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

;; becase we are putting our database under ~/tmp/for-code-reading/, 
;; we need to redefine this function:
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
      (setq directory (concat (getenv "HOME") "/tmp/for-code-reading/" (cscope-canonicalize-directory directory))
	    this-directory directory)
      (while this-directory
	(when (or (file-exists-p (concat remote-prefix this-directory cscope-database-file))
                  (file-exists-p (concat remote-prefix this-directory cscope-index-file)))
          (progn
            (setq database-dir (substring
                                this-directory 
                                (length
                                 (concat (getenv "HOME") "/tmp/for-code-reading/"))))
            (throw 'done (concat "" database-dir))))
	(when (string-match "^\\(/\\|[A-Za-z]:[\\/]\\)$" this-directory)
            (throw 'done (concat "" (expand-file-name "~/.gtags-dir/"))))
	(setq this-directory (file-name-as-directory
			      (file-name-directory
			       (directory-file-name this-directory))))
	))
    ))

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

(defconst emacs-mode-ctags-lang-map
  '(("emacs-lisp" . "lisp")
    ("c" . "c++")
    ("csharp" . "C#")))

(defconst emacs-mode-ctags-tag-filter
  '(("c" . "| perl -ne '@f = split; print unless $f[1] =~ m/^member|macro$/'")
    ("php" . "| perl -ne '@f = split; print unless $f[1] =~ m/^variable$/'")))


(defun tag-this-file (&optional output-buf)
  (interactive)
  (save-excursion
    (save-window-excursion
      (shell-command
       (let ((mode-name-minus-mode 
              (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
         (concat "ctags-exuberant --language-force="
                 (shell-quote-argument 
                  (or (cdr (assoc mode-name-minus-mode emacs-mode-ctags-lang-map))
                      mode-name-minus-mode))
                 " -xu "
                 (shell-quote-argument (format "%s" (my-buffer-file-name-local (current-buffer))))
                 (cdr (assoc mode-name-minus-mode emacs-mode-ctags-tag-filter))))
       output-buf))))

(defvar grep-func-call-history nil)
(defun grep-func-call ()
  (interactive)
  (let ((grep-history grep-func-call-history)
	(my-grep-command "grep-func-call -e pat")
	(current-prefix-arg 4))
    (nodup-ring-insert cscope-marker-ring (point-marker))
    (let ((file (my-buffer-file-name (current-buffer)))
	  (mode-name-minus-mode
	   (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
      (if (file-remote-p file)
	  (let ((process-environment tramp-remote-process-environment))
	    (setenv "GTAGS_START_FILE" (file-remote-p file 'localname))
	    (setenv "GTAGS_LANG_FORCE" (or (cdr (assoc mode-name-minus-mode emacs-mode-ctags-lang-map))
					   mode-name-minus-mode))
	    (setq tramp-remote-process-environment process-environment))
	(setenv "GTAGS_START_FILE" file)
	(setenv "GTAGS_LANG_FORCE" (or (cdr (assoc mode-name-minus-mode emacs-mode-ctags-lang-map))
				       mode-name-minus-mode))))
    (call-interactively 'grep)
    (setq grep-func-call-history grep-history)))

(defun code-line-number-from-tag-line (line)
  (goto-line line)
  (let ((subs (split-string (current-line-string))))
    (string-to-number
     (if (string-equal (car subs) "operator")
	 (cadddr subs) ;operator +=      function    183 /home...
       (caddr subs))))) ;region_iterator  struct      189 /home...

(defun ctags-beginning-of-defun (&optional arg)
  (interactive "^p")
  (goto-line 
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
             (code-line-number-from-tag-line (- mid -1 (or arg 1))))))))))
                    
                                     
(global-set-key [(control x) (w)] 'where-are-we)

(defvar code-reading-file "~/.code-reading")

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

(global-set-key [(control x) (c)] 'visit-code-reading)
             

  ;; if it's on a `error' line, i.e. entry 0 in the following, it
  ;; means we are actually on 10th entry, we need go to entry 9

  ;; if we are on entry 1, then we need call `prev-error'.

    ;; 0 /usr/share/pyshared/html2text.py:270:                     if a:
    ;; 1     class _html2text(sgmllib.SGMLParser):
    ;; 2         ...
    ;; 3         def handle_tag(self, tag, attrs, start):
    ;; 4             ...
    ;; 5             if tag == "a":
    ;; 6                 ...
    ;; 7                 else:
    ;; 8                     if self.astack:
    ;; 9                         ...
    ;; 10 =>                      if a:
  
(defmacro current-line-string ()
 `(buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

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

(defvar waw-mode-map nil
  "Keymap for where-are-we-mode.")

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

(setq waw-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "\C-m" 'waw-ret-key)
        (define-key map [(return)] 'waw-ret-key)
        (define-key map [(meta p)] 'previous-error-no-select)
        (define-key map [(meta n)] 'next-error-no-select)
        map))

(put 'waw-mode 'mode-class 'special)
(defun waw-mode ()
  "Major mode for output from \\[where-are-we]."
  (interactive)
  (kill-all-local-variables)
  (use-local-map waw-mode-map)
  (setq major-mode 'waw-mode)
  (setq mode-name "Where-are-we")
  (setq next-error-function 'waw-next-error)
  (run-mode-hooks 'waw-mode-hook))

(defun java-bt-ret-key ()
  (interactive)
  (let ((start-line-str (current-line-string)))
    (if (string-match "(.*:[0-9]+)" start-line-str)
        (next-error 0))))

(defvar java-bt-mode-map nil
  "Keymap for java-bt-mode.")

(defvar java-bt-tag-alist nil
  "backtrace/code tag alist.")

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

(setq java-bt-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "\C-m" 'java-bt-ret-key)
        (define-key map [(return)] 'java-bt-ret-key)
        (define-key map [(meta p)] 'previous-error-no-select)
        (define-key map [(meta n)] 'next-error-no-select)
        map))

(put 'java-bt-mode 'mode-class 'special)
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

(defvar boe-default-indent-col 0)
(make-variable-buffer-local 'boe-default-indent-col)

(defun indent-same-space-as-prev-line ()
  (interactive)
  (let* ((col1 (current-column))
         (col2 (save-excursion
                 (if (eolp)
                     col1
                   (search-forward-regexp "\\S " (line-end-position))
                   (current-column))))
         (col2- (save-excursion
                  (forward-line -1)
                  (move-to-column col1)
                  (when (looking-at "\\S ")
                    (search-forward-regexp "\\s "))
                  (search-forward-regexp "\\S " (line-end-position))
                  (current-column))))
    (delete-region col1 col2)
    (insert (make-string (- col2- col1 1) ? ))))

(global-set-key [(meta shift ? )] 'indent-same-space-as-prev-line)
              
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

(defun my-buffer-file-name-local (buf)
  (let ((name (my-buffer-file-name buf)))
    (or (file-remote-p name 'localname)
        name)))


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

(defun switch-buffer-same-filename-rev ()
  (interactive)
  (switch-buffer-same-filename t))

(global-set-key [(meta s) ?c] 'switch-buffer-same-filename)
(global-set-key [(ctrl x) ? ] 'switch-buffer-same-filename)
(global-set-key [(ctrl x) ?\S- ] 'switch-buffer-same-filename-rev)

(defcustom remote-sudo-prefix "/scp:root@localhost:"
  "The prefix for visiting a file's remote counterpart or with sudo permission")

(defun sudoedit ()
  (interactive)
  (if (file-remote-p (buffer-file-name))
      (find-alternate-file (replace-regexp-in-string "^/scp:.*?:" "" (buffer-file-name)))    
    (find-alternate-file (concat remote-sudo-prefix (buffer-file-name)))))

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


(setq w3m-fill-column 100)
(require 'guess-offset)
(setq org-agenda-files '( "~/notes"))
(setq org-log-done 'note)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-;") 'query-replace-regexp)
(global-set-key [(control x) (control z)] 'keyboard-quit)
(global-set-key (kbd "s-h") help-map)
(global-set-key (kbd "C-{") 'beginning-of-buffer)
(global-set-key (kbd "C-}") 'end-of-buffer)
(global-set-key (kbd "M-.") 'gtags-grep)
(global-set-key (kbd "C-.") 'gtags-grep)
(global-set-key (kbd "M-s f") 'grep-func-call)
(global-set-key (kbd "M-s e") 'bhj-occur-make-errors)
;(global-set-key [?\C-,] (lookup-key global-map [?\C-x]))
(eval-after-load "diff-mode"
  '(define-key diff-mode-map (kbd "M-g") (lookup-key global-map (kbd "M-g"))))
(global-set-key [?\C-'] 'hippie-expand)
(keyboard-translate ?\C-x ?\C-h)
(keyboard-translate ?\C-h ?\C-x)
;(global-set-key (kbd "C-h") 'execute-extended-command)

(require 'bbdb-autoloads)
(require 'bbdb)
(load "bbdb-com" t)
(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
;; (bbdb-insinuate-sc)
(setq bbdb-north-american-phone-numbers nil)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
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
	("From" . "linkedin.com")
	("From" . "bear.eee168.com")
	))
(setq bbdb-notice-hook (quote (bbdb-auto-notes-hook)))

(setq bbdb-file "~/windows-config/.bbdb")
(add-hook 'gnus-select-group-hook
	  (lambda ()
	    (make-local-variable 'bbdb/news-auto-create-p)
	    (make-local-variable 'bbdb/mail-auto-create-p)
	    (if (string-match "^\\*Summary nntp" (buffer-name))
		(setq bbdb/news-auto-create-p nil bbdb/mail-auto-create-p nil)
	      (setq bbdb/news-auto-create-p 'bbdb-ignore-some-messages-hook bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook))))
;; (defun bbdb-bhj-unify-eee168 (record)
;;   (bbdb-record-putprop record 'net (replace-regexp-in-string "@adsnexus.com\\|@eee168.com" "@eee168.com" net)))

(setq bbdb/gnus-update-records-mode '(if (and (boundp 'auto-create-p) (null auto-create-p))
					 (progn
					   (message "hello searching")
					   'searching)
				       (message "hello annotating")
				       'annotating))

(defun my-bbdb-canonicalize (addr)
  (replace-regexp-in-string "@adsnexus.com\\|@eee168.com" "@eee168.com" net))
(setq bbdb-canonicalize-net-hook
      'my-bbdb-canonicalize)


;; (setq bbdb-create-hook '(bbdb-creation-date-hook bbdb-bhj-unify-eee168))
;; (letf* ((old-all-completions (symbol-function 'all-completions))
;; 	  (old-try-completion (symbol-function 'try-completion)))

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
         :base-directory "~/windows-config/org/"
         :publishing-directory "~/public_html"
         :section-numbers nil
         :table-of-contents nil)))

