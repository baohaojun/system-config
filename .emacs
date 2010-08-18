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

(require 'muse-mode)     ; load authoring mode
(require 'muse-html)     ; load publishing styles I use
(require 'muse-project)
(setq muse-project-alist
      '(("Website" ("~/Pages" :default "index")
         (:base "html" :path "/scp:bhj@192.168.0.46:/var/www/rayzer_doc"))))

(setq muse-project-alist
      '(("rayzer_doc" ("~/rayzer_doc" :default "index")
         (:base "html" :path "/scp:bhj@192.168.0.46:/var/www/rayzer_doc"))))

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

(autoload 'sdim-use-package "sdim" "Shadow dance input method")
(register-input-method
 "sdim" "euc-cn" 'sdim-use-package "影舞笔")

(setq default-input-method "sdim")


(let ((x "~/.emacs_d/UnicodeData.txt"))
  (when (file-exists-p x)
    (setq describe-char-unicodedata-file x)))


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
  (c-set-style "k&r")
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (c-set-offset 'innamespace 0)
  (setq c-basic-offset 4))

(defun linux-c++-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c++-mode) 
  (local-set-key [?\C-\M-a] 'bhj-c-beginning-of-defun)
  (local-set-key [?\C-\M-e] 'bhj-c-end-of-defun)
  (local-set-key [?\C-c ?\C-d] 'c-down-conditional)
  (c-set-style "k&r")
  (setq tab-width 4)
  (c-set-offset 'innamespace 0)
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

(put 'narrow-to-region 'disabled nil)

(define-key global-map [(meta .)] 'cscope-find-global-definition)
(define-key global-map [(meta control ,)] 'cscope-pop-mark)
(define-key global-map [(meta control .)] 'cscope-pop-mark-back)

(eval-after-load "gtags" '(progn 
                            (define-key gtags-mode-map [(meta *)] 'cscope-pop-mark)
                            (define-key gtags-select-mode-map [(meta *)] 'cscope-pop-mark)))

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

(standard-display-ascii ?\221 "\`")
(standard-display-ascii ?\222 "\'")
(standard-display-ascii ?\223 "\"")
(standard-display-ascii ?\224 "\"")
(standard-display-ascii ?\227 "\-")
(standard-display-ascii ?\225 "\*")

(defvar last-error-from-cscope nil)
(defun bhj-next-error ()
  (interactive)
  (if
      (or
       (string-equal (buffer-name) "*cscope*")
       (string-equal (buffer-name (window-buffer (next-window))) "*cscope*")
       (and (not (next-error-buffer-p (current-buffer)))
            (not (next-error-buffer-p (window-buffer (next-window))))
            last-error-from-cscope))
      (progn
        (cscope-next-symbol)
        (setq last-error-from-cscope t))
    (setq last-error-from-cscope nil)
    (next-error)
    (delete-other-windows)
    (with-current-buffer next-error-last-buffer
      (message "%s" (buffer-substring (line-beginning-position) (line-end-position))))))

(defun bhj-previous-error ()
  (interactive)
  (if (or
       (string-equal (buffer-name) "*cscope*")
       (string-equal (buffer-name (window-buffer (next-window))) "*cscope*")
       (and (not (next-error-buffer-p (current-buffer)))
            (not (next-error-buffer-p (window-buffer (next-window))))
            last-error-from-cscope))
      (progn
        (cscope-prev-symbol)
        (setq last-error-from-cscope t))
    (setq last-error-from-cscope nil)
    (previous-error)
    (delete-other-windows)
    (with-current-buffer next-error-last-buffer
      (message "%s" (buffer-substring (line-beginning-position) (line-end-position))))))

(global-set-key [(meta n)] 'bhj-next-error)
(global-set-key [(meta p)] 'bhj-previous-error)



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
  (if (eq system-type 'cygwin)
      (insert 
       (replace-regexp-in-string
        "^/.?scp:.*?@.*?:" "" 
        (expand-file-name default-directory)))
    (insert default-directory))
  (insert "'"))

;; old time motorola usage
;; (defcustom bhj-clt-branch "dbg_zch68_a22242_ringtone-hx11i"
;;   "the cleartool branch to use for mkbranch")

;; (defun bhj-clt-insert-branch ()
;;   (interactive)
;;   (insert bhj-clt-branch))
;; (define-key minibuffer-local-shell-command-map [(control meta b )] 'bhj-clt-insert-branch)

(define-key minibuffer-local-map [(control meta f)] 'bhj-clt-insert-file-name)

(define-key minibuffer-local-shell-command-map [(control meta d )] 'bhj-insert-pwdu)
(fset 'bhj-clt-co-mkbranch
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217761 99 108 116 101 110 118 32 126 47 98 105 110 47 99 108 116 45 99 111 45 109 107 98 114 97 110 99 104 32 134217730 32 134217734 return] 0 "%d")) arg) (call-interactively 'bhj-reread-file)))




(defvar last-grep-marker nil)

(defvar cscope-marker-ring (make-ring 32)
  "Ring of markers which are locations from which cscope was invoked.")

(defvar cscope-marker-ring-poped (make-ring 32)
  "Ring of markers which are locations poped from cscope-marker-ring.")

;; (defcustom bhj-grep-default-directory "/pscp:a22242@10.194.131.91:/"
;;   "the default directory in which to run grep")
(keydef "C-M-g" (progn
                  (let ((current-prefix-arg 4)
                        ;; (default-directory (eval bhj-grep-default-directory))
                        (grep-use-null-device nil))
                    (ring-insert cscope-marker-ring (point-marker))
                    (call-interactively 'grep))))

(defvar grep-find-file-history nil)

(defvar grep-rgrep-history nil)
    

(global-set-key [(meta s) ?r] 
                (lambda ()
                  (interactive)
                  (let ((grep-history grep-rgrep-history)
                        (current-prefix-arg 4))
                    (ring-insert cscope-marker-ring (point-marker))
                    (call-interactively 'grep)
                    (setq grep-rgrep-history grep-history))))

(global-set-key [(meta s) ?p] 
                (lambda ()
                  (interactive)
                  (let ((grep-history grep-find-file-history)
                        (current-prefix-arg 4))
                    (ring-insert cscope-marker-ring (point-marker))
                    (call-interactively 'grep)
                    (setq grep-find-file-history grep-history))))
                    

(global-set-key [(control meta o)]
                (lambda()(interactive)
                  (let 
                      ((regexp (if mark-active 
                                   (buffer-substring-no-properties (region-beginning)
                                                                   (region-end))
                                 (current-word))))
                    (progn     
                      (ring-insert cscope-marker-ring (point-marker))
                      (setq regexp 
                            (read-string (format "List lines matching regexp [%s]: " regexp) nil nil regexp))
                      (if (eq major-mode 'antlr-mode)
                          (let ((occur-excluded-properties t))
                            (occur regexp))
                        (occur regexp))))))


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
 '(auth-sources (quote ((:source "~/.authinfo" :host t :protocol t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/tmp"))))
 '(canlock-password "78f140821d1f56625e4e7e035f37d6d06711d112")
 '(case-fold-search t)
 '(cscope-display-cscope-buffer t)
 '(cscope-do-not-update-database t)
 '(cscope-program "gtags-cscope-bhj")
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
 '(grep-command "beagle-grep.sh -e")
 '(grep-use-null-device nil)
 '(htmlize-output-type (quote font))
 '(ido-enable-regexp t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".*\\.\\(loc\\|org\\|mkelem\\)")))
 '(indent-tabs-mode nil)
 '(ispell-program-name "aspell")
 '(keyboard-coding-system (quote cp936))
 '(lisp-mode-hook (quote ((lambda nil (make-local-variable (quote cscope-symbol-chars)) (setq cscope-symbol-chars "-A-Za-z0-9_")))))
 '(longlines-auto-wrap nil)
 '(makefile-mode-hook (quote ((lambda nil (make-local-variable (quote cscope-symbol-chars)) (setq cscope-symbol-chars "-A-Za-z0-9_")))))
 '(message-dont-reply-to-names (quote (".*haojun.*")))
 '(message-mail-alias-type (quote ecomplete))
 '(mm-text-html-renderer (quote w3m))
 '(muse-html-charset-default "utf-8")
 '(muse-publish-date-format "%m/%e/%Y")
 '(nnmail-expiry-wait (quote never))
 '(normal-erase-is-backspace nil)
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((c-style . whitesmith) (major-mode . sh-mode) (py-indent-offset . 4) (sh-indentation . 2) (c-font-lock-extra-types "FILE" "bool" "language" "linebuffer" "fdesc" "node" "regexp") (TeX-master . t) (indent-tab-mode . t))))
 '(save-place t nil (saveplace))
 '(senator-minor-mode-hook (quote (ignore)))
 '(session-initialize (quote (de-saveplace session places keys menus)) nil (session))
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote parenthesis))
 '(starttls-use-gnutls t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tooltip-mode nil)
 '(tooltip-use-echo-area t)
 '(tramp-syntax (quote ftp))
 '(tramp-verbose 0)
 '(transient-mark-mode t)
 '(w32-symlinks-handle-shortcuts t)
 '(w32-use-w32-font-dialog nil)
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

(global-set-key [(control meta /)] 'skeleton-display-abbrev)


(defun skeleton-display-abbrev (&optional choose)
  "Display the next possible abbrev for the text before point."
  (interactive (list t))
  (when (looking-back "\\w\\|_" 1)
    (let* ((end (point))
           (start (save-excursion
                    (search-backward-regexp "\\(\\_<.*?\\)")))
           (word (when start (buffer-substring-no-properties start end)))
           (match (when (and word
                             (not (zerop (length word))))
                    (skeleton-display-matches word choose))))
      (when (and choose match)
        (delete-region start end)
        (insert match)))))

(defun skeleton-display-matches (word &optional choose)
  (let* ((strlist (nreverse (skeleton-get-matches-order word)))
         (matches (concat 
                   (mapconcat 'identity (delete word (delete-dups strlist)) "\n")
                   "\n"))
	 (line 0)
	 (max-lines (when matches (- (length (split-string matches "\n")) 2)))
	 (message-log-max nil)
	 command highlight)
    (if (not matches)
	(progn
	  (message "No skeleton matches")
	  nil)
      (if (= max-lines 0)
          (nth line (split-string matches "\n"))
      (if (not choose)
	  (progn
	    (message "%s" matches)
	    nil)
	(setq highlight (ecomplete-highlight-match-line matches line))
	(while (not (memq (setq command (read-event highlight)) '(? return)))
	  (cond
	   ((eq command ?\M-n)
	    (setq line (% (1+ line) (1+ max-lines))))
	   ((eq command ?\M-p)
	    (setq line (% (+ max-lines line) (1+ max-lines)))))
	  (setq highlight (ecomplete-highlight-match-line matches line)))
	(when (eq command 'return)
	  (nth line (split-string matches "\n"))))))))
    
(defun skeleton-get-matches-order (skeleton)
  "get all the words that contains every character of the SKELETON from the current buffer"
  (let ((skeleton-re (mapconcat 'string skeleton ".*")))
    (save-excursion
      (beginning-of-buffer)
      (setq strlist nil)
      (while (not (eobp))
        (setq origpt (point))
        (if (setq endpt (re-search-forward "\\(\\_<.*?\\_>\\)" nil t))
            (let ((substr (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
              (when (string-match skeleton-re substr)
                (setq strlist (cons substr strlist))))
          (end-of-buffer)))
      strlist)))

(defun skeleton-get-matches-no-order (skeleton)
  "get all the words that contains every character of the SKELETON from the current buffer"
  (let ((skeleton-list (string-to-list skeleton)))
    (save-excursion
      (beginning-of-buffer)
      (setq strlist nil)
      (while (not (eobp))
        (setq origpt (point))
        (if (setq endpt (re-search-forward "\\(\\_<.*?\\_>\\)" nil t))
            (let ((substr (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
                  (skeleton-list skeleton-list)
                  (match-ok t))
              (while skeleton-list
                (setq c (car skeleton-list)
                      skeleton-list (cdr skeleton-list))
                (unless (string-match (string c) substr)
                  (setq match-ok nil
                        skeleton-list nil)))
              (when match-ok
                (setq strlist (cons substr strlist))))
          (end-of-buffer)))
      strlist)))

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


(setenv "IN_EMACS" "true")
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
(desktop-save-mode 1)
(require 'saveplace)
(setq-default save-place t)
(require 'color-theme)
(condition-case nil
    (color-theme-initialize)
  (error nil))
(color-theme-arjen)
(server-start)
;(w32-register-hot-key [A-tab])
(setq org-publish-project-alist
      '(("org"
         :base-directory "~/windows-config/org/"
         :publishing-directory "~/public_html"
         :section-numbers nil
         :table-of-contents nil
         :style "<link rel=\"stylesheet\"
                     href=\"mystyle.css\"
                     type=\"text/css\">")))



(defun markdown-nobreak-p ()
  "Returns nil if it is ok for fill-paragraph to insert a line
  break at point"
  ;; are we inside in square brackets
  (or (looking-back "\\[[^]]*")
      (save-excursion
        (beginning-of-line)
        (looking-at "    \\|\t"))))


(setq weblogger-pre-struct-hook
      (list
       (lambda ()
         (interactive)
         (message-goto-body)
         (longlines-mode 0)
         (auto-fill-mode 0)
         (shell-command-on-region
          (point) (point-max) "markdown" nil t nil nil)
         (flet ((longlines-wrap-region (beg end) nil)) 
           (longlines-mode 1)
           (auto-fill-mode 1)
           (add-hook 'fill-nobreak-predicate 'markdown-nobreak-p))
         )))

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
         (longlines-mode 0)
         (auto-fill-mode 0)
         (shell-command-on-region
          (point) (point-max) "unmarkdown" nil t nil nil)
         (set-buffer-modified-p nil)
         (auto-fill-mode 0)
         (flet ((longlines-wrap-region (beg end) nil)) 
           (longlines-mode 1)
           (auto-fill-mode 1)
           (add-hook 'fill-nobreak-predicate 'markdown-nobreak-p)))))

(setq weblogger-pre-setup-headers-hook
      (list
       (lambda ()
         (interactive)
         (longlines-mode 0)
         (auto-fill-mode 0))))

(setq weblogger-post-setup-headers-hook
      (list
       (lambda ()
         (interactive)
         (flet ((longlines-wrap-region (beg end) nil)) 
           (longlines-mode 1)
           (auto-fill-mode 1)
           (add-hook 'fill-nobreak-predicate 'markdown-nobreak-p))
         (set-buffer-modified-p nil))))

(require 'longlines)

(defun longlines-encode-region (beg end &optional buffer)
  "Replace each soft newline between BEG and END with exactly one space.
Hard newlines are left intact.  The optional argument BUFFER exists for
compatibility with `format-alist', and is ignored."
  (save-excursion
    (let ((reg-max (max beg end))
          (mod (buffer-modified-p)))

      (goto-char (min beg end))

      (while (search-forward-regexp "\n *" reg-max t)
        (let ((cur-char-pos (match-beginning 0)))

          (unless (get-text-property cur-char-pos 'hard)
            (if (or 
                 (aref (char-category-set
                        (or (char-after (match-end 0))
                            ? )) 
                       ?c)
                 (aref (char-category-set 
                        (or 
                         (char-before cur-char-pos)
                         ? )) 
                       ?c))
                (replace-match "")
              (replace-match " ")))

      (set-buffer-modified-p mod)
      end)))))

(defun longlines-encode-string (str1)
  "Return a copy of STRING with each soft newline replaced by a space.
     Hard newlines are left intact."
  (let* ((str2 (make-string (length str1) ?\0))
        (mstart (string-match "\n *" str1))
        (mend (match-end 0))
        (lastpos 0)
        (str2pos 0))

    (while (and mstart mend (< (1+ mend) (length str1)))
      (when (null (get-text-property mstart 'hard str1))
        (let ((substr (substring str1 lastpos mstart)))
          (store-substring str2 str2pos substr)
          (setq str2pos (+ str2pos (length substr)))

          (unless (or (aref (char-category-set (aref str1 (1+ (match-end 0)))) ?c)
                      (aref (char-category-set (aref str1 (1- mstart))) ?c))
            (aset str2 str2pos ? )
            (setq str2pos (1+ str2pos)))))

      (setq lastpos (1+ mend))
      (setq mstart (string-match "\n *" str1 lastpos)))

    (let ((last_line (substring str1 lastpos)))
      (store-substring str2 str2pos last_line)
      (substring str2 0 (+ str2pos (length last_line))))
    ))

;; becase we are putting our database under ~/tmp/for-code-reading/, 
;; we need to redefine this function:
(defun cscope-search-directory-hierarchy (directory)
  "Look for a cscope database in the directory hierarchy.
Starting from DIRECTORY, look upwards for a cscope database."
  (let (this-directory database-dir)
    (catch 'done
      (if (file-regular-p directory)
	  (throw 'done directory))
      (setq directory (concat (getenv "HOME") "/tmp/for-code-reading/" (cscope-canonicalize-directory directory))
	    this-directory directory)
      (while this-directory
	(if (or (file-exists-p (concat this-directory cscope-database-file))
		(file-exists-p (concat this-directory cscope-index-file)))
	    (progn
	      (setq database-dir (substring
                                  this-directory 
                                  (length
                                   (concat (getenv "HOME") "/tmp/for-code-reading/"))))
	      (throw 'done database-dir)
	      )
          )
	(if (string-match "^\\(/\\|[A-Za-z]:[\\/]\\)$" this-directory)
            (throw 'done (expand-file-name "~/.gtags-dir/")))
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
    (ring-insert cscope-marker-ring-poped (point-marker))

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
    (ring-insert cscope-marker-ring (point-marker))

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
