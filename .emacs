;; Red Hat Linux default .emacs initialization file  ; -*- mode: emacs-lisp -*-

(setq load-path
      (cons (expand-file-name "~/.emacs_d/lisp") load-path))

(when (eq system-type 'windows-nt)
  (let ((default-directory "d:/tools/emacs-site-lisp/")) (load-file "d:/tools/emacs-site-lisp/subdirs.el"))
  (if (file-exists-p "c:/cygwin/bin/bash.exe")
      (setq cygwin-drive "c:")
    (setq cygwin-drive "d:"))
  (setq cygwin-mount-cygwin-bin-directory (concat cygwin-drive "/cygwin/bin"))
  (setq load-path
	(cons "d:/tools/emacs-site-lisp/" load-path))
  (add-to-list 'load-path "~/bin/windows/gnuserv")
  (require 'cygwin-mount)
  (global-set-key[(f2)](lambda()(interactive)(call-process "bash" nil nil nil "~/bin/windows/ehelp" (current-word))))
  (global-set-key[(f3)](lambda()(interactive)(call-process "bash" nil nil nil "~/bin/windows/ehelph2" (current-word))))
  (defsubst nsi-point (position)
    "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:
  bol  -- beginning of line    eol  -- end of line           bod  -- beginning of def or class
  eod  -- end of def or class  bob  -- beginning of buffer   eob  -- end of buffer
  boi  -- back to indentation  bos  -- beginning of statement
This function does not modify point or mark."
    (let ((here (point)))
      (cond
       ((eq position 'bol) (beginning-of-line))
       ((eq position 'eol) (end-of-line))
       ((eq position 'bod) (nsi-beginning-of-def-or-class))
       ((eq position 'eod) (nsi-end-of-def-or-class))
       ;; Kind of funny, I know, but useful for py-up-exception.
       ((eq position 'bob) (beginning-of-buffer))
       ((eq position 'eob) (end-of-buffer))
       ((eq position 'boi) (back-to-indentation))
       ((eq position 'bos) (nsi-goto-initial-line))
       (t (error "Unknown buffer position requested: %s" position))
       )
      (prog1
          (point)
        (goto-char here))))
  (autoload 'nsi-mode "nsi-mode" "nsi editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.nsi$" . nsi-mode))

  (put 'cygwin-mount-name-hook-function 'safe-magic t)
  (cygwin-mount-activate)
  (require 'w32-symlinks))


;; (set-frame-font "Microsoft Yahei-10")
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'han (font-spec :family "Microsoft Yahei" :size 13))
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'symbol (font-spec :family "Microsoft Yahei" :size 13))
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'cjk-misc (font-spec :family "Microsoft Yahei" :size 13))
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'bopomofo (font-spec :family "Microsoft Yahei" :size 13))

(set-frame-font "Monaco-10")
(set-face-font 'italic "-*-Courier New-normal-i-*-*-13-*-*-*-c-*-iso8859-1")
(set-face-font 'bold-italic "-*-Courier New-bold-i-*-*-13-*-*-*-c-*-iso8859-1")


(set-fontset-font (frame-parameter nil 'font)
                  'han (font-spec :family "Simsun" :size 13))
(set-fontset-font (frame-parameter nil 'font)
                  'symbol (font-spec :family "Simsun" :size 13))
(set-fontset-font (frame-parameter nil 'font)
                  'cjk-misc (font-spec :family "Simsun" :size 13))
(set-fontset-font (frame-parameter nil 'font)
                  'bopomofo (font-spec :family "Simsun" :size 13))

;; (set-fontset-font (frame-parameter nil 'font)
;;                   'han (font-spec :family "Simsun" :size 13))
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'symbol (font-spec :family "Simsun" :size 13))
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'cjk-misc (font-spec :family "Simsun" :size 13))
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'bopomofo (font-spec :family "Simsun" :size 13))

(add-to-list 'load-path "~/.emacs_d/weblogger")




;(require 'csharp-mode.el)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(require 'emms-setup)
(require 'w3m-load)
(require 'gnuserv)
(require 'tramp)
(require 'weblogger)
(require 'session)
(require 'ibuffer)
(require 'browse-kill-ring)
(require 'ido)
(require 'thumbs)
(require 'emacs-wiki)
(require 'keydef)
(require 'grep-buffers)
(require 'htmlize)
;(require 'htmlize-hack)

(require 'muse-mode)     ; load authoring mode

(require 'muse-html)     ; load publishing styles I use

(require 'muse-latex)

(require 'muse-project)

(setq muse-project-alist
      '(("Website" ("~/Pages" :default "index")
         (:base "html" :path "~/public_html")
         (:base "latex" :path "~/public_latex")
         (:base "pdf" :path "~/public_pdf"))))

(emms-standard)
(emms-default-players)

(setq-default abbrev-mode t)                                                                   
(read-abbrev-file "~/.abbrev_defs")                                                            
(setq save-abbrevs t)   






(setq null-device "/dev/null")

(grep-compute-defaults)
(defun grep-default-command ()
  "Compute the default grep command for C-u M-x grep to offer."
  (let ((tag-default (shell-quote-argument (grep-tag-default)))
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

(setq gnuserv-frame (selected-frame))





(register-input-method
 "chinese-wubi86" "Chinese-CNS" 'quail-use-package
 "WuBi" "WuBi"
 "quail/wubi86")

(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(load "preview-latex.el" nil t t)

; set unicode data file location. (used by what-cursor-position)
(let ((x "~/.emacs_d/UnicodeData.txt"))
  (when (file-exists-p x)
    (setq describe-char-unicodedata-file x)))


(setq weblogger-entry-mode-hook '(flyspell-mode))

(defcustom bhj-grep-regexp "^\\(?:\\s *\\)\\(?:.*/\\)*\\([^/]+?\\):\\([0-9]+?\\):\\(?:.+?\\)$"
  "the regexp for bhj grep")
(make-variable-buffer-local 'bhj-grep-regexp)
(defun bhj-grep-this-line ()
  (interactive)
  (save-excursion
    (setq bhj-grep-buffer-name (current-buffer))
    (beginning-of-line)
    (and (re-search-forward bhj-grep-regexp  nil t)

         (let ((buffer-of-match (match-string 1))
               (current-line (buffer-substring (line-beginning-position) (line-end-position)))
               (line-of-match (string-to-number (match-string 2))))
           (setq bhj-grep-point (point-marker))
           (unwind-protect
               (if (bufferp (get-buffer buffer-of-match))
                   (progn (switch-to-buffer buffer-of-match)
                          (goto-line line-of-match)
                          (message current-line))
                 (locate buffer-of-match))
             ())))))

(defun bhj-grep-next-line ()
  (interactive)
  (set-buffer bhj-grep-buffer-name)
  (goto-char bhj-grep-point)
  (forward-line)
  (beginning-of-line)
  (and (re-search-forward bhj-grep-regexp nil t)

       (let ((buffer-of-match (match-string 1))
             (current-line (buffer-substring (line-beginning-position) (line-end-position)))
             (line-of-match (string-to-number (match-string 2))))
         (setq bhj-grep-point (point-marker))
         (unwind-protect
             (if (bufferp (get-buffer buffer-of-match))
                 (progn (goto-char bhj-grep-point)
                        (switch-to-buffer buffer-of-match)
                        (goto-line line-of-match)
                        (message current-line))
               (locate buffer-of-match))

           ()))))

(defun bhj-grep-prev-line ()
  (interactive)
  (set-buffer bhj-grep-buffer-name)
  (goto-char bhj-grep-point)
  (beginning-of-line)
  (and (re-search-backward bhj-grep-regexp nil t)
       
       (let ((buffer-of-match (match-string 1))
             (current-line (buffer-substring (line-beginning-position) (line-end-position)))
             (line-of-match (string-to-number (match-string 2))))
         ;;(message "matching buffer %s, line-of-match %d" buffer-of-match line-of-match)
         (setq bhj-grep-point (point-marker))
         (unwind-protect
             (if (bufferp (get-buffer buffer-of-match))
                 (progn (goto-char bhj-grep-point)
                        (switch-to-buffer buffer-of-match)
                        (goto-line line-of-match)
                        (message current-line))
               (locate buffer-of-match))
           ()))))

(defun bhj-reread-file ()
  (interactive)
  (find-file buffer-file-truename))

(global-set-key [?\M-g ?t] 'bhj-grep-this-line)
(global-set-key [?\M-g ?f] 'bhj-grep-next-line)
(global-set-key [?\M-g ?b] 'bhj-grep-prev-line)




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
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

(defun linux-c++-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c++-mode) 
  (local-set-key [?\C-\M-a] 'bhj-c-beginning-of-defun)
  (local-set-key [?\C-\M-e] 'bhj-c-end-of-defun)
  (local-set-key [?\C-c ?\C-d] 'c-down-conditional)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8))

(setq auto-mode-alist (cons '(".*\\.[c]$" . linux-c-mode)
                            auto-mode-alist))
(setq auto-mode-alist (cons '(".*\\.cpp$" . linux-c++-mode)
                            auto-mode-alist))
(setq auto-mode-alist (cons '(".*\\.h$" . linux-c++-mode)
                            auto-mode-alist))

(when window-system

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


  ;; emacs wiki




  (setq emacs-wiki-publishing-directory "/var/www/home/a22242/WiKi")
  (setq emacs-wiki-directories '("~/WiKi"))
  (setq emacs-wiki-meta-charset "gbk")

  (setq emacs-wiki-style-sheet
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"../main.css\">"
        emacs-wiki-footer-date-format "%Y-%m-%d %H:%M:%S %Z"
        )

  (setq emacs-wiki-inline-relative-to 'emacs-wiki-publishing-directory)

  

  (setq emacs-wiki-projects
        `(("default" . ((emacs-wiki-directories . ("~/WiKi"))))
          ("work" . ((fill-column . 65)
                     (emacs-wiki-publishing-directory . "/var/www/home/a22242/WorkWiKi")
                     (emacs-wiki-directories . ("~/WorkWiKi/"))))))

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

  (prefer-coding-system 'chinese-gb18030-unix)
  (set-default-coding-systems 'chinese-gb18030-unix)

  (defun weekrep ()
    (interactive)
    (call-process "wr" nil t nil "-6"))

  (put 'upcase-region 'disabled nil)

  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))


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
(keydef "C-S-M" (gnus-group-browse-foreign-server "nnml:"))

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
  (call-process "cygpath" nil t nil "-u" default-directory)
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

(defun bhj-lqqm-pic-collect ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "emacs-lqqm-collect.sh"))

(defun bhj-lqqm-pic-view ()
  (interactive)
  (shell-command "emacs-lqqm-view.sh"))

(add-hook 'w3m-mode-hook 
          (lambda () 
            (local-set-key [(left)] 'backward-char)
            (local-set-key [(right)] 'forward-char)
            (local-set-key [(down)] 'next-line)
            (local-set-key [(up)] 'previous-line)
            (local-set-key [(n)] 'next-line)
            (local-set-key [(p)] 'previous-line)
            (setq w3m-fill-column 100)
            (text-scale-increase 2)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'gnus-article-mode-hook                    ;;
;;           (lambda ()                                 ;;
;;             (make-local-variable 'w3m-fill-column))) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (make-local-variable 'w3m-fill-column)))


(add-hook 'term-mode-hook
          (lambda ()
            (local-set-key [(control shift c)] 'bhj-lqqm-pic-collect)
            (local-set-key [(control shift v)] 'bhj-lqqm-pic-view)))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list (list "d:/tools/emacswin/info/" "d:/local/share/info" (concat cygwin-drive "/cygwin/usr/share/info")))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/tmp"))))
 '(bhj-clt-branch "dbg_zch68_a22242_soundmgr")
 '(bhj-grep-default-directory (quote default-directory))
 '(canlock-password "78f140821d1f56625e4e7e035f37d6d06711d112")
 '(case-fold-search t)
 '(default-input-method "chinese-wubi86")
 '(delete-old-versions t)
 '(describe-char-unidata-list (quote (name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value mirrored old-name iso-10646-comment uppercase lowercase titlecase)))
 '(dictem-server "localhost")
 '(dictionary-server "bhj3")
 '(ecb-compile-window-height nil)
 '(ecb-layout-name "left10")
 '(ecb-layout-window-sizes (quote (("top1" (0.25842696629213485 . 0.32075471698113206) (0.21910112359550563 . 0.32075471698113206) (0.5337078651685393 . 0.32075471698113206)))))
 '(ecb-new-ecb-frame nil)
 '(ecb-options-version "2.32")
 '(ecb-ping-program "false")
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-buffer-style (quote image))
 '(ecomplete-database-file-coding-system (quote gb18030))
 '(emms-player-mplayer-command-name "mplayer.exe")
 '(emms-player-mplayer-parameters (quote ("-slave" "-quiet" "-really-quiet" "-vo" "null")))
 '(font-lock-maximum-decoration 2)
 '(gdb-find-source-frame t)
 '(gdb-same-frame t)
 '(gdb-show-main t)
 '(grep-use-null-device (quote auto-detect))
 '(htmlize-output-type (quote font))
 '(ido-enable-regexp t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".*\\.\\(loc\\|org\\|mkelem\\)")))
 '(ido-save-directory-list-file "/home/bhj/.ido.last")
 '(indent-tabs-mode nil)
 '(ispell-program-name "aspell")
 '(keyboard-coding-system (quote cp936))
 '(locate-command "~/bin/locateEmacs.exe")
 '(mail-sources (quote ((file :path (concat cygwin-drive "/cygwin/var/spool/mail/bhj")))))
 '(message-mail-alias-type (quote ecomplete))
 '(mm-text-html-renderer (quote w3m))
 '(muse-html-charset-default "chinese-gb18030")
 '(muse-html-style-sheet "<style type=\"text/css\">
BODY {
	font-size : 10pt;
	font-family : Luxi Serif, Serif, Tahoma, helvetica, serif;
	background : white;
}

body.framed {
              padding: 10pt;
              border-style:none;
              }

body.ascii{
            background-color : Black;
            }

P{
   text-indent : 2em;
   }

p.banner {
           text-align : center;
           }

P.mmix{
        text-indent : 0;
        }

P.tip{
       color: blue;
       }

OL{
    list-style-type: decimal;
    padding-bottom : 8px;
    text-indent : 0;
    /*background: gray;*/
    }

OL.arabic {
            list-style-type: decimal;
            }
OL.cjk {
         list-style-type: cjk-ideographic;
         }

OL.roman{
          list-style-type: lower-roman;
          }


H2 {
     color: navy;
     text-align : left;
     }

A{
   color: MidnightBlue;
   text-decoration: none;
   }

A:Hover {
          text-decoration: none;
          background-color: bisque;
          caption-side : left;
          }

HR {
     color: #2E8B57;
     height: 1pt;
     }

UL {
     list-style-type : circle;
     }

div{
     border : 1pt;
     border-color : white;
     border-style : solid;
     padding: 10pt;
     }

IMG{
     border : 0;
     margin : 4pt 4pt 4pt;
     }

img.formula {
              border : thin;
              border-color: blue;
              border-style : solid;
              }


pre {
      border: #777777 1px solid;
      padding: 0.5em;
      margin-left: 1em;
      margin-right: 2em;
      white-space: pre;
      background-color: snow;
      color: #006400 ;
      font-family : Luxi Mono, Luxi Sans, Monospace, sans-serif;
      font-size: 90%;
      }


pre.syntax {
             background: lightblue;
             }

pre.error {
            background-color: pink;
            }

pre.url {
          background: bisque;
          }

pre.verse { 
            background: pink;}
em {
     color: red;
     text-decoration:none;
     font-style:normal;
     }

em.phrase {
            color: blue;
            text-decoration:none;
            font-style:normal;
            }

h1 {
	border-top: solid black 1px;
	border-bottom: solid black 1px;
	padding: 0.1em;
	padding-top: 0.2em;
	padding-left: 0.3em;
	padding-bottom: 0.3em;
     }

h1.home{
         color: #102C4A;
         }

h2 { 
     color:#666;
     border-top: solid black 1px;
     border-bottom: solid black 1px;
     padding: 0.1em;
     padding-top: 0.2em;
     padding-left: 0.3em;
     padding-bottom: 0.3em;
     background: LightGray;
     }

h3 { 
     color:#666;
     border-left: solid black 1px;
     border-bottom: solid black 1px;
     padding: 0.1em;
     padding-top: 0.2em;
     padding-left: 0.3em;
     padding-bottom: 0.3em;
     }

h4 { 
     color:#666;
     padding: 2px;
     margin-top: 5px;
     }

H5 {
     color: red;
     text-align: left;
     }


LI {
     margin-top: 3ex;
     }

LI.explained {
               }

p.adage{ 
         background: lightyellow; 
         border: green; 
         border-style: solid;
         }

p.script {
           font-size: small;
           text-align:right;
           }


div.tl{ 
        position: fixed;
        width: 7%;
        top: .4cm;
        right: 0;
        left: .4cm;
        bottom: auto;
        margin:0;
        padding: 5px;
        font-size:  28px;
        color:#666;
        border-bottom: 5px solid #000;
        margin: 0px;
        margin-bottom: 8px;
        }

    </style>")
 '(muse-latex-header "\\documentclass{article}

\\usepackage[english]{babel}
\\usepackage{ucs}
\\usepackage[utf8x]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}
\\usepackage{float}
\\usepackage[pdftex]{graphicx}

\\def\\museincludegraphics{%
  \\begingroup
  \\catcode`\\|=0
  \\catcode`\\\\=12
  \\catcode`\\#=12
  \\includegraphics[width=0.75\\textwidth]
}

\\begin{document}

\\title{<lisp>(muse-publish-escape-specials-in-string
  (muse-publishing-directive \"title\") 'document)</lisp>}
\\author{<lisp>(muse-publishing-directive \"author\")</lisp>}
\\date{<lisp>(muse-publishing-directive \"date\")</lisp>}

\\maketitle

<lisp>(and muse-publish-generate-contents
           (not muse-latex-permit-contents-tag)
           \"\\\\tableofcontents
\\\\newpage\")</lisp>

")
 '(muse-latex-markup-strings (quote ((image-with-desc . "\\begin{figure}[H]
\\centering\\museincludegraphics{%s.%s}|endgroup
\\caption{%s}
\\end{figure}") (image . "\\begin{figure}[H]
\\centering\\museincludegraphics{%s.%s}|endgroup
\\end{figure}") (image-link . "%% %s
\\museincludegraphics{%s.%s}|endgroup") (anchor-ref . "\\ref{%s}") (url . "\\url{%s}") (url-and-desc . "\\href{%s}{%s}\\footnote{%1%}") (link . "\\href{%s}{%s}\\footnote{%1%}") (link-and-anchor . "\\href{%1%}{%3%}\\footnote{%1%}") (email-addr . "\\verb|%s|") (anchor . "\\label{%s}") (emdash . "---") (comment-begin . "% ") (rule . "\\vspace{.5cm}\\hrule\\vspace{.5cm}") (no-break-space . "~") (line-break . "\\\\") (enddots . "\\ldots{}") (dots . "\\dots{}") (part . "\\part{") (part-end . "}") (chapter . "\\chapter{") (chapter-end . "}") (section . "\\section{") (section-end . "}") (subsection . "\\subsection{") (subsection-end . "}") (subsubsection . "\\subsubsection{") (subsubsection-end . "}") (section-other . "\\paragraph{") (section-other-end . "}") (footnote . "\\footnote{") (footnote-end . "}") (footnotetext . "\\footnotetext[%d]{") (begin-underline . "\\underline{") (end-underline . "}") (begin-literal . "\\texttt{") (end-literal . "}") (begin-emph . "\\emph{") (end-emph . "}") (begin-more-emph . "\\textbf{") (end-more-emph . "}") (begin-most-emph . "\\textbf{\\emph{") (end-most-emph . "}}") (begin-verse . "\\begin{verse}
") (end-verse-line . " \\\\") (verse-space . "~~~~") (end-verse . "
\\end{verse}") (begin-example . "\\begin{quote}
\\begin{verbatim}") (end-example . "\\end{verbatim}
\\end{quote}") (begin-center . "\\begin{center}
") (end-center . "
\\end{center}") (begin-quote . "\\begin{quote}
") (end-quote . "
\\end{quote}") (begin-cite . "\\cite{") (begin-cite-author . "\\citet{") (begin-cite-year . "\\citet{") (end-cite . "}") (begin-uli . "\\begin{itemize}
") (end-uli . "
\\end{itemize}") (begin-uli-item . "\\item ") (begin-oli . "\\begin{enumerate}
") (end-oli . "
\\end{enumerate}") (begin-oli-item . "\\item ") (begin-dl . "\\begin{description}
") (end-dl . "
\\end{description}") (begin-ddt . "\\item[") (end-ddt . "] \\mbox{}
"))))
 '(muse-publish-date-format "%m/%e/%Y")
 '(nnmail-expiry-wait (quote never))
 '(normal-erase-is-backspace nil)
 '(preview-gs-command "c:/gs/gs8.54/bin/gswin32c.exe")
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((sh-indentation . 2) (c-font-lock-extra-types "FILE" "bool" "language" "linebuffer" "fdesc" "node" "regexp") (TeX-master . t) (indent-tab-mode . t))))
 '(save-place t nil (saveplace))
 '(senator-minor-mode-hook (quote (ignore)))
 '(session-initialize (quote (de-saveplace session places keys menus)) nil (session))
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote parenthesis))
 '(smtpmail-auth-credentials (quote (("smtp.bizmail.yahoo.com" 465 "haojun.bao@borqs.com" "TingWoAiNi!"))))
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tooltip-mode nil)
 '(tooltip-use-echo-area t)
 '(tramp-syntax (quote ftp))
 '(tramp-verbose 0)
 '(transient-mark-mode t)
 '(user-full-name "Bao Haojun")
 '(user-mail-address "haojun.bao@borqs.com")
 '(w32-symlinks-handle-shortcuts t)
 '(w32-use-w32-font-dialog nil)
 '(w3m-bookmark-file "c:/Documents and Settings/bhj/.w3m_bookmark.html")
 '(w3m-charset-coding-system-alist (quote ((x-sjis . shift_jis) (x-shift_jis . shift_jis) (x-shift-jis . shift_jis) (x-euc-jp . euc-japan) (shift-jis . shift_jis) (x-unknown . undecided) (unknown . undecided) (windows-874 . tis-620) (iso-2022-jp-3 . iso-2022-7bit-ss2) (us_ascii . raw-text) (gb2312 . gb18030))))
 '(w3m-default-display-inline-images t)
 '(w3m-dirlist-cgi-program "~/bin/w3m-dirlist.exe")
 '(w3m-toggle-inline-images-permanently nil)
 '(weblogger-config-alist (quote (("yo2.cn" ("user" . "baohaojun@gmail.com") ("server-url" . "http://baohaojun.yo2.cn/xmlrpc.php") ("weblog" . "1")) ("bhj3" ("user" . "admin") ("server-url" . "http://bhj3/blog/xmlrpc.php") ("weblog" . "1")) ("default" ("user" . "baohaojun@gmail.com") ("server-url" . "http://baohaojun.yo2.cn/xmlrpc.php") ("weblog" . "1")))))
 '(woman-file-compression-regexp "\\.\\(g?z\\|bz2\\)\\(.lnk\\)?\\'")
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "d:/local/man")))
 '(woman-use-own-frame nil))




(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(set-keyboard-coding-system 'chinese-gb18030)
(set-selection-coding-system 'chinese-gb18030)

(gnuserv-start)

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
;(w32-register-hot-key [A-tab])
