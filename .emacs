;; Red Hat Linux default .emacs initialization file  ; -*- mode: emacs-lisp -*-


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq clipmon--autoinsert nil)

(setq org-emphasis-regexp-components
      '(
        "！？；：，。、  \t('\"{（“"              ;pre
        "- —；：，。、 \t.!,:!?？;！'\")}（）\\”" ;post
        " \t\r\n,"                                ;border *forbidden*
        "."                                       ;body-regexp
        1                                         ; newline
        ))

(when (file-exists-p "~/.config/system-config/vim-is-evil")
  (setq warning-minimum-level :error))

(let ((warning-minimum-level :error))
  (server-start))

(setq custom-file (expand-file-name "custom.el" "~/system-config/etc/"))

;;; Code:
(when (file-exists-p (format "~/system-config/.emacs_d/lisp/bhj-%s.el" (symbol-name system-type)))
  (load (format "~/system-config/.emacs_d/lisp/bhj-%s.el" (symbol-name system-type))))

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))

(when (file-exists-p "/usr/share/emacs/site-lisp/debian-startup.el")
  (load "/usr/share/emacs/site-lisp/debian-startup.el")
  (debian-startup 'emacs))

(when (file-exists-p (expand-file-name "~/.emacs-path.el"))
  (load-file (expand-file-name "~/.emacs-path.el")))

(setq load-path
      (nconc (list
              "/usr/share/emacs/site-lisp/gnus"
              "/opt/local/share/emacs/site-lisp/gnus"
              (expand-file-name "~/src/gnus")
              (expand-file-name "~/src/bbdb/lisp")
              (expand-file-name (concat "~/system-config/.emacs_d/" (symbol-name system-type)))
              (expand-file-name "~/system-config/.emacs_d/lisp")
              (expand-file-name "~/system-config/.emacs_d/skeleton-complete")
              (expand-file-name "~/system-config/.emacs_d/org-confluence")
              (expand-file-name "~/system-config/.emacs_d/org-jira")
              (expand-file-name "~/system-config/.emacs_d/mo-git-blame")
              (expand-file-name "~/system-config/.emacs_d/lisp/ext")
              (expand-file-name "~/system-config/.emacs_d/weblogger")
              (expand-file-name "~/src/github/helm")
              (expand-file-name "~/src/github/org-mode/contrib/lisp"))
             load-path))

(when (file-exists-p (expand-file-name "~/.config/system-config/emacs.gen.el"))
  (load (expand-file-name "~/.config/system-config/emacs.gen.el")))

(when (and
       (> emacs-major-version 23)
       (file-exists-p "~/src/github/emacs.d/init.el")
       (not (string= (getenv "ORG2PDF") "true")))
  (setq load-path
        (nconc (list
                (expand-file-name "~/src/github/emacs.d")
                (expand-file-name "~/src/github/emacs.d/lisp")
                )
               load-path))
  (load "~/src/github/emacs.d/init.el"))

(keydef "C-S-g" (let ((grep-buffers-buffer-name "*grep-buffers*")) (grep-buffers)))
(keydef (w3m "C-c e") (lambda()(interactive)(call-process "/bin/bash" nil nil nil "/q/bin/windows/w3m-external" w3m-current-url)))

(when  (or (eq system-type 'cygwin) (eq system-type 'windows-nt))
  ;;press F2 to get MSDN help
  (global-set-key[(f2)](lambda()(interactive)(call-process "/bin/bash" nil nil nil "/q/bin/windows/ehelp" (current-word)))))

(when (eq system-type 'windows-nt)
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  (require 'w32-symlinks))


(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (load "bhj-fonts.el"))

(autoload 'csharp-mode "csharp-mode")



(autoload 'sdim-use-package "sdim" "Shadow dance input method")
(register-input-method
 "sdim" "euc-cn" 'sdim-use-package "影舞笔")

(mapc (lambda (x) (add-hook x (lambda ()
                                (setq beginning-of-defun-function #'ajoke--beginning-of-defun-function)
                                (local-set-key [?\C-\M-a] 'beginning-of-defun)
                                (local-set-key [?\C-\M-e] 'end-of-defun))))
      (list 'c-mode-hook 'c++-mode-hook 'csharp-mode-hook 'java-mode-hook))

(mapc (lambda (x) (add-hook x (lambda ()
                                (setq beginning-of-defun-function nil)
                                (local-set-key [?\C-\M-a] 'beginning-of-defun)
                                (local-set-key [?\C-\M-e] 'end-of-defun))))
      (list 'php-mode-hook))

(defun csharp-end-of-defun (&optional arg)
  (interactive)
  (if (eq major-mode 'csharp-mode)
      (beginning-of-defun-raw -1)
    (end-of-defun arg)))

(mapc (lambda (x) (add-hook x (lambda ()
                           (local-set-key [?\C-c ?\C-d] 'c-down-conditional)
                           (c-set-offset 'innamespace 0)
                           (c-set-offset 'substatement-open 0))))
      (list 'c-mode-hook 'c++-mode-hook))

(auto-image-file-mode)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)


;; enable visual feedback on selections

;;popup the manual page, try:)
(put 'narrow-to-region 'disabled nil)

(define-key esc-map [(meta .)] 'ajoke-pop-mark-back)
(define-key esc-map [(meta \,)] 'ajoke-pop-mark)
(define-key esc-map [(meta \;)] 'anzu-query-replace-regexp)
(define-key esc-map [(\?)] 'undo-tree-redo)
(global-set-key [(meta control \,)] 'ajoke-pop-mark)
(global-set-key [(meta control .)] 'ajoke-pop-mark-back)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)


(put 'upcase-region 'disabled nil)


(fset 'grep-buffers-symbol-at-point 'current-word)



(standard-display-ascii ?\221 "\`")
(standard-display-ascii ?\222 "\'")
(standard-display-ascii ?\223 "\"")
(standard-display-ascii ?\224 "\"")
(standard-display-ascii ?\227 "\-")
(standard-display-ascii ?\225 "\*")

;; old time motorola usage
;; (defcustom bhj-clt-branch "dbg_zch68_a22242_ringtone-hx11i"
;;   "the cleartool branch to use for mkbranch")

;; (defun bhj-clt-insert-branch ()
;;   (interactive)
;;   (insert bhj-clt-branch))
;; (define-key minibuffer-local-shell-command-map [(control meta b )] 'bhj-clt-insert-branch)


(defvar last-grep-marker nil)

(defcustom bhj-force-cleanup-buffer nil
  "force to clean up the buffer before save"
  :type 'boolean)

(make-variable-buffer-local 'bhj-force-cleanup-buffer)

(defvar ajoke-output-buffer-name "*ajoke*"
  "The name of the ajoke output buffer.")

(defvar grep-find-file-history nil)

(defvar grep-rgrep-history nil)


(setq my-grep-command "beagrep -s -e pat") ;; should not put it into custom, the custom will be read every time and so the `(let ((grep-command ..' scheme will fail


(defvar grep-beatags-history nil)
(defvar bhj-occur-regexp nil)

(put 'scroll-left 'disabled nil)

(fset 'bhj-bhjd
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\"" 0 "%d")) arg)))
(fset 'bhj-preview-markdown
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 49 67108896 3 2 134217848 98 104 106 45 109 105 109 101 tab return 3 return 80 24 111 67108911 24 111] 0 "%d")) arg)))

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

(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)


;; Command to point VS.NET at our current file & line

;; Command to toggle a VS.NET breakpoint at the current line.

;; Run the debugger.


(setenv "EMACS" "t")
(require 'saveplace)
(setq-default abbrev-mode t)
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)

;;;###autoload
(defun random-theme(&optional specified-theme)
  (interactive)
  (let ((next-theme-list (custom-available-themes))
        (current-theme)
        (next-theme))
    (dolist (theme custom-enabled-themes)
      (setq next-theme-list (delete theme next-theme-list)
            current-theme theme)
      )
    (setq next-theme (or specified-theme
                         (and current-prefix-arg (intern (completing-read "Which theme?" (custom-available-themes) nil t nil nil (symbol-name current-theme))))
                         (let ((theme (nth (random (length next-theme-list)) next-theme-list)))
                           (message "loaded theme: %s" theme)
                           theme)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme next-theme)))

(unless (boundp 'bhj-no-random-theme)
  (condition-case nil
      (random-theme)
    (error nil)))

(defun knuth-mode ()
  (interactive)
  (shell-command "knuth-mode"))

;; use cperl-mode instead of perl-mode
(setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))

(setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(defvar grep-func-call-history nil)

(defmacro set-remote-env (name val)
  `(let ((process-environment tramp-remote-process-environment))
     (setenv ,name ,val)
     (setq tramp-remote-process-environment process-environment)))

(defvar code-reading-file "~/.code-reading")

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

(defvar waw-mode-map nil
  "Keymap for where-are-we-mode.")

(put 'waw-mode 'mode-class 'special)

(defvar java-bt-mode-map nil
  "Keymap for java-bt-mode.")

(defvar java-bt-tag-alist nil
  "backtrace/code tag alist.")

(put 'java-bt-mode 'mode-class 'special)

(defvar boe-default-indent-col 0)
(make-variable-buffer-local 'boe-default-indent-col)

(define-key esc-map [(shift backspace)] 'back-to-indent-same-space-as-prev-line)




(when (eq system-type 'windows-nt)
  (setq file-name-coding-system 'gbk)
  (set-selection-coding-system 'gbk))

(when (eq system-type 'windows-nt)
  (setq nntp-authinfo-file "~/../.authinfo"
        auth-sources '((:source "~/../.authinfo" :host t :protocol t))))

;; (unless (or (eq system-type 'windows-nt)
;;             (eq system-type 'darwin))
;;   (load-file "~/system-config/.emacs_d/lisp/my-erc-config.el"))


(define-key esc-map [(meta d)] 'bhj-do-dictionary)
(define-key esc-map [(meta s)] 'bhj-do-search)


(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)


(defadvice hack-dir-local-variables
  (around hack-remote-file-p first activate)
  "Hack (hack-dir-local-variables) to make it work with remote files."
  (require 'cl)
  (let ((saved-file-remote-p (symbol-function 'file-remote-p)))
    (flet ((file-remote-p (file &optional identification connected)
                          (cond
                           ((string-match "^/scp:" file) nil)
                           ((string-match "/smb/" file) t)
                           (t (funcall saved-file-remote-p file identification connected)))))
      ad-do-it)))

(add-hook 'vc-git-log-view-mode-hook
          (lambda ()
            (when (string= log-view-message-re "^commit *\\([0-9a-z]+\\)")
              (setq log-view-message-re "^commit +\\([0-9a-z]+\\)"))))


(add-hook 'grep-mode-hook
          (lambda ()
            (setq compilation-directory-matcher (default-value 'compilation-directory-matcher))))

(load "bhj-setq.el")
(load "bhj-set-key.el")
(load "bhj-autoloads.el")
(load "bhj-eval-after-load.el")
(bbyac-global-mode 1)
(bhj-grep-global-mode 1)
(when (file-exists-p "~/external/.emacs_d/emacs-wide")
  (load "~/external/.emacs_d/emacs-wide"))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p (concat "~/system-config/.by-user/" (getenv "USER") "/.emacs"))
  (load (concat "~/system-config/.by-user/" (getenv "USER") "/.emacs")))
(when (file-exists-p "~/.config/system-config/vim-is-evil")
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (scroll-bar-mode 1)
  (evil-mode 1)
  (load "sc-evil.el")
  (tabbar-mode 1)
  (setq-default initial-scratch-message
                (concat initial-scratch-message
                        "请访问此网页查看 Emacs 作弊手册： http://baohaojun.github.io/emacs-cheat-sheet.html")))

(when (file-exists-p "~/.config/system-config/.emacs")
  (load "~/.config/system-config/.emacs"))

(when (file-exists-p "~/.local-config/.emacs")
  (load "~/.local-config/.emacs"))

(when (file-exists-p "/usr/share/emacs/site-lisp/mu4e/mu4e.el")
  (load "~/system-config/.emacs_d/lisp/init-mu4e.el"))


(require 'fence-edit)
(global-set-key (kbd "C-c '") 'fence-edit-code-at-point)
(load "move-by-indentation.el")

(require 'netease-music)
(setq netease-music-api "http://localhost:3000")
