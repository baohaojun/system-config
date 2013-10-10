
;; Red Hat Linux default .emacs initialization file  ; -*- mode: emacs-lisp -*-

(when (and (file-exists-p "/etc/emacs/site-start.d/00debian-vars.el")
           (not (fboundp 'debian-file->string)))
  (load "/usr/share/emacs/site-lisp/debian-startup.el")
  (setq debian-emacs-flavor
        (intern
         (concat "emacs" (replace-regexp-in-string "\\..*" "" emacs-version))))
  (let ((flavor 'emacs))
    (mapc (lambda (file)
            (load file))
          (directory-files "/etc/emacs/site-start.d/" t ".*.el"))))
(when (file-exists-p (expand-file-name "~/.emacs-path.el"))
  (load-file (expand-file-name "~/.emacs-path.el")))

(setq load-path
      (nconc (list
              "/usr/share/emacs/site-lisp/gnus"
              "/opt/local/share/emacs/site-lisp/gnus"
              (expand-file-name "~/src/gnus")
              (expand-file-name "~/src/bbdb/lisp")
              (expand-file-name (concat "~/.emacs_d/" (symbol-name system-type)))
              (expand-file-name "~/.emacs_d/lisp")
              (expand-file-name "~/.emacs_d/skeleton-complete")
              (expand-file-name "~/.emacs_d/org-confluence")
              (expand-file-name "~/.emacs_d/org-jira")
              (expand-file-name "~/.emacs_d/mo-git-blame")
              (expand-file-name "~/.emacs_d/lisp/ext")
              (expand-file-name "~/.emacs_d/weblogger")
              (expand-file-name "~/.emacs_d/org2blog")
              (expand-file-name "~/src/github/org-mode/lisp")
              (expand-file-name "~/src/github/helm")
              (expand-file-name "~/src/github/org-mode/contrib/lisp"))
             load-path))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(require 'mmm-auto)

(when  (or (eq system-type 'cygwin) (eq system-type 'windows-nt))
  ;;press F2 to get MSDN help
  (global-set-key[(f2)](lambda()(interactive)(call-process "/bin/bash" nil nil nil "/q/bin/windows/ehelp" (current-word))))
  (setq locate-command "locateEmacs.sh"))

(when (eq system-type 'windows-nt)
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  (require 'w32-symlinks))


(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (load "bhj-fonts"))

(autoload 'csharp-mode "csharp-mode")



(autoload 'sdim-use-package "sdim" "Shadow dance input method")
(register-input-method
 "sdim" "euc-cn" 'sdim-use-package "影舞笔")

(mapcar (lambda (x) (add-hook x (lambda ()
                    (setq beginning-of-defun-function #'ajoke--beginning-of-defun-function)
                    (local-set-key [?\C-\M-a] 'beginning-of-defun)
                    (local-set-key [?\C-\M-e] 'end-of-defun))))
        (list 'c-mode-hook 'c++-mode-hook 'csharp-mode-hook 'java-mode-hook))

(mapcar (lambda (x) (add-hook x (lambda ()
                    (local-set-key [?\C-c ?\C-d] 'c-down-conditional)
                    (c-set-offset 'innamespace 0)
                    (c-set-offset 'substatement-open 0))))
        (list 'c-mode-hook 'c++-mode-hook))






(auto-image-file-mode)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)


(require 'browse-kill-ring-autoloads)
(browse-kill-ring-default-keybindings)

;; enable visual feedback on selections

;;popup the manual page, try:)
(put 'narrow-to-region 'disabled nil)



(require 'org2blog-autoloads)

(global-set-key [(meta control \,)] 'ajoke-pop-mark)
(global-set-key [(meta control .)] 'ajoke-pop-mark-back)
(prefer-coding-system 'gbk)
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

(keydef "C-S-g" (progn (setq grep-buffers-buffer-name "*grep-buffers*")(grep-buffers)))





;; old time motorola usage
;; (defcustom bhj-clt-branch "dbg_zch68_a22242_ringtone-hx11i"
;;   "the cleartool branch to use for mkbranch")

;; (defun bhj-clt-insert-branch ()
;;   (interactive)
;;   (insert bhj-clt-branch))
;; (define-key minibuffer-local-shell-command-map [(control meta b )] 'bhj-clt-insert-branch)


(define-key minibuffer-local-map [(meta shift f)] 'bhj-clt-insert-file-name)
(define-key minibuffer-local-map [(meta shift e)] 'bhj-edit-grep-pattern)

(define-key minibuffer-local-map [(meta shift d )] 'bhj-insert-pwdu)

(defvar last-grep-marker nil)
(defvar bhj-force-cleanup-buffer nil)
(make-variable-buffer-local 'bhj-force-cleanup-buffer)

(defvar ajoke-output-buffer-name "*ajoke*"
  "The name of the ajoke output buffer.")

(keydef "M-g r" (progn
                  (let ((current-prefix-arg 4)
                        ;; (default-directory (eval bhj-grep-default-directory))
                        (grep-use-null-device nil))
                    (nodup-ring-insert ajoke--marker-ring (point-marker))
                    (call-interactively 'grep-bhj-dir))))

(defvar grep-find-file-history nil)

(defvar grep-rgrep-history nil)


(setq my-grep-command "beagrep -e pat") ;; should not put it into custom, the custom will be read every time and so the `(let ((grep-command ..' scheme will fail


(defvar grep-gtags-history nil)
(defvar grep-imenu-history nil)









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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(auth-sources (quote ((:source "~/.authinfo" :host t :protocol t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/tmp"))))
 '(before-save-hook (quote (confirm-risky-remote-edit cleanup-buffer-safe fix-latex-cjk)))
 '(canlock-password "78f140821d1f56625e4e7e035f37d6d06711d112")
 '(case-fold-search t)
 '(delete-old-versions t)
 '(describe-char-unidata-list (quote (name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value mirrored old-name iso-10646-comment uppercase lowercase titlecase)))
 '(dictem-server "localhost")
 '(dictionary-server "localhost")
 '(ecb-options-version "2.40")
 '(eclim-executable "eclim")
 '(eclim-interactive-completion-function (quote anything-completing-read))
 '(ecomplete-database-file-coding-system (quote utf-8))
 '(edebug-eval-macro-args t)
 '(emacs-lisp-mode-hook (quote ((lambda nil (make-local-variable (quote ajoke-symbol-chars)) (setq ajoke-symbol-chars "-A-Za-z0-9_")))))
 '(fill-column 70)
 '(gdb-find-source-frame t)
 '(gdb-many-windows t)
 '(gdb-same-frame t)
 '(gdb-show-main t)
 '(gnus-article-date-headers (quote (local lapsed)))
 '(gnus-ignored-newsgroups "")
 '(gnus-propagate-marks t)
 '(grep-use-null-device nil)
 '(helm-idle-delay 0.1)
 '(helm-input-idle-delay 0.1)
 '(ido-enable-regexp t)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".*\\.\\(loc\\|org\\|mkelem\\)")))
 '(imenu-max-item-length nil)
 '(imenu-space-replacement " ")
 '(install-elisp-repository-directory "~/.emacs_d/lisp/")
 '(ispell-program-name "aspell")
 '(jira-host "bible")
 '(jira-url "http://bible/jira/rpc/xmlrpc")
 '(jiralib-url "http://bible/jira")
 '(keyboard-coding-system (quote cp936))
 '(lisp-mode-hook (quote ((lambda nil (make-local-variable (quote ajoke-symbol-chars)) (setq ajoke-symbol-chars "-A-Za-z0-9_")))))
 '(longlines-auto-wrap nil)
 '(major-mode (quote text-mode))
 '(makefile-mode-hook (quote ((lambda nil (make-local-variable (quote ajoke-symbol-chars)) (setq ajoke-symbol-chars "-A-Za-z0-9_")))))
 '(message-dont-reply-to-names (quote (".*haojun.*" "hjbao")))
 '(message-mail-alias-type nil)
 '(mmm-submode-decoration-level 2)
 '(nnmail-expiry-wait (quote never))
 '(normal-erase-is-backspace nil)
 '(org-clock-x11idle-program-name "xprintidle")
 '(org-directory "~/doc/projects")
 '(org-export-html-coding-system (quote utf-8))
 '(org-export-html-postamble (lambda nil (buffer-file-name)))
 '(org-export-html-style-extra "<link rel=\"stylesheet\" href=\"css/default.css\" type=\"text/css\">
 <link rel=\"shortcut icon\" href=\"/poison.png\" type=\"image/png\" />

    <script type=\"text/javascript\" src=\"js/jquery-1.7.1.min.js\"></script>
    <script type=\"text/javascript\">
        var BYB = {};
    </script>
    <script type=\"text/javascript\">
        BYB.includeScript = function(file,callback){
            var _doc = document.getElementsByTagName('head')[0];
            var js = document.createElement('script');
            js.setAttribute('type', 'text/javascript');
            js.setAttribute('src', file);
            _doc.appendChild(js);

            if (!/*@cc_on!@*/0) { //if not IE
                //Firefox2、Firefox3、Safari3.1+、Opera9.6+ support js.onload
                js.onload = function () {
                    callback();
                }
            } else {
                //IE6、IE7 support js.onreadystatechange
                js.onreadystatechange = function () {
                    if (js.readyState == 'loaded' || js.readyState == 'complete') {
                        callback();
                    }
                }
            }
            return false;
        }
    </script>")
 '(org-export-html-style-include-default nil)
 '(org-export-htmlize-output-type (quote css))
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "of %s"))))
 '(org-mobile-directory "~/today/MobileOrg" t)
 '(org-mobile-inbox-for-pull "~/today/MobileOrg/from-mobile.org")
 '(org2blog/wp-confirm-post t)
 '(org2blog/wp-use-tags-as-categories t)
 '(require-final-newline t)
 '(safe-local-variable-values (quote ((bhj-force-cleanup-buffer . t) (bhj-grep-dir . "~/src/android/") (bhj-grep-dir . ~/src/android/) (sh-indent-comment . t) (c-style . whitesmith) (major-mode . sh-mode) (py-indent-offset . 4) (sh-indentation . 2) (c-font-lock-extra-types "FILE" "bool" "language" "linebuffer" "fdesc" "node" "regexp") (TeX-master . t) (indent-tab-mode . t))))
 '(save-place t nil (saveplace))
 '(scalable-fonts-allowed t)
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
 '(tramp-syntax (quote ftp))
 '(tramp-verbose 0)
 '(transient-mark-mode t)
 '(twittering-convert-fix-size nil)
 '(user-full-name "Bao Haojun")
 '(vc-follow-symlinks t)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Git Hg)))
 '(vc-ignore-dir-regexp "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'\\|/smb/")
 '(w32-symlinks-handle-shortcuts t)
 '(w32-use-w32-font-dialog nil)
 '(w3m-default-display-inline-images t)
 '(weblogger-config-alist (quote (("default" "https://storage.msn.com/storageservice/MetaWeblog.rpc" "thomasbhj" "" "MyBlog") ("csdn" "http://blog.csdn.net/flowermonk/services/MetaBlogApi.aspx" "flowermonk" "" "814038"))))
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/local/man")))
 '(woman-use-own-frame nil)
 '(x-select-enable-clipboard t)
 '(x-select-enable-primary t)
 '(yas-also-auto-indent-first-line t)
 '(yas-global-mode t nil (yasnippet))
 '(yas-prompt-functions (quote (yas/ido-prompt yas/no-prompt)))
 '(yas-snippet-dirs (quote ("~/.emacs_d/yasnippet/snippets" "~/.emacs_d/yasnippet-snippets")) nil (yasnippet))
 '(yas-trigger-key "M-TAB"))

(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)



(require 'skeleton-complete)
(skeleton-complete-global-mode 1)


(keydef "C-M-j" 'bhj-jdk-help)
(keydef (w3m "C-c e") (lambda()(interactive)(call-process "/bin/bash" nil nil nil "/q/bin/windows/w3m-external" w3m-current-url)))


;; Command to point VS.NET at our current file & line

;; Command to toggle a VS.NET breakpoint at the current line.

;; Run the debugger.


(setenv "EMACS" "t")
(require 'saveplace)
(setq-default abbrev-mode t)
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)

(defun random-theme()
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme (let ((theme (nth (random (length (custom-available-themes))) (custom-available-themes))))
                (message "loaded theme: %s" theme)
                theme)))

(unless (boundp 'bhj-no-random-theme)
  (condition-case nil
      (random-theme)
    (error nil)))




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

(require 'bbdb-autoloads)

(add-hook 'bbdb-canonicalize-net-hook
      'my-bbdb-canonicalize)


(when (eq system-type 'windows-nt)
  (setq nntp-authinfo-file "~/../.authinfo"
        auth-sources '((:source "~/../.authinfo" :host t :protocol t))))
(when nil
  (require 'twittering-mode)
  (twittering-enable-unread-status-notifier)
  (setq-default twittering-icon-mode t)
  (setq twittering-use-ssl nil
        twittering-oauth-use-ssl nil)
  (setq twittering-icon-mode 1)
  (setq twittering-enabled-services '(sina))
  (setq twittering-accounts '((sina (username "baohj_zj@hotmail.com")
                                    (auth oauth))))

  (setq twittering-initial-timeline-spec-string `(":home@sina")))


;; (unless (or (eq system-type 'windows-nt)
;;             (eq system-type 'darwin))
;;   (load-file "~/.emacs_d/lisp/my-erc-config.el"))


(define-key esc-map [(meta d)] 'bhj-do-dictionary)


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

(require 'helm-config)
(helm-mode 1)

(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
(define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)

(add-hook 'vc-git-log-view-mode-hook
          (lambda ()
            (when (string= log-view-message-re "^commit *\\([0-9a-z]+\\)")
              (setq log-view-message-re "^commit +\\([0-9a-z]+\\)"))))


(add-hook 'python-mode-hook
          (lambda ()
            (setq imenu-create-index-function #'ajoke-create-index-function))
          t)

(add-hook 'grep-mode-hook
          (lambda ()
            (setq compilation-directory-matcher (default-value 'compilation-directory-matcher))))

(load "bhj-setq.el")
(load "bhj-set-key.el")
(load "bhj-autoloads.el")
(load "bhj-eval-after-load.el")


(condition-case nil
    (server-start)
  (error (message "emacs server start failed")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
