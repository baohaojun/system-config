(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width 80)
 '(ahk-syntax-directory "~/system-config/doc/ahk/Syntax/")
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(auth-sources (quote ((:source "~/.authinfo" :host t :protocol t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/tmp"))))
 '(before-save-hook
   (quote
    (confirm-risky-remote-edit cleanup-buffer-safe fix-latex-cjk ajoke-get-imports-if-java-mode)))
 '(canlock-password "78f140821d1f56625e4e7e035f37d6d06711d112")
 '(case-fold-search t)
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(default-input-method "sdim")
 '(delete-old-versions t)
 '(describe-char-unidata-list
   (quote
    (name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value mirrored old-name iso-10646-comment uppercase lowercase titlecase)))
 '(dictem-server "localhost")
 '(dictionary-server "localhost")
 '(ecb-options-version "2.40")
 '(eclim-executable "eclim")
 '(eclim-interactive-completion-function (quote anything-completing-read))
 '(ecomplete-database-file-coding-system (quote utf-8))
 '(edebug-eval-macro-args t)
 '(fill-column 70)
 '(gdb-find-source-frame t)
 '(gdb-many-windows t)
 '(gdb-same-frame t)
 '(gdb-show-main t)
 '(gnus-article-date-headers (quote (local lapsed)))
 '(gnus-group-name-charset-group-alist (quote (("ali\\..*" . utf-8) (".*" . chinese-iso-8bit))))
 '(gnus-ignored-newsgroups "")
 '(gnus-propagate-marks t)
 '(grep-use-null-device nil)
 '(helm-idle-delay 0.1)
 '(helm-input-idle-delay 0.1)
 '(ido-enable-regexp t)
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".*\\.\\(loc\\|org\\|mkelem\\)")))
 '(imenu-max-item-length nil)
 '(imenu-space-replacement " ")
 '(install-elisp-repository-directory "~/system-config/.emacs_d/lisp/")
 '(ispell-program-name "aspell")
 '(jira-host "bible")
 '(jira-url "http://bible/jira/rpc/xmlrpc")
 '(jiralib-url "http://bible/jira")
 '(keyboard-coding-system (quote cp936))
 '(lisp-mode-hook
   (quote
    ((lambda nil
       (make-local-variable
        (quote ajoke-symbol-chars))
       (setq ajoke-symbol-chars "-A-Za-z0-9_")))))
 '(longlines-auto-wrap nil)
 '(major-mode (quote text-mode))
 '(makefile-mode-hook
   (quote
    ((lambda nil
       (make-local-variable
        (quote ajoke-symbol-chars))
       (setq ajoke-symbol-chars "-A-Za-z0-9_")))))
 '(message-dont-reply-to-names (quote (".*haojun.*" "hjbao")))
 '(message-mail-alias-type nil)
 '(mmm-submode-decoration-level 2)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(nnmail-expiry-wait (quote never))
 '(normal-erase-is-backspace nil)
 '(org-clock-x11idle-program-name "xprintidle")
 '(org-directory "~/doc/projects")
 '(org-export-html-coding-system (quote utf-8))
 '(org-export-html-postamble (lambda nil (buffer-file-name)))
 '(org-export-html-style-extra
   "<link rel=\"stylesheet\" href=\"css/default.css\" type=\"text/css\">
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
 '(org-export-with-sub-superscripts (quote {}))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "of %s"))))
 '(org-mobile-directory "~/today/MobileOrg")
 '(org-mobile-inbox-for-pull "~/doc/projects/from-mobile.org")
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-use-sub-superscripts (quote {}))
 '(org2blog/wp-confirm-post t)
 '(org2blog/wp-use-tags-as-categories t)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((bhj-grep-dir . "~/src/android/.repo")
     (bhj-grep-dir . "~/src/android")
     (bhj-help-qt-prog . "search-msdn-lucky")
     (ac-clang-flags "-Wno-c++11-extensions" "-include" "/home/bhj/src/github/vc-include/fix-it.h" "-I/home/bhj/.cache/vc/include" "-I/home/bhj/.cache/vc/atlmfc/include" "-I/home/bhj/.cache/vc/sdk/include" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Public/Smartisan/" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/./ConnectivityToolKit/" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/RFToolKit" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/QMSLRFTest" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Bin" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/ConnectivityToolKit" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Dependence" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/ImeiTool" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/LogLib" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/PowerSupply" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Public" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/QMSLGeneral" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/QMSLRFCalibration" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/QMSLRFTest" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/RFToolKit" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/SFCLib" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/StationMonitor" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/TestFrame" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/TestFrame/GridCell" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/TestFrame/GridTreeCtrl" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/TestFrame/ListCtrl" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/TestFrame/OSwrappers" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/TestFrame/res" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/TestFrame/TabCtrl" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Public/LitePoint" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Public/NI" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Public/Qualcomm" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Public/Smartisan" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Public/SQLite" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Public/tinyxml" "-I/home/bhj/src/tools/SmartisanTestFrame/branch_DailyDev/Public/VISA" "-I/usr/include/x86_64-linux-gnu/qt5/QtConcurrent" "-I/usr/include/x86_64-linux-gnu/qt5/QtCore" "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus" "-I/usr/include/x86_64-linux-gnu/qt5/QtGui" "-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGL" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGLExtensions" "-I/usr/include/x86_64-linux-gnu/qt5/QtPlatformSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtPrintSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtSql" "-I/usr/include/x86_64-linux-gnu/qt5/QtTest" "-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets" "-I/usr/include/x86_64-linux-gnu/qt5/QtXml" "-I/usr/include/x86_64-linux-gnu/qt5" "-I/usr/include/c++/4.9" "-I/usr/include/x86_64-linux-gnu/c++/4.9" "-I/usr/include/c++/4.9/backward" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include" "-I/usr/local/include" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include" "-I.")
     (flycheck-mode)
     (ac-clang-flags "-I/usr/include/x86_64-linux-gnu/qt5/QtConcurrent" "-I/usr/include/x86_64-linux-gnu/qt5/QtCore" "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus" "-I/usr/include/x86_64-linux-gnu/qt5/QtGui" "-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGL" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGLExtensions" "-I/usr/include/x86_64-linux-gnu/qt5/QtPlatformSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtPrintSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtSql" "-I/usr/include/x86_64-linux-gnu/qt5/QtTest" "-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets" "-I/usr/include/x86_64-linux-gnu/qt5/QtXml" "-I/usr/include/x86_64-linux-gnu/qt5" "-I/usr/include/c++/4.9" "-I/usr/include/x86_64-linux-gnu/c++/4.9" "-I/usr/include/c++/4.9/backward" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include" "-I/usr/local/include" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include" "-I.")
     (eval activate-input-method "sdim")
     (require-final-newline)
     (encoding . utf-8)
     (global-robe-mode)
     (nxml-child-indent . 4)
     (sgml-basic-offset . 4)
     (bhj-force-cleanup-buffer . t)
     (bhj-grep-dir . "~/src/android/")
     (bhj-grep-dir . ~/src/android/)
     (sh-indent-comment . t)
     (c-style . whitesmith)
     (major-mode . sh-mode)
     (py-indent-offset . 4)
     (sh-indentation . 2)
     (c-font-lock-extra-types "FILE" "bool" "language" "linebuffer" "fdesc" "node" "regexp")
     (TeX-master . t)
     (indent-tab-mode . t))))
 '(save-place t nil (saveplace))
 '(scalable-fonts-allowed t)
 '(senator-minor-mode-hook (quote (ignore)))
 '(session-initialize (quote (de-saveplace session places keys menus)))
 '(session-use-package t nil (session))
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
 '(twittering-convert-fix-size nil)
 '(user-full-name "Bao Haojun")
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches "-w")
 '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Git Hg)))
 '(vc-ignore-dir-regexp
   "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'\\|/smb/")
 '(w32-symlinks-handle-shortcuts t)
 '(w32-use-w32-font-dialog nil)
 '(w3m-default-display-inline-images t)
 '(weblogger-config-alist
   (quote
    (("default" "https://storage.msn.com/storageservice/MetaWeblog.rpc" "thomasbhj" "" "MyBlog")
     ("csdn" "http://blog.csdn.net/flowermonk/services/MetaBlogApi.aspx" "flowermonk" "" "814038"))))
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/local/man")))
 '(woman-use-own-frame nil)
 '(x-select-enable-clipboard t)
 '(x-select-enable-primary t)
 '(yas-also-auto-indent-first-line t)
 '(yas-global-mode t nil (yasnippet))
 '(yas-prompt-functions (quote (yas-completing-prompt)))
 '(yas-snippet-dirs
   (quote
    ("~/system-config/.emacs_d/yasnippet/snippets" "~/system-config/.emacs_d/yasnippet-snippets")) nil (yasnippet))
 '(yas-trigger-key "M-TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
