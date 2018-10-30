(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width 80)
 '(after-save-hook
   (quote
    (sc--after-save sanityinc/set-mode-for-new-scripts executable-make-buffer-file-executable-if-script-p)))
 '(ahk-syntax-directory "~/system-config/doc/ahk/Syntax/")
 '(auth-sources (quote ((:source "~/.authinfo" :host t :protocol t))))
 '(auto-save-file-name-transforms
   (quote
    (("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
     (".*" "~/.emacs.d/tmp" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/tmp"))))
 '(bbyac-max-chars 80)
 '(before-save-hook
   (quote
    (confirm-risky-remote-edit cleanup-buffer-safe ajoke-get-imports-if-java-mode)))
 '(canlock-password "78f140821d1f56625e4e7e035f37d6d06711d112")
 '(case-fold-search t)
 '(company-idle-delay nil)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default)))
 '(debug-on-error nil)
 '(debug-on-quit nil)
 '(default-input-method "sdim")
 '(delete-old-versions t)
 '(describe-char-unidata-list
   (quote
    (name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value mirrored old-name iso-10646-comment uppercase lowercase titlecase)))
 '(dictem-server "localhost")
 '(dictionary-server "localhost")
 '(douban-music-default-channel 6)
 '(ecb-options-version "2.40")
 '(eclim-executable "eclim")
 '(eclim-interactive-completion-function (quote anything-completing-read))
 '(ecomplete-database-file-coding-system (quote utf-8))
 '(edebug-eval-macro-args t)
 '(emojify-emoji-styles (quote (unicode)))
 '(fence-edit-blocks
   (quote
    (("^[[:blank:]]*\\(?:```\\|~~~\\)[ ]?\\([^[:space:]]+\\|{[^}]*}\\)?\\(?:[[:space:]]*?\\)$" "^[[:blank:]]*\\(?:```\\|~~~\\)\\s *?$" 1)
     ("<!\\[CDATA\\[#!/bin/bash" "#!end-of-fence" sh)
     ("<!\\[CDATA\\[<!--start html-->" "<!--end html-->\\]\\]>" html)
     ("<!--start nxml-->" "<!--end nxml-->" nxml)
     ("{%\\(.*?\\)-mode%}" ".*{%/\\(.*?\\)-mode%}" 1))))
 '(fill-column 70)
 '(gc-cons-threshold 3500000)
 '(gdb-find-source-frame t)
 '(gdb-many-windows t)
 '(gdb-same-frame t)
 '(gdb-show-main t)
 '(global-emojify-mode t)
 '(global-origami-mode t)
 '(gnus-article-date-headers (quote (local lapsed)))
 '(gnus-group-name-charset-group-alist (quote (("ali\\..*" . utf-8) (".*" . chinese-iso-8bit))))
 '(gnus-ignored-newsgroups "")
 '(gnus-propagate-marks t)
 '(grep-use-null-device nil)
 '(gui-select-enable-clipboard t)
 '(helm-company-fuzzy-match nil)
 '(helm-idle-delay 0.1)
 '(helm-input-idle-delay 0.1)
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (("#49483E" . 0)
    ("#67930F" . 20)
    ("#349B8D" . 30)
    ("#21889B" . 50)
    ("#968B26" . 60)
    ("#A45E0A" . 70)
    ("#A41F99" . 85)
    ("#49483E" . 100)))
 '(ido-enable-regexp t)
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".*\\.\\(loc\\|org\\|mkelem\\)")))
 '(imenu-max-item-length nil)
 '(imenu-space-replacement " ")
 '(install-elisp-repository-directory "~/system-config/.emacs_d/lisp/")
 '(ispell-program-name "aspell")
 '(jira-host "localhost.18888")
 '(jira-url "http://localhost:18888/rpc/xmlrpc")
 '(jiralib-url "http://localhost:8081/")
 '(keyboard-coding-system (quote cp936))
 '(linum-format " %7i ")
 '(lisp-mode-hook
   (quote
    ((lambda nil
       (make-local-variable
        (quote ajoke-symbol-chars))
       (setq ajoke-symbol-chars "-A-Za-z0-9_")))))
 '(longlines-auto-wrap nil)
 '(magit-use-overlays nil)
 '(makefile-mode-hook
   (quote
    ((lambda nil
       (make-local-variable
        (quote ajoke-symbol-chars))
       (setq ajoke-symbol-chars "-A-Za-z0-9_")))))
 '(max-lisp-eval-depth 80000)
 '(max-specpdl-size 50000)
 '(message-dont-reply-to-names (quote (".*haojun.*" "hjbao")))
 '(message-mail-alias-type nil)
 '(mmm-global-mode nil nil (mmm-mode))
 '(mmm-submode-decoration-level 2)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(mu4e-compose-forward-as-attachment t)
 '(nnmail-expiry-wait (quote never))
 '(normal-erase-is-backspace nil)
 '(org-clock-x11idle-program-name "xprintidle")
 '(org-directory "~/system-config/doc/projects")
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
 '(org-jira-default-jql
   "(reporter = currentUser() or assignee = currentUser()) and resolution = unresolved
   ORDER BY priority DESC, created ASC")
 '(org-latex-classes
   (quote
    (("中文文章" "\\documentclass[11pt,dvipdfmx,CJKbookmarks]{article}
\\usepackage{CJKutf8}
\\usepackage{atbegshi}
\\AtBeginShipoutFirst{\\special{pdf:tounicode UTF8-UTF16}} % for UTF-8
 [DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org-mobile-directory "~/today/MobileOrg")
 '(org-mobile-inbox-for-pull "~/system-config/doc/projects/from-mobile.org")
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-use-sub-superscripts (quote {}))
 '(org2blog/wp-confirm-post t)
 '(org2blog/wp-use-tags-as-categories t)
 '(package-selected-packages
   (quote
    (ghub go-mode web-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized company-shell company-statistics helm-company ivy ivy-bibtex ivy-dired-history ivy-erlang-complete ivy-gitlab ivy-hydra ivy-pages ivy-pass ivy-purpose ivy-rich ivy-rtags ivy-todo ivy-xcdoc ivy-youtube qml-mode cmake-mode emojify jedi org tabbar evil helm ddskk flycheck-clojure flycheck yari whole-line-or-region tidy smarty-mode session rinari redshank project-local-variables org-fstree mwe-log-commands lively keydef js-comint ipretty ibuffer-vc htmlize hl-sexp hippie-expand-slime highlight-quoted highlight-escape-sequences hi2 hayoo guide-key gitignore-mode github-clone gitconfig-mode git-messenger git-gutter-fringe git-blame ghci-completion fullframe fill-column-indicator f expand-region exec-path-from-shell erlang elisp-slime-nav elein dts-mode dsvn dired-sort dired+ diminish diff-hl default-text-scale darcsum csv-nav csv-mode csharp-mode crontab-mode coffee-mode clojure-test-mode cljsbuild-mode cl-lib-highlight bug-reference-github bbyac bbdb-vcard auto-complete-clang auto-compile anzu aggressive-indent ace-jump-mode ac-slime ac-nrepl ac-inf-ruby ac-helm ac-clang ac-cider)))
 '(rainbow-identifiers-cie-l*a*b*-lightness 70)
 '(rainbow-identifiers-cie-l*a*b*-saturation 20)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-D__PIC__" "-std=c++11" "-stdlib=libc++" "-I/usr/lib/llvm-3.8/include" "-I/usr/lib/llvm-3.8/lib/clang/3.8.1/include" "-I/usr/include/x86_64-linux-gnu/qt5/QtConcurrent" "-I/home/bhj/src/github/Wrench/qt-solutions/qtsingleapplication/src/" "-I/usr/include/x86_64-linux-gnu/qt5/QtCore" "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus" "-I/usr/include/x86_64-linux-gnu/qt5/QtGui" "-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGL" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGLExtensions" "-I/usr/include/x86_64-linux-gnu/qt5/QtPlatformSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtPrintSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtSql" "-I/usr/include/x86_64-linux-gnu/qt5/QtTest" "-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets" "-I/usr/include/x86_64-linux-gnu/qt5/QtXml" "-I/usr/include/x86_64-linux-gnu/qt5" "-I/usr/include" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include/lua5.2" "-I/usr/include/x86_64-linux-gnu/c++/6" "-I/usr/include/c++/6" "-I/home/bhj/tmp/build-wrench" "-I/usr/local/include" "-I/home/bhj/src/github/Wrench/QHotKey/QHotkey" "-I.")
     (emojify-mode . 1)
     (read-only-mode . t)
     (view-mode . t)
     (create-lockfiles)
     (org-html-table-default-attributes :border "2" :rules "all" :frame "border")
     (org-html-table-default-attributes quote
                                        (:border "2" :rules "all" :frame "border"))
     (buffer-file-coding-system . gbk-dos)
     (flycheck-clang-include-path "/usr/include/c++/4.9" "/usr/include/x86_64-linux-gnu/c++/4.9" "/usr/include/c++/4.9/backward" "/usr/lib/gcc/x86_64-linux-gnu/4.9/include" "/usr/local/include" "/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed" "/usr/include/x86_64-linux-gnu" "/usr/include" ".")
     (no-byte-compile t)
     (bhj-grep-dir . "~/src/android/.repo")
     (bhj-grep-dir . "~/src/android")
     (bhj-help-qt-prog . "search-msdn-lucky")
     (flycheck-mode)
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
 '(save-place-mode t nil (saveplace))
 '(scalable-fonts-allowed t)
 '(select-enable-primary t)
 '(senator-minor-mode-hook (quote (ignore)))
 '(session-initialize (quote (de-saveplace session places keys menus)))
 '(session-use-package t nil (session))
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote parenthesis))
 '(skk-user-directory "~/system-config/.ddskk")
 '(starttls-use-gnutls t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tooltip-mode nil)
 '(tooltip-use-echo-area t)
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
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(woman-manpath (quote ("/usr/man" "/usr/share/man" "/usr/local/man")))
 '(woman-use-own-frame nil)
 '(yas-also-auto-indent-first-line t)
 '(yas-prompt-functions (quote (yas-completing-prompt)))
 '(yas-snippet-dirs
   (quote
    ("~/src/github/private-config/etc/yasnippet" "~/system-config/.emacs_d/yasnippet/snippets" "~/system-config/.emacs_d/yasnippet-snippets")))
 '(yas-trigger-key "M-TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
