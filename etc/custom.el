(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width 80)
 '(after-save-hook
   '(sc--after-save sanityinc/set-mode-for-new-scripts executable-make-buffer-file-executable-if-script-p))
 '(ahk-syntax-directory "~/system-config/doc/ahk/Syntax/")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(auth-sources '((:source "~/.authinfo" :host t :protocol t)))
 '(auto-save-file-name-transforms
   '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
     (".*" "~/.emacs.d/tmp" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/tmp")))
 '(bbyac-max-chars 80)
 '(beacon-color "#ff9da4")
 '(before-save-hook '(confirm-risky-remote-edit cleanup-buffer-safe))
 '(canlock-password "78f140821d1f56625e4e7e035f37d6d06711d112")
 '(case-fold-search t)
 '(clipmon-mode t)
 '(company-idle-delay nil)
 '(compilation-message-face 'default)
 '(cperl-auto-newline nil)
 '(custom-enabled-themes '(sanityinc-solarized-dark))
 '(custom-safe-themes
   '("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default))
 '(debug-on-error nil)
 '(default-input-method "sdim")
 '(delete-old-versions t)
 '(describe-char-unidata-list
   '(name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value mirrored old-name iso-10646-comment uppercase lowercase titlecase))
 '(dictem-server "localhost")
 '(dictionary-server "localhost")
 '(douban-music-default-channel 6)
 '(ecb-options-version "2.40")
 '(eclim-executable "eclim")
 '(eclim-interactive-completion-function 'anything-completing-read)
 '(ecomplete-database-file-coding-system 'utf-8)
 '(edebug-eval-macro-args t)
 '(emms-player-list '(emms-player-mplayer))
 '(emojify-emoji-styles '(unicode))
 '(fci-rule-color "#003f8e")
 '(fence-edit-blocks
   '(("^[[:blank:]]*\\(?:```\\|~~~\\)[ ]?\\([^[:space:]]+\\|{[^}]*}\\)?\\(?:[[:space:]]*?\\)$" "^[[:blank:]]*\\(?:```\\|~~~\\)\\s *?$" 1)
     ("<!\\[CDATA\\[#!.*/bin/bash" "]]>" sh)
     ("<!\\[CDATA\\[<!--start html-->" "<!--end html-->" html)
     ("<!--start nxml-->" "<!--end nxml-->" nxml)
     ("{%\\(.*?\\)-mode%}" ".*{%/#1-mode%}" 1)))
 '(fill-column 70)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flycheck-shellcheck-excluded-warnings '("2034"))
 '(frame-background-mode 'dark)
 '(gc-cons-threshold 3500000)
 '(gdb-find-source-frame t)
 '(gdb-many-windows t)
 '(gdb-same-frame t)
 '(gdb-show-main t)
 '(global-emojify-mode t)
 '(global-origami-mode nil)
 '(global-undo-tree-mode t)
 '(gnus-article-date-headers '(local lapsed))
 '(gnus-group-name-charset-group-alist '(("ali\\..*" . utf-8) (".*" . chinese-iso-8bit)))
 '(gnus-ignored-newsgroups "")
 '(gnus-propagate-marks t)
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
   '("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".*\\.\\(loc\\|org\\|mkelem\\)"))
 '(imenu-max-item-length nil)
 '(imenu-space-replacement " ")
 '(install-elisp-repository-directory "~/system-config/.emacs_d/lisp/")
 '(ispell-program-name "aspell")
 '(ivy-initial-inputs-alist nil)
 '(jira-host "localhost.18888")
 '(jira-url "http://localhost:18888/rpc/xmlrpc")
 '(keyboard-coding-system 'cp936)
 '(kill-ring-max 200)
 '(linum-format " %7i ")
 '(lisp-mode-hook
   '((lambda nil
       (make-local-variable 'ajoke-symbol-chars)
       (setq ajoke-symbol-chars "-A-Za-z0-9_"))))
 '(longlines-auto-wrap nil)
 '(magit-use-overlays nil)
 '(makefile-mode-hook
   '((lambda nil
       (make-local-variable 'ajoke-symbol-chars)
       (setq ajoke-symbol-chars "-A-Za-z0-9_"))))
 '(max-lisp-eval-depth 80000)
 '(max-specpdl-size 50000)
 '(message-dont-reply-to-names '(".*haojun.*" "hjbao"))
 '(message-mail-alias-type nil)
 '(mmm-global-mode nil nil (mmm-mode))
 '(mmm-submode-decoration-level 2)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(mu4e-compose-forward-as-attachment t)
 '(netease-music-user-id 395205956)
 '(nnmail-expiry-wait 'never)
 '(normal-erase-is-backspace nil)
 '(org-clock-x11idle-program-name "xprintidle")
 '(org-directory "~/system-config/doc/projects")
 '(org-export-html-coding-system 'utf-8)
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
 '(org-export-with-sub-superscripts '{})
 '(org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "of %s")))
 '(org-html-htmlize-output-type 'css)
 '(org-jira-coding-system 'utf-8)
 '(org-jira-done-states '("Closed" "Resolved" "Done" "完成"))
 '(org-latex-classes
   '(("beamer" "\\documentclass[presentation]{beamer}
\\usepackage{bxdpx-beamer}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("中文文章" "\\documentclass[11pt,dvipdfmx,CJKbookmarks]{article}
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
     ("中文演示" "\\documentclass[presentation,dvipdfmx,CJKbookmarks]{beamer}
\\usepackage{bxdpx-beamer}
\\usepackage{CJKutf8}
\\usepackage{atbegshi}
\\AtBeginShipoutFirst{\\special{pdf:tounicode UTF8-UTF16}} % for UTF-8
 [DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
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
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
 '(org-mobile-directory "~/today/MobileOrg")
 '(org-mobile-inbox-for-pull "~/system-config/doc/projects/from-mobile.org")
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m))
 '(org-src-lang-modes
   '(("plantuml" . fundamental)
     ("ocaml" . tuareg)
     ("elisp" . emacs-lisp)
     ("ditaa" . artist)
     ("asymptote" . asy)
     ("dot" . graphviz-dot)
     ("sqlite" . sql)
     ("calc" . fundamental)
     ("C" . c)
     ("cpp" . c++)
     ("C++" . c++)
     ("screen" . shell-script)
     ("shell" . sh)
     ("bash" . sh)))
 '(org-support-shift-select nil)
 '(org-use-sub-superscripts '{})
 '(org2blog/wp-confirm-post t)
 '(org2blog/wp-use-tags-as-categories t)
 '(package-selected-packages
   '(uptimes dotenv-mode daemons dsvn htmlize gnuplot flycheck-ledger ledger-mode origami regex-tool info-colors flycheck-clojure cider elein cljsbuild-mode clojure-mode slime-company hippie-expand-slime slime cask-mode cl-libify flycheck-package highlight-quoted macrostep cl-lib-highlight aggressive-indent immortal-scratch auto-compile ipretty elisp-slime-nav paredit-everywhere paredit nginx-mode company-terraform terraform-mode docker-compose-mode dockerfile-mode docker yaml-mode toml-mode flycheck-rust racer rust-mode sqlformat projectile-rails yard-mode bundler goto-gem yari robe ruby-compilation inf-ruby rspec-mode ruby-hash-syntax psci psc-ide purescript-mode flycheck-elm elm-test-runner elm-mode dhall-mode reformatter dante haskell-mode company-anaconda anaconda-mode pip-requirements restclient httprepl haml-mode css-eldoc skewer-less sass-mode rainbow-mode tagedit org-pomodoro writeroom-mode org-cliplink company-php smarty-mode php-mode add-node-modules-path skewer-mode js-comint prettier-js typescript-mode coffee-mode js2-mode json-mode erlang csv-mode markdown-mode textile-mode cmd-to-echo alert github-clone bug-reference-github yagist git-commit magit-todos magit git-timemachine gitconfig-mode gitignore-mode git-blamed vc-darcs darcsum browse-at-remote whitespace-cleanup-mode guide-key highlight-escape-sequences whole-line-or-region move-dup page-break-lines multiple-cursors avy expand-region symbol-overlay rainbow-delimiters goto-line-preview beacon mode-line-bell vlf list-unicode-display unfill mmm-mode switch-window company-quickhelp company ivy-xref swiper projectile counsel ivy flycheck-color-mode-line flycheck ibuffer-vc anzu diff-hl diredfl disable-mouse default-text-scale command-log-mode scratch diminish project-local-variables yasnippet oauth2 lua-mode keydef csharp-mode dts-mode helm bbdb browse-kill-ring wgrep exec-path-from-shell fullframe seq lsp-mode eglot jq-mode tree-mode color-moccur py-autopep8 emms-player-simple-mpv emms-bilibili lyrics graphviz-dot-mode confluence ghub go-mode web-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized company-shell company-statistics ivy-dired-history ivy-gitlab ivy-pages ivy-pass ivy-purpose ivy-todo ivy-xcdoc ivy-youtube qml-mode cmake-mode org tabbar tidy session rinari redshank org-fstree mwe-log-commands lively hl-sexp hi2 hayoo git-messenger git-gutter-fringe git-blame ghci-completion dired-sort dired+ csv-nav clojure-test-mode bbyac bbdb-vcard auto-complete-clang ace-jump-mode ac-slime ac-nrepl ac-inf-ruby ac-helm ac-clang ac-cider))
 '(rainbow-identifiers-cie-l*a*b*-lightness 70)
 '(rainbow-identifiers-cie-l*a*b*-saturation 20)
 '(recentf-exclude nil)
 '(require-final-newline t)
 '(safe-local-variable-values
   '((org-html-postamble)
     (company-clang-arguments "-D__PIC__" "-I/home/bhj/src/reading/gtk+3.0-3.22.11" "-I/usr/lib/llvm-7/lib/clang/7.0.1/include/" "-I/home/bhj/src/reading/gtk+3.0-3.22.11/gdk" "-I/usr/include/glib-2.0" "-pthread" "-I/usr/include/at-spi-2.0" "-I/usr/include/at-spi2-atk/2.0" "-I/usr/include/atk-1.0" "-I/usr/include/cairo" "-I/usr/include/colord-1" "-I/usr/include/dbus-1.0" "-I/usr/include/freetype2" "-I/usr/include/gdk-pixbuf-2.0" "-I/usr/include/gio-unix-2.0/" "-I/usr/include/glib-2.0" "-I/usr/include/gobject-introspection-1.0" "-I/usr/include/harfbuzz" "-I/usr/include/json-glib-1.0" "-I/usr/include/libpng16" "-I/usr/include/libsoup-2.4" "-I/usr/include/libxml2" "-I/usr/include/pango-1.0" "-I/usr/include/pixman-1" "-I/usr/include/rest-0.7" "-I/usr/lib/x86_64-linux-gnu/dbus-1.0/include" "-I/usr/lib/x86_64-linux-gnu/glib-2.0/include" "-I/usr/include/pango-1.0" "-I/usr/include/linux" "-I/usr/include/harfbuzz" "-I/usr/include/pango-1.0" "-I/usr/include/fribidi" "-I/usr/include/gdk-pixbuf-2.0" "-I/usr/include/cairo" "-I/usr/include/gio-unix-2.0" "-I/usr/include/libmount" "-I/usr/include/blkid" "-I/usr/include/cairo" "-I/usr/include/glib-2.0" "-I/usr/lib/x86_64-linux-gnu/glib-2.0/include" "-I/usr/include/pixman-1" "-I/usr/include/uuid" "-I/usr/include/freetype2" "-I/usr/include/libpng16" "-I/usr/include/libdrm" "-I/usr/lib/llvm-3.8/include" "-I/usr/lib/llvm-3.8/lib/clang/3.8.1/include" "-I/usr/include/x86_64-linux-gnu/qt5/QtConcurrent" "-I/home/bhj/src/github/Wrench/qt-solutions/qtsingleapplication/src/" "-I/usr/include/x86_64-linux-gnu/qt5/QtCore" "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus" "-I/usr/include/x86_64-linux-gnu/qt5/QtGui" "-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGL" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGLExtensions" "-I/usr/include/x86_64-linux-gnu/qt5/QtPlatformSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtPrintSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtSql" "-I/usr/include/x86_64-linux-gnu/qt5/QtTest" "-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets" "-I/usr/include/x86_64-linux-gnu/qt5/QtXml" "-I/usr/include/x86_64-linux-gnu/qt5" "-I/usr/include" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include/lua5.2" "-I/usr/include/x86_64-linux-gnu/c++/6" "-I/usr/include/c++/6" "-I/home/bhj/tmp/build-wrench" "-I/usr/local/include" "-I/home/bhj/src/github/Wrench/QHotKey/QHotkey" "-I.")
     (company-clang-arguments "-D__PIC__" "-I/home/bhj/src/reading/gtk+3.0-3.22.11" "-I/home/bhj/src/reading/gtk+3.0-3.22.11/gdk" "-I/usr/include/glib-2.0" "-pthread" "-I/usr/include/at-spi-2.0" "-I/usr/include/at-spi2-atk/2.0" "-I/usr/include/atk-1.0" "-I/usr/include/cairo" "-I/usr/include/colord-1" "-I/usr/include/dbus-1.0" "-I/usr/include/freetype2" "-I/usr/include/gdk-pixbuf-2.0" "-I/usr/include/gio-unix-2.0/" "-I/usr/include/glib-2.0" "-I/usr/include/gobject-introspection-1.0" "-I/usr/include/harfbuzz" "-I/usr/include/json-glib-1.0" "-I/usr/include/libpng16" "-I/usr/include/libsoup-2.4" "-I/usr/include/libxml2" "-I/usr/include/pango-1.0" "-I/usr/include/pixman-1" "-I/usr/include/rest-0.7" "-I/usr/lib/x86_64-linux-gnu/dbus-1.0/include" "-I/usr/lib/x86_64-linux-gnu/glib-2.0/include" "-I/usr/include/pango-1.0" "-I/usr/include/linux" "-I/usr/include/harfbuzz" "-I/usr/include/pango-1.0" "-I/usr/include/fribidi" "-I/usr/include/gdk-pixbuf-2.0" "-I/usr/include/cairo" "-I/usr/include/gio-unix-2.0" "-I/usr/include/libmount" "-I/usr/include/blkid" "-I/usr/include/cairo" "-I/usr/include/glib-2.0" "-I/usr/lib/x86_64-linux-gnu/glib-2.0/include" "-I/usr/include/pixman-1" "-I/usr/include/uuid" "-I/usr/include/freetype2" "-I/usr/include/libpng16" "-I/usr/include/libdrm" "-I/usr/lib/llvm-3.8/include" "-I/usr/lib/llvm-3.8/lib/clang/3.8.1/include" "-I/usr/include/x86_64-linux-gnu/qt5/QtConcurrent" "-I/home/bhj/src/github/Wrench/qt-solutions/qtsingleapplication/src/" "-I/usr/include/x86_64-linux-gnu/qt5/QtCore" "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus" "-I/usr/include/x86_64-linux-gnu/qt5/QtGui" "-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGL" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGLExtensions" "-I/usr/include/x86_64-linux-gnu/qt5/QtPlatformSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtPrintSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtSql" "-I/usr/include/x86_64-linux-gnu/qt5/QtTest" "-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets" "-I/usr/include/x86_64-linux-gnu/qt5/QtXml" "-I/usr/include/x86_64-linux-gnu/qt5" "-I/usr/include" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include/lua5.2" "-I/usr/include/x86_64-linux-gnu/c++/6" "-I/usr/include/c++/6" "-I/home/bhj/tmp/build-wrench" "-I/usr/local/include" "-I/home/bhj/src/github/Wrench/QHotKey/QHotkey" "-I.")
     (company-clang-arguments "-D__PIC__" "-I/home/bhj/src/reading/gtk+3.0-3.22.11" "-I/home/bhj/src/reading/gtk+3.0-3.22.11/gdk" "-I/usr/include/glib-2.0" "-pthread" "-I/usr/include/pango-1.0" "-I/usr/include/linux" "-I/usr/include/harfbuzz" "-I/usr/include/pango-1.0" "-I/usr/include/fribidi" "-I/usr/include/gdk-pixbuf-2.0" "-I/usr/include/cairo" "-I/usr/include/gio-unix-2.0" "-I/usr/include/libmount" "-I/usr/include/blkid" "-I/usr/include/cairo" "-I/usr/include/glib-2.0" "-I/usr/lib/x86_64-linux-gnu/glib-2.0/include" "-I/usr/include/pixman-1" "-I/usr/include/uuid" "-I/usr/include/freetype2" "-I/usr/include/libpng16" "-I/usr/include/libdrm" "-I/usr/lib/llvm-3.8/include" "-I/usr/lib/llvm-3.8/lib/clang/3.8.1/include" "-I/usr/include/x86_64-linux-gnu/qt5/QtConcurrent" "-I/home/bhj/src/github/Wrench/qt-solutions/qtsingleapplication/src/" "-I/usr/include/x86_64-linux-gnu/qt5/QtCore" "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus" "-I/usr/include/x86_64-linux-gnu/qt5/QtGui" "-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGL" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGLExtensions" "-I/usr/include/x86_64-linux-gnu/qt5/QtPlatformSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtPrintSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtSql" "-I/usr/include/x86_64-linux-gnu/qt5/QtTest" "-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets" "-I/usr/include/x86_64-linux-gnu/qt5/QtXml" "-I/usr/include/x86_64-linux-gnu/qt5" "-I/usr/include" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include/lua5.2" "-I/usr/include/x86_64-linux-gnu/c++/6" "-I/usr/include/c++/6" "-I/home/bhj/tmp/build-wrench" "-I/usr/local/include" "-I/home/bhj/src/github/Wrench/QHotKey/QHotkey" "-I.")
     (eval org-shifttab 2)
     (company-clang-arguments "-D__PIC__" "-std=c++11" "-stdlib=libc++" "-I/usr/lib/llvm-3.8/include" "-I/usr/lib/llvm-3.8/lib/clang/3.8.1/include" "-I/usr/include/x86_64-linux-gnu/qt5/QtConcurrent" "-I/home/bhj/src/github/Wrench/qt-solutions/qtsingleapplication/src/" "-I/usr/include/x86_64-linux-gnu/qt5/QtCore" "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus" "-I/usr/include/x86_64-linux-gnu/qt5/QtGui" "-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGL" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGLExtensions" "-I/usr/include/x86_64-linux-gnu/qt5/QtPlatformSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtPrintSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtSql" "-I/usr/include/x86_64-linux-gnu/qt5/QtTest" "-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets" "-I/usr/include/x86_64-linux-gnu/qt5/QtXml" "-I/usr/include/x86_64-linux-gnu/qt5" "-I/usr/include" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include/lua5.2" "-I/usr/include/x86_64-linux-gnu/c++/6" "-I/usr/include/c++/6" "-I/home/bhj/tmp/build-wrench" "-I/usr/local/include" "-I/home/bhj/src/github/Wrench/QHotKey/QHotkey" "-I.")
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
     (indent-tab-mode . t)))
 '(save-place-mode t nil (saveplace))
 '(scalable-fonts-allowed t)
 '(select-enable-primary t)
 '(senator-minor-mode-hook '(ignore))
 '(send-mail-function 'smtpmail-send-it)
 '(session-initialize '(de-saveplace session places keys menus))
 '(session-use-package t nil (session))
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t nil (paren))
 '(show-paren-style 'parenthesis)
 '(skk-jisyo-edit-user-accepts-editing t)
 '(skk-user-directory "~/system-config/.ddskk")
 '(starttls-use-gnutls t)
 '(text-mode-hook '(text-mode-hook-identify))
 '(tooltip-mode nil)
 '(tooltip-use-echo-area t)
 '(transient-mark-mode t)
 '(twittering-convert-fix-size nil)
 '(user-full-name "Bao Haojun")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a")))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches "-w")
 '(vc-handled-backends '(RCS CVS SVN SCCS Bzr Git Hg))
 '(vc-ignore-dir-regexp
   "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'\\|/smb/")
 '(w32-symlinks-handle-shortcuts t)
 '(w32-use-w32-font-dialog nil)
 '(w3m-default-display-inline-images t)
 '(weblogger-config-alist
   '(("default" "https://storage.msn.com/storageservice/MetaWeblog.rpc" "thomasbhj" "" "MyBlog")
     ("csdn" "http://blog.csdn.net/flowermonk/services/MetaBlogApi.aspx" "flowermonk" "" "814038")))
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(whitespace-action '(auto-cleanup))
 '(whitespace-cleanup-mode-ignore-modes
   '(fundamental-mode markdown-mode special-mode view-mode comint-mode cider-repl-mode haskell-interactive-mode))
 '(whitespace-cleanup-mode-only-if-initially-clean nil)
 '(whitespace-global-modes '(not fundamental-mode))
 '(woman-manpath '("/usr/man" "/usr/share/man" "/usr/local/man"))
 '(woman-use-own-frame nil)
 '(yas-also-auto-indent-first-line t)
 '(yas-prompt-functions '(yas-completing-prompt))
 '(yas-snippet-dirs
   '("~/.local-config/etc/yasnippet" "~/system-config/.emacs_d/yasnippet/snippets" "~/system-config/.emacs_d/yasnippet-snippets"))
 '(yas-trigger-key "M-TAB")
 '(yas-wrap-around-region t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
