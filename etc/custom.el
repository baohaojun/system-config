(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width 80)
 '(ahk-syntax-directory "~/system-config/doc/ahk/Syntax/")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
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
    ("e02ba8d4d8165ae7d9f3f8da11483c3273c8d794fddbf798caf381c6941a6142" "3632cf223c62cb7da121be0ed641a2243f7ec0130178722554e613c9ab3131de" "573e46dadf8c2623256a164831cfe9e42d5c700baed1f8ecd1de0675072e23c2" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" "a31c86c0a9ba5d06480b02bb912ae58753e09f13edeb07af8927d67c3bb94d68" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "c0dd5017b9f1928f1f337110c2da10a20f76da0a5b14bb1fec0f243c4eb224d4" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "ef43b291f7e96826d3d9bae61434a93020d0f529d609bc8be5b331980e8448d7" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "806d8c827b214f5f60348114bd27c6dcb5d19047f7ac482ad61e8077a6c5ea60" "930a202ae41cb4417a89bc3a6f969ebb7fcea5ffa9df6e7313df4f7a2a631434" "579e9950513524d8739e08eae289419cfcb64ed9b7cc910dd2e66151c77975c4" "56cb99174fad69feba8edd6663c592e77d77332fb4c4bb5be058ef459a426463" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "d6e27431f8cafb4a9136aebb1d4061f895b120bf88d34ff60c390d9905bd4e36" "569dc84822fc0ac6025f50df56eeee0843bffdeceff2c1f1d3b87d4f7d9fa661" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
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
 '(fci-rule-color "#444444")
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
 '(gui-select-enable-clipboard t)
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
 '(linum-format " %7i ")
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
 '(rainbow-identifiers-cie-l*a*b*-lightness 70)
 '(rainbow-identifiers-cie-l*a*b*-saturation 20)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((ac-clang-flags "-DQT_BOOTSTRAPPED" "-I/usr/include/x86_64-linux-gnu/qt5/QtConcurrent" "-I/usr/include/x86_64-linux-gnu/qt5/QtCore" "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus" "-I/usr/include/x86_64-linux-gnu/qt5/QtGui" "-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGL" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGLExtensions" "-I/usr/include/x86_64-linux-gnu/qt5/QtPlatformSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtPrintSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtSql" "-I/usr/include/x86_64-linux-gnu/qt5/QtTest" "-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets" "-I/usr/include/x86_64-linux-gnu/qt5/QtXml" "-I/usr/include/x86_64-linux-gnu/qt5" "-I/usr/include/c++/4.9" "-I/usr/include/x86_64-linux-gnu/c++/4.9" "-I/usr/include/c++/4.9/backward" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include" "-I/usr/local/include" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include" "-I/usr/include/lua5.2" "-I/home/bhj/tmp/build-t1" "-I.")
     (ac-clang-flags "-D__KERNEL__" "-include" "/home/bhj/src/linux/include/generated/autoconf.h" "-I/home/bhj/src/linux/arch/arm/mach-msm/include/" "-I/home/bhj/src/linux/include/" "-I/home/bhj/src/linux/arch/arm/include/" "-I/home/bhj/src/linux/arch/arm/include/generated/" "-I.")
     (ac-clang-flags "-include" "/home/bhj/src/linux/include/generated/autoconf.h" "-I/home/bhj/src/linux/arch/arm/mach-msm/include/" "-I/home/bhj/src/linux/include/" "-I/home/bhj/src/linux/arch/arm/include/" "-I/home/bhj/src/linux/arch/arm/include/generated/" "-I.")
     (ac-clang-flags "-include" "/home/bhj/src/linux/include/generated/autoconf.h" "-I/home/bhj/src/linux/include/" "-I/home/bhj/src/linux/arch/arm/include/" "-I/home/bhj/src/linux/arch/arm/include/generated/" "-I.")
     (ac-clang-flags "-I/home/bhj/src/linux/include/" "-I/home/bhj/src/linux/arch/arm/include/" "-I/home/bhj/src/linux/arch/arm/include/generated/" "-I.")
     (ac-clang-flags "-I/home/bhj/src/linux/include/" "-I/home/bhj/src/linux/arch/arm/include/" "-I.")
     (ac-clang-flags "-I/home/bhj/src/linux/include/" "-I.")
     (ac-clang-flags "-I/usr/include/x86_64-linux-gnu/qt5/QtConcurrent" "-I/usr/include/x86_64-linux-gnu/qt5/QtCore" "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus" "-I/usr/include/x86_64-linux-gnu/qt5/QtGui" "-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGL" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGLExtensions" "-I/usr/include/x86_64-linux-gnu/qt5/QtPlatformSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtPrintSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtSql" "-I/usr/include/x86_64-linux-gnu/qt5/QtTest" "-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets" "-I/usr/include/x86_64-linux-gnu/qt5/QtXml" "-I/usr/include/x86_64-linux-gnu/qt5" "-I/usr/include/c++/4.9" "-I/usr/include/x86_64-linux-gnu/c++/4.9" "-I/usr/include/c++/4.9/backward" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include" "-I/usr/local/include" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include" "-I/usr/include/lua5.2" "-I/home/bhj/tmp/build-t1" "-I.")
     (ac-clang-flags "-I/usr/include/x86_64-linux-gnu/qt5/QtConcurrent" "-I/usr/include/x86_64-linux-gnu/qt5/QtCore" "-I/usr/include/x86_64-linux-gnu/qt5/QtDBus" "-I/usr/include/x86_64-linux-gnu/qt5/QtGui" "-I/usr/include/x86_64-linux-gnu/qt5/QtNetwork" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGL" "-I/usr/include/x86_64-linux-gnu/qt5/QtOpenGLExtensions" "-I/usr/include/x86_64-linux-gnu/qt5/QtPlatformSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtPrintSupport" "-I/usr/include/x86_64-linux-gnu/qt5/QtSql" "-I/usr/include/x86_64-linux-gnu/qt5/QtTest" "-I/usr/include/x86_64-linux-gnu/qt5/QtWidgets" "-I/usr/include/x86_64-linux-gnu/qt5/QtXml" "-I/usr/include/x86_64-linux-gnu/qt5" "-I/usr/include/c++/4.9" "-I/usr/include/x86_64-linux-gnu/c++/4.9" "-I/usr/include/c++/4.9/backward" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include" "-I/usr/local/include" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include" "-I/usr/include/lua5.2" "-I.")
     (ac-clang-flags "-I/usr/include/c++/4.9" "-I/usr/include/x86_64-linux-gnu/c++/4.9" "-I/usr/include/c++/4.9/backward" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include" "-I/usr/local/include" "-I/usr/lib/gcc/x86_64-linux-gnu/4.9/include-fixed" "-I/usr/include/x86_64-linux-gnu" "-I/usr/include" "-I/home/bhj/src/android/bionic/libc/include" "-I.")
     (bhj-grep-dir . "~/src/android/.repo")
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
 '(select-enable-primary t)
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
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
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
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil)
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
 '(yas-also-auto-indent-first-line t)
 '(yas-prompt-functions (quote (yas-completing-prompt)))
 '(yas-trigger-key "M-TAB"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
