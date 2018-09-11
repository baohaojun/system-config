;;; cmd-to-echo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cmd-to-echo" "cmd-to-echo.el" (0 0 0 0))
;;; Generated autoloads from cmd-to-echo.el

(autoload 'cmd-to-echo "cmd-to-echo" "\
Start the COMMAND with the given OPTIONS.
The output of the command will be shown in the echo area.

\(fn COMMAND OPTIONS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cmd-to-echo" '("cmd-to-echo-")))

;;;***

;;;### (autoloads nil nil ("cmd-to-echo-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cmd-to-echo-autoloads.el ends here
