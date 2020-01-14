;;; uptimes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "uptimes" "uptimes.el" (0 0 0 0))
;;; Generated autoloads from uptimes.el

(autoload 'uptimes-save "uptimes" "\
Write the uptimes to `uptimes-database'.

\(fn)" t nil)

(autoload 'uptimes "uptimes" "\
Display the last and top `uptimes-keep-count' uptimes.

\(fn)" t nil)

(autoload 'uptimes-current "uptimes" "\
Display the uptime for the current Emacs session.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "uptimes" '("uptimes-")))

;;;***

;;;### (autoloads nil nil ("uptimes-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; uptimes-autoloads.el ends here
