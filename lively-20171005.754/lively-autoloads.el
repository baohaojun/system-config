;;; lively-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lively" "lively.el" (0 0 0 0))
;;; Generated autoloads from lively.el

(autoload 'lively "lively" "\
Make the expression before point lively.

\(fn)" t nil)

(autoload 'lively-region "lively" "\
Make the region from START to END lively.

\(fn START END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lively" '("lively-")))

;;;***

;;;### (autoloads nil nil ("lively-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lively-autoloads.el ends here
