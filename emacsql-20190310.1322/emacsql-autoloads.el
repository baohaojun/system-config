;;; emacsql-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emacsql" "emacsql.el" (0 0 0 0))
;;; Generated autoloads from emacsql.el

(autoload 'emacsql-show-last-sql "emacsql" "\
Display the compiled SQL of the s-expression SQL expression before point.
A prefix argument causes the SQL to be printed into the current buffer.

\(fn &optional PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql" '("emacsql-")))

;;;***

;;;### (autoloads nil "emacsql-compiler" "emacsql-compiler.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from emacsql-compiler.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emacsql-compiler" '("emacsql-")))

;;;***

;;;### (autoloads nil nil ("emacsql-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emacsql-autoloads.el ends here
