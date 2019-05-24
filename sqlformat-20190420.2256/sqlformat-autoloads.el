;;; sqlformat-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sqlformat" "sqlformat.el" (0 0 0 0))
;;; Generated autoloads from sqlformat.el
 (autoload 'sqlformat-buffer "sqlformat" nil t)
 (autoload 'sqlformat-region "sqlformat" nil t)
 (autoload 'sqlformat-on-save-mode "sqlformat" nil t)

(autoload 'sqlformat "sqlformat" "\
Reformat SQL in region from BEG to END using `sqlformat-region'.
If no region is active, the current statement (paragraph) is reformatted.
Install the \"sqlparse\" (Python) package to get \"sqlformat\", or
\"pgformatter\" to get \"pg_format\".

\(fn BEG END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sqlformat" '("sqlformat-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sqlformat-autoloads.el ends here
