;;; lsp-latex-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-latex" "lsp-latex.el" (0 0 0 0))
;;; Generated autoloads from lsp-latex.el

(autoload 'lsp-latex-forward-search-with-pdf-tools "lsp-latex" "\
Forward search with pdf-tools, from TEX-FILE line LINE to PDF-FILE.
This function is partially copied from
`pdf-sync-forward-search' and `pdf-sync-forward-correlate'.

\(fn TEX-FILE PDF-FILE LINE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-latex" '("lsp-latex-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-latex-autoloads.el ends here
