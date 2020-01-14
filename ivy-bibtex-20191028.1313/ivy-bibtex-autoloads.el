;;; ivy-bibtex-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bibtex-completion" "bibtex-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from bibtex-completion.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bibtex-completion" '("bibtex-completion-")))

;;;***

;;;### (autoloads nil "ivy-bibtex" "ivy-bibtex.el" (0 0 0 0))
;;; Generated autoloads from ivy-bibtex.el

(autoload 'ivy-bibtex "ivy-bibtex" "\
Search BibTeX entries using ivy.

With a prefix ARG the cache is invalidated and the bibliography
reread.

If LOCAL-BIB is non-nil, display that the BibTeX entries are read
from the local bibliography. This is set internally by
`ivy-bibtex-with-local-bibliography'.

\(fn &optional ARG LOCAL-BIB)" t nil)

(autoload 'ivy-bibtex-with-local-bibliography "ivy-bibtex" "\
Search BibTeX entries with local bibliography.

With a prefix ARG the cache is invalidated and the bibliography
reread.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-bibtex" '("ivy-bibtex-")))

;;;***

;;;### (autoloads nil nil ("ivy-bibtex-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-bibtex-autoloads.el ends here
