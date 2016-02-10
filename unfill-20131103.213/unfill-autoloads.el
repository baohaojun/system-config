;;; unfill-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (toggle-fill-unfill unfill-region unfill-paragraph)
;;;;;;  "unfill" "unfill.el" (21286 54476 83599 779000))
;;; Generated autoloads from unfill.el

(autoload 'unfill-paragraph "unfill" "\
Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

\(fn)" t nil)

(autoload 'unfill-region "unfill" "\
Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'.

\(fn START END)" t nil)

(autoload 'toggle-fill-unfill "unfill" "\
Remove or add line ending chars on current paragraph.  This command is similar to a toggle of `fill-paragraph'.  When there is a text selection, act on the region.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("unfill-pkg.el") (21286 54476 154211 145000))

;;;***

(provide 'unfill-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; unfill-autoloads.el ends here
