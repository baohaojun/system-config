;;; cl-libify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cl-libify" "cl-libify.el" (0 0 0 0))
;;; Generated autoloads from cl-libify.el

(autoload 'cl-libify "cl-libify" "\
Replace cl symbol names between BEG and END with their cl-lib equivalents.

If no region is supplied, this operates on the entire
buffer.  With prefix argument PROMPT, ask the user to confirm each
replacement.

\(fn BEG END)" t nil)

(autoload 'cl-libify-mark-cl-symbols-obsolete "cl-libify" "\
Make all the `cl' vars and functions obsolete so that byte compilation will flag their use.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cl-libify" '("cl-libify-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cl-libify-autoloads.el ends here
