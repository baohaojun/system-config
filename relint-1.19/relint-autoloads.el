;;; relint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "relint" "relint.el" (0 0 0 0))
;;; Generated autoloads from relint.el

(autoload 'relint-file "relint" "\
Scan FILE, an elisp file, for regexp-related errors.

\(fn FILE)" t nil)

(autoload 'relint-directory "relint" "\
Scan all *.el files in DIR for regexp-related errors.

\(fn DIR)" t nil)

(autoload 'relint-current-buffer "relint" "\
Scan the current buffer for regexp errors.
The buffer must be in emacs-lisp-mode." t nil)

(autoload 'relint-buffer "relint" "\
Scan BUFFER for regexp errors. Return list of diagnostics.
Each element in the returned list has the form

  (MESSAGE EXPR-POS ERROR-POS STRING STRING-IDX SEVERITY),

where MESSAGE is the message string, EXPR-POS the location of the
flawed expression, ERROR-POS the exact position of the error or
nil if unavailable, STRING is nil or a string to which the
message pertains, STRING-IDX is nil or an index into STRING,
and SEVERITY is `error' or `warning'.
The intent is that ERROR-POS is the position in the buffer that
corresponds to STRING at STRING-IDX, if such a location can be
determined.

\(fn BUFFER)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "relint" '("relint-")))

;;;***

;;;### (autoloads nil "relint-test" "relint-test.el" (0 0 0 0))
;;; Generated autoloads from relint-test.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "relint-test" '("relint-test--")))

;;;***

;;;### (autoloads nil nil ("relint-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; relint-autoloads.el ends here
