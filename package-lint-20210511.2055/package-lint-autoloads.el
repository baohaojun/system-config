;;; package-lint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "package-lint" "package-lint.el" (0 0 0 0))
;;; Generated autoloads from package-lint.el

(autoload 'package-lint-describe-symbol-history "package-lint" "\
Show the version history of SYM, if any.

\(fn SYM)" t nil)

(autoload 'package-lint-buffer "package-lint" "\
Get linter errors and warnings for BUFFER.

Returns a list, each element of which is list of

   (LINE COL TYPE MESSAGE)

where TYPE is either 'warning or 'error.

Current buffer is used if none is specified.

\(fn &optional BUFFER)" nil nil)

(autoload 'package-lint-current-buffer "package-lint" "\
Display lint errors and warnings for the current buffer." t nil)

(autoload 'package-lint-batch-and-exit "package-lint" "\
Run `package-lint-buffer' on the files remaining on the command line.
Use this only with -batch, it won't work interactively.

When done, exit Emacs with status 1 in case of any errors, otherwise exit
with status 0.  The variable `package-lint-batch-fail-on-warnings' controls
whether or not warnings alone produce a non-zero exit code." nil nil)

(autoload 'package-lint-looks-like-a-package-p "package-lint" "\
Return non-nil if the current buffer appears to be intended as a package." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "package-lint" '("package-lint-")))

;;;***

;;;### (autoloads nil nil ("package-lint-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; package-lint-autoloads.el ends here
