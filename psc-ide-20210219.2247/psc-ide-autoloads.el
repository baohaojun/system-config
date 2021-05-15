;;; psc-ide-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "psc-ide" "psc-ide.el" (0 0 0 0))
;;; Generated autoloads from psc-ide.el

(autoload 'psc-ide-mode "psc-ide" "\
psc-ide-mode definition

If called interactively, enable Psc-Ide mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'psc-ide-xref-backend "psc-ide" "\
Xref backend for psc-ide." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psc-ide" '("company-psc-ide-backend" "psc-ide-")))

;;;***

;;;### (autoloads nil "psc-ide-backported" "psc-ide-backported.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from psc-ide-backported.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psc-ide-backported" '("psc-ide-ident-")))

;;;***

;;;### (autoloads nil "psc-ide-flycheck" "psc-ide-flycheck.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from psc-ide-flycheck.el

(autoload 'psc-ide-flycheck-insert-suggestion "psc-ide-flycheck" "\
Replace error with suggestion from psc compiler." t nil)

(autoload 'psc-ide-flycheck-setup "psc-ide-flycheck" "\
Setup Flycheck purescript." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psc-ide-flycheck" '("psc-ide-flycheck-")))

;;;***

;;;### (autoloads nil "psc-ide-protocol" "psc-ide-protocol.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from psc-ide-protocol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psc-ide-protocol" '("psc-ide-")))

;;;***

;;;### (autoloads nil nil ("psc-ide-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; psc-ide-autoloads.el ends here
