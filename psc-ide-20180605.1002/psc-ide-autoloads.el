;;; psc-ide-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "psc-ide" "psc-ide.el" (0 0 0 0))
;;; Generated autoloads from psc-ide.el

(autoload 'psc-ide-mode "psc-ide" "\
psc-ide-mode definition

\(fn &optional ARG)" t nil)

(autoload 'psc-ide-xref-backend "psc-ide" "\
psc-ide backend for Xref.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psc-ide" '("psc-ide-" "company-psc-ide-backend")))

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
Replace error with suggestion from psc compiler.

\(fn)" t nil)

(autoload 'psc-ide-flycheck-setup "psc-ide-flycheck" "\
Setup Flycheck purescript.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psc-ide-flycheck" '("psc-ide-flycheck-")))

;;;***

;;;### (autoloads nil "psc-ide-protocol" "psc-ide-protocol.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from psc-ide-protocol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psc-ide-protocol" '("psc-ide-" "wrap-psc-ide-callback")))

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
