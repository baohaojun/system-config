;;; company-terraform-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-terraform" "company-terraform.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-terraform.el

(autoload 'company-terraform "company-terraform" "\
Main entry point for a company backend.
Read `company-mode` function docs for the semantics of this function.

\(fn COMMAND &optional ARG &rest IGNORED)" nil nil)

(autoload 'company-terraform-init "company-terraform" "\
Add terraform to the company backends.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-terraform" '("company-terraform-")))

;;;***

;;;### (autoloads nil "company-terraform-data" "company-terraform-data.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-terraform-data.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-terraform-data" '("company-terraform-")))

;;;***

;;;### (autoloads nil nil ("company-terraform-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-terraform-autoloads.el ends here
