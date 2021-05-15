;;; ghub-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "buck" "buck.el" (0 0 0 0))
;;; Generated autoloads from buck.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "buck" '("buck-default-host")))

;;;***

;;;### (autoloads nil "ghub" "ghub.el" (0 0 0 0))
;;; Generated autoloads from ghub.el

(autoload 'ghub-clear-caches "ghub" "\
Clear all caches that might negatively affect Ghub.

If a library that is used by Ghub caches incorrect information
such as a mistyped password, then that can prevent Ghub from
asking the user for the correct information again.

Set `url-http-real-basic-auth-storage' to nil
and call `auth-source-forget+'." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ghub" '("ghub-" "url-http-handle-authentication@unauthorized-bugfix")))

;;;***

;;;### (autoloads nil "ghub-graphql" "ghub-graphql.el" (0 0 0 0))
;;; Generated autoloads from ghub-graphql.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ghub-graphql" '("ghub-")))

;;;***

;;;### (autoloads nil "glab" "glab.el" (0 0 0 0))
;;; Generated autoloads from glab.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "glab" '("glab-default-host")))

;;;***

;;;### (autoloads nil "gogs" "gogs.el" (0 0 0 0))
;;; Generated autoloads from gogs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gogs" '("gogs-default-host")))

;;;***

;;;### (autoloads nil "gsexp" "gsexp.el" (0 0 0 0))
;;; Generated autoloads from gsexp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gsexp" '("gsexp-")))

;;;***

;;;### (autoloads nil "gtea" "gtea.el" (0 0 0 0))
;;; Generated autoloads from gtea.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gtea" '("gtea-default-host")))

;;;***

;;;### (autoloads nil nil ("ghub-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ghub-autoloads.el ends here
