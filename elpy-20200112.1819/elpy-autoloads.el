;;; elpy-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elpy" "elpy.el" (0 0 0 0))
;;; Generated autoloads from elpy.el

(autoload 'elpy-enable "elpy" "\
Enable Elpy in all future Python buffers.

\(fn &optional IGNORED)" t nil)

(autoload 'elpy-mode "elpy" "\
Minor mode in Python buffers for the Emacs Lisp Python Environment.

This mode fully supports virtualenvs. Once you switch a
virtualenv using \\[pyvenv-workon], you can use
\\[elpy-rpc-restart] to make the elpy Python process use your
virtualenv.

\\{elpy-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'elpy-config "elpy" "\
Configure Elpy.

This function will pop up a configuration buffer, which is mostly
a customize buffer, but has some more options.

\(fn)" t nil)

(autoload 'elpy-version "elpy" "\
Display the version of Elpy.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy" '("elpy-")))

;;;***

;;;### (autoloads nil "elpy-django" "elpy-django.el" (0 0 0 0))
;;; Generated autoloads from elpy-django.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy-django" '("elpy-")))

;;;***

;;;### (autoloads nil "elpy-profile" "elpy-profile.el" (0 0 0 0))
;;; Generated autoloads from elpy-profile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy-profile" '("elpy-profile-")))

;;;***

;;;### (autoloads nil "elpy-refactor" "elpy-refactor.el" (0 0 0 0))
;;; Generated autoloads from elpy-refactor.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy-refactor" '("elpy-refactor")))

;;;***

;;;### (autoloads nil "elpy-rpc" "elpy-rpc.el" (0 0 0 0))
;;; Generated autoloads from elpy-rpc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy-rpc" '("elpy-" "with-elpy-rpc-virtualenv-activated")))

;;;***

;;;### (autoloads nil "elpy-shell" "elpy-shell.el" (0 0 0 0))
;;; Generated autoloads from elpy-shell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elpy-shell" '("elpy-")))

;;;***

;;;### (autoloads nil nil ("elpy-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elpy-autoloads.el ends here
