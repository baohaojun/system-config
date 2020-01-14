;;; psci-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "psci" "psci.el" (0 0 0 0))
;;; Generated autoloads from psci.el

(autoload 'psci "psci" "\
Run an inferior instance of \"psci\" inside Emacs, in PROJECT-ROOT-FOLDER.
If not supplied, the root folder will be guessed using
`projectile-project-root' (if available), otherwise it will
default to the current buffer's directory.

\(fn PROJECT-ROOT-FOLDER)" t nil)

(autoload 'psci-mode "psci" "\
Major mode for `run-psci'.

\\<psci-mode-map>

\(fn)" t nil)

(autoload 'psci/load-current-file! "psci" "\
Load the current file in the psci repl.

\(fn)" t nil)

(autoload 'psci/load-module! "psci" "\
Load the module inside the repl session.

\(fn)" t nil)

(autoload 'psci/reset! "psci" "\
Reset the current status of the repl session.

\(fn)" t nil)

(autoload 'psci/quit! "psci" "\
Quit the psci session.

\(fn)" t nil)

(autoload 'inferior-psci-mode "psci" "\
psci minor mode to define default bindings.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psci" '("inferior-psci-mode-map" "psci")))

;;;***

;;;### (autoloads nil "psci-dev" "psci-dev.el" (0 0 0 0))
;;; Generated autoloads from psci-dev.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "psci-dev" '("psci/--debug-setup!")))

;;;***

;;;### (autoloads nil nil ("psci-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; psci-autoloads.el ends here
