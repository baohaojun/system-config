;;; elein-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (elein-run-task elein-run-cmd elein-reswank elein-kill-swank
;;;;;;  elein-swank) "elein" "elein.el" (21286 54632 619978 609000))
;;; Generated autoloads from elein.el

(autoload 'elein-swank "elein" "\
Launch lein swank and connect slime to it.  Interactively, a
PREFIX means launch a standalone swank session without a
project.

\(fn &optional PREFIX)" t nil)

(autoload 'elein-kill-swank "elein" "\
Kill swank process started by lein swank.

\(fn)" t nil)

(autoload 'elein-reswank "elein" "\
Kill current lisp, restart lein swank and connect slime to it.

\(fn)" t nil)

(autoload 'elein-run-cmd "elein" "\
Run 'lein ARGS' using `compile' in the project root directory.

\(fn ARGS)" t nil)

(autoload 'elein-run-task "elein" "\
Run 'lein TASK' using `compile' in the project root directory.

\(fn TASK)" t nil)

;;;***

;;;### (autoloads nil nil ("elein-pkg.el") (21286 54632 707229 77000))

;;;***

(provide 'elein-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elein-autoloads.el ends here
