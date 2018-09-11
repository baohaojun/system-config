;;; command-log-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "command-log-mode" "command-log-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from command-log-mode.el

(autoload 'command-log-mode "command-log-mode" "\
Toggle keyboard command logging.

\(fn &optional ARG)" t nil)

(autoload 'clm/toggle-command-log-buffer "command-log-mode" "\
Toggle the command log showing or not.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "command-log-mode" '("command-log-mode-" "clm/" "global-command-log-mode")))

;;;***

;;;### (autoloads nil nil ("command-log-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; command-log-mode-autoloads.el ends here
