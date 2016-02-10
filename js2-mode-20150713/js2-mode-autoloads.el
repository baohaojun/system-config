;;; js2-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "js2-imenu-extras" "js2-imenu-extras.el" (21927
;;;;;;  14993 549877 72000))
;;; Generated autoloads from js2-imenu-extras.el

(autoload 'js2-imenu-extras-setup "js2-imenu-extras" "\


\(fn)" nil nil)

(autoload 'js2-imenu-extras-mode "js2-imenu-extras" "\
Toggle Imenu support for frameworks and structural patterns.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "js2-mode" "js2-mode.el" (21927 14993 457877
;;;;;;  971000))
;;; Generated autoloads from js2-mode.el

(autoload 'js2-highlight-unused-variables-mode "js2-mode" "\
Toggle highlight of unused variables.

\(fn &optional ARG)" t nil)

(autoload 'js2-minor-mode "js2-mode" "\
Minor mode for running js2 as a background linter.
This allows you to use a different major mode for JavaScript editing,
such as `js-mode', while retaining the asynchronous error/warning
highlighting features of `js2-mode'.

\(fn &optional ARG)" t nil)

(autoload 'js2-mode "js2-mode" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("js2-mode-pkg.el") (21927 14993 998636
;;;;;;  903000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; js2-mode-autoloads.el ends here
