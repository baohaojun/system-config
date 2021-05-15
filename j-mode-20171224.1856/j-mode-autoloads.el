;;; j-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "j-console" "j-console.el" (0 0 0 0))
;;; Generated autoloads from j-console.el

(autoload 'j-console "j-console" "\
Ensures a running j-console-cmd session and switches focus to
the containing buffer" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "j-console" '("inferior-j-mode" "j-console-")))

;;;***

;;;### (autoloads nil "j-font-lock" "j-font-lock.el" (0 0 0 0))
;;; Generated autoloads from j-font-lock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "j-font-lock" '("j-")))

;;;***

;;;### (autoloads nil "j-help" "j-help.el" (0 0 0 0))
;;; Generated autoloads from j-help.el

(autoload 'j-help-lookup-symbol "j-help" "\
Lookup symbol in dictionary

\(fn SYMBOL)" t nil)

(autoload 'j-help-lookup-symbol-at-point "j-help" "\
Determine the symbol nearest to POINT and look it up in the dictionary

\(fn POINT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "j-help" '("group-by" "if-let" "j-help-")))

;;;***

;;;### (autoloads nil "j-mode" "j-mode.el" (0 0 0 0))
;;; Generated autoloads from j-mode.el

(autoload 'j-mode "j-mode" "\
Major mode for editing J" t nil)

(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "j-mode" '("j-mode-")))

;;;***

;;;### (autoloads nil nil ("j-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; j-mode-autoloads.el ends here
