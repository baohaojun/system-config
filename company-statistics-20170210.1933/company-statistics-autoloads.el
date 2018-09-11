;;; company-statistics-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-statistics" "company-statistics.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-statistics.el

(defvar company-statistics-mode nil "\
Non-nil if Company-Statistics mode is enabled.
See the `company-statistics-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `company-statistics-mode'.")

(custom-autoload 'company-statistics-mode "company-statistics" nil)

(autoload 'company-statistics-mode "company-statistics" "\
Statistical sorting for company-mode.  Ranks completion candidates by
the frequency with which they have been chosen in recent (as given by
`company-statistics-size') history.

Turning this mode on and off preserves the statistics.  They are also
preserved automatically between Emacs sessions in the default
configuration.  You can customize this behavior with
`company-statistics-auto-save', `company-statistics-auto-restore' and
`company-statistics-file'.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-statistics" '("company-s")))

;;;***

;;;### (autoloads nil nil ("company-statistics-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-statistics-autoloads.el ends here
