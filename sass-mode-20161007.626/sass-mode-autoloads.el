;;; sass-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sass-mode" "sass-mode.el" (0 0 0 0))
;;; Generated autoloads from sass-mode.el

(autoload 'sass-mode "sass-mode" "\
Major mode for editing Sass files.

\\{sass-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sass-mode" '("sass-")))

;;;***

;;;### (autoloads nil nil ("sass-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sass-mode-autoloads.el ends here
