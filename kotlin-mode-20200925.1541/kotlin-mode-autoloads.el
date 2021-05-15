;;; kotlin-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kotlin-mode" "kotlin-mode.el" (0 0 0 0))
;;; Generated autoloads from kotlin-mode.el

(autoload 'kotlin-mode "kotlin-mode" "\
Major mode for editing Kotlin.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.kts?\\'" . kotlin-mode) t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kotlin-mode" '("kotlin-")))

;;;***

;;;### (autoloads nil "kotlin-mode-lexer" "kotlin-mode-lexer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from kotlin-mode-lexer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kotlin-mode-lexer" '("kotlin-mode--")))

;;;***

;;;### (autoloads nil nil ("kotlin-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kotlin-mode-autoloads.el ends here
