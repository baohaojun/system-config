;;; goto-line-preview-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "goto-line-preview" "goto-line-preview.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from goto-line-preview.el

(autoload 'goto-line-preview "goto-line-preview" "\
Preview goto line.
LINE-NUM : Target line number to navigate to.

\(fn)" t nil)

(define-obsolete-function-alias 'goto-line-preview-goto-line 'goto-line-preview)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "goto-line-preview" '("goto-line-preview-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; goto-line-preview-autoloads.el ends here
