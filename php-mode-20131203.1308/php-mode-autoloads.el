;;; php-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (php-mode php-extra-constants php) "php-mode" "php-mode.el"
;;;;;;  (21286 54542 367793 210000))
;;; Generated autoloads from php-mode.el

(let ((loads (get 'php 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php 'custom-loads (cons '"php-mode" loads))))

(defvar php-extra-constants 'nil "\
A list of additional strings to treat as PHP constants.")

(custom-autoload 'php-extra-constants "php-mode" t)

(add-to-list 'interpreter-mode-alist (cons "php" 'php-mode))

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

(dolist (pattern '("\\.php[s345t]?\\'" "\\.phtml\\'" "Amkfile" "\\.amk$")) (add-to-list 'auto-mode-alist `(,pattern . php-mode) t))

;;;***

;;;### (autoloads nil nil ("php-mode-pkg.el") (21286 54542 448248
;;;;;;  378000))

;;;***

(provide 'php-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; php-mode-autoloads.el ends here
