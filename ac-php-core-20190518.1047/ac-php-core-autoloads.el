;;; ac-php-core-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ac-php-core" "ac-php-core.el" (0 0 0 0))
;;; Generated autoloads from ac-php-core.el

(let ((loads (get 'ac-php 'custom-loads))) (if (member '"ac-php-core" loads) nil (put 'ac-php 'custom-loads (cons '"ac-php-core" loads))))

(autoload 'ac-php-core-eldoc-setup "ac-php-core" "\
Enable the ElDoc support for the PHP language.
Configure the variable `eldoc-documentation-function' and
call the command `eldoc-mode'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-php-core" '("ac-php-" "case-fold")))

;;;***

;;;### (autoloads nil nil ("ac-php-comm-tags-data.el" "ac-php-core-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-php-core-autoloads.el ends here
