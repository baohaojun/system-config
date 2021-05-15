;;; flycheck-relint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck-relint" "flycheck-relint.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from flycheck-relint.el

(autoload 'flycheck-relint-setup "flycheck-relint" "\
Setup flycheck-package.
Add `emacs-lisp-relint' to `flycheck-checkers' and set up the checker chain." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-relint" '("flycheck-relint--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-relint-autoloads.el ends here
