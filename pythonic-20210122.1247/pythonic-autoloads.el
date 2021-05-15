;;; pythonic-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pythonic" "pythonic.el" (0 0 0 0))
;;; Generated autoloads from pythonic.el

(autoload 'pythonic-activate "pythonic" "\
Activate python VIRTUALENV.

\(fn VIRTUALENV)" t nil)

(autoload 'pythonic-deactivate "pythonic" "\
Deactivate python virtual environment." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pythonic" '("pythonic-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pythonic-autoloads.el ends here
