;;; magit-gh-pulls-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magit-gh-pulls" "magit-gh-pulls.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magit-gh-pulls.el

(autoload 'magit-gh-pulls-mode "magit-gh-pulls" "\
Pull requests support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-gh-pulls "magit-gh-pulls" "\
Unconditionally turn on `magit-pulls-mode'.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magit-gh-pulls" '("magit-" "-magit-gh-pulls-filter-and-split-host-lines")))

;;;***

;;;### (autoloads nil nil ("magit-gh-pulls-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-gh-pulls-autoloads.el ends here
