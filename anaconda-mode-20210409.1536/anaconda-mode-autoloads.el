;;; anaconda-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "anaconda-mode" "anaconda-mode.el" (0 0 0 0))
;;; Generated autoloads from anaconda-mode.el

(autoload 'anaconda-mode "anaconda-mode" "\
Code navigation, documentation lookup and completion for Python.

If called interactively, enable Anaconda mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\\{anaconda-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'anaconda-eldoc-mode "anaconda-mode" "\
Toggle echo area display of Python objects at point.

If called interactively, enable Anaconda-Eldoc mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "anaconda-mode" '("anaconda-" "turn-o")))

;;;***

;;;### (autoloads nil nil ("anaconda-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; anaconda-mode-autoloads.el ends here
