;;; paredit-everywhere-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "paredit-everywhere" "paredit-everywhere.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from paredit-everywhere.el

(autoload 'paredit-everywhere-mode "paredit-everywhere" "\
A cut-down version of paredit which can be used in non-lisp buffers.

If called interactively, enable Paredit-Everywhere mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "paredit-everywhere" '("paredit-everywhere-mode-map" "turn-off-paredit-everywhere-mode")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; paredit-everywhere-autoloads.el ends here
