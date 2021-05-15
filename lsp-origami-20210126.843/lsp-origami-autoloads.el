;;; lsp-origami-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-origami" "lsp-origami.el" (0 0 0 0))
;;; Generated autoloads from lsp-origami.el

(autoload 'lsp-origami-try-enable "lsp-origami" "\
Turn on `origami-mode' locally and try to enable `lsp-origami-mode'." t nil)

(autoload 'lsp-origami-mode "lsp-origami" "\
Toggle code folding support for origami.

If called interactively, enable Lsp-Origami mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-origami" '("lsp-origami--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-origami-autoloads.el ends here
