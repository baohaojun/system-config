;;; skewer-less-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "skewer-less" "skewer-less.el" (0 0 0 0))
;;; Generated autoloads from skewer-less.el

(autoload 'skewer-less-mode "skewer-less" "\
Minor mode allowing LESS stylesheet manipulation via `skewer-mode'.

If called interactively, enable Skewer-Less mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

For this to work properly, the lessc command must be available on
`exec-path', and `skewer' must be running.

\(fn &optional ARG)" t nil)

(autoload 'skewer-less-eval-buffer "skewer-less" "\
When skewer appears to be active, ask for a reload." t nil)

(autoload 'skewer-less-eval-region "skewer-less" "\
Process the region from BEG to END with \"lessc\", and pass it to `skewer-css'.

\(fn BEG END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "skewer-less" '("skewer-less-mode-map")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; skewer-less-autoloads.el ends here
