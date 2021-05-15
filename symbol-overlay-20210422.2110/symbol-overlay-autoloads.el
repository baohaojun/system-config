;;; symbol-overlay-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "symbol-overlay" "symbol-overlay.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from symbol-overlay.el

(autoload 'symbol-overlay-mode "symbol-overlay" "\
Minor mode for auto-highlighting symbol at point.

If called interactively, enable Symbol-Overlay mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'symbol-overlay-put "symbol-overlay" "\
Toggle all overlays of symbol at point." t nil)

(autoload 'symbol-overlay-count "symbol-overlay" "\
Show count of symbol at point." t nil)

(autoload 'symbol-overlay-remove-all "symbol-overlay" "\
Remove all highlighted symbols in the buffer.
When called interactively, then also reset
`symbol-overlay-keywords-alist'." t nil)

(autoload 'symbol-overlay-save-symbol "symbol-overlay" "\
Copy symbol at point." t nil)

(autoload 'symbol-overlay-toggle-in-scope "symbol-overlay" "\
Toggle overlays to be showed in buffer or only in scope." t nil)

(autoload 'symbol-overlay-echo-mark "symbol-overlay" "\
Jump back to the mark." t nil)

(autoload 'symbol-overlay-jump-next "symbol-overlay" "\
Jump to the next location of symbol at point." t nil)

(autoload 'symbol-overlay-jump-prev "symbol-overlay" "\
Jump to the previous location of symbol at point." t nil)

(autoload 'symbol-overlay-jump-first "symbol-overlay" "\
Jump to the first location." t nil)

(autoload 'symbol-overlay-jump-last "symbol-overlay" "\
Jump to the last location." t nil)

(autoload 'symbol-overlay-jump-to-definition "symbol-overlay" "\
Jump to the definition of symbol at point.
The definition syntax should be defined in a function stored in
`symbol-overlay-definition-function' that returns the definition's regexp
with the input symbol." t nil)

(autoload 'symbol-overlay-switch-forward "symbol-overlay" "\
Switch forward to another symbol." t nil)

(autoload 'symbol-overlay-switch-backward "symbol-overlay" "\
Switch backward to another symbol." t nil)

(autoload 'symbol-overlay-isearch-literally "symbol-overlay" "\
Isearch symbol at point literally." t nil)

(autoload 'symbol-overlay-query-replace "symbol-overlay" "\
Query replace symbol at point." t nil)

(autoload 'symbol-overlay-rename "symbol-overlay" "\
Rename symbol at point on all its occurrences." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "symbol-overlay" '("symbol-overlay-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; symbol-overlay-autoloads.el ends here
