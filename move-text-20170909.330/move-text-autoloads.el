;;; move-text-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "move-text" "move-text.el" (0 0 0 0))
;;; Generated autoloads from move-text.el

(autoload 'move-text--total-lines "move-text" "\
Convenience function to get the total lines in the buffer / or narrowed buffer.

\(fn)" nil nil)

(autoload 'move-text--at-first-line-p "move-text" "\
Predicate, is the point at the first line?

\(fn)" nil nil)

(autoload 'move-text--at-penultimate-line-p "move-text" "\
Predicate, is the point at the penultimate line?

\(fn)" nil nil)

(autoload 'move-text--last-line-is-just-newline "move-text" "\
Predicate, is last line just a newline?

\(fn)" nil nil)

(autoload 'move-text--at-last-line-p "move-text" "\
Predicate, is the point at the last line?

\(fn)" nil nil)

(autoload 'move-text-line-up "move-text" "\
Move the current line up.

\(fn)" t nil)

(autoload 'move-text-line-down "move-text" "\
Move the current line down.

\(fn)" t nil)

(autoload 'move-text-region "move-text" "\
Move the current region (START END) up or down by N lines.

\(fn START END N)" t nil)

(autoload 'move-text-region-up "move-text" "\
Move the current region (START END) up by N lines.

\(fn START END N)" t nil)

(autoload 'move-text-region-down "move-text" "\
Move the current region (START END) down by N lines.

\(fn START END N)" t nil)

(autoload 'move-text-up "move-text" "\
Move the line or region (START END) up by N lines.

\(fn &optional START END N)" t nil)

(autoload 'move-text-down "move-text" "\
Move the line or region (START END) down by N lines.

\(fn &optional START END N)" t nil)

(autoload 'move-text-default-bindings "move-text" "\
Use default bindings for move-text-up and move-text-down (M-up / M-down).

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "move-text" '("move-text-get-region-and-prefix")))

;;;***

;;;### (autoloads nil nil ("move-text-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; move-text-autoloads.el ends here
