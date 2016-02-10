;;; macrostep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (macrostep-expand macrostep-mode) "macrostep" "macrostep.el"
;;;;;;  (21286 54585 523878 833000))
;;; Generated autoloads from macrostep.el

(autoload 'macrostep-mode "macrostep" "\
Minor mode for inline expansion of macros in Emacs Lisp source buffers.

\\<macrostep-keymap>Progressively expand macro forms with \\[macrostep-expand], collapse them with \\[macrostep-collapse],
and move back and forth with \\[macrostep-next-macro] and \\[macrostep-prev-macro].
Use \\[macrostep-collapse-all] or collapse all visible expansions to
quit and return to normal editing.

\\{macrostep-keymap}

\(fn &optional ARG)" t nil)

(autoload 'macrostep-expand "macrostep" "\
Expand the Elisp macro form following point by one step.

Enters `macrostep-mode' if it is not already active, making the
buffer temporarily read-only. If macrostep-mode is active and the
form following point is not a macro form, search forward in the
buffer and expand the next macro form found, if any.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("macrostep-pkg.el") (21286 54585 610933
;;;;;;  725000))

;;;***

(provide 'macrostep-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; macrostep-autoloads.el ends here
