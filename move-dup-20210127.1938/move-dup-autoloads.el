;;; move-dup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "move-dup" "move-dup.el" (0 0 0 0))
;;; Generated autoloads from move-dup.el

(autoload 'move-dup-move-region "move-dup" "\
Interactive function to move the current selection N lines.

If the selection is not a rectangle, this function will expand
the selection to a rectangle via the function
`move-dup-ensure-rectangle' and move it accordingly.  If the prefix N
is positive, this function moves the rectangle forward N lines;
otherwise backward.

\(fn &optional N)" t nil)

(autoload 'move-dup-move-line "move-dup" "\
Interactive function to move the current line by N lines.

If the prefix N is positive, this function moves the current line
forward N lines; otherwise backward.

\(fn &optional N)" t nil)

(autoload 'move-dup-move-lines-up "move-dup" "\
Interactive function to move the current line or selection up.

If the prefix N is positive, this function moves the current line
or selection up N lines; otherwise down.

\(fn &optional N)" t nil)

(autoload 'move-dup-move-lines-down "move-dup" "\
Interactive function to move the current line or selection down.

If the prefix N is positive, this function moves the current line
or selection down N lines; otherwise up.

\(fn &optional N)" t nil)

(autoload 'move-dup-duplicate-up "move-dup" "\
Interactive function to duplicate the current line or selection upward.

If the prefix N is positive, this function makes N duplicates of
the current line or selection and place them above the current
line or selection.

\(fn &optional N)" t nil)

(autoload 'move-dup-duplicate-down "move-dup" "\
Interactive function to duplicate the current line or selection downward.

If the prefix N is positive, this function makes N duplicates of
the current line or selection and place them below the current
line or selection.

\(fn &optional N)" t nil)

(autoload 'move-dup-mode "move-dup" "\
Minor mode for Eclipse-like moving and duplicating lines or
rectangles with default key bindings.

If called interactively, enable Move-Dup mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

The default key bindings are:

\([M-up]     . move-dup-move-lines-up)
\([M-down]   . move-dup-move-lines-down)
\([C-M-up]   . move-dup-duplicate-up)
\([C-M-down] . move-dup-duplicate-down)

\(fn &optional ARG)" t nil)

(put 'global-move-dup-mode 'globalized-minor-mode t)

(defvar global-move-dup-mode nil "\
Non-nil if Global Move-Dup mode is enabled.
See the `global-move-dup-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-move-dup-mode'.")

(custom-autoload 'global-move-dup-mode "move-dup" nil)

(autoload 'global-move-dup-mode "move-dup" "\
Toggle Move-Dup mode in all buffers.
With prefix ARG, enable Global Move-Dup mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Move-Dup mode is enabled in all buffers where
`move-dup-on' would do it.
See `move-dup-mode' for more information on Move-Dup mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "move-dup" '("move-dup-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; move-dup-autoloads.el ends here
