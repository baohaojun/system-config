;;; consult-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "consult" "consult.el" (0 0 0 0))
;;; Generated autoloads from consult.el

(autoload 'consult-multi-occur "consult" "\
Improved version of `multi-occur' based on `completing-read-multiple'.

See `multi-occur' for the meaning of the arguments BUFS, REGEXP and NLINES.

\(fn BUFS REGEXP &optional NLINES)" t nil)

(autoload 'consult-outline "consult" "\
Jump to an outline heading, obtained by matching against `outline-regexp'.

This command supports narrowing to a heading level and candidate preview.
The symbol at point is added to the future history." t nil)

(autoload 'consult-mark "consult" "\
Jump to a marker in the buffer-local `mark-ring'.

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history." t nil)

(autoload 'consult-global-mark "consult" "\
Jump to a marker in `global-mark-ring'.

The command supports preview of the currently selected marker position.
The symbol at point is added to the future history." t nil)

(autoload 'consult-line "consult" "\
Search for a matching line and jump to the line beginning.

The default candidate is a non-empty line closest to point.
This command obeys narrowing. Optional INITIAL input can be provided.
The search starting point is changed if the START prefix argument is set.
The symbol at point and the last `isearch-string' is added to the future history.

\(fn &optional INITIAL START)" t nil)

(autoload 'consult-keep-lines "consult" "\
Select a subset of the lines in the current buffer with live preview.

The selected lines are kept and the other lines are deleted. When called
interactively, the lines selected are those that match the minibuffer input. In
order to match the inverse of the input, prefix the input with `! '. When
called from elisp, the filtering is performed by a FILTER function. This
command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

\(fn &optional FILTER INITIAL)" t nil)

(autoload 'consult-focus-lines "consult" "\
Hide or show lines using overlays.

The selected lines are shown and the other lines hidden. When called
interactively, the lines selected are those that match the minibuffer input. In
order to match the inverse of the input, prefix the input with `! '. With
optional prefix argument SHOW reveal the hidden lines. When called from elisp,
the filtering is performed by a FILTER function. This command obeys narrowing.

FILTER is the filter function.
INITIAL is the initial input.

\(fn &optional SHOW FILTER INITIAL)" t nil)

(autoload 'consult-goto-line "consult" "\
Read line number and jump to the line with preview.

Jump directly if a line number is given as prefix ARG. The command respects
narrowing and the settings `consult-goto-line-numbers' and
`consult-line-numbers-widen'.

\(fn &optional ARG)" t nil)

(autoload 'consult-recent-file "consult" "\
Find recent using `completing-read'." t nil)

(autoload 'consult-file-externally "consult" "\
Open FILE externally using the default application of the system.

\(fn FILE)" t nil)

(autoload 'consult-completion-in-region "consult" "\
Prompt for completion of region in the minibuffer if non-unique.

The function is called with 4 arguments: START END COLLECTION PREDICATE.
The arguments and expected return value are as specified for
`completion-in-region'. Use as a value for `completion-in-region-function'.

The function can be configured via `consult-config'.

    (setf (alist-get #'consult-completion-in-region consult-config)
      '(:completion-styles (basic)))

These configuration options are supported:

    * :cycle-threshold - Cycling threshold (def: `completion-cycle-threshold')
    * :completion-styles - Use completion styles (def: `completion-styles')
    * :require-match - Require matches when completing (def: nil)
    * :prompt - The prompt string shown in the minibuffer

\(fn START END COLLECTION &optional PREDICATE)" nil nil)

(autoload 'consult-mode-command "consult" "\
Run a command from any of the given MODES.

If no MODES are specified, use currently active major and minor modes.

\(fn &rest MODES)" t nil)

(autoload 'consult-yank-from-kill-ring "consult" "\
Select STRING from the kill ring and insert it.
With prefix ARG, put point at beginning, and mark at end, like `yank' does.

This command behaves like `yank-from-kill-ring' in Emacs 28, which also offers
a `completing-read' interface to the `kill-ring'. Additionally the Consult
version supports preview of the selected string.

\(fn STRING &optional ARG)" t nil)

(autoload 'consult-yank-pop "consult" "\
If there is a recent yank act like `yank-pop'.

Otherwise select string from the kill ring and insert it.
See `yank-pop' for the meaning of ARG.

This command behaves like `yank-pop' in Emacs 28, which also offers a
`completing-read' interface to the `kill-ring'. Additionally the Consult
version supports preview of the selected string.

\(fn &optional ARG)" t nil)

(autoload 'consult-yank-replace "consult" "\
Select STRING from the kill ring.

If there was no recent yank, insert the string.
Otherwise replace the just-yanked string with the selected string.

There exists no equivalent of this command in Emacs 28.

\(fn STRING)" t nil)

(autoload 'consult-bookmark "consult" "\
If bookmark NAME exists, open it, otherwise create a new bookmark with NAME.

The command supports preview of file bookmarks and narrowing. See the
variable `consult-bookmark-narrow' for the narrowing configuration.

\(fn NAME)" t nil)

(autoload 'consult-apropos "consult" "\
Select pattern and call `apropos'.

The default value of the completion is the symbol at point." t nil)

(autoload 'consult-complex-command "consult" "\
Select and evaluate command from the command history.

This command can act as a drop-in replacement for `repeat-complex-command'." t nil)

(autoload 'consult-history "consult" "\
Insert string from HISTORY of current buffer.

In order to select from a specific HISTORY, pass the history variable as argument.

\(fn &optional HISTORY)" t nil)

(autoload 'consult-isearch "consult" "\
Read a search string with completion from history.

This replaces the current search string if Isearch is active, and
starts a new Isearch session otherwise." t nil)

(autoload 'consult-minor-mode-menu "consult" "\
Enable or disable minor mode.

This is an alternative to `minor-mode-menu-from-indicator'." t nil)

(autoload 'consult-theme "consult" "\
Disable current themes and enable THEME from `consult-themes'.

The command supports previewing the currently selected theme.

\(fn THEME)" t nil)

(autoload 'consult-buffer "consult" "\
Enhanced `switch-to-buffer' command with support for virtual buffers.

The command supports recent files, bookmarks, views and project files as virtual
buffers. Buffers are previewed. Furthermore narrowing to buffers (b), files (f),
bookmarks (m) and project files (p) is supported via the corresponding keys. In
order to determine the project-specific files and buffers, the
`consult-project-root-function' is used. See `consult-buffer-sources' and
`consult--multi' for the configuration of the virtual buffer sources." t nil)

(autoload 'consult-buffer-other-window "consult" "\
Variant of `consult-buffer' which opens in other window." t nil)

(autoload 'consult-buffer-other-frame "consult" "\
Variant of `consult-buffer' which opens in other frame." t nil)

(autoload 'consult-kmacro "consult" "\
Run a chosen keyboard macro.

With prefix ARG, run the macro that many times.
Macros containing mouse clicks are omitted.

\(fn ARG)" t nil)

(autoload 'consult-grep "consult" "\
Search for regexp with grep in DIR with INITIAL input.

The input string is split, the first part of the string is passed to
the asynchronous grep process and the second part of the string is
passed to the completion-style filtering. The input string is split at
a punctuation character, which is given as the first character of the
input string. The format is similar to Perl-style regular expressions,
e.g., /regexp/. Furthermore command line options can be passed to
grep, specified behind --.

Example: #async-regexp -- grep-opts#filter-string

The symbol at point is added to the future history. If `consult-grep'
is called interactively with a prefix argument, the user can specify
the directory to search in. By default the project directory is used
if `consult-project-root-function' is defined and returns non-nil.
Otherwise the `default-directory' is searched.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-git-grep "consult" "\
Search for regexp with grep in DIR with INITIAL input.

See `consult-grep' for more details.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-ripgrep "consult" "\
Search for regexp with rg in DIR with INITIAL input.

See `consult-grep' for more details.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-find "consult" "\
Search for regexp with find in DIR with INITIAL input.

The find process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

\(fn &optional DIR INITIAL)" t nil)

(autoload 'consult-locate "consult" "\
Search for regexp with locate with INITIAL input.

The locate process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

\(fn &optional INITIAL)" t nil)

(autoload 'consult-man "consult" "\
Search for regexp with man with INITIAL input.

The man process is started asynchronously, similar to `consult-grep'.
See `consult-grep' for more details regarding the asynchronous search.

\(fn &optional INITIAL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult" '("consult-")))

;;;***

;;;### (autoloads nil "consult-compile" "consult-compile.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from consult-compile.el

(autoload 'consult-compile-error "consult-compile" "\
Jump to a compilation error in the current buffer.

This command collects entries from compilation buffers and grep
buffers related to the current buffer.  The command supports
preview of the currently selected error." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-compile" '("consult-compile--")))

;;;***

;;;### (autoloads nil "consult-flymake" "consult-flymake.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from consult-flymake.el

(autoload 'consult-flymake "consult-flymake" "\
Jump to Flymake diagnostic." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-flymake" '("consult-flymake--")))

;;;***

;;;### (autoloads nil "consult-icomplete" "consult-icomplete.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from consult-icomplete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-icomplete" '("consult-icomplete--refresh")))

;;;***

;;;### (autoloads nil "consult-imenu" "consult-imenu.el" (0 0 0 0))
;;; Generated autoloads from consult-imenu.el

(autoload 'consult-imenu "consult-imenu" "\
Select item from flattened `imenu' using `completing-read' with preview.

The command supports preview and narrowing. See the variable
`consult-imenu-config', which configures the narrowing.

See also `consult-project-imenu'." t nil)

(autoload 'consult-project-imenu "consult-imenu" "\
Select item from the imenus of all buffers from the same project.

In order to determine the buffers belonging to the same project, the
`consult-project-root-function' is used. Only the buffers with the
same major mode as the current buffer are used. See also
`consult-imenu' for more details." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-imenu" '("consult-imenu-")))

;;;***

;;;### (autoloads nil "consult-org" "consult-org.el" (0 0 0 0))
;;; Generated autoloads from consult-org.el

(autoload 'consult-org-heading "consult-org" "\
Jump to an Org heading.

MATCH and SCOPE are as in `org-map-entries' and determine which
entries are offered.  By default, all entries of the current
buffer are offered.

\(fn &optional MATCH SCOPE)" t nil)

(autoload 'consult-org-agenda "consult-org" "\
Jump to an Org agenda heading.

By default, all agenda entries are offered. MATCH is as in
`org-map-entries' and can used to refine this.

\(fn &optional MATCH)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-org" '("consult-org--")))

;;;***

;;;### (autoloads nil "consult-register" "consult-register.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from consult-register.el

(autoload 'consult-register-window "consult-register" "\
Enhanced drop-in replacement for `register-preview'.

BUFFER is the window buffer.
SHOW-EMPTY must be t if the window should be shown for an empty register list.

\(fn BUFFER &optional SHOW-EMPTY)" nil nil)

(autoload 'consult-register-format "consult-register" "\
Enhanced preview of register REG.

This function can be used as `register-preview-function'.

\(fn REG)" nil nil)

(autoload 'consult-register "consult-register" "\
Load register and either jump to location or insert the stored text.

This command is useful to search the register contents. For quick access to
registers it is still recommended to use the register functions
`consult-register-load' and `consult-register-store' or the built-in built-in
register access functions. The command supports narrowing, see
`consult-register-narrow'. Marker positions are previewed. See
`jump-to-register' and `insert-register' for the meaning of prefix ARG.

\(fn &optional ARG)" t nil)

(autoload 'consult-register-load "consult-register" "\
Do what I mean with a REG.

For a window configuration, restore it. For a number or text, insert it. For a
location, jump to it. See `jump-to-register' and `insert-register' for the
meaning of prefix ARG.

\(fn REG &optional ARG)" t nil)

(autoload 'consult-register-store "consult-register" "\
Store register dependent on current context, showing an action menu.

With an active region, store/append/prepend the contents, optionally deleting
the region when a prefix ARG is given. With a numeric prefix ARG, store/add the
number. Otherwise store point, frameset, window or kmacro.

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-register" '("consult-register-")))

;;;***

;;;### (autoloads nil "consult-selectrum" "consult-selectrum.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from consult-selectrum.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-selectrum" '("consult-selectrum--")))

;;;***

;;;### (autoloads nil "consult-vertico" "consult-vertico.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from consult-vertico.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-vertico" '("consult-vertico--")))

;;;***

;;;### (autoloads nil "consult-xref" "consult-xref.el" (0 0 0 0))
;;; Generated autoloads from consult-xref.el

(autoload 'consult-xref "consult-xref" "\
Show xrefs with preview in the minibuffer.

This function can be used for `xref-show-xrefs-function'.
See `xref-show-xrefs-function' for the description of the
FETCHER and ALIST arguments.

\(fn FETCHER &optional ALIST)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "consult-xref" '("consult-xref--")))

;;;***

;;;### (autoloads nil nil ("consult-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; consult-autoloads.el ends here
