;;; purescript-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "purescript-align-imports" "purescript-align-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-align-imports.el

(autoload 'purescript-align-imports "purescript-align-imports" "\
Align all the imports in the buffer." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-align-imports" '("purescript-align-imports-")))

;;;***

;;;### (autoloads nil "purescript-collapse" "purescript-collapse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-collapse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-collapse" '("purescript-collapse")))

;;;***

;;;### (autoloads nil "purescript-decl-scan" "purescript-decl-scan.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-decl-scan.el

(autoload 'purescript-ds-create-imenu-index "purescript-decl-scan" "\
Function for finding `imenu' declarations in PureScript mode.
Finds all declarations (classes, variables, imports, instances and
datatypes) in a PureScript file for the `imenu' package." nil nil)

(autoload 'turn-on-purescript-decl-scan "purescript-decl-scan" "\
Unconditionally activate `purescript-decl-scan-mode'." t nil)

(autoload 'purescript-decl-scan-mode "purescript-decl-scan" "\
Toggle PureScript declaration scanning minor mode on or off.
With a prefix argument ARG, enable minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

See also info node `(purescript-mode)purescript-decl-scan-mode' for
more details about this minor mode.

Top-level declarations are scanned and listed in the menu item
\"Declarations\" (if enabled via option
`purescript-decl-scan-add-to-menubar').  Selecting an item from this
menu will take point to the start of the declaration.

\\[beginning-of-defun] and \\[end-of-defun] move forward and backward to the start of a declaration.

This may link with `purescript-doc-mode'.

For non-literate and LaTeX-style literate scripts, we assume the
common convention that top-level declarations start at the first
column.  For Bird-style literate scripts, we assume the common
convention that top-level declarations start at the third column,
ie. after \"> \".

Anything in `font-lock-comment-face' is not considered for a
declaration.  Therefore, using PureScript font locking with comments
coloured in `font-lock-comment-face' improves declaration scanning.

Literate PureScript scripts are supported: If the value of
`purescript-literate' (set automatically by `literate-purescript-mode')
is `bird', a Bird-style literate script is assumed.  If it is nil
or `tex', a non-literate or LaTeX-style literate script is
assumed, respectively.

Invokes `purescript-decl-scan-mode-hook' on activation.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-decl-scan" '("literate-purescript-ds-" "purescript-d")))

;;;***

;;;### (autoloads nil "purescript-font-lock" "purescript-font-lock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-font-lock.el

(autoload 'purescript-font-lock-choose-keywords "purescript-font-lock" nil nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-font-lock" '("purescript-" "turn-o")))

;;;***

;;;### (autoloads nil "purescript-indent" "purescript-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-indent.el

(autoload 'turn-on-purescript-indent "purescript-indent" "\
Turn on ``intelligent'' PureScript indentation mode." nil nil)

(autoload 'purescript-indent-mode "purescript-indent" "\
``Intelligent'' PureScript indentation mode.
This deals with the layout rule of PureScript.
\\[purescript-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.
Other special keys are:
    \\[purescript-indent-insert-equal]
      inserts an =
    \\[purescript-indent-insert-guard]
      inserts an |
    \\[purescript-indent-insert-otherwise]
      inserts an | otherwise =
these functions also align the guards and rpurs of the current definition
    \\[purescript-indent-insert-where]
      inserts a where keyword
    \\[purescript-indent-align-guards-and-rpurs]
      aligns the guards and rpurs of the region
    \\[purescript-indent-put-region-in-literate]
      makes the region a piece of literate code in a literate script

Invokes `purescript-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-indent" '("purescript-indent-" "turn-off-purescript-indent")))

;;;***

;;;### (autoloads nil "purescript-indentation" "purescript-indentation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-indentation.el

(autoload 'purescript-indentation-mode "purescript-indentation" "\
PureScript indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode.

If called interactively, enable Purescript-Indentation mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-purescript-indentation "purescript-indentation" "\
Turn on the purescript-indentation minor mode." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-indentation" '("on-parse-error" "parse-error" "purescript-")))

;;;***

;;;### (autoloads nil "purescript-mode" "purescript-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from purescript-mode.el

(autoload 'purescript-version "purescript-mode" "\
Show the `purescript-mode` version in the echo area.
With prefix argument HERE, insert it at point.
When FULL is non-nil, use a verbose version string.
When MESSAGE is non-nil, display a message with the version.

\(fn &optional HERE)" t nil)

(autoload 'purescript-mode-view-news "purescript-mode" "\
Display information on recent changes to purescript-mode." t nil)

(autoload 'purescript-customize "purescript-mode" "\
Browse the purescript customize sub-tree.
This calls 'customize-browse' with purescript as argument and makes
sure all purescript customize definitions have been loaded." t nil)

(defvar purescript-mode-map (let ((map (make-sparse-keymap))) (define-key map (kbd "C-c C-.") 'purescript-mode-format-imports) (define-key map [remap delete-indentation] 'purescript-delete-indentation) map) "\
Keymap used in PureScript mode.")

(autoload 'purescript-mode "purescript-mode" "\
Major mode for editing PureScript programs.

See also Info node `(purescript-mode)Getting Started' for more
information about this mode.

\\<purescript-mode-map>
Literate scripts are supported via `literate-purescript-mode'.
The variable `purescript-literate' indicates the style of the script in the
current buffer.  See the documentation on this variable for more details.

Use `purescript-version' to find out what version of PureScript mode you are
currently using.

Additional PureScript mode modules can be hooked in via `purescript-mode-hook';
see documentation for that variable for more details.

\(fn)" t nil)

(autoload 'literate-purescript-mode "purescript-mode" "\
As `purescript-mode' but for literate scripts.

\(fn)" t nil)
(add-to-list 'auto-mode-alist '("\\.purs\\'" . purescript-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-mode" '("purescript-")))

;;;***

;;;### (autoloads nil "purescript-move-nested" "purescript-move-nested.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-move-nested.el

(autoload 'purescript-move-nested "purescript-move-nested" "\
Shift the nested off-side-rule block adjacent to point by COLS columns to the right.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" nil nil)

(autoload 'purescript-move-nested-right "purescript-move-nested" "\
Increase indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

(autoload 'purescript-move-nested-left "purescript-move-nested" "\
Decrease indentation of the following off-side-rule block adjacent to point.

Use a numeric prefix argument to indicate amount of indentation to apply.

In Transient Mark mode, if the mark is active, operate on the contents
of the region instead.

\(fn COLS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-move-nested" '("purescript-")))

;;;***

;;;### (autoloads nil "purescript-navigate-imports" "purescript-navigate-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-navigate-imports.el

(autoload 'purescript-navigate-imports "purescript-navigate-imports" "\
Cycle the PureScript import lines or return to point (with prefix arg).

\(fn &optional RETURN)" t nil)

(autoload 'purescript-navigate-imports-go "purescript-navigate-imports" "\
Go to the first line of a list of consequtive import lines. Cycles." t nil)

(autoload 'purescript-navigate-imports-return "purescript-navigate-imports" "\
Return to the non-import point we were at before going to the module list.
   If we were originally at an import list, we can just cycle through easily." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-navigate-imports" '("purescript-navigate-imports-")))

;;;***

;;;### (autoloads nil "purescript-presentation-mode" "purescript-presentation-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-presentation-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-presentation-mode" '("purescript-present")))

;;;***

;;;### (autoloads nil "purescript-show" "purescript-show.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from purescript-show.el

(autoload 'purescript-show-replace "purescript-show" "\
Replace the given region containing a Show value with a pretty
  printed collapsible version.

\(fn START END)" nil nil)

(autoload 'purescript-show-parse-and-insert "purescript-show" "\
Parse a `string' containing a Show instance value and insert
  it pretty printed into the current buffer.

\(fn GIVEN)" nil nil)

(autoload 'purescript-show-parse "purescript-show" "\
Parse the given input into a tree.

\(fn GIVEN)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-show" '("purescript-show-" "sexp-show")))

;;;***

;;;### (autoloads nil "purescript-simple-indent" "purescript-simple-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-simple-indent.el

(autoload 'purescript-simple-indent-mode "purescript-simple-indent" "\
Simple PureScript indentation mode that uses simple heuristic.
In this minor mode, `indent-for-tab-command' (bound to <tab> by
default) will move the cursor to the next indent point in the
previous nonblank line, whereas `purescript-simple-indent-backtab'
\(bound to <backtab> by default) will move the cursor the
previous indent point.  An indent point is a non-whitespace
character following whitespace.

If called interactively, enable Purescript-Simple-Indent mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

Runs `purescript-simple-indent-hook' on activation.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-purescript-simple-indent "purescript-simple-indent" "\
Turn on function `purescript-simple-indent-mode'." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-simple-indent" '("purescript-simple-indent" "turn-off-purescript-simple-indent")))

;;;***

;;;### (autoloads nil "purescript-sort-imports" "purescript-sort-imports.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-sort-imports.el

(autoload 'purescript-sort-imports "purescript-sort-imports" nil t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-sort-imports" '("purescript-sort-imports-")))

;;;***

;;;### (autoloads nil "purescript-str" "purescript-str.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from purescript-str.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-str" '("purescript-str-")))

;;;***

;;;### (autoloads nil "purescript-string" "purescript-string.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-string.el

(autoload 'purescript-trim "purescript-string" "\


\(fn STRING)" nil nil)

(autoload 'purescript-string-take "purescript-string" "\
Take n chars from string.

\(fn STRING N)" nil nil)

(autoload 'purescript-is-prefix-of "purescript-string" "\
Is x string a prefix of y string?

\(fn X Y)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-string" '("purescript-string")))

;;;***

;;;### (autoloads nil "purescript-unicode-input-method" "purescript-unicode-input-method.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from purescript-unicode-input-method.el

(autoload 'turn-on-purescript-unicode-input-method "purescript-unicode-input-method" "\
Set input method `purescript-unicode'.
See Info node `Unicode(purescript-mode)' for more details." t nil)

;;;***

;;;### (autoloads nil "purescript-utils" "purescript-utils.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from purescript-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-utils" '("purescript-utils-")))

;;;***

;;;### (autoloads nil "purescript-yas" "purescript-yas.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from purescript-yas.el

(autoload 'purescript-yas-complete "purescript-yas" "\


\(fn &rest ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "purescript-yas" '("purescript-yas-")))

;;;***

;;;### (autoloads nil nil ("purescript-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; purescript-mode-autoloads.el ends here
