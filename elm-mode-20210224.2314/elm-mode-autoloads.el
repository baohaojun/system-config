;;; elm-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elm-font-lock" "elm-font-lock.el" (0 0 0 0))
;;; Generated autoloads from elm-font-lock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elm-font-lock" '("elm-")))

;;;***

;;;### (autoloads nil "elm-format" "elm-format.el" (0 0 0 0))
;;; Generated autoloads from elm-format.el
 (autoload 'elm-format-buffer "elm-format" nil t)
 (autoload 'elm-format-on-save-mode "elm-format" nil t)

(define-obsolete-function-alias 'elm-mode-format-buffer 'elm-format-buffer "20190113")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elm-format" '("elm-format-")))

;;;***

;;;### (autoloads nil "elm-imenu" "elm-imenu.el" (0 0 0 0))
;;; Generated autoloads from elm-imenu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elm-imenu" '("elm-imenu-")))

;;;***

;;;### (autoloads nil "elm-indent" "elm-indent.el" (0 0 0 0))
;;; Generated autoloads from elm-indent.el

(autoload 'elm-indent-mode "elm-indent" "\
``Intelligent'' Elm indentation mode.

If called interactively, enable Elm-Indent mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

This deals with the layout rules of Elm.

\\[elm-indent-cycle] starts the cycle which proposes new
possibilities as long as the TAB key is pressed.  Any other key
or mouse click terminates the cycle and is interpreted except for
RET which merely exits the cycle.

Other special keys are:

    \\[elm-indent-insert-equal]
      inserts an =

Invokes `elm-indent-hook' if not nil.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elm-indent" '("elm-indent-")))

;;;***

;;;### (autoloads nil "elm-indent-simple" "elm-indent-simple.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from elm-indent-simple.el

(autoload 'elm-indent-simple-mode "elm-indent-simple" "\
\"Stupid\" Elm indentation mode.

If called interactively, enable Elm-Indent-Simple mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elm-indent-simple" '("elm-indent-simple-")))

;;;***

;;;### (autoloads nil "elm-interactive" "elm-interactive.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from elm-interactive.el

(autoload 'elm-interactive-mode "elm-interactive" "\
Major mode for `elm-interactive'.

\\{elm-interactive-mode-map}

\(fn)" t nil)

(autoload 'elm-interactive "elm-interactive" "\
Run an inferior instance of `elm-repl' inside Emacs." t nil)

(define-obsolete-function-alias 'run-elm-interactive 'elm-interactive "2020-04")

(autoload 'elm-repl-load "elm-interactive" "\
Load an interactive REPL if there isn't already one running.
Changes the current root directory to be the directory with the closest
package json if one exists otherwise sets it to be the working directory
of the file specified." t nil)

(autoload 'elm-repl-push "elm-interactive" "\
Push the region from BEG to END to an interactive REPL.

\(fn BEG END)" t nil)

(autoload 'elm-repl-push-decl "elm-interactive" "\
Push the current top level declaration to the REPL." t nil)

(autoload 'elm-reactor "elm-interactive" "\
Run the Elm reactor process." t nil)

(define-obsolete-function-alias 'run-elm-reactor 'elm-reactor "2020-04")

(autoload 'elm-preview-buffer "elm-interactive" "\
Preview the current buffer using Elm reactor (in debug mode if DEBUG is truthy).

\(fn DEBUG)" t nil)

(autoload 'elm-preview-main "elm-interactive" "\
Preview the main elm file using Elm reactor (in debug mode if DEBUG is truthy).

\(fn DEBUG)" t nil)

(autoload 'elm-compile-buffer "elm-interactive" "\
Compile the current buffer into OUTPUT.

\(fn &optional OUTPUT)" t nil)

(autoload 'elm-compile-main "elm-interactive" "\
Compile the main elm file into OUTPUT.

\(fn &optional OUTPUT)" t nil)

(autoload 'elm-compile-clean-imports "elm-interactive" "\
Remove unused imports from the current buffer, PROMPT optionally before deleting.

\(fn &optional PROMPT)" t nil)

(autoload 'elm-sort-imports "elm-interactive" "\
Sort the import list in the current buffer." t nil)

(autoload 'elm-compile-add-annotations "elm-interactive" "\
Add missing type annotations to the current buffer, PROMPT optionally before inserting.

\(fn &optional PROMPT)" t nil)

(autoload 'elm-create-package "elm-interactive" "\
Generate a new package definition in the current directory." t nil)

(autoload 'elm-package-catalog "elm-interactive" "\
Show the package catalog, refreshing the list if REFRESH is truthy.

\(fn REFRESH)" t nil)

(autoload 'elm-package-refresh-contents "elm-interactive" "\
Refresh the package list." t nil)

(autoload 'elm-import "elm-interactive" "\
Import a module, refreshing if REFRESH is truthy.

\(fn REFRESH)" t nil)

(autoload 'elm-expose-at-point "elm-interactive" "\
Exposes identifier at point." t nil)

(autoload 'elm-documentation-lookup "elm-interactive" "\
Lookup the documentation for a function, refreshing if REFRESH is truthy.

\(fn REFRESH)" t nil)

(autoload 'elm-package-mode "elm-interactive" "\
Special mode for elm-package.

\\{elm-package-mode-map}

\(fn)" t nil)

(autoload 'elm-test-project "elm-interactive" "\
Run the elm-test command on the current project." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elm-interactive" '("elm-")))

;;;***

;;;### (autoloads nil "elm-mode" "elm-mode.el" (0 0 0 0))
;;; Generated autoloads from elm-mode.el

(defvar elm-mode-hook '(elm-indent-mode) "\
Hook called by `elm-mode'.")

(custom-autoload 'elm-mode-hook "elm-mode" t)

(autoload 'elm-mode "elm-mode" "\
Major mode for editing Elm source code.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elm-mode" '("elm-")))

;;;***

;;;### (autoloads nil "elm-tags" "elm-tags.el" (0 0 0 0))
;;; Generated autoloads from elm-tags.el

(autoload 'elm-mode-generate-tags "elm-tags" "\
Generate a TAGS file for the current project." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elm-tags" '("elm-tags-")))

;;;***

;;;### (autoloads nil "elm-util" "elm-util.el" (0 0 0 0))
;;; Generated autoloads from elm-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elm-util" '("elm-")))

;;;***

;;;### (autoloads nil nil ("elm-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elm-mode-autoloads.el ends here
