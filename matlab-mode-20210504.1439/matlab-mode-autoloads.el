;;; matlab-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cedet-matlab" "cedet-matlab.el" (0 0 0 0))
;;; Generated autoloads from cedet-matlab.el

(autoload 'matlab-cedet-setup "cedet-matlab" "\
Setup support for CEDET tools for use with MATLAB." t nil)

;;;***

;;;### (autoloads nil "company-matlab-shell" "company-matlab-shell.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from company-matlab-shell.el

(autoload 'company-matlab-shell "company-matlab-shell" "\
A `company-mode' completion backend for `matlab-shell'.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-matlab-shell" '("company-matlab-shell-")))

;;;***

;;;### (autoloads nil "linemark" "linemark.el" (0 0 0 0))
;;; Generated autoloads from linemark.el

(autoload 'enable-visual-studio-bookmarks "linemark" "\
Bind the viss bookmark functions to F2 related keys.
\\<global-map>
\\[viss-bookmark-toggle]     - To=ggle a bookmark on this line.
\\[viss-bookmark-next-buffer]   - Move to the next bookmark.
\\[viss-bookmark-prev-buffer]   - Move to the previous bookmark.
\\[viss-bookmark-clear-all-buffer] - Clear all bookmarks." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "linemark" '("linemark-" "viss-bookmark-")))

;;;***

;;;### (autoloads nil "matlab" "matlab.el" (0 0 0 0))
;;; Generated autoloads from matlab.el

(autoload 'matlab-is-matlab-file "matlab" "\
Enter `matlab-mode' when file content looks like a MATLAB *.m
file or for empty files *.m files when `matlab-mode-for-new-mfiles'
indicates as such." nil nil)

(add-to-list 'magic-mode-alist '(matlab-is-matlab-file . matlab-mode))

(autoload 'matlab-mode "matlab" "\
MATLAB(R) mode is a major mode for editing MATLAB dot-m files.
\\<matlab-mode-map>
Convenient editing commands are:
 \\[matlab-comment-region]   - Comment/Uncomment out a region of code.
 \\[matlab-fill-comment-line] - Fill the current comment line.
 \\[matlab-fill-region] - Fill code and comments in region.
 \\[matlab-complete-symbol]   - Symbol completion of matlab symbolsbased on the local syntax.
 \\[matlab-indent-sexp] - Indent syntactic block of code.

Convenient navigation commands are:
 \\[matlab-beginning-of-command]   - Move to the beginning of a command.
 \\[matlab-end-of-command]   - Move to the end of a command.
 \\[matlab-beginning-of-defun] - Move to the beginning of a function.
 \\[matlab-end-of-defun] - Move do the end of a function.
 \\[matlab-forward-sexp] - Move forward over a syntactic block of code.
 \\[matlab-backward-sexp] - Move backwards over a syntactic block of code.

Convenient template insertion commands:
 \\[tempo-template-matlab-function] - Insert a function definition.
 \\[tempo-template-matlab-if] - Insert an IF END block.
 \\[tempo-template-matlab-for] - Insert a FOR END block.
 \\[tempo-template-matlab-switch] - Insert a SWITCH END statement.
 \\[matlab-insert-next-case] - Insert the next CASE condition in a SWITCH.
 \\[matlab-insert-end-block] - Insert a matched END statement.  With optional ARG, reindent.
 \\[matlab-stringify-region] - Convert plain text in region to a string with correctly quoted chars.

Variables:
  `matlab-indent-level'		Level to indent blocks.
  `matlab-cont-level'		Level to indent continuation lines.
  `matlab-cont-requires-ellipsis' Does your MATLAB support implied elipsis.
  `matlab-case-level'		Level to unindent case statements.
  `matlab-indent-past-arg1-functions'
                                Regexp of functions to indent past the first
                                  argument on continuation lines.
  `matlab-maximum-indents'      List of maximum indents during lineups.
  `matlab-comment-column'       Goal column for on-line comments.
  `fill-column'			Column used in auto-fill.
  `matlab-indent-function-body' If non-nil, indents body of MATLAB functions.
  `matlab-functions-have-end'	If non-nil, MATLAB functions terminate with end.
  `matlab-return-function'	Customize RET handling with this function.
  `matlab-fill-code'            Non-nil, auto-fill code in auto-fill-mode.
  `matlab-fill-strings'         Non-nil, auto-fill strings in auto-fill-mode.
  `matlab-verify-on-save-flag'  Non-nil, enable code checks on save.
  `matlab-highlight-block-match-flag'
                                Enable matching block begin/end keywords.
  `matlab-vers-on-startup'	If t, show version on start-up.
  `matlab-handle-simulink'      If t, enable simulink keyword highlighting.

All Key Bindings:
\\{matlab-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "matlab" '("gud-matlab-debug-active" "matlab-")))

;;;***

;;;### (autoloads nil "matlab-cgen" "matlab-cgen.el" (0 0 0 0))
;;; Generated autoloads from matlab-cgen.el
 (autoload 'matlab-insert-map-fcn "matlab-cgen" "Keymap for C-c C-c in matlab-mode" t 'keymap)

(autoload 'matlab-generate-latex "matlab-cgen" "\
Convert a MATLAB M file into a Latex document for printing.
Author: Uwe Brauer oub@eucmos.sim.ucm.es
Created: 14 Feb 2002" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "matlab-cgen" '("matlab-")))

;;;***

;;;### (autoloads nil "matlab-compat" "matlab-compat.el" (0 0 0 0))
;;; Generated autoloads from matlab-compat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "matlab-compat" '("matlab-")))

;;;***

;;;### (autoloads nil "matlab-complete" "matlab-complete.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from matlab-complete.el

(autoload 'matlab-property-function "matlab-complete" "\
Regexp of all builtin functions that take property lists." nil t)

(autoload 'matlab-complete-symbol "matlab-complete" "\
Complete a partially typed symbol in a MATLAB mode buffer.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "matlab-complete" '("matlab-")))

;;;***

;;;### (autoloads nil "matlab-maint" "matlab-maint.el" (0 0 0 0))
;;; Generated autoloads from matlab-maint.el

(autoload 'matlab-maint-minor-mode "matlab-maint" "\
Minor mode for matlab-mode maintainrs.

If called interactively, enable Matlab-Maint minor mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-matlab-maint-minor-mode 'globalized-minor-mode t)

(defvar global-matlab-maint-minor-mode nil "\
Non-nil if Global Matlab-Maint minor mode is enabled.
See the `global-matlab-maint-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-matlab-maint-minor-mode'.")

(custom-autoload 'global-matlab-maint-minor-mode "matlab-maint" nil)

(autoload 'global-matlab-maint-minor-mode "matlab-maint" "\
Toggle Matlab-Maint minor mode in all buffers.
With prefix ARG, enable Global Matlab-Maint minor mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Matlab-Maint minor mode is enabled in all buffers where
`(lambda nil Should we turn on in this buffer? Only if in the project. (let ((dir (expand-file-name default-directory)) (ml (file-name-directory (expand-file-name (locate-library matlab))))) (when (string= ml (substring dir 0 (min (length dir) (length ml)))) (matlab-maint-minor-mode 1))))' would do it.
See `matlab-maint-minor-mode' for more information on Matlab-Maint minor mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "matlab-maint" '("matlab-maint-")))

;;;***

;;;### (autoloads nil "matlab-netshell" "matlab-netshell.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from matlab-netshell.el

(autoload 'matlab-netshell-server-active-p "matlab-netshell" "\
Return non-nil if there is an active MATLAB netshell server." nil nil)

(autoload 'matlab-netshell-server-start "matlab-netshell" "\
Start the MATLAB netshell server." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "matlab-netshell" '("matlab-netshell-")))

;;;***

;;;### (autoloads nil "matlab-publish" "matlab-publish.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from matlab-publish.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "matlab-publish" '("matlab-")))

;;;***

;;;### (autoloads nil "matlab-shell" "matlab-shell.el" (0 0 0 0))
;;; Generated autoloads from matlab-shell.el

(autoload 'matlab-mode-determine-matlabroot "matlab-shell" "\
Return the MATLABROOT for the 'matlab-shell-command'." nil nil)

(autoload 'matlab-shell "matlab-shell" "\
Create a buffer with MATLAB running as a subprocess.

MATLAB shell cannot work on the MS Windows platform because MATLAB is not
a console application." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "matlab-shell" '("gud-matlab-marker-regexp-prefix" "matlab-")))

;;;***

;;;### (autoloads nil "matlab-shell-gud" "matlab-shell-gud.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from matlab-shell-gud.el

(autoload 'matlab-shell-mode-gud-enable-bindings "matlab-shell-gud" "\
Enable GUD features for `matlab-shell' in the current buffer." nil nil)

(autoload 'matlab-shell-gud-startup "matlab-shell-gud" "\
Configure GUD when a new `matlab-shell' is initialized." nil nil)

(autoload 'mlg-reset-breakpoints "matlab-shell-gud" "\
Remove all cached breakpoints." nil nil)

(autoload 'matlab-shell-gud-minor-mode "matlab-shell-gud" "\
Minor mode activated when `matlab-shell' K>> prompt is active.
This minor mode makes MATLAB buffers read only so simple keystrokes
activate debug commands.  It also enables tooltips to appear when the
mouse hovers over a symbol when debugging.
\\<matlab-shell-gud-minor-mode-map>
Debug commands are:
 \\[matlab-shell-gud-mode-edit]  - Edit file (toggle read-only)
            Allows editing file without causing MATLAB to exit debug mode.
 \\[gud-break]        - Add breakpoint (ebstop in FILE at point)
 \\[gud-remove]        - Remove breakpoint (ebclear in FILE at point)
 \\[gud-list-breakpoints]        - List breakpoints (ebstatus)
 \\[gud-step]        - Step (dbstep in)
 \\[gud-next]        - Next (dbstep)
 \\[gud-finish]        - Finish function (dbstep out)
 \\[gud-cont]        - Continue (dbcont)
 \\[matlab-shell-gud-show-symbol-value]        - Evaluate expression
 \\[mlg-show-stack]        - Where am I (ebstack)
 \\[gud-stop-subjob]        - Quit (dbquit)

If called interactively, enable Matlab-Shell-Gud minor mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'global-matlab-shell-gud-minor-mode 'globalized-minor-mode t)

(defvar global-matlab-shell-gud-minor-mode nil "\
Non-nil if Global Matlab-Shell-Gud minor mode is enabled.
See the `global-matlab-shell-gud-minor-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-matlab-shell-gud-minor-mode'.")

(custom-autoload 'global-matlab-shell-gud-minor-mode "matlab-shell-gud" nil)

(autoload 'global-matlab-shell-gud-minor-mode "matlab-shell-gud" "\
Toggle Matlab-Shell-Gud minor mode in all buffers.
With prefix ARG, enable Global Matlab-Shell-Gud minor mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Matlab-Shell-Gud minor mode is enabled in all buffers where
`(lambda nil Should we turn on in this buffer? Only if in a MATLAB mode. (when (eq major-mode 'matlab-mode) (matlab-shell-gud-minor-mode 1)))' would do it.
See `matlab-shell-gud-minor-mode' for more information on Matlab-Shell-Gud minor mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "matlab-shell-gud" '("gud-matlab-" "matlab-" "mlg-")))

;;;***

;;;### (autoloads nil "matlab-topic" "matlab-topic.el" (0 0 0 0))
;;; Generated autoloads from matlab-topic.el

(autoload 'matlab-shell-help-mode "matlab-topic" "\
Major mode for viewing MATLAB help text.
Entry to this mode runs the normal hook `matlab-shell-help-mode-hook'.

Commands:
\\{matlab-shell-help-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "matlab-topic" '("matlab-shell-")))

;;;***

;;;### (autoloads nil "mlint" "mlint.el" (0 0 0 0))
;;; Generated autoloads from mlint.el

(autoload 'mlint-minor-mode "mlint" "\
Toggle mlint minor mode, a mode for showing mlint errors.
With prefix ARG, turn mlint minor mode on iff ARG is positive.
\\{mlint-minor-mode-map\\}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mlint" '("mlint-")))

;;;***

;;;### (autoloads nil "semantic-matlab" "semantic-matlab.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from semantic-matlab.el

(autoload 'semantic-default-matlab-setup "semantic-matlab" "\
Set up a buffer for parsing of MATLAB files." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "semantic-matlab" '("matlab-" "semantic-")))

;;;***

;;;### (autoloads nil "semanticdb-matlab" "semanticdb-matlab.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from semanticdb-matlab.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "semanticdb-matlab" '("matlab-mode" "semantic")))

;;;***

;;;### (autoloads nil "srecode-matlab" "srecode-matlab.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from srecode-matlab.el

(autoload 'srecode-semantic-handle-:matlab "srecode-matlab" "\
Add macros into the dictionary DICT based on the current MATLAB buffer.
Adds the following:
FILE_SYMBOL - The file name as a symbol.
FILE_DOC_SYMBOL - The file name as a symbol for doc strings.
PACKAGE - The package this file is in, or empty if none.
FILE_CLASS - Show section if filename should be a class.
FILE_FUNCTION - Show setion if filename is a function.

On class prediction - when filling in an empty file, if the filename and
directory it is in match, for example @foo/foo.m then foo should be a classdef.

\(fn DICT)" nil nil)

;;;***

;;;### (autoloads nil "tlc" "tlc.el" (0 0 0 0))
;;; Generated autoloads from tlc.el

(autoload 'tlc-mode "tlc" "\


\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.tlc\\'" . tlc-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tlc" '("tlc-")))

;;;***

;;;### (autoloads nil nil ("matlab-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; matlab-mode-autoloads.el ends here
