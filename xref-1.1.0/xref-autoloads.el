;;; xref-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "xref" "xref.el" (0 0 0 0))
;;; Generated autoloads from xref.el

(autoload 'xref-find-backend "xref" nil nil nil)

(autoload 'xref-pop-marker-stack "xref" "\
Pop back to where \\[xref-find-definitions] was last invoked." t nil)

(autoload 'xref-marker-stack-empty-p "xref" "\
Return t if the marker stack is empty; nil otherwise." nil nil)

(autoload 'xref-find-definitions "xref" "\
Find the definition of the identifier at point.
With prefix argument or when there's no identifier at point,
prompt for it.

If sufficient information is available to determine a unique
definition for IDENTIFIER, display it in the selected window.
Otherwise, display the list of the possible definitions in a
buffer where the user can select from the list.

\(fn IDENTIFIER)" t nil)

(autoload 'xref-find-definitions-other-window "xref" "\
Like `xref-find-definitions' but switch to the other window.

\(fn IDENTIFIER)" t nil)

(autoload 'xref-find-definitions-other-frame "xref" "\
Like `xref-find-definitions' but switch to the other frame.

\(fn IDENTIFIER)" t nil)

(autoload 'xref-find-references "xref" "\
Find references to the identifier at point.
This command might prompt for the identifier as needed, perhaps
offering the symbol at point as the default.
With prefix argument, or if `xref-prompt-for-identifier' is t,
always prompt for the identifier.  If `xref-prompt-for-identifier'
is nil, prompt only if there's no usable symbol at point.

\(fn IDENTIFIER)" t nil)

(autoload 'xref-find-definitions-at-mouse "xref" "\
Find the definition of identifier at or around mouse click.
This command is intended to be bound to a mouse event.

\(fn EVENT)" t nil)

(autoload 'xref-find-apropos "xref" "\
Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'.

\(fn PATTERN)" t nil)
 (define-key esc-map "." #'xref-find-definitions)
 (define-key esc-map "," #'xref-pop-marker-stack)
 (define-key esc-map "?" #'xref-find-references)
 (define-key esc-map [?\C-.] #'xref-find-apropos)
 (define-key ctl-x-4-map "." #'xref-find-definitions-other-window)
 (define-key ctl-x-5-map "." #'xref-find-definitions-other-frame)

(autoload 'xref-references-in-directory "xref" "\
Find all references to SYMBOL in directory DIR.
Return a list of xref values.

This function uses the Semantic Symbol Reference API, see
`semantic-symref-tool-alist' for details on which tools are used,
and when.

\(fn SYMBOL DIR)" nil nil)

(autoload 'xref-matches-in-directory "xref" "\
Find all matches for REGEXP in directory DIR.
Return a list of xref values.
Only files matching some of FILES and none of IGNORES are searched.
FILES is a string with glob patterns separated by spaces.
IGNORES is a list of glob patterns for files to ignore.

\(fn REGEXP FILES DIR IGNORES)" nil nil)

(autoload 'xref-matches-in-files "xref" "\
Find all matches for REGEXP in FILES.
Return a list of xref values.
FILES must be a list of absolute file names.

\(fn REGEXP FILES)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "xref" '("xref-")))

;;;***

;;;### (autoloads nil nil ("xref-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; xref-autoloads.el ends here
