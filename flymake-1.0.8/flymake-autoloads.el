;;; flymake-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flymake" "flymake.el" (0 0 0 0))
;;; Generated autoloads from flymake.el

(autoload 'flymake-log "flymake" "\
Log, at level LEVEL, the message MSG formatted with ARGS.
LEVEL is passed to `display-warning', which is used to display
the warning.  If this form is included in a byte-compiled file,
the generated warning contains an indication of the file that
generated it.

\(fn LEVEL MSG &rest ARGS)" nil t)

(autoload 'flymake-make-diagnostic "flymake" "\
Make a Flymake diagnostic for BUFFER's region from BEG to END.
TYPE is a key to symbol and TEXT is a description of the problem
detected in this region.  DATA is any object that the caller
wishes to attach to the created diagnostic for later retrieval.

OVERLAY-PROPERTIES is an an alist of properties attached to the
created diagnostic, overriding the default properties and any
properties of `flymake-overlay-control' of the diagnostic's
type.

\(fn BUFFER BEG END TYPE TEXT &optional DATA OVERLAY-PROPERTIES)" nil nil)

(autoload 'flymake-diagnostics "flymake" "\
Get Flymake diagnostics in region determined by BEG and END.

If neither BEG or END is supplied, use the whole buffer,
otherwise if BEG is non-nil and END is nil, consider only
diagnostics at BEG.

\(fn &optional BEG END)" nil nil)

(autoload 'flymake-diag-region "flymake" "\
Compute BUFFER's region (BEG . END) corresponding to LINE and COL.
If COL is nil, return a region just for LINE.  Return nil if the
region is invalid.

\(fn BUFFER LINE &optional COL)" nil nil)

(autoload 'flymake-mode "flymake" "\
Toggle Flymake mode on or off.

Flymake is an Emacs minor mode for on-the-fly syntax checking.
Flymake collects diagnostic information from multiple sources,
called backends, and visually annotates the buffer with the
results.

Flymake performs these checks while the user is editing.
The customization variables `flymake-start-on-flymake-mode',
`flymake-no-changes-timeout' determine the exact circumstances
whereupon Flymake decides to initiate a check of the buffer.

The commands `flymake-goto-next-error' and
`flymake-goto-prev-error' can be used to navigate among Flymake
diagnostics annotated in the buffer.

The visual appearance of each type of diagnostic can be changed
by setting properties `flymake-overlay-control', `flymake-bitmap'
and `flymake-severity' on the symbols of diagnostic types (like
`:error', `:warning' and `:note').

Activation or deactivation of backends used by Flymake in each
buffer happens via the special hook
`flymake-diagnostic-functions'.

Some backends may take longer than others to respond or complete,
and some may decide to disable themselves if they are not
suitable for the current buffer. The commands
`flymake-running-backends', `flymake-disabled-backends' and
`flymake-reporting-backends' summarize the situation, as does the
special *Flymake log* buffer.

\(fn &optional ARG)" t nil)

(autoload 'flymake-mode-on "flymake" "\
Turn Flymake mode on.

\(fn)" nil nil)

(autoload 'flymake-mode-off "flymake" "\
Turn Flymake mode off.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flymake" '("flymake-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flymake-autoloads.el ends here
