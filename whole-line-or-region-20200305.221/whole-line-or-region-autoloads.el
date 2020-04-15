;;; whole-line-or-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "whole-line-or-region" "whole-line-or-region.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from whole-line-or-region.el

(autoload 'whole-line-or-region-bind-keys "whole-line-or-region" "\
Bind keys according to `whole-line-or-region-extensions-alist'." nil nil)

(autoload 'whole-line-or-region-local-mode "whole-line-or-region" "\
Toggle use of whole-line-or-region minor mode.

This minor mode allows functions to operate on the current line if
they would normally operate on a region and region is currently
undefined.

Optional ARG turns mode on iff ARG is a positive integer.

\(fn &optional ARG)" t nil)

(put 'whole-line-or-region-global-mode 'globalized-minor-mode t)

(defvar whole-line-or-region-global-mode nil "\
Non-nil if Whole-Line-Or-Region-Global mode is enabled.
See the `whole-line-or-region-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `whole-line-or-region-global-mode'.")

(custom-autoload 'whole-line-or-region-global-mode "whole-line-or-region" nil)

(autoload 'whole-line-or-region-global-mode "whole-line-or-region" "\
Toggle Whole-Line-Or-Region-Local mode in all buffers.
With prefix ARG, enable Whole-Line-Or-Region-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Whole-Line-Or-Region-Local mode is enabled in all buffers where
`whole-line-or-region--turn-on' would do it.
See `whole-line-or-region-local-mode' for more information on Whole-Line-Or-Region-Local mode.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'whole-line-or-region-mode 'whole-line-or-region-global-mode)

(autoload 'whole-line-or-region-copy-region-as-kill "whole-line-or-region" "\
Copy region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-kill-region "whole-line-or-region" "\
Kill (cut) region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-kill-ring-save "whole-line-or-region" "\
Copy region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-yank "whole-line-or-region" "\
Yank (paste) previously killed text.

If the text to be yanked was killed with a whole-line-or-region
function *as* a whole-line, then paste it as a whole line (i.e. do not
break up the current line, and do not force the user to move point).

RAW-PREFIX is used to determine which string to yank, just as `yank'
would normally use it.

Optionally, pass in string to be \"yanked\" via STRING-IN.

\(fn RAW-PREFIX &optional STRING-IN)" t nil)

(autoload 'whole-line-or-region-delete "whole-line-or-region" "\
Delete region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-comment-dwim "whole-line-or-region" "\
Call `comment-dwim' on current region or current line.

See `comment-dwim' for details of RAW-PREFIX usage.

\(fn RAW-PREFIX)" t nil)

(autoload 'whole-line-or-region-comment-dwim-2 "whole-line-or-region" "\
Call `comment-dwim' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-comment-region "whole-line-or-region" "\
Call `comment-region' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-uncomment-region "whole-line-or-region" "\
Call `uncomment-region' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "whole-line-or-region" '("whole-line-or-region-")))

;;;***

;;;### (autoloads nil nil ("whole-line-or-region-pkg.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; whole-line-or-region-autoloads.el ends here
