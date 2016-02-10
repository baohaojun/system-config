;;; whole-line-or-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (whole-line-or-region-comment-dwim-2 whole-line-or-region-comment-dwim
;;;;;;  whole-line-or-region-delete whole-line-or-region-yank whole-line-or-region-kill-ring-save
;;;;;;  whole-line-or-region-kill-region whole-line-or-region-copy-region-as-kill
;;;;;;  whole-line-or-region-mode) "whole-line-or-region" "whole-line-or-region.el"
;;;;;;  (21286 54476 703601 580000))
;;; Generated autoloads from whole-line-or-region.el

(autoload 'whole-line-or-region-mode "whole-line-or-region" "\
Toggle use of whole-line-or-region minor mode.

This minor mode allows functions to operate on the current line if
they would normally operate on a region and region is currently
undefined.

Optional ARG turns mode on iff ARG is a positive integer.

\(fn &optional ARG)" t nil)

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

;;;***

;;;### (autoloads nil nil ("whole-line-or-region-pkg.el") (21286
;;;;;;  54476 789858 218000))

;;;***

(provide 'whole-line-or-region-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; whole-line-or-region-autoloads.el ends here
