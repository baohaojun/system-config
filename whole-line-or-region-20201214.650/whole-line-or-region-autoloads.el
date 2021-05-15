;;; whole-line-or-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "whole-line-or-region" "whole-line-or-region.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from whole-line-or-region.el

(autoload 'whole-line-or-region-local-mode "whole-line-or-region" "\
Make chosen commands operate on the current line if no region is active.
Modify `whole-line-or-region-local-mode-map' to change which
commands are affected.

If called interactively, enable Whole-Line-Or-Region-Local mode
if ARG is positive, and disable it if ARG is zero or negative.
If called from Lisp, also enable the mode if ARG is omitted or
nil, and toggle it if ARG is `toggle'; disable the mode
otherwise.

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
`(lambda nil (unless (apply 'derived-mode-p whole-line-or-region-inhibit-modes) (whole-line-or-region-local-mode 1)))' would do it.
See `whole-line-or-region-local-mode' for more information on Whole-Line-Or-Region-Local mode.

\(fn &optional ARG)" t nil)

(autoload 'whole-line-or-region-copy-region-as-kill "whole-line-or-region" "\
Call `copy-region-as-kill' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-kill-region "whole-line-or-region" "\
Call `kill-region' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-kill-ring-save "whole-line-or-region" "\
Call `kill-ring-save' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-delete-region "whole-line-or-region" "\
Call `delete-region' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-comment-region "whole-line-or-region" "\
Call `comment-region' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-comment-dwim "whole-line-or-region" "\
Call `comment-dwim' on region or current line.
PREFIX is passed unchanged to `comment-dwim'.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-comment-dwim-2 "whole-line-or-region" "\
Call `comment-dwim' on region or PREFIX whole line.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-uncomment-region "whole-line-or-region" "\
Call `uncomment-region' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-comment-or-uncomment-region "whole-line-or-region" "\
Call `comment-or-uncomment-region' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-indent-rigidly-left-to-tab-stop "whole-line-or-region" "\
Call `indent-rigidly-left-to-tab-stop' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-indent-rigidly-right-to-tab-stop "whole-line-or-region" "\
Call `indent-rigidly-right-to-tab-stop' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-indent-rigidly-left "whole-line-or-region" "\
Call `indent-rigidly-left-to-tab-stop' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(autoload 'whole-line-or-region-indent-rigidly-right "whole-line-or-region" "\
Call `indent-rigidly-right-to-tab-stop' on region or PREFIX whole lines.

\(fn PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "whole-line-or-region" '("whole-line-or-region-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; whole-line-or-region-autoloads.el ends here
