;;; whitespace-cleanup-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (global-whitespace-cleanup-mode whitespace-cleanup-mode)
;;;;;;  "whitespace-cleanup-mode" "whitespace-cleanup-mode.el" (21286
;;;;;;  54477 191602 998000))
;;; Generated autoloads from whitespace-cleanup-mode.el

(autoload 'whitespace-cleanup-mode "whitespace-cleanup-mode" "\
Automatically call `whitespace-cleanup' on save.

\(fn &optional ARG)" t nil)

(defvar global-whitespace-cleanup-mode nil "\
Non-nil if Global-Whitespace-Cleanup mode is enabled.
See the command `global-whitespace-cleanup-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-whitespace-cleanup-mode'.")

(custom-autoload 'global-whitespace-cleanup-mode "whitespace-cleanup-mode" nil)

(autoload 'global-whitespace-cleanup-mode "whitespace-cleanup-mode" "\
Toggle Whitespace-Cleanup mode in all buffers.
With prefix ARG, enable Global-Whitespace-Cleanup mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Whitespace-Cleanup mode is enabled in all buffers where
`turn-on-whitespace-cleanup-mode' would do it.
See `whitespace-cleanup-mode' for more information on Whitespace-Cleanup mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("whitespace-cleanup-mode-pkg.el") (21286
;;;;;;  54477 260131 482000))

;;;***

(provide 'whitespace-cleanup-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; whitespace-cleanup-mode-autoloads.el ends here
