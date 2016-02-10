;;; maxframe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (restore-frame maximize-frame x-restore-frame x-maximize-frame
;;;;;;  w32-restore-frame w32-maximize-frame) "maxframe" "maxframe.el"
;;;;;;  (21286 54457 399545 826000))
;;; Generated autoloads from maxframe.el

(autoload 'w32-maximize-frame "maxframe" "\
Maximize the current frame (windows only)

\(fn)" t nil)

(autoload 'w32-restore-frame "maxframe" "\
Restore a minimized/maximized frame (windows only)

\(fn)" t nil)

(autoload 'x-maximize-frame "maxframe" "\
Maximize the current frame (x or mac only)

\(fn &optional THE-FRAME)" t nil)

(autoload 'x-restore-frame "maxframe" "\
Restore the current frame (x or mac only)

\(fn &optional THE-FRAME)" t nil)

(autoload 'maximize-frame "maxframe" "\
Maximizes the frame to fit the display if under a windowing
system.

\(fn &optional THE-FRAME)" t nil)

(autoload 'restore-frame "maxframe" "\
Restores a maximized frame.  See `maximize-frame'.

\(fn &optional THE-FRAME)" t nil)

(defalias 'mf 'maximize-frame)

;;;***

;;;### (autoloads nil nil ("maxframe-pkg.el") (21286 54457 476505
;;;;;;  37000))

;;;***

(provide 'maxframe-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; maxframe-autoloads.el ends here
