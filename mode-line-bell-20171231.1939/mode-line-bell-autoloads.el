;;; mode-line-bell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mode-line-bell" "mode-line-bell.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from mode-line-bell.el

(autoload 'mode-line-bell-flash "mode-line-bell" "\
Flash the mode line momentarily.

\(fn)" nil nil)

(defvar mode-line-bell-mode nil "\
Non-nil if Mode-Line-Bell mode is enabled.
See the `mode-line-bell-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mode-line-bell-mode'.")

(custom-autoload 'mode-line-bell-mode "mode-line-bell" nil)

(autoload 'mode-line-bell-mode "mode-line-bell" "\
Flash the mode line instead of ringing the bell.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mode-line-bell-autoloads.el ends here
