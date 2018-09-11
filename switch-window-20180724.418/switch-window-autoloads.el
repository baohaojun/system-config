;;; switch-window-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "switch-window" "switch-window.el" (0 0 0 0))
;;; Generated autoloads from switch-window.el

(autoload 'switch-window-then-delete "switch-window" "\
Display an overlay in each window showing a unique key.
In the mean time, user will be asked to choose the window deleted.

\(fn)" t nil)

(autoload 'switch-window-then-maximize "switch-window" "\
Display an overlay in each window showing a unique key.
In the mean time, ask user which window to maximize

\(fn)" t nil)

(autoload 'switch-window "switch-window" "\
Display an overlay in each window showing a unique key.
In the mean time, ask user for the window where move to

\(fn)" t nil)

(autoload 'switch-window-then-split-horizontally "switch-window" "\
Select a window then split it horizontally.
Argument ARG .

\(fn ARG)" t nil)

(autoload 'switch-window-then-split-vertically "switch-window" "\
Select a window then split it vertically.
Argument ARG .

\(fn ARG)" t nil)

(autoload 'switch-window-then-split-below "switch-window" "\
Select a window then split it with split-window-below's mode.
TODO: Argument ARG.

\(fn ARG)" t nil)

(autoload 'switch-window-then-split-right "switch-window" "\
Select a window then split it with split-window-right's mode.
TODO: Argument ARG .

\(fn ARG)" t nil)

(autoload 'switch-window-then-swap-buffer "switch-window" "\
Select a window then swap it buffer with current window's buffer.
TODO: Argument ARG.

\(fn ARG)" t nil)

(autoload 'switch-window-then-find-file "switch-window" "\
Select a window, then find a file in it.

Designed to replace `find-file-other-window'.

\(fn)" t nil)

(autoload 'switch-window-then-find-file-read-only "switch-window" "\
Select a window, then find a file in it, read-only.

Designed to replace `find-file-read-only-other-window'.

\(fn)" t nil)

(autoload 'switch-window-then-display-buffer "switch-window" "\
Select a window, display a buffer in it, then return.

Designed to replace `display-buffer'.

\(fn)" t nil)

(autoload 'switch-window-then-kill-buffer "switch-window" "\
Select a window, then kill its buffer, then close it.

Designed to replace `kill-buffer-and-window'.

\(fn)" t nil)

(autoload 'switch-window-then-dired "switch-window" "\
Select a window, then dired in it.

Designed to replace `dired-other-window'.

\(fn)" t nil)

(autoload 'switch-window-then-compose-mail "switch-window" "\
Select a window, then start composing mail in it.

Designed to replace `compose-mail-other-window'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "switch-window" '("switch-window-" "delete-other-window")))

;;;***

;;;### (autoloads nil "switch-window-asciiart" "switch-window-asciiart.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from switch-window-asciiart.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "switch-window-asciiart" '("switch-window-asciiart")))

;;;***

;;;### (autoloads nil "switch-window-mvborder" "switch-window-mvborder.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from switch-window-mvborder.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "switch-window-mvborder" '("switch-window-")))

;;;***

;;;### (autoloads nil nil ("switch-window-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; switch-window-autoloads.el ends here
