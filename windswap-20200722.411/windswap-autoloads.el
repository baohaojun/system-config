;;; windswap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "windswap" "windswap.el" (0 0 0 0))
;;; Generated autoloads from windswap.el

(autoload 'windswap-default-keybindings "windswap" "\
Set up keybindings for `windswap'.
Keybindings are of the form MODIFIERS-{left,right,up,down}.
Default MODIFIERS are `meta' and `shift'.
See also `windmove-default-keybindings'.

\(fn &rest MODIFIERS)" t nil)

(autoload 'windswap-right "windswap" "\
Like `windmove-right', but transpose buffers after switching windows.
ARG is as for that function.

\(fn &optional ARG)" t nil)

(autoload 'windswap-left "windswap" "\
Like `windmove-left', but transpose buffers after switching windows.
ARG is as for that function.

\(fn &optional ARG)" t nil)

(autoload 'windswap-up "windswap" "\
Like `windmove-up', but transpose buffers after switching windows.
ARG is as for that function.

\(fn &optional ARG)" t nil)

(autoload 'windswap-down "windswap" "\
Like `windmove-down', but transpose buffers after switching windows.
ARG is as for that function.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "windswap" '("windswap--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; windswap-autoloads.el ends here
