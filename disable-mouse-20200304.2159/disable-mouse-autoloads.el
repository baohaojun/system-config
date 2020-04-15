;;; disable-mouse-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "disable-mouse" "disable-mouse.el" (0 0 0 0))
;;; Generated autoloads from disable-mouse.el

(autoload 'disable-mouse-in-keymap "disable-mouse" "\
Rebind all mouse commands in MAP so that they are disabled.
When INCLUDE-TARGETS is non-nil, also disable mouse actions that
target GUI elements such as the modeline.

\(fn MAP &optional INCLUDE-TARGETS)" nil nil)

(autoload 'disable-mouse-mode "disable-mouse" "\
Disable the mouse in the current buffer.
You can still use the mouse to click into other buffers or
interact with GUI elements such as divider lines.

If called interactively, enable Disable-Mouse mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar disable-mouse-global-mode nil "\
Non-nil if Disable-Mouse-Global mode is enabled.
See the `disable-mouse-global-mode' command
for a description of this minor mode.")

(custom-autoload 'disable-mouse-global-mode "disable-mouse" nil)

(autoload 'disable-mouse-global-mode "disable-mouse" "\
Disable the mouse globally.
Interact with GUI elements such as divider lines will also be prevented.

If called interactively, enable Disable-Mouse-Global mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defalias 'global-disable-mouse-mode 'disable-mouse-global-mode)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "disable-mouse" '("disable-mouse-")))

;;;***

;;;### (autoloads nil nil ("disable-mouse-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; disable-mouse-autoloads.el ends here
