;;; emojify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emojify" "emojify.el" (0 0 0 0))
;;; Generated autoloads from emojify.el

(autoload 'emojify-set-emoji-styles "emojify" "\
Set the type of emojis that should be displayed.

STYLES is the styles emoji styles that should be used, see `emojify-emoji-styles'

\(fn STYLES)" nil nil)

(autoload 'emojify-mode "emojify" "\
Emojify mode

\(fn &optional ARG)" t nil)

(defvar global-emojify-mode nil "\
Non-nil if Global Emojify mode is enabled.
See the `global-emojify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-emojify-mode'.")

(custom-autoload 'global-emojify-mode "emojify" nil)

(autoload 'global-emojify-mode "emojify" "\
Toggle Emojify mode in all buffers.
With prefix ARG, enable Global Emojify mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Emojify mode is enabled in all buffers where
`emojify-mode' would do it.
See `emojify-mode' for more information on Emojify mode.

\(fn &optional ARG)" t nil)

(autoload 'emojify-mode-line-mode "emojify" "\
Emojify mode line

\(fn &optional ARG)" t nil)

(defvar global-emojify-mode-line-mode nil "\
Non-nil if Global Emojify-Mode-Line mode is enabled.
See the `global-emojify-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-emojify-mode-line-mode'.")

(custom-autoload 'global-emojify-mode-line-mode "emojify" nil)

(autoload 'global-emojify-mode-line-mode "emojify" "\
Toggle Emojify-Mode-Line mode in all buffers.
With prefix ARG, enable Global Emojify-Mode-Line mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Emojify-Mode-Line mode is enabled in all buffers where
`emojify-mode-line-mode' would do it.
See `emojify-mode-line-mode' for more information on Emojify-Mode-Line mode.

\(fn &optional ARG)" t nil)

(autoload 'emojify-apropos-emoji "emojify" "\
Show Emojis that match PATTERN.

\(fn PATTERN)" t nil)

(autoload 'emojify-insert-emoji "emojify" "\
Interactively prompt for Emojis and insert them in the current buffer.

This respects the `emojify-emoji-styles' variable.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emojify" '("emojify-")))

;;;***

;;;### (autoloads nil nil ("emojify-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emojify-autoloads.el ends here
