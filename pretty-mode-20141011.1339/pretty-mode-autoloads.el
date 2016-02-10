;;; pretty-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "pretty-mode" "pretty-mode.el" (21591 13450
;;;;;;  975137 264000))
;;; Generated autoloads from pretty-mode.el

(autoload 'pretty-mode "pretty-mode" "\
Toggle Pretty minor mode.
With arg, turn Pretty minor mode on if arg is positive, off otherwise.

Pretty mode builds on `font-lock-mode'. Instead of highlighting
keywords, it replaces them with symbols. For example, lambda is
displayed as λ in lisp modes.

\(fn &optional ARG)" t nil)

(defvar global-pretty-mode t "\
Non-nil if Global-Pretty mode is enabled.
See the command `global-pretty-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-pretty-mode'.")

(custom-autoload 'global-pretty-mode "pretty-mode" nil)

(autoload 'global-pretty-mode "pretty-mode" "\
Toggle Pretty mode in all buffers.
With prefix ARG, enable Global-Pretty mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Pretty mode is enabled in all buffers where
`turn-on-pretty-if-desired' would do it.
See `pretty-mode' for more information on Pretty mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-off-pretty-mode "pretty-mode" "\


\(fn)" t nil)

(autoload 'turn-on-pretty-mode "pretty-mode" "\


\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pretty-mode-autoloads.el ends here
