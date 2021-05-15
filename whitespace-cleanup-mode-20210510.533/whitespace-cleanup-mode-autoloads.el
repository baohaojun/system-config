;;; whitespace-cleanup-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "whitespace-cleanup-mode" "whitespace-cleanup-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from whitespace-cleanup-mode.el

(let ((loads (get 'whitespace-cleanup 'custom-loads))) (if (member '"whitespace-cleanup-mode" loads) nil (put 'whitespace-cleanup 'custom-loads (cons '"whitespace-cleanup-mode" loads))))

(autoload 'whitespace-cleanup-mode "whitespace-cleanup-mode" "\
Automatically call `whitespace-cleanup' on save.

If called interactively, enable Whitespace-Cleanup mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'whitespace-cleanup-mode 'safe-local-variable 'booleanp)

(put 'global-whitespace-cleanup-mode 'globalized-minor-mode t)

(defvar global-whitespace-cleanup-mode nil "\
Non-nil if Global Whitespace-Cleanup mode is enabled.
See the `global-whitespace-cleanup-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-whitespace-cleanup-mode'.")

(custom-autoload 'global-whitespace-cleanup-mode "whitespace-cleanup-mode" nil)

(autoload 'global-whitespace-cleanup-mode "whitespace-cleanup-mode" "\
Toggle Whitespace-Cleanup mode in all buffers.
With prefix ARG, enable Global Whitespace-Cleanup mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Whitespace-Cleanup mode is enabled in all buffers where
`whitespace-cleanup-mode--maybe' would do it.
See `whitespace-cleanup-mode' for more information on Whitespace-Cleanup mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "whitespace-cleanup-mode" '("whitespace-cleanup-mode-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; whitespace-cleanup-mode-autoloads.el ends here
