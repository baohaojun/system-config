;;; bbyac-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bbyac" "bbyac.el" (0 0 0 0))
;;; Generated autoloads from bbyac.el

(autoload 'bbyac-mode "bbyac" "\
Toggle the `bbyac-mode' minor mode.

\(fn &optional ARG)" t nil)

(defvar bbyac-global-mode nil "\
Non-nil if Bbyac-Global mode is enabled.
See the `bbyac-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `bbyac-global-mode'.")

(custom-autoload 'bbyac-global-mode "bbyac" nil)

(autoload 'bbyac-global-mode "bbyac" "\
Toggle Bbyac mode in all buffers.
With prefix ARG, enable Bbyac-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Bbyac mode is enabled in all buffers where
`turn-on-bbyac-mode' would do it.
See `bbyac-mode' for more information on Bbyac mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bbyac" '("turn-on-bbyac-mode" "bbyac-")))

;;;***

;;;### (autoloads nil nil ("bbyac-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bbyac-autoloads.el ends here
