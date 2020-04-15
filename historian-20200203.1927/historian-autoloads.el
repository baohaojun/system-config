;;; historian-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "historian" "historian.el" (0 0 0 0))
;;; Generated autoloads from historian.el

(autoload 'historian-save "historian" "\
Save the historian history to `historian-save-file'." t nil)

(autoload 'historian-load "historian" nil t nil)

(autoload 'historian-clear "historian" nil t nil)

(defvar historian-mode nil "\
Non-nil if Historian mode is enabled.
See the `historian-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `historian-mode'.")

(custom-autoload 'historian-mode "historian" nil)

(autoload 'historian-mode "historian" "\
historian minor mode

If called interactively, enable Historian mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "historian" '("historian-")))

;;;***

;;;### (autoloads nil nil ("historian-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; historian-autoloads.el ends here
