;;; marginalia-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "marginalia" "marginalia.el" (0 0 0 0))
;;; Generated autoloads from marginalia.el

(defvar marginalia-mode nil "\
Non-nil if Marginalia mode is enabled.
See the `marginalia-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `marginalia-mode'.")

(custom-autoload 'marginalia-mode "marginalia" nil)

(autoload 'marginalia-mode "marginalia" "\
Annotate completion candidates with richer information.

If called interactively, enable Marginalia mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'marginalia-cycle "marginalia" "\
Cycle between annotators in `marginalia-annotators'." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "marginalia" '("marginalia-")))

;;;***

;;;### (autoloads nil nil ("marginalia-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; marginalia-autoloads.el ends here
