;;; ivy-rich-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-rich" "ivy-rich.el" (0 0 0 0))
;;; Generated autoloads from ivy-rich.el

(defvar ivy-rich-mode nil "\
Non-nil if Ivy-Rich mode is enabled.
See the `ivy-rich-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-rich-mode'.")

(custom-autoload 'ivy-rich-mode "ivy-rich" nil)

(autoload 'ivy-rich-mode "ivy-rich" "\
Toggle ivy-rich mode globally.

If called interactively, enable Ivy-Rich mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'ivy-rich-reload "ivy-rich" nil nil nil)

(defvar ivy-rich-project-root-cache-mode nil "\
Non-nil if Ivy-Rich-Project-Root-Cache mode is enabled.
See the `ivy-rich-project-root-cache-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `ivy-rich-project-root-cache-mode'.")

(custom-autoload 'ivy-rich-project-root-cache-mode "ivy-rich" nil)

(autoload 'ivy-rich-project-root-cache-mode "ivy-rich" "\
Toggle ivy-rich-root-cache-mode globally.

If called interactively, enable Ivy-Rich-Project-Root-Cache mode
if ARG is positive, and disable it if ARG is zero or negative.
If called from Lisp, also enable the mode if ARG is omitted or
nil, and toggle it if ARG is `toggle'; disable the mode
otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-rich" '("ivy-rich-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-rich-autoloads.el ends here
