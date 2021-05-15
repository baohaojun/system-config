;;; eldoc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "eldoc" "eldoc.el" (0 0 0 0))
;;; Generated autoloads from eldoc.el

(defvar eldoc-minor-mode-string (purecopy " ElDoc") "\
String to display in mode line when ElDoc Mode is enabled; nil for none.")

(custom-autoload 'eldoc-minor-mode-string "eldoc" t)

(autoload 'eldoc-mode "eldoc" "\
Toggle echo area display of Lisp objects at point (ElDoc mode).

If called interactively, enable Eldoc mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

ElDoc mode is a buffer-local minor mode.  When enabled, the echo
area displays information about a function or variable in the
text where point is.  If point is on a documented variable, it
displays the first line of that variable's doc string.  Otherwise
it displays the argument list of the function called in the
expression point is on.

\(fn &optional ARG)" t nil)

(put 'global-eldoc-mode 'globalized-minor-mode t)

(defvar global-eldoc-mode t "\
Non-nil if Global Eldoc mode is enabled.
See the `global-eldoc-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-eldoc-mode'.")

(custom-autoload 'global-eldoc-mode "eldoc" nil)

(autoload 'global-eldoc-mode "eldoc" "\
Toggle Eldoc mode in all buffers.
With prefix ARG, enable Global Eldoc mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Eldoc mode is enabled in all buffers where
`turn-on-eldoc-mode' would do it.
See `eldoc-mode' for more information on Eldoc mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-eldoc-mode "eldoc" "\
Turn on `eldoc-mode' if the buffer has ElDoc support enabled.
See `eldoc-documentation-strategy' for more detail." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eldoc" '("eldoc")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eldoc-autoloads.el ends here
