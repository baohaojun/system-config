;;; envrc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "envrc" "envrc.el" (0 0 0 0))
;;; Generated autoloads from envrc.el

(autoload 'envrc-mode "envrc" "\
A local minor mode in which env vars are set by direnv.

If called interactively, enable Envrc mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'envrc-global-mode 'globalized-minor-mode t)

(defvar envrc-global-mode nil "\
Non-nil if Envrc-Global mode is enabled.
See the `envrc-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `envrc-global-mode'.")

(custom-autoload 'envrc-global-mode "envrc" nil)

(autoload 'envrc-global-mode "envrc" "\
Toggle Envrc mode in all buffers.
With prefix ARG, enable Envrc-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Envrc mode is enabled in all buffers where
`(lambda nil (unless (or (minibufferp) (file-remote-p default-directory)) (envrc-mode 1)))' would do it.
See `envrc-mode' for more information on Envrc mode.

\(fn &optional ARG)" t nil)

(autoload 'envrc-file-mode "envrc" "\
Major mode for .envrc files as used by direnv.
\\{envrc-file-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.envrc\\'" . envrc-file-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "envrc" '("envrc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; envrc-autoloads.el ends here
