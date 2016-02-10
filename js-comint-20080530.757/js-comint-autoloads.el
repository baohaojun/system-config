;;; js-comint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (inferior-js-mode switch-to-js js-load-file-and-go
;;;;;;  js-load-file js-send-buffer-and-go js-send-buffer js-send-last-sexp
;;;;;;  js-send-last-sexp-and-go js-send-region-and-go js-send-region
;;;;;;  run-js) "js-comint" "js-comint.el" (21286 54539 755787 994000))
;;; Generated autoloads from js-comint.el

(autoload 'run-js "js-comint" "\
Run an inferior Javascript process, input and output via buffer `*js*'.
If there is a process already running in `*js*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-js-program-command').
Runs the hook `inferior-js-mode-hook' (after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn CMD &optional DONT-SWITCH-P)" t nil)

(autoload 'js-send-region "js-comint" "\
Send the current region to the inferior Javascript process.

\(fn START END)" t nil)

(autoload 'js-send-region-and-go "js-comint" "\
Send the current region to the inferior Javascript process.

\(fn START END)" t nil)

(autoload 'js-send-last-sexp-and-go "js-comint" "\
Send the previous sexp to the inferior Js process.

\(fn)" t nil)

(autoload 'js-send-last-sexp "js-comint" "\
Send the previous sexp to the inferior Javascript process.

\(fn)" t nil)

(autoload 'js-send-buffer "js-comint" "\
Send the buffer to the inferior Javascript process.

\(fn)" t nil)

(autoload 'js-send-buffer-and-go "js-comint" "\
Send the buffer to the inferior Javascript process.

\(fn)" t nil)

(autoload 'js-load-file "js-comint" "\
Load a file in the javascript interpreter.

\(fn FILENAME)" t nil)

(autoload 'js-load-file-and-go "js-comint" "\
Load a file in the javascript interpreter.

\(fn FILENAME)" t nil)

(autoload 'switch-to-js "js-comint" "\
Switch to the javascript process buffer.
With argument, position cursor at end of buffer.

\(fn EOB-P)" t nil)

(autoload 'inferior-js-mode "js-comint" "\
Major mode for interacting with an inferior javascript process.

The following commands are available:
\\{inferior-js-mode-map}

A javascript process can be fired up with M-x run-js.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-js-mode-hook (in that order).

You can send text to the inferior Javascript process from othber buffers containing
Javascript source.
    switch-to-js switches the current buffer to the Javascript process buffer.
    js-send-region sends the current region to the Javascript process.


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("js-comint-pkg.el") (21286 54539 863412
;;;;;;  163000))

;;;***

(provide 'js-comint-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js-comint-autoloads.el ends here
