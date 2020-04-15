;;; js-comint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "js-comint" "js-comint.el" (0 0 0 0))
;;; Generated autoloads from js-comint.el

(autoload 'js-do-use-nvm "js-comint" "\
Enable nvm." nil nil)

(autoload 'js-comint-select-node-version "js-comint" "\
Use a given VERSION of node from nvm.

\(fn &optional VERSION)" t nil)

(autoload 'js-comint-add-module-path "js-comint" "\
Add a directory to `js-comint-module-paths'." t nil)

(autoload 'js-comint-delete-module-path "js-comint" "\
Delete a directory from `js-comint-module-paths'." t nil)

(autoload 'js-comint-save-setup "js-comint" "\
Save current setup to \".dir-locals.el\"." t nil)

(autoload 'js-comint-reset-repl "js-comint" "\
Kill existing REPL process if possible.
Create a new Javascript REPL process.
The environment variable `NODE_PATH' is setup by `js-comint-module-paths'
before the process starts." t nil)

(autoload 'js-comint-clear "js-comint" "\
Clear the Javascript REPL." t nil)

(autoload 'js-comint-start-or-switch-to-repl "js-comint" "\
Start a new repl or switch to existing repl." t nil)

(autoload 'js-comint-repl "js-comint" "\
Start a Javascript process by running CMD.
The environment variable \"NODE_PATH\" is setup by `js-comint-module-paths'.

\(fn CMD)" t nil)

(autoload 'js-comint-send-region "js-comint" "\
Send the current region to the inferior Javascript process.
If no region selected, you could manually input javascript expression." t nil)

(defalias 'js-send-region 'js-comint-send-region)

(autoload 'js-comint-send-last-sexp "js-comint" "\
Send the previous sexp to the inferior Javascript process." t nil)

(defalias 'js-send-last-sexp 'js-comint-send-last-sexp)

(autoload 'js-comint-send-buffer "js-comint" "\
Send the buffer to the inferior Javascript process." t nil)

(defalias 'js-send-buffer 'js-comint-send-buffer)

(autoload 'js-comint-load-file "js-comint" "\
Load FILE into the javascript interpreter.

\(fn FILE)" t nil)

(defalias 'js-load-file 'js-comint-load-file)

(autoload 'js-comint-mode "js-comint" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "js-comint" '("js-" "run-js" "switch-to-js")))

;;;***

;;;### (autoloads nil nil ("js-comint-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; js-comint-autoloads.el ends here
