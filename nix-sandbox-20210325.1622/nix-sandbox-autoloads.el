;;; nix-sandbox-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nix-sandbox" "nix-sandbox.el" (0 0 0 0))
;;; Generated autoloads from nix-sandbox.el

(autoload 'nix-shell-command "nix-sandbox" "\
Assemble a command from ARGS that can be executed in the specified SANDBOX.

\(fn SANDBOX &rest ARGS)" nil nil)

(autoload 'nix-compile "nix-sandbox" "\
Compile a program using the given COMMAND in SANDBOX.

\(fn SANDBOX &rest COMMAND)" t nil)

(autoload 'nix-sandbox/nix-shell "nix-sandbox" "\
Run a COMMAND in the given SANDBOX and return the output.

\(fn SANDBOX &rest COMMAND)" nil nil)

(autoload 'nix-exec-path "nix-sandbox" "\
Return the `exec-path' of the given SANDBOX.

\(fn SANDBOX)" nil nil)

(autoload 'nix-executable-find "nix-sandbox" "\
Search for an EXECUTABLE in the given SANDBOX.

\(fn SANDBOX EXECUTABLE)" nil nil)

(autoload 'nix-find-sandbox "nix-sandbox" "\
Search for a sandbox starting at PATH traversing upwards the directory tree.
If the directory contains a `shell.nix' file, the path to this
file is returned.  Otherwise if the directory contains a
`default.nix' file, the parent directory is returned.

\(fn PATH)" nil nil)

(autoload 'nix-current-sandbox "nix-sandbox" "\
Return the path of the sandbox that is closest to the current working directory." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nix-sandbox" '("nix-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nix-sandbox-autoloads.el ends here
