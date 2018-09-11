;;; slime-autoloads.el --- autoload definitions for SLIME

;; Copyright (C) 2007  Helmut Eller

;; This file is protected by the GNU GPLv2 (or later), as distributed
;; with GNU Emacs.

;;; Commentary:

;; This code defines the necessary autoloads, so that we don't need to
;; load everything from .emacs.
;;
;; JT@14/01/09: FIXME: This file should be auto-generated with autoload cookies.

;;; Code:

(autoload 'slime "slime"
  "Start a Lisp subprocess and connect to its Swank server." t)

(autoload 'slime-mode "slime"
  "SLIME: The Superior Lisp Interaction (Minor) Mode for Emacs." t)

(autoload 'slime-connect "slime"
  "Connect to a running Swank server." t)

(autoload 'slime-selector "slime"
  "Select a new by type, indicated by a single character." t)

(autoload 'hyperspec-lookup "lib/hyperspec" nil t)

(autoload 'slime-lisp-mode-hook "slime")

(autoload 'slime-scheme-mode-hook "slime")

(defvar slime-contribs nil
  "A list of contrib packages to load with SLIME.")

(autoload 'slime-setup "slime"
  "Setup some SLIME contribs.")

(define-obsolete-variable-alias 'slime-setup-contribs
  'slime-contribs "2.3.2")

(add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(provide 'slime-autoloads)

;;; slime-autoloads.el ends here
;; Local Variables:
;; no-byte-compile: t
;; End:

;;;### (autoloads nil "slime" "slime.el" (0 0 0 0))
;;; Generated autoloads from slime.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime" '("sldb-" "slime" "def-slime-selector-method" "define-sl" "??" "?q" "?i" "?v" "?l" "?d" "?e" "?c" "?n" "?p" "?t" "make-slime-" "inferior-lisp-program")))

;;;***

;;;### (autoloads nil nil ("slime-pkg.el") (0 0 0 0))

;;;***

;;;### (autoloads nil "slime-tests" "slime-tests.el" (0 0 0 0))
;;; Generated autoloads from slime-tests.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "slime-tests" '("symbol-at-point." "sexp-at-point.1" "sbcl-world-lock" "report-condition-with-circular-list" "read" "traditional-recipe" "def-slime-test" "dis" "find-definition" "flow-control" "inspector" "indentation" "inter" "end-of-file" "loop-interrupt-" "locally-bound-debugger-hook" "break" "macroexpand" "utf-8-source" "unwind-to-previous-sldb-level" "arglist" "async-eval-debugging" "comp" "narrowing")))

;;;***
