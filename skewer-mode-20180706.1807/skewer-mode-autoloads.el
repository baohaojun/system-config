;;; skewer-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cache-table" "cache-table.el" (0 0 0 0))
;;; Generated autoloads from cache-table.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cache-table" '("cache-table-")))

;;;***

;;;### (autoloads nil "skewer-bower" "skewer-bower.el" (0 0 0 0))
;;; Generated autoloads from skewer-bower.el

(autoload 'skewer-bower-refresh "skewer-bower" "\
Update the package listing and packages synchronously.

\(fn)" t nil)

(autoload 'skewer-bower-load "skewer-bower" "\
Dynamically load a library from bower into the current page.

\(fn PACKAGE &optional VERSION)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "skewer-bower" '("skewer")))

;;;***

;;;### (autoloads nil "skewer-css" "skewer-css.el" (0 0 0 0))
;;; Generated autoloads from skewer-css.el

(autoload 'skewer-css-mode "skewer-css" "\
Minor mode for interactively loading new CSS rules.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "skewer-css" '("skewer-css")))

;;;***

;;;### (autoloads nil "skewer-html" "skewer-html.el" (0 0 0 0))
;;; Generated autoloads from skewer-html.el

(autoload 'skewer-html-mode "skewer-html" "\
Minor mode for interactively loading new HTML.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "skewer-html" '("skewer-html-")))

;;;***

;;;### (autoloads nil "skewer-mode" "skewer-mode.el" (0 0 0 0))
;;; Generated autoloads from skewer-mode.el

(autoload 'list-skewer-clients "skewer-mode" "\
List the attached browsers in a buffer.

\(fn)" t nil)

(autoload 'skewer-mode "skewer-mode" "\
Minor mode for interacting with a browser.

\(fn &optional ARG)" t nil)

(autoload 'run-skewer "skewer-mode" "\
Attach a browser to Emacs for a skewer JavaScript REPL. Uses
`browse-url' to launch a browser.

With a prefix arugment (C-u), it will ask the filename of the
root document.  With two prefix arguments (C-u C-u), it will use
the contents of the current buffer as the root document.

\(fn &optional ARG)" t nil)

(autoload 'skewer-run-phantomjs "skewer-mode" "\
Connect an inferior PhantomJS process to Skewer, returning the process.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "skewer-mode" '("skewer" "phantomjs-program-name" "httpd/skewer/")))

;;;***

;;;### (autoloads nil "skewer-repl" "skewer-repl.el" (0 0 0 0))
;;; Generated autoloads from skewer-repl.el

(autoload 'skewer-repl--response-hook "skewer-repl" "\
Catches all browser messages logging some to the REPL.

\(fn RESPONSE)" nil nil)

(autoload 'skewer-repl "skewer-repl" "\
Start a JavaScript REPL to be evaluated in the visiting browser.

\(fn)" t nil)

(eval-after-load 'skewer-mode '(progn (add-hook 'skewer-response-hook #'skewer-repl--response-hook) (add-hook 'skewer-repl-mode-hook #'skewer-repl-mode-compilation-shell-hook) (define-key skewer-mode-map (kbd "C-c C-z") #'skewer-repl)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "skewer-repl" '("skewer-" "company-skewer-repl")))

;;;***

;;;### (autoloads nil "skewer-setup" "skewer-setup.el" (0 0 0 0))
;;; Generated autoloads from skewer-setup.el

(autoload 'skewer-setup "skewer-setup" "\
Fully integrate Skewer into js2-mode, css-mode, and html-mode buffers.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("skewer-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; skewer-mode-autoloads.el ends here
