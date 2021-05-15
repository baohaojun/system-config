;;; dune-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dune" "dune.el" (0 0 0 0))
;;; Generated autoloads from dune.el

(autoload 'dune-mode "dune" "\
Major mode to edit dune files.
For customization purposes, use `dune-mode-hook'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\(?:\\`\\|/\\)dune\\(?:\\.inc|-project\\)?\\'" . dune-mode))

(autoload 'dune-promote "dune" "\
Promote the correction for the current file." t nil)

(autoload 'dune-runtest-and-promote "dune" "\
Run tests in the current directory and promote the current buffer." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dune" '("dune-")))

;;;***

;;;### (autoloads nil "dune-flymake" "dune-flymake.el" (0 0 0 0))
;;; Generated autoloads from dune-flymake.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dune-flymake" '("dune-flymake-")))

;;;***

;;;### (autoloads nil "dune-watch" "dune-watch.el" (0 0 0 0))
;;; Generated autoloads from dune-watch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dune-watch" '("dune-watch-" "start-dune-watch")))

;;;***

;;;### (autoloads nil nil ("dune-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dune-autoloads.el ends here
