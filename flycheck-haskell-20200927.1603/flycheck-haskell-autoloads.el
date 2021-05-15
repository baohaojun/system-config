;;; flycheck-haskell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck-haskell" "flycheck-haskell.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from flycheck-haskell.el

(autoload 'flycheck-haskell-setup "flycheck-haskell" "\
Setup Haskell support for Flycheck.

If the current file is part of a Cabal project, configure
Flycheck to take the module paths of the Cabal projects into
account.

Also search for Cabal sandboxes and add them to the module search
path as well." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-haskell" '("flycheck-haskell-")))

;;;***

;;;### (autoloads nil nil ("flycheck-haskell-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-haskell-autoloads.el ends here
