;;; flycheck-rust-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "flycheck-rust" "flycheck-rust.el" (0 0 0 0))
;;; Generated autoloads from flycheck-rust.el

(autoload 'flycheck-rust-setup "flycheck-rust" "\
Setup Rust in Flycheck.

If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "flycheck-rust" '("flycheck-rust-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-rust-autoloads.el ends here
