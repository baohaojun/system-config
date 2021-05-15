;;; rust-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rust-cargo" "rust-cargo.el" (0 0 0 0))
;;; Generated autoloads from rust-cargo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-cargo" '("rust-")))

;;;***

;;;### (autoloads nil "rust-compile" "rust-compile.el" (0 0 0 0))
;;; Generated autoloads from rust-compile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-compile" '("cargo-compilation-regexps" "rustc-")))

;;;***

;;;### (autoloads nil "rust-mode" "rust-mode.el" (0 0 0 0))
;;; Generated autoloads from rust-mode.el

(autoload 'rust-mode "rust-mode" "\
Major mode for Rust code.

\\{rust-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-mode" '("rust-")))

;;;***

;;;### (autoloads nil "rust-playpen" "rust-playpen.el" (0 0 0 0))
;;; Generated autoloads from rust-playpen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-playpen" '("rust-")))

;;;***

;;;### (autoloads nil "rust-rustfmt" "rust-rustfmt.el" (0 0 0 0))
;;; Generated autoloads from rust-rustfmt.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-rustfmt" '("rust-")))

;;;***

;;;### (autoloads nil "rust-utils" "rust-utils.el" (0 0 0 0))
;;; Generated autoloads from rust-utils.el

(autoload 'rust-dbg-wrap-or-unwrap "rust-utils" "\
Either remove or add the dbg! macro." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rust-utils" '("rust-")))

;;;***

;;;### (autoloads nil nil ("rust-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rust-mode-autoloads.el ends here
