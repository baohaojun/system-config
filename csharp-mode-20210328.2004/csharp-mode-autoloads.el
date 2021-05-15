;;; csharp-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "csharp-compilation" "csharp-compilation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from csharp-compilation.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "csharp-compilation" '("csharp-")))

;;;***

;;;### (autoloads nil "csharp-mode" "csharp-mode.el" (0 0 0 0))
;;; Generated autoloads from csharp-mode.el

(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

(defvar csharp-mode-hook nil "\
*Hook called by `csharp-mode'.")

(custom-autoload 'csharp-mode-hook "csharp-mode" t)

(autoload 'csharp-mode "csharp-mode" "\
Major mode for editing Csharp code.

Key bindings:
\\{csharp-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "csharp-mode" '("codedoc-font-lock-" "csharp-")))

;;;***

;;;### (autoloads nil "csharp-tree-sitter" "csharp-tree-sitter.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from csharp-tree-sitter.el

(autoload 'csharp-tree-sitter-mode "csharp-tree-sitter" "\
Major mode for editing Csharp code.

Key bindings:
\\{csharp-tree-sitter-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "csharp-tree-sitter" '("csharp-" "tree-sitter-indent-csharp-tree-sitter-scopes")))

;;;***

;;;### (autoloads nil nil ("csharp-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; csharp-mode-autoloads.el ends here
