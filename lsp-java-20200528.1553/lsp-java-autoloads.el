;;; lsp-java-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-java" "lsp-java.el" (0 0 0 0))
;;; Generated autoloads from lsp-java.el

(autoload 'lsp-java-lens-mode "lsp-java" "\
Toggle run/debug overlays.

If called interactively, enable Lsp-Java-Lens mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)
(with-eval-after-load 'lsp-mode (require 'lsp-java))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-java" '("lsp-java-")))

;;;***

;;;### (autoloads nil "lsp-java-boot" "lsp-java-boot.el" (0 0 0 0))
;;; Generated autoloads from lsp-java-boot.el

(autoload 'lsp-java-boot-lens-mode "lsp-java-boot" "\
Toggle code-lens overlays.

If called interactively, enable Lsp-Java-Boot-Lens mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-java-boot" '("lsp-java-boot-")))

;;;***

;;;### (autoloads nil "lsp-jt" "lsp-jt.el" (0 0 0 0))
;;; Generated autoloads from lsp-jt.el

(autoload 'lsp-jt-lens-mode "lsp-jt" "\
Toggle code-lens overlays.

If called interactively, enable Lsp-Jt-Lens mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'lsp-jt-browser "lsp-jt" nil t nil)

(autoload 'lsp-jt-show-report "lsp-jt" nil t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-jt" '("lsp-jt-")))

;;;***

;;;### (autoloads nil nil ("lsp-java-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-java-autoloads.el ends here
