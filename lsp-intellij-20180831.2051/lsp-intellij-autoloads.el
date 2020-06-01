;;; lsp-intellij-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "lsp-intellij" "lsp-intellij.el" (0 0 0 0))
;;; Generated autoloads from lsp-intellij.el

(defface lsp-intellij-face-code-lens-run '((((background dark)) :background "dark green") (((background light)) :background "green")) "\
Face used for areas with a run configuration." :group 'lsp-intellij-faces)

(defface lsp-intellij-face-code-lens-test '((((background dark)) :background "saddle brown") (((background light)) :background "yellow")) "\
Face used for areas with a test configuration." :group 'lsp-intellij-faces)

(defface lsp-intellij-face-code-lens-test-pass '((((background dark)) :background "sea green") (((background light)) :background "green")) "\
Face used for areas with a passing test configuration." :group 'lsp-intellij-faces)

(defface lsp-intellij-face-code-lens-test-fail '((((background dark)) :background "firebrick") (((background light)) :background "red")) "\
Face used for areas with a failing test configuration." :group 'lsp-intellij-faces)

(defface lsp-intellij-face-code-lens-test-unknown '((((background dark)) :background "saddle brown") (((background light)) :background "yellow")) "\
Face used for areas with a test configuration an with unknown state." :group 'lsp-intellij-faces)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "lsp-intellij" '("lsp-intellij-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; lsp-intellij-autoloads.el ends here
