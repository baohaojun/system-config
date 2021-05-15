;;; git-commit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "git-commit" "git-commit.el" (0 0 0 0))
;;; Generated autoloads from git-commit.el
(put 'git-commit-major-mode 'safe-local-variable
    (lambda (val)
      (memq val '(text-mode
                  markdown-mode
                  org-mode
                  fundamental-mode
                  git-commit-elisp-text-mode))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-commit" '("git-commit-" "global-git-commit-mode")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; git-commit-autoloads.el ends here
