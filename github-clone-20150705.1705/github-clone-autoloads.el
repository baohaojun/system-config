;;; github-clone-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "github-clone" "github-clone.el" (21914 12544
;;;;;;  254755 661000))
;;; Generated autoloads from github-clone.el

(autoload 'github-clone "github-clone" "\
Fork and clone USER-REPO-URL into DIRECTORY.

USER-REPO-URL can be any of the forms:

  repository
  user/repository
  organization/repository
  https://github.com/user/repository
  git@github.com:user/repository.git
  https://github.com/user/repository.el.git

It will immediately clone the repository (as the origin) to
DIRECTORY. Then it prompts to fork the repository and add a
remote named after the github username to the fork.

\(fn USER-REPO-URL DIRECTORY)" t nil)

(autoload 'eshell/github-clone "github-clone" "\
Eshell alias uses current directory as default.

\(fn USER-REPO-URL &optional DIRECTORY)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; github-clone-autoloads.el ends here
