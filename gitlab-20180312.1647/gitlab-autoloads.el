;;; gitlab-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "gitlab-api" "gitlab-api.el" (0 0 0 0))
;;; Generated autoloads from gitlab-api.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-api" '("gitlab-")))

;;;***

;;;### (autoloads nil "gitlab-browse" "gitlab-browse.el" (0 0 0 0))
;;; Generated autoloads from gitlab-browse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-browse" '("gitlab-")))

;;;***

;;;### (autoloads nil "gitlab-groups" "gitlab-groups.el" (0 0 0 0))
;;; Generated autoloads from gitlab-groups.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-groups" '("gitlab-")))

;;;***

;;;### (autoloads nil "gitlab-http" "gitlab-http.el" (0 0 0 0))
;;; Generated autoloads from gitlab-http.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-http" '("perform-gitlab-request" "gitlab--")))

;;;***

;;;### (autoloads nil "gitlab-issues" "gitlab-issues.el" (0 0 0 0))
;;; Generated autoloads from gitlab-issues.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-issues" '("gitlab-")))

;;;***

;;;### (autoloads nil "gitlab-milestones" "gitlab-milestones.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from gitlab-milestones.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-milestones" '("gitlab-")))

;;;***

;;;### (autoloads nil "gitlab-mode" "gitlab-mode.el" (0 0 0 0))
;;; Generated autoloads from gitlab-mode.el

(autoload 'gitlab-show-project-description "gitlab-mode" "\
Doc string PROJECT.

\(fn PROJECT)" t nil)

(autoload 'gitlab-show-issues "gitlab-mode" "\
Show Gitlab issues.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-mode" '("gitlab-" "create-" "print-current-" "project-make-button")))

;;;***

;;;### (autoloads nil "gitlab-notes" "gitlab-notes.el" (0 0 0 0))
;;; Generated autoloads from gitlab-notes.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-notes" '("gitlab-")))

;;;***

;;;### (autoloads nil "gitlab-projects" "gitlab-projects.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from gitlab-projects.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-projects" '("gitlab-")))

;;;***

;;;### (autoloads nil "gitlab-session" "gitlab-session.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from gitlab-session.el

(autoload 'with-gitlab-auth "gitlab-session" "\
Macro which checks authentication token. If not defined, signals an
error.  Otherwise, executes `BODY'.

\(fn &rest BODY)" nil t)

;;;***

;;;### (autoloads nil "gitlab-ui" "gitlab-ui.el" (0 0 0 0))
;;; Generated autoloads from gitlab-ui.el

(autoload 'gitlab-mode "gitlab-ui" "\
Special mode for Gitlab buffers.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-ui" '("gitlab-" "helm-gitlab--buffer-name" "colorize-dot")))

;;;***

;;;### (autoloads nil "gitlab-users" "gitlab-users.el" (0 0 0 0))
;;; Generated autoloads from gitlab-users.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-users" '("gitlab-")))

;;;***

;;;### (autoloads nil "gitlab-utils" "gitlab-utils.el" (0 0 0 0))
;;; Generated autoloads from gitlab-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-utils" '("gitlab-")))

;;;***

;;;### (autoloads nil "gitlab-version" "gitlab-version.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from gitlab-version.el

(autoload 'gitlab-version "gitlab-version" "\
Get the gitlab version as string.
If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.
The returned string includes both, the version from package.el
and the library version, if both a present and different.
If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "gitlab-version" '("gitlab--library-version")))

;;;***

;;;### (autoloads nil nil ("gitlab-pkg.el" "gitlab.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gitlab-autoloads.el ends here
