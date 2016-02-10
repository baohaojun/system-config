;;; bug-reference-github-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (bug-reference-github-set-url-format) "bug-reference-github"
;;;;;;  "bug-reference-github.el" (21286 54513 235708 827000))
;;; Generated autoloads from bug-reference-github.el

(autoload 'bug-reference-github-set-url-format "bug-reference-github" "\
Automatically set `bug-reference-url-format'.
Automatically set `bug-reference-url-format' and enable
`bug-reference-prog-mode' buffers from Github repositories.

What it does is:

1. If `bug-reference-url-format' is not set and this appears to be
    part of a git working copy (we can locate a .git/config).

2. Find the git remote repository (run 'git ls-remote --get-url').

3. If the remote matches github.com set `bug-reference-url-format' to
    the correct Github issue URL (we set it buffer locally).

4. Enable `bug-reference-prog-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("bug-reference-github-pkg.el") (21286
;;;;;;  54513 305952 48000))

;;;***

(provide 'bug-reference-github-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bug-reference-github-autoloads.el ends here
