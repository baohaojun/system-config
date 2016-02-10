;;; magit-svn-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (turn-on-magit-svn magit-svn-mode magit-svn-fetch-externals
;;;;;;  magit-svn-remote-update magit-svn-dcommit magit-svn-rebase
;;;;;;  magit-svn-create-tag magit-svn-create-branch magit-svn-find-rev)
;;;;;;  "magit-svn" "magit-svn.el" (21286 54511 59702 377000))
;;; Generated autoloads from magit-svn.el

(autoload 'magit-svn-find-rev "magit-svn" "\
Find commit for svn REVISION in BRANCH.

\(fn REV &optional BRANCH)" t nil)

(autoload 'magit-svn-create-branch "magit-svn" "\
Create svn branch NAME.

\(fn NAME)" t nil)

(autoload 'magit-svn-create-tag "magit-svn" "\
Create svn tag NAME.

\(fn NAME)" t nil)

(autoload 'magit-svn-rebase "magit-svn" "\
Run git-svn rebase.

\(fn)" t nil)

(autoload 'magit-svn-dcommit "magit-svn" "\
Run git-svn dcommit.

\(fn)" t nil)

(autoload 'magit-svn-remote-update "magit-svn" "\
Run git-svn fetch.

\(fn)" t nil)

(autoload 'magit-svn-fetch-externals "magit-svn" "\
Loops through all external repos found by `magit-svn-external-directories'
   and runs git svn fetch, and git svn rebase on each of them.

\(fn)" t nil)

(autoload 'magit-svn-mode "magit-svn" "\
SVN support for Magit

\(fn &optional ARG)" t nil)

(autoload 'turn-on-magit-svn "magit-svn" "\
Unconditionally turn on `magit-svn-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("magit-svn-pkg.el") (21286 54511 145162
;;;;;;  772000))

;;;***

(provide 'magit-svn-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-svn-autoloads.el ends here
