;;; github-review-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "github-review" "github-review.el" (0 0 0 0))
;;; Generated autoloads from github-review.el

(autoload 'github-review-forge-pr-at-point "github-review" "\
Review the forge pull request at point." t nil)

(autoload 'github-review-start "github-review" "\
Start review given PR URL.

\(fn URL)" t nil)

(autoload 'github-review-approve "github-review" "\
Approve a PR (to be run from a buffer corresponding to a review)." t nil)

(autoload 'github-review-reject "github-review" "\
Reject a PR (to be run from a buffer corresponding to a review)." t nil)

(autoload 'github-review-comment "github-review" "\
Comment on a PR (to be run from a buffer corresponding to a review)." t nil)

(autoload 'github-review-mode "github-review" "\
Major mode for code review

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "github-review" '("github-review-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; github-review-autoloads.el ends here
