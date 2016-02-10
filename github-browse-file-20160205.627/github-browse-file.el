;;; github-browse-file.el --- View the file you're editing on GitHub

;; Copyright (C) 2013 Ozan Sener & Contributors

;; Author: Ozan Sener <ozan@ozansener.com>
;; Homepage: https://github.com/osener/github-browse-file
;; Version: 0.5.0
;; Package-Version: 20160205.627
;; Keywords: convenience vc git github
;; Package-Requires: ((cl-lib "0.5"))

;;; Installation:

;; Available as a package in Marmalade at http://marmalade-repo.org/
;; M-x package-install github-browse-file

;;; Commentary:

;; Call `github-browse-file' (for the git blob) or `github-browse-file-blame'
;; (for the git blame) to view current file on GitHub. With a prefix argument
;; (C-u), you can force them to use the "master" branch.

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'vc-git)

(defgroup github-browse-file nil
  "View the current file on GitHub"
  :group 'tools)

(defcustom github-browse-file-visit-url t
  "If non-nil, run `browse-url' after saving url to kill ring"
  :group 'github-browse-file
  :type 'boolean)

(defcustom github-browse-file-show-line-at-point nil
  "If non-nil, link to the current line or active region"
  :group 'github-browse-file
  :type 'boolean)

(defvar github-browse-file--view-blame nil
  "If non-nil, view \"blame\" instead of \"blob\".
This should only ever be `let'-bound, not set outright.")

(defvar github-browse-file--force-master nil
  "Whether to use \"master\" regardless of current branch
This should only ever be `let'-bound, not set outright.")

(defvar github-browse-file--magit-commit-link-modes
  '(magit-commit-mode magit-revision-mode magit-log-mode)
  "Non-file magit modes that should link to commits.")

(defun github-browse-file--relative-url ()
  "Return \"username/repo\" for current repository.

Error out if this isn't a GitHub repo."
  (let ((url (vc-git--run-command-string nil "config" "remote.origin.url")))
    (unless url (error "Not in a GitHub repo"))
    (when (and url (string-match "github.com:?/?\\(.*\\)" url))
      (replace-regexp-in-string "\\.git$" "" (match-string 1 url)))))

(defun github-browse-file--repo-relative-path ()
  "Return the path to the current file relative to the repository root."
  (let* ((root (ignore-errors (vc-git-root buffer-file-name))))
    (and root (file-relative-name buffer-file-name root))))

(defun github-browse-file--ahead-p ()
  "Return non-nil if current git HEAD is ahead of origin/master"
  (let ((rev (vc-git--run-command-string
              nil "rev-list" "--left-right" "origin/master...HEAD")))
    (and (> (length rev) 0)
         (string-equal (substring rev 0 1) ">"))))

(defun github-browse-file--remote-branch ()
  "Return the name of remote branch current branch is tracking.
If there is none return 'master'."
  (let* ((ref (replace-regexp-in-string
               "\n" ""
               (vc-git--run-command-string nil "symbolic-ref" "-q" "HEAD")))
         (origin-branch (replace-regexp-in-string
                         "\n" ""
                         (vc-git--run-command-string
                          nil "for-each-ref" "--format=%(upstream:short)" ref)))
         (branch-name (mapconcat 'identity
                                 (cdr (split-string origin-branch "/"))
                                 "/")))
    (if (eq branch-name "") "master" branch-name)))

(defun github-browse-file--current-rev ()
  "Return the SHA1 of HEAD if it is not ahead of origin/master.
If github-browse-file--force-master is non-nil, return \"master\".
Otherwise, return the name of the current  branch."
  (cond
   (github-browse-file--force-master "master")
   ((member major-mode github-browse-file--magit-commit-link-modes)
    (magit-commit-at-point))
   ((github-browse-file--ahead-p) (github-browse-file--remote-branch))
   (t (let ((rev (vc-git--run-command-string nil "rev-parse" "HEAD")))
        (and rev (replace-regexp-in-string "\n" "" rev))))))

(defun github-browse-file--browse-url (&optional anchor)
  "Load http://github.com/user/repo/file#ANCHOR in a web browser and add it to
the kill ring."
  (let ((url (concat "https://github.com/"
                     (github-browse-file--relative-url) "/"
                     (cond ((eq major-mode 'magit-status-mode) "tree")
                           ((member major-mode github-browse-file--magit-commit-link-modes) "commit")
                           (github-browse-file--view-blame "blame")
                           (t "blob")) "/"
                           (github-browse-file--current-rev) "/"
                           (github-browse-file--repo-relative-path)
                           (when anchor (concat "#" anchor)))))
    (github-browse--save-and-view url)))

(defun github-browse-file--anchor-lines ()
  "Calculate anchor from lines in active region or current line

If `github-browse-file-show-line-at-point' is non-nil, then
default to current line."
  (cond
   ((and transient-mark-mode mark-active)
    (let ((start (line-number-at-pos (region-beginning)))
          (end (line-number-at-pos (region-end))))
      (when (eq (char-before (region-end)) ?\n) (cl-decf end))
      (if (>= start end)
          (format "L%d" start)
        (format "L%d-L%d" start end))))
   (github-browse-file-show-line-at-point
    (format "L%d" (line-number-at-pos (point))))))

(defun github-browse-file--guess-commit ()
  "Guess the current git commit.
If you are in any magit mode, use `magit-commit-at-point'.
Otherwise, if the region is active, use that.
Otherwse, use `github-browse-file--current-rev'."
  (cond
   ((and (derived-mode-p 'magit-mode) (magit-commit-at-point))
    (magit-commit-at-point))
   ((region-active-p)
    (buffer-substring (region-beginning) (region-end)))
   (t (github-browse-file--current-rev))))

(defun github-browse--save-and-view (url)
  "Save url to kill ring and browse or show the url"
  (kill-new url)
  (if github-browse-file-visit-url
      (browse-url url)
    (message "GitHub: %s" url)))

;;;###autoload
(defun github-browse-file (&optional force-master)
  "Show the GitHub webpage for the current file. The URL for the webpage is
added to the kill ring. With a prefix argument, \"master\" is used
regardless of the current branch.

In Transient Mark mode, if the mark is active, highlight the contents of the
region."
  (interactive "P")
  (let ((path (github-browse-file--repo-relative-path))
        (github-browse-file--force-master force-master))
    (github-browse-file--browse-url (github-browse-file--anchor-lines))))

;;;###autoload
(defun github-browse-file-blame (&optional force-master)
  "Show the GitHub blame page for the current file. The URL for the webpage is
added to the kill ring. With a prefix argument, \"master\" is used
regardless of the current branch.

In Transient Mark mode, if the mark is active, highlight the contents of the
region."
  (interactive "P")
  (let ((github-browse-file--view-blame t))
    (github-browse-file force-master)))

;;;###autoload
(defun github-browse-commit ()
  "Show the GitHub page for the current commit."
  (interactive)
  (let* ((commit (github-browse-file--guess-commit))
         (url (concat "https://github.com/"
                      (github-browse-file--relative-url)
                      "/commit/"
                      commit)))
    (github-browse--save-and-view url)))

(provide 'github-browse-file)
;;; github-browse-file.el ends here
