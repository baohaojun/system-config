;;; git-timemachine.el --- Walk through git revisions of a file

;; Copyright (C) 2014 Peter Stiernström

;; Author: Peter Stiernström <peter@stiernstrom.se>
;; Version: 2.7
;; Package-Version: 20150624.1446
;; URL: https://github.com/pidu/git-timemachine
;; Keywords: git

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Use git-timemachine to browse historic versions of a file with p
;;; (previous) and n (next).

;;; Code:

(require 'vc-git)
(require 'cl-lib)

(defcustom git-timemachine-abbreviation-length 12
 "Number of chars from the full sha1 hash to use for abbreviation."
 :group 'git-timemachine)

(defcustom git-timemachine-show-minibuffer-details t
 "Non-nil means that details of the commit (its hash and date)
will be shown in the minibuffer while navigating commits."
 :group 'git-timemachine)

(defface git-timemachine-commit
 '((default :weight bold))
 "Face for git timemachine commit sha"
 :group 'git-timemachine)

(defface git-timemachine-minibuffer-detail-face
 '((t (:foreground "yellow")))
 "How to display the minibuffer detail"
 :group 'git-timemachine)

(defcustom git-timemachine-minibuffer-detail
 'subject
 "What to display when `git-timemachine-show-minibuffer-details` is t.
Available values are:
`commit` : The SHA hash of the commit
`subject`: The subject of the commit message"
 :type '(radio (const :tag "Commit SHA" commit) (const :tag "Commit Subject" subject))
 :group 'git-timemachine)

(defvar-local git-timemachine-directory nil)
(defvar-local git-timemachine-revision nil)
(defvar-local git-timemachine-file nil)
(defvar-local git-timemachine--revisions-cache nil)

(defun git-timemachine--revisions ()
 "List git revisions of current buffers file."
 (if git-timemachine--revisions-cache
  git-timemachine--revisions-cache
  (setq git-timemachine--revisions-cache
   (prog2
    (message "Fetching Revisions...")
    (let ((default-directory git-timemachine-directory)
          (file git-timemachine-file))
     (with-temp-buffer
      (unless (zerop (process-file vc-git-program nil t nil "--no-pager" "log" "--name-only" "--follow" "--pretty=format:%H:%ar:%ad:%s" file))
       (error "Git log command exited with non-zero exit status for file: %s" file))
      (goto-char (point-min))
      (let ((lines)
            (commit-number (/ (1+ (count-lines (point-min) (point-max))) 3)))
       (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
         (string-match "\\([^:]*\\):\\([^:]*\\):\\(.*\\):\\(.*\\)" line)
         (let ((commit (match-string 1 line))
               (date-relative (match-string 2 line))
               (date-full (match-string 3 line))
               (subject (match-string 4 line)))
          (forward-line 1)
          (let ((file-name (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
           (push (list commit file-name commit-number date-relative date-full subject) lines))))
        (setq commit-number (1- commit-number))
        (forward-line 2))
       (nreverse lines))))
    (message "Fetching Revisions...done")))))

(defun git-timemachine-show-current-revision ()
 "Show last (current) revision of file."
 (interactive)
 (git-timemachine-show-revision (car (git-timemachine--revisions))))

(defun git-timemachine--next-revision (revisions)
 "Return the revision following the current revision in REVISIONS."
 (cadr (cl-member (car git-timemachine-revision) revisions :key #'car :test #'string=)))

(defun git-timemachine-show-previous-revision ()
 "Show previous revision of file."
 (interactive)
 (git-timemachine-show-revision (git-timemachine--next-revision (git-timemachine--revisions))))

(defun git-timemachine-show-next-revision ()
 "Show next revision of file."
 (interactive)
 (git-timemachine-show-revision (git-timemachine--next-revision (reverse (git-timemachine--revisions)))))

(defun git-timemachine-show-nth-revision (rev-number)
 "Show the REV-NUMBER revision."
 (interactive "nEnter revision number: ")
 (let* ((revisions (reverse (git-timemachine--revisions)))
        (revision (nth (1- rev-number) revisions))
        (num-revisions (length revisions)))
  (if revision (git-timemachine-show-revision revision)
   (message "Only %d revisions exist." num-revisions))))

(defun git-timemachine-show-revision (revision)
 "Show a REVISION (commit hash) of the current file."
 (when revision
  (let ((current-position (point))
        (commit (car revision))
        (revision-file-name (nth 1 revision))
        (commit-index (nth 2 revision))
        (date-relative (nth 3 revision))
        (date-full (nth 4 revision))
        (subject (nth 5 revision)))
   (setq buffer-read-only nil)
   (erase-buffer)
   (let ((default-directory git-timemachine-directory))
    (process-file vc-git-program nil t nil "--no-pager" "show"
     (concat commit ":" revision-file-name)))
   (setq buffer-read-only t)
   (set-buffer-modified-p nil)
   (let* ((revisions (git-timemachine--revisions))
          (n-of-m (format "(%d/%d %s)" commit-index (length revisions) date-relative)))
    (setq mode-line-buffer-identification
     (list (propertized-buffer-identification "%12b") "@"
      (propertize (git-timemachine-abbreviate commit) 'face 'git-timemachine-commit) " name:" revision-file-name" " n-of-m)))
   (setq git-timemachine-revision revision)
   (goto-char current-position)
   (when git-timemachine-show-minibuffer-details
    (git-timemachine--show-minibuffer-details revision)))))

(defun git-timemachine--show-minibuffer-details (revision)
 "Show details for REVISION in minibuffer."
 (let ((detail
        (if (eq git-timemachine-minibuffer-detail 'commit)
         (car revision)
         (nth 5 revision)))
       (date-relative (nth 3 revision))
       (date-full (nth 4 revision)))
  (message (format "%s [%s (%s)]" (propertize detail 'face 'git-timemachine-minibuffer-detail-face) date-full date-relative))))

(defun git-timemachine-abbreviate (revision)
 "Return REVISION abbreviated to `git-timemachine-abbreviation-length' chars."
 (substring revision 0 git-timemachine-abbreviation-length))

(defun git-timemachine-quit ()
 "Exit the timemachine."
 (interactive)
 (kill-buffer))

(defun git-timemachine-kill-revision ()
 "Kill the current revisions abbreviated commit hash."
 (interactive)
 (let ((revision (car git-timemachine-revision)))
  (message revision)
  (kill-new revision)))

(defun git-timemachine-kill-abbreviated-revision ()
 "Kill the current revisions full commit hash."
 (interactive)
 (let ((revision (git-timemachine-abbreviate (car git-timemachine-revision))))
  (message revision)
  (kill-new revision)))

(define-minor-mode git-timemachine-mode
 "Git Timemachine, feel the wings of history."
 :init-value nil
 :lighter " Timemachine"
 :keymap
 '(("p" . git-timemachine-show-previous-revision)
   ("n" . git-timemachine-show-next-revision)
   ("g" . git-timemachine-show-nth-revision)
   ("q" . git-timemachine-quit)
   ("w" . git-timemachine-kill-abbreviated-revision)
   ("W" . git-timemachine-kill-revision))
 :group 'git-timemachine)

(defun git-timemachine-validate (file)
 "Validate that there is a FILE and that it belongs to a git repository.
Call with the value of 'buffer-file-name."
 (unless file
  (error "This buffer is not visiting a file"))
 (unless (vc-git-registered file)
  (error "This file is not git tracked")))

;;;###autoload
(defun git-timemachine-toggle ()
 "Toggle git timemachine mode."
 (interactive)
 (if (bound-and-true-p git-timemachine-mode)
  (git-timemachine-quit)
  (git-timemachine)))

;;;###autoload
(defun git-timemachine ()
 "Enable git timemachine for file of current buffer."
 (interactive)
 (setq git-timemachine--revisions-cache nil)
 (git-timemachine-validate (buffer-file-name))
 (let ((git-directory (expand-file-name (vc-git-root (buffer-file-name))))
       (file-name (buffer-file-name))
       (timemachine-buffer (format "timemachine:%s" (buffer-name)))
       (cur-line (line-number-at-pos))
       (mode major-mode))
  (with-current-buffer (get-buffer-create timemachine-buffer)
   (switch-to-buffer timemachine-buffer)
   (setq buffer-file-name file-name)
   (funcall mode)
   (setq git-timemachine-directory git-directory
         git-timemachine-file (file-relative-name file-name git-directory)
         git-timemachine-revision nil)
   (git-timemachine-show-current-revision)
   (goto-char (point-min))
   (forward-line (1- cur-line))
   (git-timemachine-mode))))

(provide 'git-timemachine)

;;; git-timemachine.el ends here
