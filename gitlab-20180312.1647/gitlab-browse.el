;;; gitlab-browse.el --- Gitlab API settings.

;; Copyright (C) 2016 Bryan W. Berry <bryan.berry@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;; Code:


(require 's)
(require 'f)

(defun gitlab--shell-to-string (cmd)
  "Execute CMD in a shell.
Returns stdout if command succeeds,otherwise returns nil"
  (interactive)
  (with-temp-buffer
    (let ((glob-buf-stdout (buffer-name)))
      (if (eq 0 (call-process-shell-command cmd nil glob-buf-stdout))
          (buffer-string)
        (signal 'error (list (format "command \"%s\" failed with non-zero exit code" cmd)))))))
  

(defun gitlab--get-git-exec ()
  "This duplicates functionality inside magit."
  (executable-find "git"))

(defun gitlab--get-branch ()
  "Find the current branch."
  (let
      ((cmd (format "%s rev-parse --abbrev-ref HEAD" (gitlab--get-git-exec))))
    (s-trim (gitlab--shell-to-string cmd))))


(defun gitlab--get-top-level-dir ()
  "Find the toplevel project directory."
  (let ((cmd (format "%s rev-parse --show-toplevel" (gitlab--get-git-exec))))
    (s-trim
     (gitlab--shell-to-string cmd))))

(defun gitlab--get-relevant-path ()
  "Return directory path if in dired mode.
Return full file path if viewing a file.  For all other cases, return the toplevel git directory."
  (cond
   ((string= major-mode "dired-mode") (dired-current-directory))
   ((null buffer-file-name) (gitlab--get-top-level-dir))
   (t (buffer-file-name))))

(defun gitlab--viewing-filep ()
  "Return t if currently viewing a file, nil otherwise."
  (when (not (null buffer-file-name)) t))


(defun gitlab--get-project-group+name ()
  "Gets the project's gitlab group and name."
  (let ((origin (gitlab--get-origin)))
    (if (null origin)
        nil
      (car (split-string
             (cadr (split-string origin ":" t "\n"))
             ".git" t)))))

(defun gitlab--get-origin ()
  "Get the full git url for the origin remote."
  (interactive)
  (let ((get-url-cmd (format "%s remote get-url origin" (gitlab--get-git-exec))))
    (gitlab--shell-to-string get-url-cmd)))
     

(defun gitlab--get-current-path-relative ()
  "Find the current path relative to the toplevel directory."
  (file-relative-name (gitlab--get-relevant-path) (gitlab--get-top-level-dir)))


(defun gitlab--get-line-nums ()
  "Return line number(s) if applicable.
If region selected, returns range in url form"
  (cond ((not (gitlab--viewing-filep)) "")
        ((use-region-p)
         (format "#L%d-%d"
                 (line-number-at-pos (region-beginning))
                 (line-number-at-pos (region-end))))
         (t
          (format "#L%d" (line-number-at-pos (point))))))

(defun gitlab--make-url-current-file ()
  "Create gitlab url for current file."
  (let* ((host gitlab-host)
         (branch (gitlab--get-branch))
         (group+name (gitlab--get-project-group+name))
         (path (gitlab--get-current-path-relative))
         (linenums (gitlab--get-line-nums)))
    (format "%s/%s" host (f-join group+name "blob" branch
                                 (format "%s%s" path linenums)))))


(defun gitlab-kill ()
  "Add URL for current selection to killring, if applicable.
If a Dired buffer is open, create a url to view to the directory listing
in gitlab.  If the current buffer is neither a file or Dired buffer, gets a
url for the current project in gitlab."
  (interactive)
  (let ((url (gitlab--make-url-current-file)))
    (kill-new url)
    url))


(defun gitlab-browse ()
  "Open browser to view current selection, if applicable, in gitlab.
If a Dired buffer is open, browse to the directory listing in gitlab.
If the current buffer is neither a file or Dired buffer, open the current
project in gitlab"
  (interactive)
  (browse-url (gitlab--make-url-current-file)))


(provide 'gitlab-browse)

;;; gitlab-browse.el ends here
