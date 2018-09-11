;; gitlab-mode.el --- Mode for Gitlab

;; Copyright (C) 2014, 2015, 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

;; A major mode for Gitlab

;;; Code:

;;; Code:

(require 'browse-url)
(require 'tabulated-list)
(require 'vc-git)

;; Gitlab library

(require 'gitlab-projects)
(require 'gitlab-issues)


;; Core

(defun print-current-project-id ()
  "Display current project id."
  (interactive)
  (message (concat "Current ID is: " (tabulated-list-get-id))))

(defun print-current-issue-id ()
  "Display current issue id."
  (interactive)
  (message (format "Current ID is: %d" (assoc-default 'id (tabulated-list-get-id)))))

(defun project-make-button (text &rest props)
  "Make button with TEXT propertized with PROPS."
  (let ((button-text (if (display-graphic-p)
                         text
                       (concat "[" text "]")))
        (button-face (if (display-graphic-p)
                         '(:box (:line-width 2 :color "dark grey")
                                :background "light grey"
                                :foreground "black")
                       'link)))
    (apply 'insert-text-button button-text
           'face button-face
           'follow-link t
           props)))

;; Projects
(defun gitlab-project-clone-button-action (button)
  "Action for BUTTON."
  (interactive)
  (let* ((project (gitlab-get-project (button-get button 'project-id)))
         (name (assoc-default 'path project))
         (repo (assoc-default 'ssh_url_to_repo project))
         (target-dir (read-directory-name "Clone to directory:" (car query-replace-defaults))))

    (if (file-directory-p (expand-file-name name target-dir))
        (progn
          (message "Target directory exists and is not empty. Trying to pull.")
          (let ((default-directory (file-name-as-directory (expand-file-name name target-dir))))
            (vc-git-command nil 0 nil "pull" repo)))
      (progn
        (make-directory name target-dir)
        (vc-git-command nil 0 nil "clone" repo (file-name-as-directory (expand-file-name name target-dir)))))
    (revert-buffer nil t)
    (goto-char (point-min))))


(defun gitlab-goto-project ()
  "Got to web page of the project."
  (interactive)
  (let* ((project (gitlab-get-project (tabulated-list-get-id))))
    (browse-url (assoc-default 'web_url project))))

;;;###autoload
(defun gitlab-show-project-description (project)
  "Doc string PROJECT."
  (interactive)
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (let ((desc (assoc-default 'description project))
            (homepage (assoc-default 'web_url project))
            (id (assoc-default 'id project))
            (status (number-to-string (assoc-default 'visibility_level project))))

        (insert "       Name: ")
        (princ (assoc-default 'name project))
        (princ "\n")
        (insert "       Path: ")
        (princ (assoc-default 'path_with_namespace project))
        (princ "\n\n")

        (insert " Repository: ")
        (princ (assoc-default 'ssh_url_to_repo project))
        (insert "\n\n")

        (insert "     " (propertize "Status" 'font-lock-face 'bold) ": ")
        (cond ((string= status "0")
               (insert (propertize (capitalize "Private") 'font-lock-faces 'font-lock-builtin-face)))
              ((string= status "10")
               (insert (propertize (capitalize "Internal") 'font-lock-faces 'font-lock-builtin-face)))
              ((string= status "20")
               (insert (propertize (capitalize "Public") 'font-lock-faces 'font-lock-builtin-face))))
        (insert " -- ")
        (project-make-button
         "Clone to / Pull"
         'action 'gitlab-project-clone-button-action
         'project-id id)

        (insert "\n\n")


        (insert "    " (propertize "Summary" 'font-lock-face 'bold)
                ": " (if desc desc) "\n")

        (when homepage
          (insert "   " (propertize "Homepage" 'font-lock-face 'bold) ": ")
          (help-insert-xref-button homepage 'help-url homepage)
          (insert "\n"))))))


(defun gitlab-describe-project (&optional button)
  "Describe the current project.
If optional arg BUTTON is non-nil, describe its associated project."
  (interactive)
  (let ((project (gitlab-get-project (tabulated-list-get-id))))
    (if project
        (gitlab-show-project-description project)
      (user-error "No project here"))))



(defun gitlab-issues-for-project (&optional project-id)
  "From projects buffer, opens issues buffer for project at point."
  (interactive)
  (let ((project (gitlab-get-project (or project-id (tabulated-list-get-id)))))
    (if project
	(progn
	  (pop-to-buffer "*Gitlab issues*" nil)
	  (gitlab-issues-mode)
	  (setq tabulated-list-entries
		(create-issues-entries (gitlab-list-project-issues (assoc-default 'id project))))
	  (tabulated-list-print t)
	  (tabulated-list-sort 1))
      (user-error "No project here"))))

(defun gitlab-show-projects ()
  "Show Gitlab projects."
  (interactive)
  (pop-to-buffer "*Gitlab projects*" nil)
  (gitlab-projects-mode)
  (setq tabulated-list-entries
        (create-projects-entries (gitlab-list-all-projects)))
  (tabulated-list-print t))

(defun create-projects-entries (projects)
  "Create entries for 'tabulated-list-entries from PROJECTS."
  (mapcar (lambda (p)
            (let ((id (number-to-string (assoc-default 'id p)))
                  (owner (if (assoc-default 'owner p)
                             (assoc-default 'owner p)
                           (assoc-default 'namespace p)))
                  (namespace (assoc-default 'namespace p))
                  (description (if (eq (assoc-default 'description p) nil)
                                   ""
                                 (assoc-default 'description p))))

              (list id
                    (vector ;id
                            (assoc-default 'name p)
                            (assoc-default 'name owner)
                            (assoc-default 'name namespace)
                            (replace-regexp-in-string "\^M\\|\n" " " description)))))
          projects))

;; Issues

(defun gitlab-goto-issue ()
  "Got to web page of the issue."
  (interactive)
  (browse-url (assoc-default 'web_url (tabulated-list-get-id))))

(defun gitlab-close-issue ()
  "Close issue at point."
  (interactive)
  (let ((issue (tabulated-list-get-id)))
    (gitlab-edit-issue (assoc-default 'project_id issue) (assoc-default 'id issue)
                       nil nil nil nil nil "close")
    (gitlab-issues-for-project (assoc-default 'project_id issue))))

(defun gitlab-open-issue ()
  "Reopen issue at point."
  (interactive)
  (let ((issue (tabulated-list-get-id)))
    (gitlab-edit-issue (assoc-default 'project_id issue) (assoc-default 'id issue)
                       nil nil nil nil nil "reopen")
    (gitlab-issues-for-project (assoc-default 'project_id issue))))

(defun create-issues-entries (issues)
  "Create entries for 'tabulated-list-entries from ISSUES."
  (mapcar (lambda (i)
            (let ((id (number-to-string (assoc-default 'id i)))
                  (author (assoc-default 'author i)))
              (list i
                    (vector ;id
                     (assoc-default 'state i)
                     (format "%s" (assoc-default 'project_id i))
                     (assoc-default 'name author)
                     (assoc-default 'title i)))))
          issues))

;;;###autoload
(defun gitlab-show-issues ()
  "Show Gitlab issues."
  (interactive)
  (pop-to-buffer "*Gitlab issues*" nil)
  (gitlab-issues-mode)
  (setq tabulated-list-entries
        (create-issues-entries (gitlab-list-all-issues)))
  (tabulated-list-print t))


;; Gitlab projects mode

(defvar gitlab-projects-mode-hook nil)

(defvar gitlab-projects-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "v") 'print-current-project-id)
    (define-key map (kbd "w") 'gitlab-goto-project)
    (define-key map (kbd "d") 'gitlab-describe-project)
    map)
  "Keymap for `gitlab-projects-mode' major mode.")

(define-key gitlab-projects-mode-map (kbd "i") 'gitlab-issues-for-project)

(define-derived-mode gitlab-projects-mode tabulated-list-mode "Gitlab projects"
  "Major mode for browsing Gitlab projects."
  :group 'gitlab
  (setq tabulated-list-format [;("ID" 5 t)
                               ("Name" 25 t)
                               ("Owner"  25 t)
                               ("Namespace" 25 t)
                               ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

;; Gitlab issues mode

(defvar gitlab-issues-mode-hook nil)

(defvar gitlab-issues-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "v") 'print-current-issue-id)
    (define-key map (kbd "w") 'gitlab-goto-issue)
    map)
  "Keymap for `gitlab-issues-mode' major mode.")

(define-key gitlab-issues-mode-map (kbd "c") 'gitlab-close-issue)
(define-key gitlab-issues-mode-map (kbd "o") 'gitlab-open-issue)

(define-derived-mode gitlab-issues-mode tabulated-list-mode "Gitlab issues"
  "Major mode for browsing Gitlab issues."
  :group 'gitlab
  (setq tabulated-list-format [;("ID" 5 t)
                               ("State" 10 t)
                               ("Project" 8 t)
                               ("Author" 20 t)
                               ("Title" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header))

(provide 'gitlab-mode)
;;; gitlab-mode.el ends here
