;;; ivy-gitlab.el --- Ivy interface to Gitlab

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/emacs-gitlab
;; Package-Version: 20181228.826
;; Version: 0.1.0
;; Keywords: gitlab, ivy

;; Package-Requires: ((s "1.9.0") (dash "2.9.0") (ivy "0.8.0") (gitlab "0.8"))

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

;; Provides an Ivy interface to Gitlab


;;; Code:

(require 'browse-url)
(require 'dash)
(require 'ivy)
(require 's)

;; Customization

(defgroup ivy-gitlab nil
  "Ivy interface for Emacs."
  :group 'gitlab
  :link '(url-link :tag "Github" "https://github.com/nlamirault/emacs-gitlab")
  :link '(emacs-commentary-link :tag "Commentary" "emacs-gitlab"))


(defface ivy-gitlab--title
  '((((class color) (background light)) :foreground "red" :weight semi-bold)
    (((class color) (background dark)) :foreground "green" :weight semi-bold))
  "Face of Gitlab information"
  :group 'ivy-gitlab)


;; Gitlab library

(require 'gitlab-api)
(require 'gitlab-session)
(require 'gitlab-projects)
(require 'gitlab-issues)
(require 'gitlab-ui)


;; Core

(defun ivy-gitlab--list-projects ()
  (with-gitlab-auth
   (let ((projects (gitlab-list-all-projects))
         (gitlab-projects-alist nil))
     (mapc (lambda (p)
             (add-to-list
              'gitlab-projects-alist
              (cons (format "%s" (propertize (assoc-default 'name_with_namespace p)
                                             'face
                                             'ivy-gitlab--title))
                    (list :page (assoc-default 'web_url p)
                          :name (assoc-default 'name p)
                          :project-id (assoc-default 'id p)))))
           projects)
     gitlab-projects-alist)))


(defun ivy-gitlab--list-project-issues (project-id)
  (with-gitlab-auth
   (let ((issues (gitlab-list-project-issues project-id))
         (gitlab-project-issues-alist nil))
     (mapc (lambda (i)
             (add-to-list
              'gitlab-project-issues-alist
              (cons (format "#%-4s %s :%s:"
                            (assoc-default 'iid i)
                            (propertize (decode-coding-string (assoc-default 'title i) 'utf-8)
                                        'face
                                        'ivy-gitlab--title)
                            (assoc-default 'state i))
                    (list :project-id (assoc-default 'project_id i)
                          :issue-id (assoc-default 'iid i)
                          :name (decode-coding-string (assoc-default 'title i) 'utf-8)))))
           issues)
     gitlab-project-issues-alist)))

(defun ivy-gitlab--project-names ()
  (with-gitlab-auth
   (let ((projects (gitlab-list-all-projects))
         (gitlab-project-names-alist nil))
     (mapc (lambda (p)
             (add-to-list
              'gitlab-project-names-alist
              (cons (assoc-default 'id p) (assoc-default 'name_with_namespace p))))
           projects)
     gitlab-project-names-alist)))

(defun ivy-gitlab--list-all-issues ()
  (with-gitlab-auth
   (let ((issues (gitlab-list-all-issues))
         (gitlab-all-issues-alist nil)
         (project-names (ivy-gitlab--project-names)))
     (mapc (lambda (i)
             (add-to-list
              'gitlab-all-issues-alist
              (cons (format "#%-4s %s :%s: -- %s"
                            (assoc-default 'iid i)
                            (propertize (decode-coding-string (assoc-default 'title i) 'utf-8)
                                        'face
                                        'ivy-gitlab--title)
                            (assoc-default 'state i)
                            (assoc-default (assoc-default 'project_id i) project-names))
                    (list :project-id (assoc-default 'project_id i)
                          :issue-id (assoc-default 'iid i)
                          :name (decode-coding-string (assoc-default 'title i) 'utf-8)))))
           issues)
     gitlab-all-issues-alist)))


(defun ivy-gitlab--list-projects-action-default (project)
  (browse-url (plist-get (cdr project) :page)))


(defun ivy-gitlab--list-issues-action-default (issue)
  (browse-url
   (gitlab-projects--get-issue-link
    (plist-get (cdr issue) :project-id) (plist-get (cdr issue) :issue-id))))


(defun ivy-gitlab--list-project-issues-action-default (project)
  (ivy-read "Issues: "
            (ivy-gitlab--list-project-issues (plist-get (cdr project) :project-id))
            :action #'ivy-gitlab--list-issues-action-default
            :caller 'ivy-gitlab--list-project-issues-action-default))

;; API

;;;###autoload
(defun ivy-gitlab-list-projects ()
  "List Gitlab projects using Ivy."
  (interactive)
  (ivy-read "Projects: "
            (ivy-gitlab--list-projects)
            :action #'ivy-gitlab--list-projects-action-default
            :caller 'ivy-gitlab-list-projects))



;;;###autoload
(defun ivy-gitlab-list-project-issues ()
  "List Gitlab project issues using Ivy."
  (interactive)
  (ivy-read "Projects: "
            (ivy-gitlab--list-projects)
            :action #'ivy-gitlab--list-project-issues-action-default
            :caller 'ivy-gitlab-list-project-issues))

;;;###autoload
(defun ivy-gitlab-list-all-issues ()
  "List Gitlab all issues using Ivy."
  (interactive)
  (ivy-read "Issues: "
            (ivy-gitlab--list-all-issues)
            :action #'ivy-gitlab--list-issues-action-default
            :caller 'ivy-gitlab--list-all-issues-action-default))


(provide 'ivy-gitlab)
;;; ivy-gitlab.el ends here
