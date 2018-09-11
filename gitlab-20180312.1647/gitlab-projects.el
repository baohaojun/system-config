;;; gitlab-projects.el --- Projects API

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

;; See API doc :
;; https://github.com/gitlabhq/gitlabhq/blob/master/doc/api/projects.md


;;; Code:

(require 'dash)
(require 's)

(require 'gitlab-http)


(defun gitlab-list-projects (&optional page per-page)
  "Get a list of projects accessible by the authenticated user.
PAGE: current page number
PER-PAGE: number of items on page max 100"
  (let* ((params '()))
    (when page
      (add-to-list 'params (cons 'per_page (number-to-string per-page))))
    (when per-page
      (add-to-list 'params (cons 'page (number-to-string page))))
    (perform-gitlab-request "GET"
                            "projects"
                            params
                            200)))

(defun gitlab-list-all-projects ()
  "Get a list of all projects accessible by the authenticated user."
  (interactive)
    (let* ((page 1)
           (per-page 100)
           (projects)
           (all-projects (gitlab-list-projects page per-page))
           (all-projects-count (length all-projects)))
      (while (>= all-projects-count (* page per-page))
        (setq projects (gitlab-list-projects page per-page))
        (setq all-projects (vconcat all-projects projects))
        (setq all-projects-count (length all-projects))
        (setq page (1+ page)))
      all-projects))


(defun gitlab-list-owned-projects ()
  "Get a list of projects which are owned by the authenticated user."
  (perform-gitlab-request "GET" "projects" '((owned . "true")) 200))


(defun gitlab-get-project (project-id)
  "Get a specific project, identified by PROJECT-ID."
  (perform-gitlab-request "GET"
                          (format "projects/%s"
                                  (url-hexify-string
                                   (format "%s"project-id)))
                          nil
                          200))

(defun gitlab-search-projects (name)
  "Search for projects by name which are accessible to the authenticated user.
NAME is a string contained in the project name."
  (perform-gitlab-request "GET"
                          (s-concat "projects/search/" name)
                          nil
                          200))


(defun gitlab-list-project-members (project-id)
  "Get a list of a project's team members.
PROJECT-ID is The ID or NAMESPACE/PROJECT_NAME of a project."
  (perform-gitlab-request "GET"
                          (format "projects/%s/members"
                                  (url-hexify-string
                                   (format "%s" project-id)))
                          nil
                          200))

(defun gitlab-add-project (project-name)
  "Add a specific project, identified by PROJECT-NAME."
  (perform-gitlab-request "POST"
                          (format "projects/%s"
                                  (url-hexify-string
                                   (format "%s" project-name)))
                          nil
                          201))

(defun gitlab-list-project-events (project-id)
  "Get the events for the specified project, identified by PROJECT-ID.
Sorted from newest to latest."
  (perform-gitlab-request "GET"
                          (format "projects/%s/events"
                                  (url-hexify-string
                                   (format "%s" project-id)))
                          nil
                          200))

(defun gitlab-projects--get-issue-link (project-id issue-id)
  "Create the URL to show a project's issue.
`PROJECT-ID' is the project ID
`ISSUE-ID' is the issue ID."
  (-when-let (project (gitlab-get-project project-id))
    (s-concat (gitlab--get-host)
              "/"
              (assoc-default 'path_with_namespace project)
              "/issues/"
              (number-to-string issue-id))))

(defun gitlab-list-project-labels (project-id)
  "Get the labels for the project identified by PROJECT-ID."
  (perform-gitlab-request "GET"
                          (format "projects/%s/labels"
                                  (url-hexify-string
                                   (format "%s" project-id)))
                          nil
                          200))

(defun gitlab-list-project-labels-names (project-id)
  "Get a list of the labels' names."
  (let ((labels (gitlab-list-project-labels project-id)))
    (--map (assoc-default 'name it) labels)))


(provide 'gitlab-projects)
;;; gitlab-projects.el ends here
