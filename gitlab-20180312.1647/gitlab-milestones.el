;;; gitlab-milestones.el --- Gitlab Milestones API

;; Copyright (C) 2015, 2016 Marcin Antczak <marcin.antczak@neutrico.eu>

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
;; https://github.com/gitlabhq/gitlabhq/blob/master/doc/api/milestones.md

;;; Code:

(require 's)

(require 'gitlab-http)

(defun gitlab-list-project-milestones (project-id)
  "Get a list of project milestones.

PROJECT-ID : The ID of a project"
  (perform-gitlab-request "GET"
                          (s-concat "projects/"
                                    (url-hexify-string
                                     (format "%s" project-id))
                                    "/milestones"
                                    )
                          nil
                          200))

(defun gitlab--get-milestone-uri (project-id milestone-id)
  "Create milestone uri for PROJECT-ID identified by MILESTONE-ID."
  (s-concat "projects/"
            (url-hexify-string
             (format "%s" project-id))
            "/milestones/"
            (number-to-string milestone-id)))


(defun gitlab-get-milestone (project-id milestone-id)
  "Gets a single project milestone.

PROJECT-ID : The ID of a project
MILESTONE-ID : The ID of a project milestone"
  (perform-gitlab-request  "GET"
                           (gitlab--get-milestone-uri
                            (url-hexify-string
                             (format "%s" project-id))
                            milestone-id)
                           nil
                           200))

(defun gitlab-get-milestone-issues (project-id milestone-id)
  "Gets a single project milestone.

PROJECT-ID : The ID of a project
MILESTONE-ID : The ID of a project milestone"
  (perform-gitlab-request  "GET"
                           (s-concat
                            (gitlab--get-milestone-uri
                             (url-hexify-string
                              (format "%s" project-id))
                             milestone-id)
                            "/issues")
                           nil
                           200)
  ;; (lwarn '(gitlab) :debug "URL: %s "(s-concat
  ;;                                    (gitlab--get-milestone-uri
  ;;                                     (url-hexify-string
  ;;                                      (format "%s" project-id))
  ;;                                     milestone-id)
  ;;                                    "/issues"))
  )

(defun gitlab-create-milestone (project-id milestone-title milestone-deadline milestone-description)
  "Create a project milestone.

PROJECT-ID: The ID or NAMESPACE%2FPROJECT_NAME of a project
MILESTONE-TITLE: Title of milestone"
  (perform-gitlab-request "POST"

                          (format "projects/%s/milestones"
                                   (url-hexify-string
                                    (format "%s" project-id)))

                          (format "title=%s&due_date=%s&description=%s"
                                  milestone-title
                                  milestone-deadline
                                  milestone-description)
                           201))

(defun gitlab-edit-milestone (project-id milestone-id &optional title description due-date state-event)
  "Edit milestone.

PROJECT-ID: the ID or NAMESPACE%2FPROJECT_NAME of a project
MILESTONE-ID: the ID office milestone
TITLE: title of milestone
DESCRIPTION: description of milestone
DUE-DATE: deadline
STATE-EVENT: activate or close milestone"
  (perform-gitlab-request "PUT"

                          (format "projects/%s/milestones/%s"
                                   (url-hexify-string
                                    (format "%s" project-id))
                                   milestone-id)

                          (format "%s"
                                  (concat
                                   (when title
                                     (format "&title=%s" title))
                                   (when description
                                     (format "&description=%s" description))
                                   (when due-date
                                     (format "&due_date=%s" due-date))
                                   (when state-event
                                     (format "&state_event=%s" state-event))))
                           200))


(provide 'gitlab-milestones)
;;; gitlab-milestones.el ends here
