;;; gitlab-notes.el --- Gitlab Notes API

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
;; https://github.com/gitlabhq/gitlabhq/blob/master/doc/api/notes.md

;;; Code:

(require 's)

(require 'gitlab-http)


(defun gitlab--get-notes-uri (project-id issue-id)
  "Compose notes URI for PROJECT-ID with ISSUE-ID."
  (s-concat "projects/"
            (url-hexify-string
             (format "%s" project-id))
            "/issues/"
            (number-to-string issue-id)
            "/notes"))

(defun gitlab-list-project-issue-notes (project-id issue-id &optional page per-page)
  "Get a list of project issue notes.

PROJECT-ID : The ID of a project
ISSUE-ID : The ID of a project issue
PAGE: current page number
PER-PAGE: number of items on page max 100"
  (let* ((params '()))
    (add-to-list 'params (cons 'per_page (number-to-string per-page)))
    (add-to-list 'params (cons 'page (number-to-string page)))
    (perform-gitlab-request "GET"
                            (gitlab--get-notes-uri
                             project-id
                             issue-id)
                            params
                            200)))

(defun gitlab-list-all-project-issue-notes (project-id issue-id)
  "Get a list of allproject issue notes.

PROJECT-ID : The ID of a project
ISSUE-ID : The ID of a project issue"
  (interactive)
  (let* ((page 1)
         (per-page 100)
         (notes)
         (all-notes (gitlab-list-project-issue-notes project-id issue-id page per-page))
         (all-notes-count (length all-notes)))
    (while (>= all-notes-count (* page per-page))
      (setq notes (gitlab-list-project-issue-notes project-id issue-id page per-page))
      (setq all-notes (vconcat all-notes notes))
      (setq all-notes-count (length all-notes))
      (setq page (1+ page)))
    all-notes))

(defun gitlab-get-issue-note (project-id issue-id note-id)
  "Doc PROJECT-ID ISSUE-ID NOTE-ID.")

(defun gitlab-add-issue-note (project-id issue-id body)
  "Add note for project issue.
PROJECT-ID: The ID of a project
ISSUE-ID: The ID of an issue
BODY: Body part of note"
  (perform-gitlab-request "POST"
                          (format "projects/%s/issues/%s/notes"
                                  (url-hexify-string
                                   (format "%s" project-id))
                                  issue-id)
                          (format "body=%s" body)
                          201))

(defun gitlab-edit-issue-note (project-id issue-id note-id body)
  "Add note for project issue.
PROJECT-ID: The ID of a project
ISSUE-ID: The ID of an issue
NOTE-ID: The ID of a note
BODY: Body part of note"
  (perform-gitlab-request "PUT"
                          (format "projects/%s/issues/%s/notes/%s"
                                  (url-hexify-string
                                   (format "%s" project-id))
                                  issue-id
                                  note-id)
                          (format "body=%s" body)
                          200))


(provide 'gitlab-notes)
;;; gitlab-notes.el ends here
