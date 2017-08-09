;;; gitlab-api.el --- Gitlab API settings.

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

;;; Code:

(defvar gitlab-host nil
  "The Gitlab API endpoint.")

(defvar gitlab-username nil
  "The Gitlab username.")

(defvar gitlab-password nil
  "The Gitlab account's password.")

(defvar gitlab-token-id nil
  "The Gitlab tokenID to perform HTTP requests.")

(defconst gitlab-api-version "v3"
  "The Gitlab API version.")

(defun gitlab--get-host ()
  "Retrieve the Gitlab host.
Use `gitlab-host' or GITLAB_HOST environment variable"
  (if gitlab-host
      gitlab-host
    (getenv "GITLAB_HOST")))

(defun gitlab--get-username ()
  "Retrieve the gitlab username.
Use `gitlab-username' or GITLAB_USERNAME environment variable"
  (if gitlab-username
      gitlab-username
    (getenv "GITLAB_USERNAME")))

(defun gitlab--get-password ()
  "Retrieve the Gitlab password.
Use `gitlab-password' or GITLAB_PASSWORD environment variable"
  (if gitlab-password
      gitlab-password
    (getenv "GITLAB_PASSWORD")))

(defun gitlab--get-token-id ()
  "Retrieve the Gitlab token ID.
Use `gitlab-token-id' or GITLAB_TOKEN_ID environment variable"
  (if gitlab-token-id
      gitlab-token-id
    (getenv "GITLAB_TOKEN_ID")))

(provide 'gitlab-api)
;;; gitlab-api.el ends here
