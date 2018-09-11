;;; gitlab-users.el --- Gitlab Users API

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
;; https://github.com/gitlabhq/gitlabhq/blob/master/doc/api/users.md

;;; Code:

(require 's)

(require 'gitlab-http)



(defun gitlab-list-users ()
  "Get a list of users."
  (perform-gitlab-request "GET" "users" nil 200))


(defun gitlab-get-user (user-id)
  "Gets a single user data.

`USER-ID' : The ID of a project user."
  (perform-gitlab-request  "GET" (format "%s" user-id) nil 200))


(provide 'gitlab-users)
;;; gitlab-users.el ends here
