;;; gitlab-groups.el --- Groups API

;; Copyright (C) 2014, 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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
;; https://github.com/gitlabhq/gitlabhq/blob/master/doc/api/groups.md


;;; Code:

(require 's)

(require 'gitlab-http)


(defvar gitlab-group-access-levels
  (list 10 'guest
        20 'reporter
        30 'developer
        40 'master
        50 'owner))


(defun gitlab-list-groups ()
  "Get a list of groups.
As user : my groups, as admin : all groups."
  (perform-gitlab-request "GET" "groups" nil 200))


(defun gitlab-get-group (group-id)
  "Get all details of a group.
GROUP-ID is the ID of a group."
  (perform-gitlab-request "GET"
                          (s-concat "groups/"
                                    (number-to-string group-id))
                          nil
                          200))

(defun gitlab-list-group-members (group-id)
  "Get a list of group members viewable by the authenticated user.
GROUP-ID is the ID of a group."
  (perform-gitlab-request "GET"
                          (s-concat "groups/"
                                    (number-to-string group-id)
                                    "/members")
                          nil
                          200))

(provide 'gitlab-groups)
;;; gitlab-groups.el ends here
