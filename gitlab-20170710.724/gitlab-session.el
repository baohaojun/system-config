;;; gitlab-session.el --- Session API

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
;; https://github.com/gitlabhq/gitlabhq/blob/master/doc/api/session.md

;;; Code:

(require 'gitlab-http)
(require 'gitlab-utils)



;;;###autoload
(defun gitlab-login ()
  "Open a session.
If it works, return the private token to perform HTTP request to Gitlab."
  (interactive)
  (let ((response
         (gitlab--perform-post-request
          "session"
          (list (cons "login" (gitlab--get-username))
                (cons "password" (gitlab--get-password))))))
    (if (= 201 (request-response-status-code response))
        (progn
          (message "Set Gitlab TokenID")
          (let ((id (assoc-default 'private_token
                                   (request-response-data response))))
            (setq gitlab-token-id id)))
      (error
       (signal 'gitlab-http-error
               (list (request-response-status-code response)
                     (request-response-data response)))))))

(defmacro with-gitlab-auth (&rest body)
  "Macro which check authentication token.
If not, perform a request to Gitlab to login.
Then executes `BODY'."
  `(progn
     (when (s-blank? gitlab-token-id)
       (gitlab-login))
     ,@body))


(provide 'gitlab-session)
;;; gitlab-session.el ends here
