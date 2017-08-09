;;; gitlab-http.el --- HTTP tools

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

(require 'json)

(require 'request)
(require 's)

(require 'gitlab-api)



(defun gitlab--get-rest-uri (uri)
  "Retrieve the Gitlab API complete url using the API version.
`URI` is the api path."
  (if (gitlab--get-host)
      (s-concat (gitlab--get-host) "/api/" gitlab-api-version "/" uri)
    (error (signal 'gitlab-error '("Gitlab host unknown.")))))


(defun gitlab--get-headers ()
  "Return the HTTP headers for Gitlab API."
  ;;(if (not (s-blank? gitlab-token-id))
  (list (cons "PRIVATE-TOKEN" gitlab-token-id)
        (cons "connection" "close")))
    ;;nil))


(defun gitlab--perform-get-request (uri params)
  "Doc string URI PARAMS."
  (let* ((response (request (gitlab--get-rest-uri uri)
                            :type "GET"
                            :headers (gitlab--get-headers)
                            :sync t
                            :params params
                            ;;:data params
                            :parser 'json-read)))
    response))

(defun gitlab--perform-post-request (uri params)
  "Doc string URI PARAMS."
  (let ((response (request (gitlab--get-rest-uri uri)
                           :type "POST"
                           :headers (gitlab--get-headers)
                           :sync t
                           :data params
                           :parser 'json-read)))
    response))

(defun gitlab--perform-put-request (uri params)
  "Doc string URI PARAMS."
  (let ((response (request (gitlab--get-rest-uri uri)
                           :type "PUT"
                           :headers (gitlab--get-headers)
                           :sync t
                           :data params
                           :parser 'json-read)))
    response))


(defun perform-gitlab-request (type uri params status-code)
  "Doc string TYPE URI PARAMS STATUS-CODE."
  (let ((response
         (cond ((string= type "POST")
		(gitlab--perform-post-request uri params))
	       ((string= type "GET")
		(gitlab--perform-get-request uri params))
	       ((string= type "PUT")
		(gitlab--perform-put-request uri params)))))
    (if (= status-code (request-response-status-code response))
        (request-response-data response)
      (lwarn '(gitlab)
             :error "HTTP %s Error %s on URI: %s"
             type
             (request-response-status-code response)
             uri))))


;; (defmacro with-gitlab-request (uri params status-code response-data &rest body)
;;   "Execute the forms in BODY using URI and PARAMS for HTTP request.
;; If HTTP response code isn't STATUS-CODE, raise a `gitlab-http-error'.
;; Otherwise set RESPONSE-DATA to HTTP response content."
;;   `(let ((response
;;          (gitlab--perform-get-request ,uri ,params)))
;;     (if (= ,status-code (request-response-status-code response))
;;         (let ((,response-data (request-response-data response)))
;;           ,@body)
;;       (error
;;        (signal 'gitlab-http-error
;;                (list (request-response-status-code response)
;;                      (request-response-data response)))))))




(provide 'gitlab-http)
;;; gitlab-http.el ends here
