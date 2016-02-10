;;; marmalade.el --- Elisp interface for the Emacs Lisp package server.

;; Copyright (C) 2010 Google Inc.

;; Author: Nathan Weizenbaum <nweiz@google.com>
;; URL: http://code.google.com/p/marmalade
;; Version: 0.0.4
;; Package-Requires: ((furl "0.0.2"))

;;; Commentary:

;; marmalade.el provides an Emacs Lisp interface to the Marmalade package
;; server. You can already use package.el to download and install packages in
;; Marmalade; this package adds the ability to upload them.

;; To use marmalade.el, you must set `marmalade-server' to the URL of the
;; Marmalade server to which pakages will be uploaded.
;; TODO: This should default to the main server.

;;; License:

;; Copyright (C) 2010 Google Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'furl)
(eval-when-compile (require 'cl))

(defgroup marmalade nil
  "An interface for the Marmalade package server"
  :prefix "marmalade-"
  :group 'applications)

(defcustom marmalade-server nil
  "The URL of the server to which to upload packages."
  :type 'string
  :group 'marmalade)

(defcustom marmalade-username nil
  "The Marmalade account username.
If this is not set, marmalade.el will prompt for username and
password for the first Marmalade request of each session."
  :type 'string
  :group 'marmalade)

(defcustom marmalade-token nil
  "The authentication token for the Marmalade API.
If this is not set, marmalade.el will prompt for username and
password for the first Marmalade request of each session."
  :type 'string
  :group 'marmalade)

(defun marmalade-retrieve (path callback)
  "Make a request to the Marmalade API at PATH.
Like `furl-retrieve', but the result is passed to CALLBACK as a
list of some sort."
  (furl-with-header "Accept" "text/x-script.elisp"
    (let ((furl-error-function 'marmalade-handle-error))
      (lexical-let ((callback callback))
        (furl-retrieve (concat marmalade-server "/v1/" path)
                       (lambda (str)
                         (funcall callback (read str))))))))

(defun marmalade-retrieve-synchronously (path)
  "Make a request to the Marmalade API at PATH.
Like `furl-retrieve-synchronously', but the result is returned as
a list of some sort."
  (furl-with-header "Accept" "text/x-script.elisp"
    (let ((furl-error-function 'marmalade-handle-error))
      (read (furl-retrieve-synchronously
             (concat marmalade-server "/v1/" path))))))

(defun marmalade-handle-error (err info)
  "Handle a Marmalade error by printing the response message."
  (let* ((body (furl--get-response-body))
         (msg (condition-case err (cdr (assoc 'message (read body)))
                (error (format "parsing error %S for %S" err body)))))
    (kill-buffer)
    (error (concat "Marmalade error: " msg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun marmalade-login (&optional callback)
  "Log in to Marmalade and get the username and authentication token.
Prompt interactively for the user's username and password, then
use these to retreive the token.

CALLBACK is called when the login is completed, and passed the
username and the authentication token."
  (interactive)
  (if (and marmalade-username marmalade-token)
      (when callback (funcall callback marmalade-username marmalade-token))
    (let* ((name (read-string "Marmalade username: "))
           (password (read-passwd "Marmalade password: "))
           (url-request-method "POST")
           (furl-request-data `(("name" . ,name) ("password" . ,password))))
      (lexical-let ((callback callback))
        (marmalade-retrieve
         "users/login"
         (lambda (res)
           (kill-buffer)
           (let ((token (cdr (assoc 'token res)))
                 (name (cdr (assoc 'name res))))
             (if (yes-or-no-p "Save Marmalade username and auth token? ")
                 (progn
                   (customize-save-variable 'marmalade-username name)
                   (customize-save-variable 'marmalade-token token))
               (setq marmalade-username name)
               (setq marmalade-token token))
             (when callback (funcall callback name token)))))))))

(defun marmalade-logout ()
  "Log out of Marmalade.

This does not remove a saved token."
  (interactive)
  (setq marmalade-token nil)
  (setq marmalade-username nil))

(defun marmalade-register (name email password &optional callback)
  "Register a user with NAME, EMAIL, and PASSWORD.
The authentication token is passed to CALLBACK."
  (interactive
   (list (read-string "Marmalade username: ")
         (read-string "Email: ")
         (read-passwd "Marmalade password: " 'confirm)))
  (let ((url-request-method "POST")
        (furl-request-data
         `(("name" . ,name) ("email" . ,email) ("password" . ,password))))
    (lexical-let ((callback callback))
      (marmalade-retrieve
       "users"
       (lambda (res)
         (kill-buffer)
         (let ((token (cdr (assoc 'token res)))
               (name (cdr (assoc 'name res))))
           (if (yes-or-no-p "Save Marmalade username and auth token? ")
               (progn
                 (customize-save-variable 'marmalade-username name)
                 (customize-save-variable 'marmalade-token token))
             (setq marmalade-username name)
             (setq marmalade-token token))
           (when callback (funcall callback name token))))))))

(defun marmalade-upload-buffer (buffer &optional callback)
  "Upload the package in BUFFER.
BUFFER should be visiting an Elisp file or a tarball.

CALLBACK is called with the package object once the package is
uploaded."
  (interactive "bBuffer to upload: ")
  (with-current-buffer buffer
    (lexical-let ((callback callback))
      (marmalade-login
       (lambda (name tok)
         (let ((url-request-method "POST")
               (furl-request-data `(("name" . ,name) ("token" . ,tok)))
               (furl-request-files
                `(("package" ,(file-name-nondirectory (buffer-file-name))
                   ,(with-current-buffer
                        (if (and (eq major-mode 'tar-mode) (tar-data-swapped-p))
                            tar-data-buffer
                          (current-buffer))
                      (buffer-string))
                   ,(cond
                     ((eq major-mode 'emacs-lisp-mode) "text/x-script.elisp")
                     ((eq major-mode 'tar-mode) "application/x-tar")
                     (t "application/octet-stream"))))))
           (marmalade-retrieve
            "packages"
            (lambda (res)
              (kill-buffer)
              (when callback (funcall callback (cdr (assoc 'package res))))))))))))


;;; marmalade.el ends here
