;;; github-clone.el --- Fork and clone github repos  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016  Charles L.G. Comstock

;; Author: Charles L.G. Comstock <dgtized@gmail.com>
;; Created: 2 Aug 2014
;; Version: 0.2
;; Package-Version: 20160114.653
;; URL: https://github.com/dgtized/github-clone.el
;; Keywords: vc, tools
;; Package-Requires: ((gh "0.7.2") (magit "2.1.0") (emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `github-clone' will automatically clone a repo to a specified directory, and
;; then optionally fork the repo and add a remote named after the github user to
;; the fork.

;;; Change Log:

;;  0.2 2014-10-06 Switch to hub style cloning; always clone as origin, and
;;                 optionally add a remote to user's fork named after their
;;                 username. Removes support for 'upstream' style.

;;; Todo:

;; Use `github-clone-remotes' to add named remotes easily for repositories in
;; the same network.

;;; Code:

(require 'eieio)
(require 'gh-users)
(require 'gh-repos)
(require 'magit)

(defcustom github-clone-url-slot :ssh-url
  "Which slot to use as the URL to clone."
  :type '(radio (const :tag "SSH" :ssh-url)
                (const :tag "HTTPS" :clone-url)
                (const :tag "Git" :git-url))
  :group 'github-clone)

(defun github-clone-fork (repo)
  (oref (gh-repos-fork (gh-repos-api "api") repo) :data))

(defun github-clone-info (user repo-id)
  (oref (gh-repos-repo-get (gh-repos-api "api") repo-id user) :data))

(defun github-clone-remotes (user repo-id)
  (let* ((repo (github-clone-info user repo-id))
         (forks (oref (gh-repos-forks-list (gh-repos-api "api") repo) :data)))
    (cl-loop for fork in forks
             collect (cons (oref (oref fork :owner) :login)
                           (eieio-oref fork github-clone-url-slot)))))

(defun github-clone-repo (repo directory)
  (let* ((name (oref repo :name))
         (target (if (file-exists-p directory)
                     (expand-file-name name directory)
                   directory))
         (repo-url (eieio-oref repo github-clone-url-slot)))
    (message "Cloning %s into %s from \"%s\"" name target repo-url)
    (if (not (= 0 (shell-command (format "git clone %s %s" repo-url target)
                                 "*github-clone output*")))
        (error "Failed to clone repo \"%s\" to directory \"%s\"" repo-url target))
    (magit-status-internal target)
    (when (and (not (string-equal (oref (oref repo :owner) :login)
                                  (github-clone-user-name)))
               (yes-or-no-p "Fork repo and add remote? "))
      (github-clone-fork-repo repo))))

(defun github-clone-fork-repo (repo)
  (let* ((fork (github-clone-fork repo))
         (remote (github-clone-user-name))
         (fork-url (eieio-oref fork github-clone-url-slot)))
    (if fork-url
        (progn (message "Adding remote %s -> %s" remote fork-url)
               (magit-remote-add remote fork-url))
      (error "Unable to fork %s" (eieio-oref repo github-clone-url-slot)))))

(defun github-clone-repo-name (url)
  (let ((url1 (replace-regexp-in-string "/$" "" url)))
    (cond ((string-match "\\.git$" url1)
           (github-clone-repo-name (replace-match "" nil nil url1)))
          ((string-match "\\([[:alnum:]\-_.]+\\)/\\([[:alnum:]\-_.]+\\)$" url1)
           (cons (match-string 1 url) (match-string 2 url1)))
          ((string-match "^\\([[:alnum:]\-_.]+\\)$" url1)
           (cons (github-clone-user-name) (match-string 1 url1)))
          (t (error "Cannot parse repo name %s" url1)))))

(defvar github-clone--user nil "Cache for current github login.")
(defun github-clone-user-name ()
  (unless github-clone--user
    (setq github-clone--user
          (oref (oref (gh-users-get (gh-users-api "api")) :data) :login)))
  github-clone--user)

;;;###autoload
(defun github-clone (user-repo-url directory)
  "Fork and clone USER-REPO-URL into DIRECTORY.

USER-REPO-URL can be any of the forms:

  repository
  user/repository
  organization/repository
  https://github.com/user/repository
  git@github.com:user/repository.git
  https://github.com/user/repository.el.git

It will immediately clone the repository (as the origin) to
DIRECTORY. Then it prompts to fork the repository and add a
remote named after the github username to the fork."
  (interactive
   (list (read-from-minibuffer "Url or User/Repo: ")
         (read-directory-name "Directory: " nil default-directory t)))
  (let* ((name (github-clone-repo-name user-repo-url))
         (repo (github-clone-info (car name) (cdr name))))
    (if (eieio-oref repo github-clone-url-slot)
        (github-clone-repo repo directory)
      (error "Repository %s does not exist" user-repo-url))))

;;;###autoload
(defun eshell/github-clone (user-repo-url &optional directory)
  "Eshell alias uses current directory as default."
  (funcall 'github-clone user-repo-url (or directory default-directory)))

(provide 'github-clone)
;;; github-clone.el ends here
