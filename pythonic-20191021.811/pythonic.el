;;; pythonic.el --- Utility functions for writing pythonic emacs package.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2019 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/pythonic
;; Package-Version: 20191021.811
;; Version: 0.1.1
;; Package-Requires: ((emacs "25.1") (s "1.9") (f "0.17.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'python)
(require 'cl-lib)
(require 'tramp)
(require 's)
(require 'f)

(defgroup pythonic nil
  "Utility functions for writing pythonic emacs package."
  :group 'python)


;;; Connection predicates.

(defun pythonic-local-p ()
  "Determine local virtual environment."
  (not (pythonic-remote-p)))

(defun pythonic-remote-p ()
  "Determine remote virtual environment."
  (and (tramp-tramp-file-p (pythonic-aliased-path default-directory))
       t))

(defun pythonic-remote-docker-p ()
  "Determine docker remote virtual environment."
  (and (pythonic-remote-p)
       (s-equals-p (pythonic-remote-method) "docker")))

(defun pythonic-remote-ssh-p ()
  "Determine ssh remote virtual environment."
  (and (pythonic-remote-p)
       (s-equals-p (pythonic-remote-method) "ssh")))

(defun pythonic-remote-vagrant-p ()
  "Determine vagrant remote virtual environment."
  (and (pythonic-remote-p)
       (s-equals-p (pythonic-remote-host) "localhost")
       (s-equals-p (pythonic-remote-user) "vagrant")))


;;; Connection properties.

(defun pythonic-remote-method ()
  "Get tramp method of the connection to the remote python interpreter."
  (tramp-file-name-method (tramp-dissect-file-name (pythonic-aliased-path default-directory))))

(defun pythonic-remote-user ()
  "Get user of the connection to the remote python interpreter."
  (tramp-file-name-user (tramp-dissect-file-name (pythonic-aliased-path default-directory))))

(defun pythonic-remote-host ()
  "Get host of the connection to the remote python interpreter."
  (let ((hostname (tramp-file-name-host (tramp-dissect-file-name (pythonic-aliased-path default-directory)))))
    (replace-regexp-in-string "#.*\\'" "" hostname)))

(defun pythonic-remote-port ()
  "Get port of the connection to the remote python interpreter."
  (let ((port (tramp-file-name-port (tramp-dissect-file-name (pythonic-aliased-path default-directory)))))
    ;; In Emacs 25, `tramp-file-name-port' returns number,
    ;; in Emacs 26, it returns string. This condition makes them compatible.
    (if (stringp port)
        (string-to-number port)
      port)))


;;; File names.

(defvar pythonic-directory-aliases nil)

(defun pythonic-aliased-path (path)
  "Get aliased PATH."
  (let ((alias-tuple (cl-find-if
                      (lambda (it)
                        (or (f-same-p (car it) path)
                            (f-ancestor-of-p (car it) path)))
                      pythonic-directory-aliases)))
    (if (null alias-tuple)
        path
      (f-join (cadr alias-tuple)
              (f-relative path (car alias-tuple))))))

(defun pythonic-unaliased-path (alias)
  "Get real path from ALIAS."
  (let ((alias-tuple (cl-find-if
                      (lambda (it)
                        (or (f-same-p (cadr it) alias)
                            (f-ancestor-of-p (cadr it) alias)))
                      pythonic-directory-aliases)))
    (if (null alias-tuple)
        alias
      (f-join (car alias-tuple)
              (f-relative alias (cadr alias-tuple))))))

(defun pythonic-has-alias-p (path)
  "Check if given PATH has alias."
  (not (null (cl-find-if
              (lambda (it)
                (or (f-same-p (car it) path)
                    (f-ancestor-of-p (car it) path)))
              pythonic-directory-aliases))))

(defun pythonic-python-readable-file-name (filename)
  "Emacs to Python FILENAME conversion.
Take FILENAME from the perspective of the localhost and translate
it to the FILENAME Python process can read.  Python can be
running locally or remotely.  FILENAME can have local or tramp
format.  Result will have local format."
  (let ((alias (pythonic-aliased-path filename)))
    (if (tramp-tramp-file-p alias)
        (tramp-file-name-localname (tramp-dissect-file-name alias))
      alias)))

(defun pythonic-emacs-readable-file-name (filename)
  "Python to Emacs FILENAME conversion.
Take FILENAME from the perspective of the python interpreter and
translate it to the FILENAME Emacs `find-file' command can
understand.  Python can be running locally or remotely.  FILENAME
should have local format.  Result can have local or tramp
format."
  (when (tramp-tramp-file-p filename)
    (error "%s can not be tramp path" filename))
  (if (pythonic-remote-p)
      (let* ((directory (pythonic-aliased-path default-directory))
             (connection (substring directory 0
                                    (- (length directory)
                                       (length (tramp-file-name-localname (tramp-dissect-file-name directory)))))))
        (pythonic-unaliased-path (concat connection filename)))
    filename))


;;; Docker Compose.

(defcustom pythonic-docker-compose-filename "docker-compose.yml"
  "File name of the docker-compose project file."
  :type 'string
  :safe 'stringp)

(defcustom pythonic-docker-compose-service-name nil
  "Name of the default service to execute commands."
  :type 'string
  :safe 'stringp)

(defvar pythonic-read-docker-compose-file-code "
from __future__ import print_function
import json, sys, yaml
print(json.dumps(yaml.safe_load(open(sys.argv[-1], 'r'))))
")

(defun pythonic-get-docker-compose-project ()
  "Get directory where `pythonic-docker-compose-filename' is present."
  (let ((project (locate-dominating-file default-directory pythonic-docker-compose-filename)))
    (when project
      (f-full project))))

(defun pythonic-get-docker-compose-filename (project)
  "Get full path to the docker-compose PROJECT configuration file."
  (f-join project pythonic-docker-compose-filename))

(defun pythonic-read-docker-compose-file (filename)
  "Read docker-compose project configuration FILENAME."
  (let ((json-key-type 'string)
        (json-array-type 'list))
    (json-read-from-string
     (with-output-to-string
       (with-current-buffer
           standard-output
         (call-process "python" nil t nil "-c" pythonic-read-docker-compose-file-code filename))))))

(defun pythonic-get-docker-compose-volumes (struct)
  "Get docker volume list from the compose STRUCT."
  (let (volumes)
    (dolist (service (cdr (assoc "services" struct)))
      (dolist (volume (cdr (assoc "volumes" service)))
        (when (s-starts-with-p "." volume)
          (push (cons (car service) (s-split ":" volume)) volumes))))
    volumes))

(defun pythonic-get-docker-compose-container (filename service)
  "Get container name from the FILENAME project for SERVICE name."
  (s-trim
   ;; FIXME:
   ;;
   ;; It is possible to have many running containers for given
   ;; service.
   ;;
   ;; Use container name, not the hash.  This way we can survive
   ;; service recreation.
   (with-output-to-string
     (with-current-buffer
         standard-output
       (call-process "docker-compose" nil t nil
                     "--file" filename "ps" "--quiet" service)))))

(defun pythonic-set-docker-compose-alias ()
  "Build alias string for current docker-compose project."
  (hack-dir-local-variables-non-file-buffer)
  (unless
      (or (tramp-tramp-file-p default-directory)
          (pythonic-has-alias-p default-directory))
    (let ((project (pythonic-get-docker-compose-project)))
      (when project
        (let* ((filename (pythonic-get-docker-compose-filename project))
               (struct (pythonic-read-docker-compose-file filename))
               (volumes (pythonic-get-docker-compose-volumes struct))
               ;; FIXME: Each service can have many volumes.  It
               ;; should appears once in the selection and all volumes
               ;; should be added to the alias list.
               (volume (if (< 1 (length volumes))
                             (assoc
                              (if pythonic-docker-compose-service-name
                                  pythonic-docker-compose-service-name
                                (completing-read "Service: " (mapcar #'car volumes) nil t))
                              volumes)
                         (car volumes)))
               (service (car volume))
               (sub-project (f-join project (cadr volume)))
               (mount (caddr volume))
               (container (pythonic-get-docker-compose-container filename service))
               ;; FIXME: Get actual user for the connection string.
               (connection (format "/docker:root@%s:%s" container mount))
               (alias (list sub-project connection)))
          (unless (s-blank-p container)
            (push alias pythonic-directory-aliases))
          alias)))))


;;; Processes.

(defvar pythonic-interpreter python-shell-interpreter
  "Interpreter to use for pythonic process calls.")

(cl-defun pythonic-call-process (&key file buffer display args cwd)
  "Pythonic wrapper around `call-process'.

FILE is the input file. BUFFER is the output destination. DISPLAY
specifies to redisplay BUFFER on new output. ARGS is the list of

arguments passed to `call-process'. CWD will be working directory
for running process."
  (let ((default-directory (pythonic-aliased-path (or cwd default-directory))))
    (python-shell-with-environment
      (apply #'process-file pythonic-interpreter file buffer display args))))

(cl-defun pythonic-start-process (&key process buffer args cwd filter sentinel (query-on-exit t))
  "Pythonic wrapper around `start-process'.

PROCESS is a name of the created process. BUFFER is a output
destination. ARGS are the list of args passed to
`start-process'. CWD will be working directory for running
process.  FILTER must be a symbol of process filter function if
necessary.  SENTINEL must be a symbol of process sentinel
function if necessary.  QUERY-ON-EXIT will be corresponding
process flag."
  (let ((default-directory (pythonic-aliased-path (or cwd default-directory))))
    (python-shell-with-environment
      (let ((process (apply #'start-file-process process buffer pythonic-interpreter args)))
        (when filter
          (set-process-filter process filter))
        (when sentinel
          (set-process-sentinel process sentinel))
        (set-process-query-on-exit-flag process query-on-exit)
        process))))


;;; Commands.

;;;###autoload
(defun pythonic-activate (virtualenv)
  "Activate python VIRTUALENV."
  (interactive "DEnv: ")
  (setq python-shell-virtualenv-root (pythonic-python-readable-file-name virtualenv)))

;;;###autoload
(defun pythonic-deactivate ()
  "Deactivate python virtual environment."
  (interactive)
  (setq python-shell-virtualenv-root nil))

(provide 'pythonic)

;;; pythonic.el ends here
