;;; daemons-openrc.el --- UI for managing init system daemons (services) -*- lexical-binding: t -*-

;; Copyright (c) 2019 Mitescu George Dan
;;
;; Author: Mitescu George dan
;; URL: https://github.com/cbowdon/daemons.el
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: September 20, 2019
;; Modified: September 20, 2019
;; Version: 2.0.0
;; Keywords: unix convenience
;; Package-Requires: ((emacs "25.1"))
;;
;;; Commentary:
;; This file provides OpenRC support for daemons.el.

;;; Code:
(require 'seq)
(require 'daemons)

(daemons-define-submodule daemons-openrc
  "Daemons submodule for OpenRC."

  :test (and (eq system-type 'gnu/linux)
             (equal 0 (daemons--shell-command "which rc-service")))
  :commands
  '((status . (lambda (name) (format "rc-service %s status" name)))
    (start . (lambda (name) (format "rc-service %s start" name)))
    (stop . (lambda (name) (format "rc-service %s stop" name)))
    (restart . (lambda (name) (format "rc-service %s restart" name))))

  :list (daemons-openrc--list)

  :headers (daemons-openrc--list-headers))

(defun daemons-openrc--parse-list-item (raw-rc-output)
  "Parse a single line from rc-update into a tabulated list item."
  (let* ((runlevels '("boot" "default" "nonetwork" "shutdown" "sysinit"))
	 (parts (split-string raw-rc-output "|" t))
         (name (s-trim (car parts)))
	 (unparsed-levels (cadr parts))
         (run-level-statuses
	  (seq-map (lambda (level)
		     (if (s-contains? level unparsed-levels)
			 level ""))
		   runlevels)))
    (list name (apply 'vector (cons name run-level-statuses)))))

(defun daemons-openrc--list ()
  "Return a list of daemons on a OpenRC system."
  (thread-last "rc-update -v show"
    (daemons--shell-command-to-string)
    (daemons--split-lines)
    (seq-map 'daemons-openrc--parse-list-item)))

(defun daemons-openrc--list-headers ()
  "Return the list of headers for a OpenRC ‘daemons-mode’ buffer."
  (apply 'vector
         (cons '("Daemon (service)" 30 t)
               (seq-map
                (lambda (x)
                  (list (number-to-string x) 10 t))
                (number-sequence 0 4)))))

(provide 'daemons-openrc)
;;; daemons-openrc.el ends here
