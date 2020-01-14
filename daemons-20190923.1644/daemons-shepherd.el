;;; daemons-shepherd.el --- UI for managing init system daemons (services) -*- lexical-binding: t -*-

;; Copyright (c) 2018 Jelle Licht <jlicht@fsfe.org>
;;
;; Author: Jelle Licht
;; URL: https://github.com/cbowdon/daemons.el
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: March 07, 2018
;; Modified: March 07, 2018
;; Version: 2.0.0
;; Keywords: unix convenience
;; Package-Requires: ((emacs "25.1"))
;;
;;; Commentary:
;; This file provides GNU Shepherd support for daemons.el.

;;; Code:
(require 'seq)
(require 'daemons)

(daemons-define-submodule daemons-shepherd
  "Daemons submodule for GNU Shepherd."

  :test (and (eq system-type 'gnu/linux)
             (equal 0 (daemons--shell-command "which herd")))
  :commands
  '((status . (lambda (name) (format "herd status %s" name)))
    (start . (lambda (name) (format "herd start %s" name)))
    (stop . (lambda (name) (format "herd stop %s" name)))
    (restart . (lambda (name) (format "herd restart %s" name))))

  :list (daemons-shepherd--list)

  :headers [("Daemon (service)" 60 t) ("Active" 40 t)])

(defun daemons-shepherd--parse-list-item (raw-shepherd-output)
  "Parse a single line from RAW-SHEPHERD-OUTPUT into a tabulated list item."
  (let* ((parts (split-string raw-shepherd-output))
         (name (cadr parts))
         (running (car parts)))
    (list name (vector
                name
                (if (string-match-p "^\+" running)
                    "started"
                  "stopped")))))

(defun daemons-shepherd--item-is-service-p (item)
  "Non-nil if ITEM (output-line of `herd status root') describes a service."
  (string-match-p "^ [\+\-] " item))

(defun daemons-shepherd--list ()
  "Return a list of daemons on a shepherd system."
  (thread-last  "herd status"
    (daemons--shell-command-to-string)
    (daemons--split-lines)
    (seq-filter 'daemons-shepherd--item-is-service-p)
    (seq-map 'daemons-shepherd--parse-list-item)))

(provide 'daemons-shepherd)
;;; daemons-shepherd.el ends here
