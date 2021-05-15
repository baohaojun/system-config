;;; daemons-systemd.el --- UI for managing init system daemons (services) -*- lexical-binding: t -*-

;; Copyright (c) 2018 Chris Bowdon
;;
;; Author: Chris Bowdon
;; URL: https://github.com/cbowdon/daemons.el
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: February 13, 2018
;; Modified: December 15, 2018
;; Version: 2.1.0
;; Keywords: unix convenience
;; Package-Requires: ((emacs "25.1"))
;;
;;; Commentary:
;; This file provides systemd support for daemons.el.

;;; Code:
(require 'seq)
(require 'daemons)

(defcustom daemons-systemd-is-user nil
  "Determines if we want to run commands as `--user'"
  :type 'boolean
  :group 'daemons)

(defun daemons-systemd--cmd ()
  "Appends `--user' to the `systemctl' call if `daemons-systemd-is-user' is set"
  (if daemons-systemd-is-user
      "systemctl --user"
    "systemctl"))

(daemons-define-submodule daemons-systemd
  "Daemons submodule for systemd."

  :test (and (eq system-type 'gnu/linux)
             (equal 0 (daemons--shell-command "which systemctl")))
  :commands
  '((status . (lambda (name) (format "%s status %s" (daemons-systemd--cmd) name)))
    (start . (lambda (name) (format "%s start %s" (daemons-systemd--cmd) name)))
    (stop . (lambda (name) (format "%s stop %s" (daemons-systemd--cmd) name)))
    (restart . (lambda (name) (format "%s restart %s" (daemons-systemd--cmd) name)))
    (reload . (lambda (name) (format "%s reload %s" (daemons-systemd--cmd) name)))
    (enable . (lambda (name) (format "%s enable %s" (daemons-systemd--cmd) name)))
    (disable . (lambda (name) (format "%s disable %s" (daemons-systemd--cmd) name))))

  :list (daemons-systemd--list)

  :headers [("Daemon (service)" 60 t) ("Enabled" 40 t)])

(defun daemons-systemd--parse-list-item (raw-systemctl-output)
  "Parse a single line from RAW-SYSTEMCTL-OUTPUT into a tabulated list item."
  (let* ((parts (split-string raw-systemctl-output))
         (name (replace-regexp-in-string "\.service" "" (car parts)))
         (enabled (cadr parts)))
    (list name (vector name enabled))))

(defun daemons-systemd--item-is-simple-service-p (item)
  "Non-nil if ITEM (output of `daemons-systemd--parse-list-item') is not a template service."
  (not (string-match-p "@$" (car item))))

(defun daemons-systemd--list ()
  "Return a list of daemons on a systemd system."
  (thread-last  (format "%s list-unit-files --type=service --no-legend" (daemons-systemd--cmd))
    (daemons--shell-command-to-string)
    (daemons--split-lines)
    (seq-map 'daemons-systemd--parse-list-item)
    (seq-filter 'daemons-systemd--item-is-simple-service-p)))

(defun daemons-systemd-toggle-user ()
  "Toggle showing of user services"
  (interactive)
  (setq daemons-systemd-is-user (not daemons-systemd-is-user))
  (revert-buffer))

(provide 'daemons-systemd)
;;; daemons-systemd.el ends here
