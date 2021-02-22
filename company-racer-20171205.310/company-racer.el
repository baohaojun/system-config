;;; company-racer.el --- Company integration for racer -*- lexical-binding: t -*-

;; Copyright (C) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/company-racer
;; Package-Version: 20171205.310
;; Package-Commit: a00381c9d416f375f783fcb6ae8d40669ce1f567
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (company "0.8.0") (deferred "0.3.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; > **NOTE**: [emacs-racer][] already offers completion support for through
;; > `completion-at-point-functions', so installing both packages could be
;; > unnecessary.
;;
;; A company backend for [racer][].
;;
;; Setup:
;;
;; Install and configure [racer][].  And add to your `init.el':
;;
;;     (require 'company-racer)
;;
;;     (with-eval-after-load 'company
;;       (add-to-list 'company-backends 'company-racer))
;;
;; Check https://github.com/company-mode/company-mode for details.
;;
;; Troubleshooting:
;;
;; + [racer][] requires to set the environment variable with
;;   `RUST_SRC_PATH' and needs to be an absolute path:
;;
;;       (unless (getenv "RUST_SRC_PATH")
;;         (setenv "RUST_SRC_PATH" (expand-file-name "~/path/to/rust/src")))

;; TODO:
;;
;; + [ ] Add support for find-definition (maybe not in this package.)
;;
;; [racer]: https://github.com/phildawes/racer
;; [emacs-racer]: https://github.com/racer-rust/emacs-racer
;; [rust-lang]: http://www.rust-lang.org/

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'company)
(require 'thingatpt)
(require 'deferred)
(require 'format-spec)

(defgroup company-racer nil
  "Company integration for rust-mode"
  :group 'company)

(defcustom company-racer-executable "racer"
  "Path to the racer binary."
  :type 'file
  :group 'company-racer)

(defcustom company-racer-rust-src (getenv "RUST_SRC_PATH")
  "Path to rust lang sources, needs to be an absolute path.

If non nil overwrites the value of the environment variable 'RUST_SRC_PATH'."
  :type 'directory
  :group 'company-racer)

(defcustom company-racer-annotation-format "%c : %m"
  "String to format `company-racer' annotations.

%c   context
%m   matchstr"
  :type 'string
  :group 'company-racer)

;; TODO: is there a better way to do this?
(defvar company-racer-temp-file nil)

(defun company-racer-prefix ()
  "Get a prefix from current position."
  (company-grab-symbol-cons "\\.\\|::" 2))

(defun company-racer-complete-at-point ()
  "Call racer complete for PREFIX, return a deferred object."
  (let ((process-environment (if company-racer-rust-src
                                 (append (list
                                          (format "RUST_SRC_PATH=%s" (expand-file-name company-racer-rust-src)))
                                         process-environment)
                               process-environment)))
    (let ((line     (number-to-string (count-lines (point-min) (min (1+ (point)) (point-max)))))
          (column   (number-to-string (- (point) (line-beginning-position))))
          (filename (or (buffer-file-name) "")))
      (write-region nil nil company-racer-temp-file nil 0)
      (deferred:process company-racer-executable "complete" line column filename company-racer-temp-file))))

(defun company-racer-parse-candidate (line)
  "Return a completion candidate from a LINE."
  (let* ((match (and (string-prefix-p "MATCH" line) (string-join (cdr (split-string line " ")) " ")))
         (values (and match (split-string match ","))))
    (and values
         (cl-multiple-value-bind (matchstr line column filepath matchtype contextstr) values
           (put-text-property 0 1 :line (string-to-number line) matchstr)
           (put-text-property 0 1 :column (string-to-number column) matchstr)
           (put-text-property 0 1 :filepath filepath matchstr)
           (put-text-property 0 1 :matchtype matchtype matchstr)
           (put-text-property 0 1 :contextstr contextstr matchstr)
           matchstr))))

(defun company-racer-candidates (callback)
  "Return candidates for PREFIX with CALLBACK."
  (deferred:nextc
    (company-racer-complete-at-point)
    (lambda (output)
      (let ((candidates (cl-loop for line in (split-string output "\n")
                                 for candidate = (company-racer-parse-candidate line)
                                 unless (null candidate)
                                 collect candidate)))
        (funcall callback candidates)))))

(defun company-racer-meta (candidate)
  "Return meta string for a CANDIDATE."
  (get-text-property 0 :contextstr candidate))

(defun company-racer-annotation (candidate)
  "Return an annotation string for a CANDIDATE."
  (let ((matchtype (get-text-property 0 :matchtype candidate))
        (contextstr (get-text-property 0 :contextstr candidate)))
    (format-spec company-racer-annotation-format `((?m . ,matchtype)
                                                   (?c . ,contextstr)))))

(defun company-racer-location (candidate)
  "Return location for a CANDIDATE."
  (cons (get-text-property 0 :filepath candidate)
        (get-text-property 0 :line candidate)))

;;;###autoload
(defun company-racer (command &optional arg &rest ignored)
  "`company-mode' completion back-end for racer.
Provide completion info according to COMMAND and ARG.  IGNORED, not used."
  (interactive (list 'interactive))
  (cl-case command
    (init (and (null company-racer-temp-file)
               (setq company-racer-temp-file (make-temp-file "company-racer"))))
    (interactive (company-begin-backend 'company-racer))
    (prefix (and (derived-mode-p 'rust-mode)
                 (not (company-in-string-or-comment))
                 (or (company-racer-prefix) 'stop)))
    (candidates (cons :async 'company-racer-candidates))
    (annotation (company-racer-annotation arg))
    (location (company-racer-location arg))
    (meta (company-racer-meta arg))))

(provide 'company-racer)

;;; company-racer.el ends here
