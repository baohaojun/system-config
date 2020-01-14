;;; ivy-xref.el --- Ivy interface for xref results -*- lexical-binding: t -*-

;; Copyright (C) 2017  Alex Murray <murray.alex@gmail.com>

;; Author: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/ivy-xref
;; Package-Version: 20191126.401
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ivy "0.10.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This packages provides ivy as the interface for selection from xref results.

;;;; Setup

;; (require 'ivy-xref) ; unless installed from a package
;; (setq xref-show-xrefs-function 'ivy-xref-show-xrefs)

;;; Code:
(require 'xref)
(require 'ivy)

(defgroup ivy-xref nil
  "Select xref results using ivy."
  :prefix "ivy-xref-"
  :group 'ivy
  :link '(url-link :tag "Github" "https://github.com/alexmurray/ivy-xref"))

(defcustom ivy-xref-use-file-path nil
  "Whether to display the file path."
  :type 'boolean
  :group 'ivy-xref)

(defcustom ivy-xref-remove-text-properties nil
  "Whether to display the candidates with their original faces."
  :type 'boolean
  :group 'ivy-xref)

(defun ivy-xref-make-collection (xrefs)
  "Transform XREFS into a collection for display via `ivy-read'."
  (let ((collection nil))
    (dolist (xref xrefs)
      (with-slots (summary location) xref
        (let* ((line (xref-location-line location))
               (file (xref-location-group location))
               (candidate
                 (concat
                  (propertize
                   (concat
                    (if ivy-xref-use-file-path
                        file
                      (file-name-nondirectory file))
                    (if (integerp line)
                        (format ":%d: " line)
                      ": "))
                   'face 'compilation-info)
                  (progn
                    (when ivy-xref-remove-text-properties
                      (set-text-properties 0 (length summary) nil summary))
                    summary))))
          (push `(,candidate . ,location) collection))))
    (nreverse collection)))

;;;###autoload
(defun ivy-xref-show-xrefs (fetcher alist)
  "Show the list of xrefs returned by FETCHER and ALIST via ivy."
  ;; call the original xref--show-xref-buffer so we can be used with
  ;; dired-do-find-regexp-and-replace etc which expects to use the normal xref
  ;; results buffer but then bury it and delete the window containing it
  ;; immediately since we don't want to see it - see
  ;; https://github.com/alexmurray/ivy-xref/issues/2
  (let* ((xrefs (if (functionp fetcher)
                    ;; Emacs 27
                    (or (assoc-default 'fetched-xrefs alist)
                        (funcall fetcher))
                    fetcher))
         (buffer (xref--show-xref-buffer fetcher alist)))
    (quit-window)
    (let ((orig-buf (current-buffer))
          (orig-pos (point))
          done)
      (ivy-read "xref: " (ivy-xref-make-collection xrefs)
                :require-match t
                :action (lambda (candidate)
                          (setq done (eq 'ivy-done this-command))
                          (condition-case err
                              (let* ((marker (xref-location-marker (cdr candidate)))
                                     (buf (marker-buffer marker)))
                                (with-current-buffer buffer
                                  (select-window
                                   ;; function signature changed in
                                   ;; 2a973edeacefcabb9fd8024188b7e167f0f9a9b6
                                   (if (version< emacs-version "26.0.90")
                                       (xref--show-pos-in-buf marker buf t)
                                     (xref--show-pos-in-buf marker buf)))))
                            (user-error (message (error-message-string err)))))
                :unwind (lambda ()
                          (unless done
                            (switch-to-buffer orig-buf)
                            (goto-char orig-pos)))
                :caller 'ivy-xref-show-xrefs))
    ;; honor the contact of xref--show-xref-buffer by returning its original
    ;; return value
    buffer))

;;;###autoload
(defun ivy-xref-show-defs (fetcher alist)
  "Show the list of definitions returned by FETCHER and ALIST via ivy.
Will jump to the definition if only one is found."
  (let ((xrefs (funcall fetcher)))
    (cond
     ((not (cdr xrefs))
      (xref-pop-to-location (car xrefs)
                            (assoc-default 'display-action alist)))
     (t
      (ivy-xref-show-xrefs fetcher
                           (cons (cons 'fetched-xrefs xrefs)
                                 alist))))))

(provide 'ivy-xref)
;;; ivy-xref.el ends here
