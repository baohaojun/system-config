;;; purescript-collapse.el --- Collapse expressions

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(define-button-type 'purescript-collapse-toggle-button
  'action 'purescript-collapse-toggle-button-callback
  'follow-link t
  'help-echo "Click to expand…")

(defun purescript-collapse (beg end)
  "Collapse."
  (interactive "r")
  (goto-char end)
  (let ((break nil))
    (while (and (not break)
                (search-backward-regexp "[[({]" beg t 1))
      (unless (eq (get-text-property (point) 'face) 'font-lock-string-face)
        (let ((orig (point)))
          (purescript-collapse-sexp)
          (goto-char orig)
          (forward-char -1)
          (when (= (point) orig)
            (setq break t)))))))

(defun purescript-collapse-sexp ()
  "Collapse the sexp starting at point."
  (let ((beg (point)))
    (forward-sexp)
    (let ((end (point)))
      (let ((o (make-overlay beg end)))
        (overlay-put o 'invisible t)
        (let ((start (point)))
          (insert "…")
          (let ((button (make-text-button start (point)
                                          :type 'purescript-collapse-toggle-button)))
            (button-put button 'overlay o)
            (button-put button 'hide-on-click t)))))))

(defun purescript-collapse-toggle-button-callback (btn)
  "The callback to toggle the overlay visibility."
  (let ((overlay (button-get btn 'overlay)))
    (when overlay
      (overlay-put overlay
                   'invisible
                   (not (overlay-get overlay
                                     'invisible)))))
  (button-put btn 'invisible t)
  (delete-region (button-start btn) (button-end btn)))

(provide 'purescript-collapse)
