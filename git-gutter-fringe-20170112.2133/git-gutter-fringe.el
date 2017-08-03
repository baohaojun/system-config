;;; git-gutter-fringe.el --- Fringe version of git-gutter.el -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-git-gutter-fringe
;; Package-Version: 20170112.2133
;; Version: 0.23
;; Package-Requires: ((git-gutter "0.88") (fringe-helper "0.1.1") (cl-lib "0.5") (emacs "24"))

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

;; Show git diff information in fringe. You can use this package
;; only GUI Emacs, not working no window emacs.

;; To use this package, add following code to your init.el or .emacs
;;
;; (require 'git-gutter-fringe)
;; (global-git-gutter-mode t)
;;
;; ; If you want to show git diff information at right fringe
;; (setq git-gutter-fr:side 'right-fringe)
;;

;;; Code:

(require 'cl-lib)

(defvar git-gutter-fringe nil)

(require 'git-gutter)
(require 'fringe-helper)

(defface git-gutter-fr:modified
    '((t (:inherit (git-gutter:modified fringe))))
  "Face of modified"
  :group 'git-gutter)

(defface git-gutter-fr:added
    '((t (:inherit (git-gutter:added fringe))))
  "Face of added"
  :group 'git-gutter)

(defface git-gutter-fr:deleted
    '((t (:inherit (git-gutter:deleted fringe))))
  "Face of deleted"
  :group 'git-gutter)

(defcustom git-gutter-fr:side 'left-fringe
  "Side of show diff information"
  :type '(choice (const :tag "Right Fringe" right-fringe)
                 (const :tag "Left Fringe" left-fringe))
  :group 'git-gutter)

(fringe-helper-define 'git-gutter-fr:added nil
  "...XX..."
  "...XX..."
  "...XX..."
  "XXXXXXXX"
  "XXXXXXXX"
  "...XX..."
  "...XX..."
  "...XX...")

(fringe-helper-define 'git-gutter-fr:deleted nil
  "........"
  "........"
  "........"
  "XXXXXXXX"
  "XXXXXXXX"
  "........"
  "........"
  "........")

(fringe-helper-define 'git-gutter-fr:modified nil
  "........"
  "..XXXX.."
  "..XXXX.."
  "..XXXX.."
  "..XXXX.."
  "..XXXX.."
  "..XXXX.."
  "........")

(defvar git-gutter-fr:bitmap-references nil)

(defsubst git-gutter-fr:select-sign (type)
  (cl-case type
    (modified 'git-gutter-fr:modified)
    (added    'git-gutter-fr:added)
    (deleted  'git-gutter-fr:deleted)))

(defsubst git-gutter-fr:select-face (type)
  (cl-case type
    (modified 'git-gutter-fr:modified)
    (added    'git-gutter-fr:added)
    (deleted  'git-gutter-fr:deleted)))

(defun git-gutter-fr:init ()
  (make-local-variable 'git-gutter-fr:bitmap-references))

(defun git-gutter-fr:view-diff-infos (diffinfos)
  (when git-gutter-fr:bitmap-references
    (git-gutter:clear-gutter))
  (save-excursion
    (goto-char (point-min))
    (cl-loop with curline = 1
             for info in diffinfos
             for start-line = (git-gutter-hunk-start-line info)
             for end-line = (git-gutter-hunk-end-line info)
             for type = (git-gutter-hunk-type info)
             do
             (let (beg end)
               (forward-line (- start-line curline))
               (setq beg (point))
               (if (eq type 'deleted)
                   (progn
                     (setq end beg))
                 (forward-line (- end-line start-line))
                 (setq end (point)))
               (let* ((sign (git-gutter-fr:select-sign type))
                      (face (git-gutter-fr:select-face type))
                      (reference (fringe-helper-insert-region
                                  beg end sign git-gutter-fr:side face)))
                 (overlay-put reference 'git-gutter t)
                 (dolist (ov (overlays-in beg (1+ end)))
                   (when (eq (overlay-get ov 'fringe-helper-parent) reference)
                     (overlay-put ov 'git-gutter t)))
                 (push reference git-gutter-fr:bitmap-references))
               (setq curline end-line)))))

;; @@@ Somtimes `fringe-helper-remove' does not work for clearing overlays
;;(defun git-gutter-fr:clear-overlay (reference)
;;  (fringe-helper-remove reference)
;;  t)

(defun git-gutter-fr:clear ()
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (or (overlay-get ov 'git-gutter)
              (let ((parent (overlay-get ov 'fringe-helper-parent)))
                (and parent
                     (or
                      ;; Parent has been deleted
                      (null (overlay-buffer parent))
                      ;; or parent was created by git-gutter
                      (overlay-get parent 'git-gutter)))))
      (delete-overlay ov)))
  (setq git-gutter-fr:bitmap-references nil))

(setq git-gutter:init-function      'git-gutter-fr:init
      git-gutter:view-diff-function 'git-gutter-fr:view-diff-infos
      git-gutter:clear-function     'git-gutter-fr:clear
      git-gutter:window-width -1)

(provide 'git-gutter-fringe)

;;; git-gutter-fringe.el ends here
