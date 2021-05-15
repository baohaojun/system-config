;;; whole-line-or-region.el --- Operate on current line if region undefined  -*- lexical-binding: t -*-

;; This file is not part of Emacs

;; Copyright (C) 2011-2020 Steve Purcell
;; Copyright (C) 2001 by Joseph L. Casadonte Jr.

;; Author:          Joe Casadonte <emacs@northbound-train.com>
;; Author:          Steve Purcell <steve@sanityinc.com>
;; Maintainer:      Steve Purcell <steve@sanityinc.com>
;; Created:         July 1, 2001
;; Keywords:        convenience wp
;; Package-Commit: 3c1a55103e52c7a3010ee0043af22484d23a441e
;; Package-Version: 20201214.650
;; Package-X-Original-Version: 0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.6"))
;; Homepage:  https://github.com/purcell/whole-line-or-region

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

;; This minor mode allows functions to operate on the current line if
;; they would normally operate on a region and region is currently
;; undefined.
;;
;; The primary use for this is to kill (cut) the current line if no
;; region is defined, and kill-region is invoked.  It basically saves
;; you the effort of going to the begining of the line, selecting the
;; text up to the end of the line, and killing.  Similarly, when
;; yanking, it's smart enough to know that the string to be yanked was
;; killed as a whole line, and it should be yanked as one, too.  So
;; you don't need to position yourself at the start of the line before
;; yanking.  If region *is* defined, though, all functions act as
;; normal.

;;; Usage:

;; M-x `whole-line-or-region-local-mode'

;; Use `whole-line-or-region-local-mode' to toggle the overridden
;; behaviour on & off on a per-buffer basis.  This remaps common
;; commands such as `copy-region-as-kill', `kill-ring-save' etc.
;;
;; To enable the mode for all buffers, enable
;; `whole-line-or-region-global-mode'.

;; In order to extend this package for additional region-based
;; functions, you must understand how those functions work, and write
;; a new stub function that will be used to replace it.  Several
;; helper functions are defined in this package; which one to use
;; depends on whether or not the original function wants region passed
;; into it, or assumes region is defined before being called.
;;
;; After defining the new stub, however you do it, the package needs
;; to know about it so that it can toggle its use on and off as the
;; mode toggles on and off.  For that you should modify
;; `whole-line-or-region-local-mode-map' to remap the underlying
;; command to your stub function, e.g.

;; (with-eval-after-load 'whole-line-or-region
;;   (define-key whole-line-or-region-local-mode-map
;;               [remap the-base-command]
;;               'my-stub-command))

;;; Code:

(require 'cl-lib)

(defgroup whole-line-or-region nil
  "Customization group for whole-line-or-region minor mode."
  :group 'editing-basics
  :group 'convenience)

;;; Keymap
(defvar whole-line-or-region-local-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap kill-region] 'whole-line-or-region-kill-region)
    (define-key map [remap kill-ring-save] 'whole-line-or-region-kill-ring-save)
    (define-key map [remap copy-region-as-kill] 'whole-line-or-region-copy-region-as-kill)
    (define-key map [remap delete-region] 'whole-line-or-region-delete-region)
    (define-key map [remap comment-dwim] 'whole-line-or-region-comment-dwim-2)
    (define-key map [remap comment-region] 'whole-line-or-region-comment-region)
    (define-key map [remap uncomment-region] 'whole-line-or-region-uncomment-region)
    map)
  "Minor mode map for `whole-line-or-region-mode'.")

;;;###autoload
(define-minor-mode whole-line-or-region-local-mode
  "Make chosen commands operate on the current line if no region is active.
Modify `whole-line-or-region-local-mode-map' to change which
commands are affected."
  :lighter " WLR"
  :keymap 'whole-line-or-region-local-mode-map)

(defcustom whole-line-or-region-inhibit-modes '(pdf-view-mode)
  "Inhibit the effect of `whole-line-or-region-global-mode' in these major modes."
  :type '(list symbol))

;;;###autoload
(define-globalized-minor-mode whole-line-or-region-global-mode
  whole-line-or-region-local-mode
  (lambda ()
    (unless (apply 'derived-mode-p whole-line-or-region-inhibit-modes)
      (whole-line-or-region-local-mode 1))))



;;; Internal helpers
(defmacro whole-line-or-region-preserve-column (&rest body)
  "Ensure the current column is kept the same after executing BODY."
  (let ((init (cl-gensym)))
    `(let ((,init (current-column)))
       (prog1
           (progn ,@body)
         (move-to-column ,init)))))



;;; Yank handler

(defun whole-line-or-region-yank-handler (string)
  "Yank handler which operates at the beginning of the line.
STRING is the string being yanked."
  (if (and (not (and delete-selection-mode mark-active))
           whole-line-or-region-local-mode)
      (whole-line-or-region-preserve-column
       (unless (bolp)
         (forward-line 0)
         (push-mark nil t))
       (whole-line-or-region-insert-clean string t))
    (whole-line-or-region-insert-clean string nil)))

(defun whole-line-or-region-insert-clean (string &optional ensure-newline)
  "Insert STRING and remove `yank-excluded-properties'.
This is needed because the yank handler may move point, and is
therefore registered with the NOEXCLUDE flag: this causes `yank'
to not remove the excluded properties itself.

When ENSURE-NEWLINE is non-nil, add a newline if there was none."
  (let ((beg (point-marker))
        end)
    (insert string)
    (remove-yank-excluded-properties beg (point))
    (when (and ensure-newline (not (eq (char-before) ?\n)))
      (insert "\n"))
    (setq end (point-marker))
    (setq yank-undo-function
          (lambda (_beg _end) (delete-region beg end)))))


;;; Helpers for wrapping commands

(defun whole-line-or-region-use-region-p ()
  "Return non-nil if we expect underlying commands to use the region."
  (and mark-active
       (or use-empty-active-region
           (> (region-end) (region-beginning)))))

(defmacro whole-line-or-region-filter-with-yank-handler (&rest body)
  "Execute BODY with `filter-buffer-substring-function' bound.
The binding ensure killed strings have a yank handler attached."
  (let ((orig (cl-gensym)))
    `(let* ((,orig filter-buffer-substring-function)
            (filter-buffer-substring-function
             (lambda (&rest args)
               (let ((s (apply ,orig args)))
                 (put-text-property 0 (length s) 'yank-handler
                                    '(whole-line-or-region-yank-handler nil t)
                                    s)
                 s))))
       ,@body)))

(defun whole-line-or-region-wrap-region-kill (f num-lines)
  "Wrap a region function F, such as `kill-region'.

Such functions are expected to accept the arguments (BEG END &optional REGION).

If NUM-LINES is non-zero and the region is inactive, it denotes
the number of lines to operate upon, where positive numbers
indicate lines after point, and negative numbers represent lines
preceding point."
  (if (whole-line-or-region-use-region-p)
      (funcall f (region-beginning) (region-end) 'region)
    (whole-line-or-region-filter-with-yank-handler
     (whole-line-or-region-preserve-column
      (funcall f
               (line-beginning-position 1)
               (line-beginning-position (+ 1 num-lines))
               nil)))))

(defun whole-line-or-region-wrap-beg-end (f num-lines &rest rest)
  "Wrap function F and call it passing the possibly-expanded region.
F is assumed to take the parameters (BEG END &rest REST),
which will all be passed through unchanged, except that BEG and END may
be adjusted.

If NUM-LINES is non-zero and the region is inactive, it denotes
the number of lines to operate upon, where positive numbers
indicate lines after point, and negative numbers represent lines
preceding point."
  (if (whole-line-or-region-use-region-p)
      (apply f (region-beginning) (region-end) rest)
    (apply f
           (line-beginning-position 1)
           (line-beginning-position (+ 1 num-lines))
           rest)))

(defun whole-line-or-region-wrap-modified-region (f num-lines &rest rest)
  "Wrap function F and call it passing the possibly-expanded region.
F is assumed to take the parameters (&rest REST), which will all
be passed through unchanged.

The current region will be temporarily expanded if necessary
while F is called.

If NUM-LINES is non-zero and the region is inactive, it denotes
the number of lines to operate upon, where positive numbers
indicate lines after point, and negative numbers represent lines
preceding point."
  (if (whole-line-or-region-use-region-p)
      (apply f rest)
    (save-excursion
      (set-mark (line-beginning-position 1))
      (goto-char (line-beginning-position (+ 1 num-lines)))
      (apply f rest))))


;;; Wrappers for commonly-used functions

;;;###autoload
(defun whole-line-or-region-copy-region-as-kill (prefix)
  "Call `copy-region-as-kill' on region or PREFIX whole lines."
  (interactive "p")
  (whole-line-or-region-wrap-region-kill 'copy-region-as-kill prefix))

;;;###autoload
(defun whole-line-or-region-kill-region (prefix)
  "Call `kill-region' on region or PREFIX whole lines."
  (interactive "p")
  (whole-line-or-region-wrap-region-kill 'kill-region prefix))

;;;###autoload
(defun whole-line-or-region-kill-ring-save (prefix)
  "Call `kill-ring-save' on region or PREFIX whole lines."
  (interactive "p")
  (whole-line-or-region-wrap-region-kill 'kill-ring-save prefix))

;;;###autoload
(defun whole-line-or-region-delete-region (prefix)
  "Call `delete-region' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-wrap-beg-end 'delete-region prefix))

;;;###autoload
(defun whole-line-or-region-comment-region (prefix)
  "Call `comment-region' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-wrap-beg-end 'comment-region prefix))

;;;###autoload
(defun whole-line-or-region-comment-dwim (prefix)
  "Call `comment-dwim' on region or current line.
PREFIX is passed unchanged to `comment-dwim'."
  (interactive "*P")
  (whole-line-or-region-wrap-modified-region 'comment-dwim 1 prefix))

;;;###autoload
(defun whole-line-or-region-comment-dwim-2 (prefix)
  "Call `comment-dwim' on region or PREFIX whole line."
  (interactive "*p")
  (whole-line-or-region-wrap-modified-region 'comment-dwim prefix nil))

;;;###autoload
(defun whole-line-or-region-uncomment-region (prefix)
  "Call `uncomment-region' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-wrap-beg-end 'uncomment-region prefix t))

;;;###autoload
(defun whole-line-or-region-comment-or-uncomment-region (prefix)
  "Call `comment-or-uncomment-region' on region or PREFIX whole lines."
  (interactive "P")
  (whole-line-or-region-wrap-beg-end 'comment-or-uncomment-region prefix t))

;;;###autoload
(defun whole-line-or-region-indent-rigidly-left-to-tab-stop (prefix)
  "Call `indent-rigidly-left-to-tab-stop' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-wrap-beg-end 'indent-rigidly-left-to-tab-stop prefix))

;;;###autoload
(defun whole-line-or-region-indent-rigidly-right-to-tab-stop (prefix)
  "Call `indent-rigidly-right-to-tab-stop' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-wrap-beg-end 'indent-rigidly-right-to-tab-stop prefix))

;;;###autoload
(defun whole-line-or-region-indent-rigidly-left (prefix)
  "Call `indent-rigidly-left-to-tab-stop' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-wrap-beg-end 'indent-rigidly-left prefix))

;;;###autoload
(defun whole-line-or-region-indent-rigidly-right (prefix)
  "Call `indent-rigidly-right-to-tab-stop' on region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-wrap-beg-end 'indent-rigidly-right prefix))

(provide 'whole-line-or-region)

;;; whole-line-or-region.el ends here
