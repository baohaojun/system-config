;;; default-text-scale.el --- Easily adjust the font size in all frames

;; Copyright (C) 2015  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: frames, faces
;; Package-Version: 20150227.956
;; Package-X-Original-Version: 0

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

;; This package provides commands for increasing or decreasing the
;; default font size in all GUI Emacs frames -- it is like an
;; Emacs-wide version of `text-scale-mode'.

;; Usage:

;;     (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
;;     (global-set-key (kbd "C-M--") 'default-text-scale-decrease)

;;; Code:

(defgroup default-text-scale nil
  "Adjusting the default font size across all frames."
  :group 'faces
  :prefix "default-text-scale-")

(defcustom default-text-scale-amount 10
  "Increment by which to adjust the :height of the default face."
  :type 'integer
  :group 'default-text-scale)

(defun default-text-scale-increment (delta)
  "Adjust the default font height by DELTA on every graphical frame.
The pixel size of the frame will be kept approximately the same,
to the extent possible, as with the function `set-frame-font'.
DELTA should be a multiple of 10, to match the units used by
the :height face attribute."
  (interactive "nIncrement (e.g. 10, -5)? ")
  (unless (display-multi-font-p (selected-frame))
    (error "Cannot adjust default text scale from a non-graphical frame"))
  (let* ((cur-height (face-attribute 'default :height))
         (new-height (+ cur-height delta)))
    ;; Modify the special "user" theme, which is always combined
    ;; with any other loaded theme(s).  An alternative approach
    ;; would be modifying the default face's face-override-spec
    ;; property (see `face-spec-set'), but that produces more
    ;; redraws
    (custom-push-theme 'theme-face 'default 'user 'set `((t (:height ,new-height))))
    (dolist (f (frame-list))
      (when (display-multi-font-p f)
        (let ((pixel-height (* (frame-parameter f 'height)
                               (frame-char-height f)))
              (pixel-width  (* (frame-parameter f 'width)
                               (frame-char-width f))))
          (face-spec-recalc 'default f)
          (unless (frame-parameter f 'fullscreen)
            (modify-frame-parameters
             f
             `((height . ,(round pixel-height (frame-char-height f)))
               (width . ,(round pixel-width  (frame-char-width f))))))))
      (with-selected-frame f
        (run-hooks 'after-setting-font-hook)))
    (message "Default font size is now %d" (/ new-height 10))))

;;;###autoload
(defun default-text-scale-increase ()
  "Increase the height of the default face by `default-text-scale-amount'."
  (interactive)
  (default-text-scale-increment default-text-scale-amount))

;;;###autoload
(defun default-text-scale-decrease ()
  "Decrease the height of the default face by `default-text-scale-amount'."
  (interactive)
  (default-text-scale-increment (- default-text-scale-amount)))


(provide 'default-text-scale)
;;; default-text-scale.el ends here
