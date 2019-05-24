;;; mode-line-bell.el --- Flash the mode line instead of ringing the bell  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience
;; Package-Version: 20181029.516

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Enable the global minor mode `mode-line-bell-mode' to set
;; `ring-bell-function' to a function that will briefly flash the mode
;; line when the bell is rung.

;;; Code:

(defgroup mode-line-bell nil
  "Flash the mode line instead of ringing the bell."
  :group 'frames)

(defcustom mode-line-bell-flash-time 0.05
  "Length of time to flash the mode line when the bell is rung."
  :type 'float
  :safe 'floatp)

(defvar mode-line-bell--flashing nil
  "If non-nil, the mode line is currently flashing.")

(defun mode-line-bell--begin-flash ()
  "Begin flashing the mode line."
  (unless mode-line-bell--flashing
    (invert-face 'mode-line)
    (setq mode-line-bell--flashing t)))

(defun mode-line-bell--end-flash ()
  "Finish flashing the mode line."
  (when mode-line-bell--flashing
    (invert-face 'mode-line)
    (setq mode-line-bell--flashing nil)))

;;;###autoload
(defun mode-line-bell-flash ()
  "Flash the mode line momentarily."
  (unless mode-line-bell--flashing
    (run-with-timer mode-line-bell-flash-time nil 'mode-line-bell--end-flash)
    (mode-line-bell--begin-flash)))

;;;###autoload
(define-minor-mode mode-line-bell-mode
  "Flash the mode line instead of ringing the bell."
  :lighter nil
  :global t
  (setq-default ring-bell-function (when mode-line-bell-mode
                                     'mode-line-bell-flash)))


(provide 'mode-line-bell)
;;; mode-line-bell.el ends here
