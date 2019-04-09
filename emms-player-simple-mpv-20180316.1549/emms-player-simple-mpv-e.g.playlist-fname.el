;;; emms-player-simple-mpv-e.g.playlist-fname.el --- A setting example of TQ event hooks -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 momomo5717

;; Author: momomo5717
;; URL: https://github.com/momomo5717/

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

;; A setting example for current playlist's filename.
;;
;; (require 'emms-player-simple-mpv-e.g.playlist-fname)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-mode-line)

(defvar emms-mode-line-cycle) ; Suppres a warning message.
(defvar emms-mode-line-icon-before-format)
(defvar emms-mode-line-icon-image-cache)

(defun emms-player-simple-mpv-update-playlist-filename (fname)
  "Set the current playlist's FNAME to `emms-mode-line-string'."
  (when (and (stringp fname)
             (eq (emms-track-type (emms-playlist-current-selected-track))
                 'playlist))
    (setq emms-mode-line-string
          (cond
           ((and (fboundp 'emms-mode-line-cycle-mode-line-function)
                 (or emms-mode-line-cycle
                     (eq emms-mode-line-mode-line-function
                         'emms-mode-line-cycle-mode-line-function)))
            (emms-mode-line-cycle-mode-line-function fname))
           ((and (featurep 'emms-mode-line-icon)
                 (eq emms-mode-line-mode-line-function 'emms-mode-line-icon-function))
            (concat " "
                    emms-mode-line-icon-before-format
                    (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
                    (format emms-mode-line-format fname)))
           (t (format emms-mode-line-format fname))))))

(add-hook 'emms-player-simple-mpv-tq-event-filename-functions
          'emms-player-simple-mpv-update-playlist-filename)

(provide 'emms-player-simple-mpv-e.g.playlist-fname)
;;; emms-player-simple-mpv-e.g.playlist-fname.el ends here
