;;; emms-player-simple-mpv-e.g.time-display.el --- A setting example of TQ event hooks -*- lexical-binding: t -*-

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

;; A setting example for `emms-playing-time-display'.
;;
;; (require 'emms-player-simple-mpv-e.g.time-display)

;;; Code:
(require 'emms-player-simple-mpv)
(require 'emms-mode-line)
(require 'emms-playing-time)

(defvar emms-mode-line-cycle) ; Suppress a warning message.
(defvar emms-state-mode)
(declare-function emms-state-set-total-playing-time "ext:emms-state")

;; Update playing-time
(defun emms-player-simple-mpv-reset-playing-time-display-timer (&optional speed)
  "Reset `emms-playing-time'.
SPEED is a mpv property of speed."
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "time-pos")
   (float-time)
   (lambda (sent-time ans-ls)
     (let ((time (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
           (update-fn (cond ((bound-and-true-p emms-state-mode) 'emms-state-playing-time-step)
                            (emms-playing-time-p 'emms-playing-time-display))))
       (when (and (emms-player-simple-mpv-playing-p) (numberp time)
                  (functionp update-fn))
         (when emms-playing-time-display-timer
           (emms-cancel-timer emms-playing-time-display-timer)
           (setq emms-playing-time-display-timer nil))
         (unless emms-player-paused-p
           (setq time (+ time (- (float-time) sent-time))))
         (setq emms-playing-time (1- (floor time)))
         (let (emms-mode-line-cycle)
           (funcall update-fn))
         (force-mode-line-update t)
         (unless emms-player-paused-p
           (setq speed (or (and (numberp speed)
                                (<= 0.01 speed) (<= speed 100) speed)
                           (and (emms-player-simple-mpv-last-speed-available-p)
                                emms-player-simple-mpv-last-speed)))
           (setq emms-playing-time-display-timer
                 (if speed
                     (run-at-time (/ (- 1.0 (- time (ffloor time))) speed)
                                  (/ 1.0 speed)
                                  update-fn)
                   (run-at-time (- 1.0 (- time  (ffloor time)))
                                1.0
                                update-fn)))))))))

(add-hook 'emms-player-simple-mpv-tq-event-unpause-hook
          'emms-player-simple-mpv-reset-playing-time-display-timer)

(add-hook 'emms-player-simple-mpv-tq-event-playback-restart-hook
          'emms-player-simple-mpv-reset-playing-time-display-timer)

(add-hook 'emms-player-simple-mpv-tq-event-speed-functions
          'emms-player-simple-mpv-reset-playing-time-display-timer)

;; Update info-playing-time
(defvar emms-player-simple-mpv-info-playing-time-ignore-types nil
  "List of type.")

(defun emms-player-simple-mpv-set-info-playing-time (duration)
  "Set info-playing-time to DURATION."
  (when (and (numberp duration) (> duration 0.0)
             (not (memq (emms-track-type (emms-playlist-current-selected-track))
                        emms-player-simple-mpv-info-playing-time-ignore-types)))
    (emms-track-set
     (emms-playlist-current-selected-track)
     'info-playing-time (floor duration))
    (when (bound-and-true-p emms-state-mode) (emms-state-set-total-playing-time))))

(add-hook 'emms-player-simple-mpv-tq-event-duration-functions
          'emms-player-simple-mpv-set-info-playing-time)

(provide 'emms-player-simple-mpv-e.g.time-display)
;;; emms-player-simple-mpv-e.g.time-display.el ends here
