;;; emms-player-simple-mpv-e.g.hydra.el --- A setting example for hydra -*- lexical-binding: t -*-

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

;; This provides a setting example of hydra for emms-player-simple-mpv.
;;
;; Setting `emms-player-simple-mpv-hydra/body' to a global key bind would be useful.
;; (global-set-key (kbd "<f2> m") 'emms-player-simple-mpv-hydra/body)

;;; Code:
(require 'emms-player-simple-mpv-control-functions)
(require 'hydra nil t)

;; This setting example emulates default key bindings of mpv player as mutch as possible.

(defvar emms-player-simple-mpv-hydra-docstring
  "
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃      Keyboard Control for emms simple player of mpv      ┃
┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
   _Q_      Quit emms-player-simple-mpv-hydra.
  ─────────────────────────────
   _<left>_ and _<right>_
          Seek backward/forward 5 seconds.
   _S-<left>_ and _S-<right>_
          Seek backward/forward 1 seconds.
   _<down>_ and <up>
          Seek backward/forward 1 minute.
   _S-<down>_ and S-<upt>
          Seek backward/forward 5 seconds.
  ─────────────────────────────
   _\[_ and _\]_
          Decrease/increase current playback speed by 10 %%%%.
   _\{_ and _\}_
          Halve/double current playback speed.
   _<backspace>_
          Reset playback speed to normal.
  ─────────────────────────────
   _<_ and _>_
          Go backward/forward in the playlist.
  _<return>_
          Go forward in the playlist.
  ─────────────────────────────
  _p_ / _SPC_
          Pause (pressing again unpauses).
  ─────────────────────────────
   _q_      Stop playing and quit.
  ─────────────────────────────
   _/_ and _*_
          Decrease/increase volume.
   _9_ and _0_
          Decrease/increase volume.
  ─────────────────────────────
   _m_      Mute sound.
  ─────────────────────────────
   _f_      Toggle fullscreen.
  ─────────────────────────────
   _T_      Toggle stay-on-top.
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛

"
  "Docstring for `emms-player-simple-mpv-hydra/body'.")


(eval-after-load "hydra"
  `(defhydra emms-player-simple-mpv-hydra
     (:foreign-keys warn :hint nil)
     ,emms-player-simple-mpv-hydra-docstring
     ("Q" nil)
     ("<left>"    (lambda () (interactive) (emms-seek -5)))
     ("S-<left>"  (lambda () (interactive) (emms-seek -1)))
     ("<down>"    (lambda () (interactive) (emms-seek -60)))
     ("S-<down>"  (lambda () (interactive) (emms-seek -5)))
     ("<right>"   (lambda () (interactive) (emms-seek 5)))
     ("S-<right>" (lambda () (interactive) (emms-seek 1)))
     ("<up>"      (lambda () (interactive) (emms-seek 60)))
     ("S-<up>"    (lambda () (interactive) (emms-seek 5)))
     ("["  emms-player-simple-mpv-speed-decrease)
     ("]"  emms-player-simple-mpv-speed-increase)
     ("{"  emms-player-simple-mpv-speed-halve)
     ("}"  emms-player-simple-mpv-speed-double)
     ("<backspace>" emms-player-simple-mpv-speed-normal)
     ("<" emms-player-simple-mpv-playlist-prev)
     (">" emms-player-simple-mpv-playlist-next)
     ("<return>" emms-player-simple-mpv-playlist-next)
     ("p" emms-pause)
     ("SPC" emms-pause)
     ("q" (lambda () (interactive)
            (when (y-or-n-p "emms-stop?")
              (emms-stop))) :exit t)
     ("/" emms-volume-lower)
     ("*" emms-volume-raise)
     ("9" emms-volume-lower)
     ("0" emms-volume-raise)
     ("m" emms-player-simple-mpv-mute)
     ("f" emms-player-simple-mpv-fullscreen)
     ("T" emms-player-simple-mpv-ontop)))

(provide 'emms-player-simple-mpv-e.g.hydra)
;;; emms-player-simple-mpv-e.g.hydra.el ends here
