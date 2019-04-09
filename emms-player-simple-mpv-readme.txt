This is an extension of emms-player-simple.el for mpv JSON IPC.
It provides macros and functions for defining emms simple players of mpv.
emms-player-simple-mpv-control-functions.el provides other functions to control mpv.

Further information is available from:
https://github.com/momomo5717/emms-player-simple-mpv


Other Requirements:

  + mpv v0.10.0 or later
  + Unix Sockets

Setup:

(require 'emms-player-simple-mpv)
This plugin provides control functions (e.g. ab-loop, speed, fullscreen).
(require 'emms-player-simple-mpv-control-functions)

Usage:

An example of setting like emms-player-mplayer.el
`emms-player-my-mpv' is defined in this case.
(define-emms-simple-player-mpv my-mpv '(file url streamlist playlist)
    (concat "\\`\\(http[s]?\\|mms\\)://\\|"
            (apply #'emms-player-simple-regexp
                   "aac" "pls" "m3u"
                   emms-player-base-format-list))
    "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")

(emms-player-simple-mpv-add-to-converters
 'emms-player-my-mpv "." '(playlist)
 (lambda (track-name) (format "--playlist=%s" track-name)))

(add-to-list 'emms-player-list 'emms-player-my-mpv)

Playing YouTube playlist in reverse order.
`emms-player-my-mpv-ytpl-reverse' will be defined in this case.
(define-emms-simple-player-mpv my-mpv-ytpl-reverse '(url)
  "\\`http[s]://www\\.youtube\\.com/playlist\\?list="
  "mpv" "--no-terminal" "--force-window=no" "--audio-display=no"
  "--ytdl" "--ytdl-raw-options=playlist-reverse=")

(add-to-list 'emms-player-list 'emms-player-my-mpv-ytpl-reverse)

M-x emms-player-simple-mpv-playlist-popup can display playlist

The following example configuration files are available:

  + emms-player-simple-mpv-e.g.time-display.el
  + emms-player-simple-mpv-e.g.playlist-fname.el
  + emms-player-simple-mpv-e.g.hydra.el
