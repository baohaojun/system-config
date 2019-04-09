This provides EMMS players and stream lists of Japan radio stations.

* Available in Japan due to access restriction
    Radiko, らじる★らじる, 超！A&G+, 音泉, 響, アニたまどっとコム,
    animate.tv

* Available anywhere
    SimulRadio, ListenRadio, Sea Side Communications, Lantis, ファミ通.com

The following plugins are available:
+ emms-streams-jp-radios.el
+ emms-streams-jp-radios-anything.el
+ emms-streams-jp-radios-helm.el ( helm v1.7.9 or later )
+ emms-streams-jp-radios-counsel.el ( ivy 0.8.0 or later )

Further information is available from:
https://github.com/momomo5717/emms-player-mpv-jp-radios  ( README.org )

Other Requirements:

+ mpv v0.7 or later
  + ffmpeg ( the build with –enable-librtmp (for Radiko, らじる★らじる) )
+ wget (for Radiko, アニたまどっとコム)
+ swftools (for Radiko)

Setup:

(add-to-list 'load-path "/path/to/emms-player-mpv-jp-radios")
(require 'emms-player-mpv-jp-radios)

Adding all emms jp radio players
(emms-player-mpv-jp-radios-add-all)

Adding separetely
e.g. Only for Radiko and らじる★らじる
(emms-player-mpv-jp-radios-add "radiko" "radiru")

Usage:

M-x emms-streams
stationName is radiko, radiru, etc.
M-x emms-stream-stationName-add-bookmark

Some functions can update cache of stream list.
Updating synchronously
C-u M-x emms-stream-stationName-add-bookmark
Updating asynchronously
C-u -1 M-x emms-stream-stationName-add-bookmark

emms-streams-jp-radios.el provides `emms-streams-jp-radios' and
`emms-stream-jp-radios-popup' like `emms-streams'.

If `anything' is installed, `emms-streams-jp-radios-anything' is available.
(autoload 'emms-streams-jp-radios-anything
  "emms-streams-jp-radios-anything" nil t)

If `helm' is installed, `emms-streams-jp-radios-helm' is available.
(autoload 'emms-streams-jp-radios-helm
  "emms-streams-jp-radios-helm" nil t)

If `ivy' is installed, `emms-streams-jp-radios-counsel' is available.
(autoload 'emms-streams-jp-radios-counsel
  "emms-streams-jp-radios-counsel" nil t)
