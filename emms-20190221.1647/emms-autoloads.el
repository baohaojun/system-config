;;; emms-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "emms" "emms.el" (0 0 0 0))
;;; Generated autoloads from emms.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms" '("emms-" "define-emms-" "with-current-emms-playlist")))

;;;***

;;;### (autoloads nil "emms-bookmarks" "emms-bookmarks.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from emms-bookmarks.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-bookmarks" '("emms-bookmarks-")))

;;;***

;;;### (autoloads nil "emms-browser" "emms-browser.el" (0 0 0 0))
;;; Generated autoloads from emms-browser.el

(autoload 'emms-browser "emms-browser" "\
Launch or switch to the EMMS Browser.

\(fn)" t nil)

(autoload 'emms-smart-browse "emms-browser" "\
Display browser and playlist.
Toggle between selecting browser, playlist or hiding both. Tries
to behave sanely if the user has manually changed the window
configuration.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-browser" '("emms-" "case-fold-string")))

;;;***

;;;### (autoloads nil "emms-cache" "emms-cache.el" (0 0 0 0))
;;; Generated autoloads from emms-cache.el

(autoload 'emms-cache-enable "emms-cache" "\
Enable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-disable "emms-cache" "\
Disable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-toggle "emms-cache" "\
Toggle caching of Emms track data.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-cache" '("emms-cache")))

;;;***

;;;### (autoloads nil "emms-compat" "emms-compat.el" (0 0 0 0))
;;; Generated autoloads from emms-compat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-compat" '("emms-")))

;;;***

;;;### (autoloads nil "emms-cue" "emms-cue.el" (0 0 0 0))
;;; Generated autoloads from emms-cue.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-cue" '("emms-")))

;;;***

;;;### (autoloads nil "emms-history" "emms-history.el" (0 0 0 0))
;;; Generated autoloads from emms-history.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-history" '("emms-history-")))

;;;***

;;;### (autoloads nil "emms-i18n" "emms-i18n.el" (0 0 0 0))
;;; Generated autoloads from emms-i18n.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-i18n" '("emms-i18n-")))

;;;***

;;;### (autoloads nil "emms-info" "emms-info.el" (0 0 0 0))
;;; Generated autoloads from emms-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-info" '("emms-info-")))

;;;***

;;;### (autoloads nil "emms-info-libtag" "emms-info-libtag.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from emms-info-libtag.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-info-libtag" '("emms-info-libtag")))

;;;***

;;;### (autoloads nil "emms-info-metaflac" "emms-info-metaflac.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-info-metaflac.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-info-metaflac" '("emms-info-metaflac")))

;;;***

;;;### (autoloads nil "emms-info-mp3info" "emms-info-mp3info.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-info-mp3info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-info-mp3info" '("emms-info-mp3")))

;;;***

;;;### (autoloads nil "emms-info-ogginfo" "emms-info-ogginfo.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-info-ogginfo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-info-ogginfo" '("emms-info-ogginfo")))

;;;***

;;;### (autoloads nil "emms-info-opusinfo" "emms-info-opusinfo.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-info-opusinfo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-info-opusinfo" '("emms-info-opusinfo")))

;;;***

;;;### (autoloads nil "emms-last-played" "emms-last-played.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from emms-last-played.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-last-played" '("emms-last-played-")))

;;;***

;;;### (autoloads nil "emms-librefm-scrobbler" "emms-librefm-scrobbler.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-librefm-scrobbler.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-librefm-scrobbler" '("emms-librefm-scrobbler-")))

;;;***

;;;### (autoloads nil "emms-librefm-stream" "emms-librefm-stream.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-librefm-stream.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-librefm-stream" '("emms-librefm-stream")))

;;;***

;;;### (autoloads nil "emms-lyrics" "emms-lyrics.el" (0 0 0 0))
;;; Generated autoloads from emms-lyrics.el

(autoload 'emms-lyrics-enable "emms-lyrics" "\
Enable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-disable "emms-lyrics" "\
Disable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-toggle "emms-lyrics" "\
Toggle displaying emms lyrics.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-lyrics" '("emms-lyrics")))

;;;***

;;;### (autoloads nil "emms-mark" "emms-mark.el" (0 0 0 0))
;;; Generated autoloads from emms-mark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-mark" '("emms-mark-")))

;;;***

;;;### (autoloads nil "emms-metaplaylist-mode" "emms-metaplaylist-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-metaplaylist-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-metaplaylist-mode" '("emms-metaplaylist-mode")))

;;;***

;;;### (autoloads nil "emms-mode-line" "emms-mode-line.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from emms-mode-line.el

(autoload 'emms-mode-line-enable "emms-mode-line" "\
Turn on `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-disable "emms-mode-line" "\
Turn off `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-toggle "emms-mode-line" "\
Toggle `emms-mode-line'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-mode-line" '("emms-mode-line")))

;;;***

;;;### (autoloads nil "emms-mode-line-icon" "emms-mode-line-icon.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-mode-line-icon.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-mode-line-icon" '("emms-mode-line-icon-")))

;;;***

;;;### (autoloads nil "emms-player-mpd" "emms-player-mpd.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from emms-player-mpd.el

(autoload 'emms-player-mpd-clear "emms-player-mpd" "\
Clear the MusicPD playlist.

\(fn)" t nil)

(autoload 'emms-player-mpd-connect "emms-player-mpd" "\
Connect to MusicPD and retrieve its current playlist.

Afterward, the status of MusicPD will be tracked.

This also has the effect of changing the current EMMS playlist to
be the same as the current MusicPD playlist.  Thus, this
function is useful to call if the contents of the EMMS playlist
buffer get out-of-sync for some reason.

\(fn)" t nil)

(autoload 'emms-player-mpd-show "emms-player-mpd" "\
Describe the current EMMS track in the minibuffer.

If INSERTP is non-nil, insert the description into the current
buffer instead.

If CALLBACK is a function, call it with the current buffer and
description as arguments instead of displaying the description or
inserting it.

This function uses `emms-show-format' to format the current track.
It differs from `emms-show' in that it asks MusicPD for the current track,
rather than EMMS.

\(fn &optional INSERTP CALLBACK)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-player-mpd" '("emms-")))

;;;***

;;;### (autoloads nil "emms-player-mpg321-remote" "emms-player-mpg321-remote.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-player-mpg321-remote.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-player-mpg321-remote" '("emms-player-mpg321-remote")))

;;;***

;;;### (autoloads nil "emms-player-mplayer" "emms-player-mplayer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-player-mplayer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-player-mplayer" '("emms-player-mplayer-" "mplayer")))

;;;***

;;;### (autoloads nil "emms-player-mpv" "emms-player-mpv.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from emms-player-mpv.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-player-mpv" '("emms-player-mpv")))

;;;***

;;;### (autoloads nil "emms-player-simple" "emms-player-simple.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-player-simple.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-player-simple" '("emms-player-" "alsaplayer" "fluidsynth" "timidity" "mpg321" "mikmod" "playsound" "speexdec" "ogg123" "define-emms-simple-player")))

;;;***

;;;### (autoloads nil "emms-player-vlc" "emms-player-vlc.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from emms-player-vlc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-player-vlc" '("emms-player-vlc-" "vlc")))

;;;***

;;;### (autoloads nil "emms-player-xine" "emms-player-xine.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from emms-player-xine.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-player-xine" '("emms-" "xine")))

;;;***

;;;### (autoloads nil "emms-playing-time" "emms-playing-time.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-playing-time.el

(autoload 'emms-playing-time-enable-display "emms-playing-time" "\
Display playing time on mode line.

\(fn)" t nil)

(autoload 'emms-playing-time-disable-display "emms-playing-time" "\
Remove playing time from mode line.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-playing-time" '("emms-playing-time")))

;;;***

;;;### (autoloads nil "emms-playlist-limit" "emms-playlist-limit.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-playlist-limit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-playlist-limit" '("emms-playlist-limit-" "define-emms-playlist-limit")))

;;;***

;;;### (autoloads nil "emms-playlist-mode" "emms-playlist-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-playlist-mode.el

(autoload 'emms-playlist-mode "emms-playlist-mode" "\
A major mode for Emms playlists.
\\{emms-playlist-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-playlist-mode" '("emms")))

;;;***

;;;### (autoloads nil "emms-playlist-sort" "emms-playlist-sort.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-playlist-sort.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-playlist-sort" '("emms-" "define-emms-playlist-sort")))

;;;***

;;;### (autoloads nil "emms-score" "emms-score.el" (0 0 0 0))
;;; Generated autoloads from emms-score.el

(autoload 'emms-score-enable "emms-score" "\
Turn on emms-score.

\(fn)" t nil)

(autoload 'emms-score-disable "emms-score" "\
Turn off emms-score.

\(fn)" t nil)

(autoload 'emms-score-toggle "emms-score" "\
Toggle emms-score.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-score" '("emms-score")))

;;;***

;;;### (autoloads nil "emms-setup" "emms-setup.el" (0 0 0 0))
;;; Generated autoloads from emms-setup.el

(autoload 'emms-minimalistic "emms-setup" "\
An Emms setup script.
Invisible playlists and all the basics for playing media.

\(fn)" nil nil)

(autoload 'emms-all "emms-setup" "\
An Emms setup script.
Everything included in the `emms-minimalistic' setup and adds all
the stable features which come with the Emms distribution.

\(fn)" nil nil)

(autoload 'emms-default-players "emms-setup" "\
Set `emms-player-list' to `emms-setup-default-player-list'.

\(fn)" nil nil)

(autoload 'emms-devel "emms-setup" "\


\(fn)" nil nil)

(autoload 'emms-standard "emms-setup" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-setup" '("emms-setup-default-player-list")))

;;;***

;;;### (autoloads nil "emms-show-all" "emms-show-all.el" (0 0 0 0))
;;; Generated autoloads from emms-show-all.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-show-all" '("emms-show-all")))

;;;***

;;;### (autoloads nil "emms-source-file" "emms-source-file.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from emms-source-file.el
 (autoload 'emms-play-file "emms-source-file" nil t)
 (autoload 'emms-add-file "emms-source-file" nil t)
 (autoload 'emms-play-directory "emms-source-file" nil t)
 (autoload 'emms-add-directory "emms-source-file" nil t)
 (autoload 'emms-play-directory-tree "emms-source-file" nil t)
 (autoload 'emms-add-directory-tree "emms-source-file" nil t)
 (autoload 'emms-play-find "emms-source-file" nil t)
 (autoload 'emms-add-find "emms-source-file" nil t)
 (autoload 'emms-play-dired "emms-source-file" nil t)
 (autoload 'emms-add-dired "emms-source-file" nil t)

(autoload 'emms-source-file-directory-tree "emms-source-file" "\
Return a list of all files under DIR that match REGEX.
This function uses `emms-source-file-directory-tree-function'.

\(fn DIR REGEX)" nil nil)

(autoload 'emms-source-file-regex "emms-source-file" "\
Return a regexp that matches everything any player (that supports
files) can play.

\(fn)" nil nil)

(autoload 'emms-locate "emms-source-file" "\
Search for REGEXP and display the results in a locate buffer

\(fn REGEXP)" t nil)
 (autoload 'emms-play-url "emms-source-file" nil t)
 (autoload 'emms-add-url "emms-source-file" nil t)
 (autoload 'emms-play-streamlist "emms-source-file" nil t)
 (autoload 'emms-add-streamlist "emms-source-file" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-source-file" '("streamlist" "url" "emms-" "dire" "file" "find")))

;;;***

;;;### (autoloads nil "emms-source-playlist" "emms-source-playlist.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-source-playlist.el
 (autoload 'emms-play-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory-tree
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory-tree
          "emms-source-file" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-source-playlist" '("pls-playlist" "playlist" "emms-" "m3u-playlist" "native-playlist")))

;;;***

;;;### (autoloads nil "emms-stream-info" "emms-stream-info.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from emms-stream-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-stream-info" '("emms-stream-info-")))

;;;***

;;;### (autoloads nil "emms-streams" "emms-streams.el" (0 0 0 0))
;;; Generated autoloads from emms-streams.el

(autoload 'emms-streams "emms-streams" "\
Opens the EMMS Streams interface.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-streams" '("emms-")))

;;;***

;;;### (autoloads nil "emms-tag-editor" "emms-tag-editor.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from emms-tag-editor.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-tag-editor" '("emms-tag-editor-")))

;;;***

;;;### (autoloads nil "emms-url" "emms-url.el" (0 0 0 0))
;;; Generated autoloads from emms-url.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-url" '("emms-")))

;;;***

;;;### (autoloads nil "emms-volume" "emms-volume.el" (0 0 0 0))
;;; Generated autoloads from emms-volume.el

(autoload 'emms-volume-raise "emms-volume" "\
Raise the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-lower "emms-volume" "\
Lower the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-mode-plus "emms-volume" "\
Raise volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

(autoload 'emms-volume-mode-minus "emms-volume" "\
Lower volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-volume" '("emms-volume-")))

;;;***

;;;### (autoloads nil "emms-volume-amixer" "emms-volume-amixer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-volume-amixer.el

(autoload 'emms-volume-amixer-change "emms-volume-amixer" "\
Change amixer master volume by AMOUNT.

\(fn AMOUNT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-volume-amixer" '("emms-volume-amixer-c")))

;;;***

;;;### (autoloads nil "emms-volume-pulse" "emms-volume-pulse.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from emms-volume-pulse.el

(autoload 'emms-volume-pulse-change "emms-volume-pulse" "\
Change PulseAudio volume by AMOUNT.

\(fn AMOUNT)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "emms-volume-pulse" '("emms-volume-")))

;;;***

;;;### (autoloads nil "jack" "jack.el" (0 0 0 0))
;;; Generated autoloads from jack.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jack" '("jack-")))

;;;***

;;;### (autoloads nil "later-do" "later-do.el" (0 0 0 0))
;;; Generated autoloads from later-do.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "later-do" '("later-do")))

;;;***

;;;### (autoloads nil nil ("emms-maint.el" "emms-pkg.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; emms-autoloads.el ends here
