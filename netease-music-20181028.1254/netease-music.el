;;; netease-music.el --- listen netease music

;; Copyright (C) 2018  hiro方圆
;; Version: 1.0
;; Package-Version: 20181028.1254
;; URL: https://github.com/nicehiro/netease-music
;; Package-Requires: ((names "0.5") (emacs "25"))
;; Author: hiro方圆 <wfy11235813@gmail.com>
;; Keywords: multimedia Chinese music

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

;; This library was developed for Chinese streaming music - Netease cloud music.
;; netease-music-init-frame Initialize netease-music buffer.
;; netease-music-jump-into Jump into the playlist.  You can use "Enter" too if you use evil.
;; netease-music-jump-into Play current song.  You can use "Enter" too if you use evil.
;; netease-music-play-next Play next song in this playlist.  You can use "n" too if you use evil.
;; netease-music-search Search songs.
;; netease-music-i-like-it Collect song to your "favoriate song list".

;;; Code:

(require 'json)
(require 'url)
(require 'org)
(require 'names)

(defgroup netease-music nil
  "Netease music plugin."
  :prefix "netease-music-"
  :group 'multimedia
  :link '(url-link :tag "Github" "https://github.com/nicehiro/netease-music"))

(define-namespace netease-music-

(defclass song ()
  ((name)
   (artist)
   (album)
   (song-id)
   (artist-id)
   (album-id)))

(defclass playlist ()
  ((name)
   (id)
   (description)
   (user-id)))

(defclass admin ()
  ((name)
   (level)
   (listenSongs)
   (signature)))

(defclass mv ()
  ((name)
   (artist-name)
   (artist-id)
   (mv-id)
   (publish-time)))

(defcustom player "mplayer"
  "Netease music player.  Default player is mplayer."
  :type 'string)

(defcustom username nil
  "Your netease music username."
  :type 'string)

(defvar password nil
  "Your netease music password."
  )

(defvar play-loop-flag nil
  "Should loop the current song")

(defcustom user-id nil
  "Your netease music user id."
  :type 'string)

(defconst buffer-name-search "Search Results"
  "Search window buffer's name.")

(defvar process nil
  "The process of netease music player.")

(defvar status ""
  "Netease music player status.")

(defvar play-list ()
  "Your Play List.")

(defvar current-playing-song ()
  "This is current playing song.")

(defun format-current-playing-song (name artist album song-id artist-id)
  "Format current playing song with song's NAME, ARTIST, ALBUM SONG-ID and ARTIST-ID."
  (setf (slot-value current-playing-song 'name) name)
  (setf (slot-value current-playing-song 'artist) artist)
  (setf (slot-value current-playing-song 'album) album)
  (setf (slot-value current-playing-song 'song-id) song-id)
  (setf (slot-value current-playing-song 'artist-id) artist-id))

(defvar songs-list ()
  "Songs list.  A playlist's all songs, and you can add other song into it.")

(defvar search-songs-list ()
  "Search songs list.")

(defvar mvs-list ()
  "MVs list.")

(defvar api nil
  "NetEase Music API ADDRESS.")

(defconst login-url "/login/cellphone"
  "Login url pattern.")

(defconst playlist-url "/user/playlist"
  "Playlist url pattern.")

(defconst playlist-detail-url "/playlist/detail"
  "Playlist detail url pattern.")

(defconst user-detail-url "/user/detail"
  "User detail url pattern.")

(defconst play-list-url "/user/playlist"
  "User playlist.")

(defconst song-url "/song/url"
  "Music real url.")

(defconst lyric-url "/lyric"
  "Lyric url.")

(defconst personal-fm-url "/personal_fm"
  "Personal f.m. url.")

(defconst search-url "/search"
  "Search url.")

(defconst like-url "/like"
  "I like it url.")

(defconst recommend-url "/recommend/songs"
  "Recommend songs url.")

(defconst artist-details-url "/artists"
  "Artist details url.")

(defconst artist-mv-url "/artist/mv"
  "Artist mv url.")

(defconst get-mv-url "/mv/detail"
  "Get mv url.")

(defconst get-mv-args "?mvid=%s"
  "Get mv args.")

(defconst artist-mv-args "?id=%s"
  "Artist mv args.")

(defconst login-args "?phone=%s&password=%s"
  "Login args.")

(defconst user-detail-args "?uid=%s"
  "User detail args.")

(defconst playlist-args "?uid=%s"
  "Playlist args.")

(defconst playlist-detail-args "?id=%s"
  "Playlist detail args.")

(defconst song-args "?id=%s"
  "Song args.")

(defconst lyric-args "?id=%s"
  "Lyric args.")

(defconst search-args "?keywords=%s"
  "Search args.")

(defconst like-args "?id=%s"
  "I like it args.")

(defconst artist-details-args "?id=%s"
  "Artist details args.")

(defmacro format-args (url-name args)
  "Format URL-NAME with ARGS."
  `(format ,url-name ,@args))

(defconst netease-music-title
  "* NetEase Music\n %s  等级：%s 听歌数：%s \n私人FM\n%s \n** %s \n%s \n")

(defun format-netease-title (banner-string description)
  "Format netease title with BANNER-STRING & DESCRIPTION."
  (format netease-music-title
          (slot-value admin-ins 'name)
          (slot-value admin-ins 'level)
          (slot-value admin-ins 'listenSongs)
          ""
          banner-string
          description))

(defun set-song-name (tracks)
  "Return song name about this song.
Argument TRACKS is json string."
  (cdr (assoc 'name tracks)))

(defun set-song-id (tracks)
  "Return song id about this song.
Argument TRACKS is json string."
  (cdr (assoc 'id tracks)))

(defun set-artist-name (tracks)
  "Return artist name about this song.
Argument TRACKS is json string."
  (let* ((arlist (cdr (or (assoc 'artists tracks) (assoc 'ar tracks))))
         (count (length arlist))
         (artist-names nil))
    (dotimes (index count artist-names)
      (let ((name (cdr (assoc 'name (aref arlist index)))))
        (setq artist-names (cons name artist-names))))
    (string-join artist-names ", ")))

(defun set-artist-id (tracks)
  "Return artist id about this song.
Argument TRACKS is json string."
  (let* ((count (length (cdr (or (assoc 'artists tracks) (assoc 'ar tracks)))))
         (artist-id (cdr (assoc 'id (aref (cdr (or (assoc 'artists tracks) (assoc 'ar tracks))) 0)))))
    artist-id))

(defun set-album-name (tracks)
  "Return album name about this song.
Argument TRACKS is json string."
  (cdr (assoc 'name (assoc 'al tracks))))

(defun set-album-id (tracks)
  "Return album name about this song.
Argument TRACKS is json string."
  (cdr (assoc 'id (assoc 'al tracks))))

(defun format-song-detail (tracks instance)
  "Use json string TRACKS to initialize an song's INSTANCE."
  (setf (slot-value instance 'name) (set-song-name tracks))
  (setf (slot-value instance 'song-id) (set-song-id tracks))
  (setf (slot-value instance 'artist) (set-artist-name tracks))
  (setf (slot-value instance 'album) (set-album-name tracks))
  (setf (slot-value instance 'artist-id) (set-artist-id tracks))
  (setf (slot-value instance 'album-id) (set-album-id tracks)))

(defun set-artist-id-for-search-result (tracks)
  "Return artist id about this song.
Argument TRACKS is json string."
  (let* ((count (length (cdr (assoc 'artists tracks))))
         (artist-id (cdr (assoc 'id (aref (cdr (assoc 'artists tracks)) 0)))))
    artist-id))

(defun format-song-detail-for-search-result (tracks instance)
  "Use json string TRACKS to initialize an song's INSTANCE."
  (setf (slot-value instance 'name) (set-song-name tracks))
  (setf (slot-value instance 'song-id) (set-song-id tracks))
  (setf (slot-value instance 'artist) (set-artist-name tracks))
  (setf (slot-value instance 'album) (set-album-name tracks))
  (setf (slot-value instance 'artist-id) (set-artist-id-for-search-result tracks)))

(defun set-playlist-name (json)
  "Return playlist name from JSON string."
  (cdr (assoc 'name json)))

(defun set-playlist-description (json)
  "Return playlist description from JSON string."
  (let ((description (cdr (assoc 'description json))))
    (if (equal description nil)
        "暂无歌单简介"
      description)))

(defun set-playlist-userid (json)
  "Return playlist user-id from JSON string."
  (cdr (assoc 'userId json)))

(defun format-playlist-detail (instance json id)
  "Format playlist INSTANCE with JSON string and playlist ID."
  (setf (slot-value instance 'user-id) (set-playlist-userid json))
  (setf (slot-value instance 'name) (set-playlist-name json))
  (setf (slot-value instance 'description) (set-playlist-description json))
  (setf (slot-value instance 'id) id))

(defun set-user-id (json)
  "Return user's id from JSON."
  (cdr (assoc 'id (cdr (assoc 'account json)))))

(defun set-user-nickname (json)
  "Return user nickname from JSON string."
  (cdr (assoc 'nickname (cdr (assoc 'profile json)))))

(defun set-user-level (json)
  "Return user netease-music level from JSON string."
  (cdr (assoc 'level json)))

(defun set-user-listenSongs (json)
  "Retutn user listensongs count from JSON string."
  (cdr (assoc 'listenSongs json)))

(defun set-user-signature (json)
  "Return user signature from JSON.  Default is nil."
  (cdr (assoc 'signature (cdr (assoc 'profile json)))))

(defun set-user-avatar-url (json)
  "Return user avatar-url from JSON."
  (cdr (assoc 'avatarUrl (cdr (assoc 'profile json)))))

(defvar admin-ins (make-instance 'admin)
  "When you login will create a user instance.")

(defun format-user-detail (id)
  "Initialize user details with user ID."
  (let* ((json (request user-detail-url (format-args netease-music-user-detail-args (id)))))
    (setf (slot-value admin-ins 'name) (set-user-nickname json))
    (setf (slot-value admin-ins 'level) (set-user-level json))
    (setf (slot-value admin-ins 'listenSongs) (set-user-listenSongs json))
    (setf (slot-value admin-ins 'signature) (set-user-signature json))))

(defvar user-id nil
  "User id.")

(defvar user-password nil
  "User password.")

(defvar avatar-url nil
  "User avatar url.")

(defun format-request-url (url args)
  "Format request url with URL pattern and ARGS."
  (url-unhex-string (concat api url args)))

(define-derived-mode mode org-mode "netease-music"
  "Key bindings of netease-music-mode.")

(defun request (url-pattern args)
  "Return json by requesting the url.  The url is consists of URL-PATTERN and ARGS."
  (let (json)
    (with-current-buffer (url-retrieve-synchronously
                          (format-request-url url-pattern args))
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (setq json (json-read-from-string
                  (buffer-substring-no-properties (point) (point-max))))
      (kill-buffer (current-buffer)))
    json))

(defun get-playlist ()
  "Format playlist detail to a dict."
  (let* ((json (request play-list-url
                        (format-args netease-music-playlist-args (netease-music-user-id))))
         (detail (cdr (assoc 'playlist json))))
    (setq play-list ())
    (dotimes (i (length detail))
      (let* ((lst (aref detail i))
             (playlist-ins (make-instance 'playlist))
             (list-id (cdr (assoc 'id lst)))
             (name (cdr (assoc 'name lst))))
        (format-playlist-detail playlist-ins lst list-id)
        (push (cons name playlist-ins) play-list))))
  (setq play-list (reverse-list play-list)))

(defun get-playlist-tracks (json)
  "Get tracks from playlist from JSON string."
  (cdr (assoc 'tracks (cdr (assoc 'playlist json)))))

(defun get-song-from-tracks (json index)
  "Get the specific ordered song from JSON string.
Argument: INDEX, the song's order."
  (aref json index))

(defun get-playlist-detail (id)
  "Get playlist's songs through playlist ID."
  (let* ((json (request playlist-detail-url
                        (format-args netease-music-playlist-detail-args (id))))
         (tracks (get-playlist-tracks json)))
    (get-songs-from-tracks tracks)))

(defun get-recommend-songs ()
  "Get recommend songs."
  (let* ((json (request recommend-url ""))
         (tracks (cdr (assoc 'recommend json))))
    (get-songs-from-tracks tracks)))

(defun get-songs-from-tracks (tracks)
  "Get songs from TRACKS."
  (setq songs-list ())
  (dotimes (index (length tracks))
    (let* ((song-json (get-song-from-tracks tracks index))
           (song-name (cdr (assoc 'name song-json)))
           (song-id (cdr (assoc 'id song-json)))
           ;; TODO: get song-id from songs-list
           (song-ins (make-instance 'song)))
      (format-song-detail song-json song-ins)
      (push (cons song-id song-ins) songs-list)))
  (setq songs-list (reverse-list songs-list)))

(defun search ()
  "Search songs.  Multiple keywords can be separated by SPC."
  (interactive)
  (let* ((keywords (read-string "Please input the keywords you want to search: "))
         (json (request search-url
                        (format-args netease-music-search-args (keywords))))
         (songs (cdr (assoc 'songs (cdr (assoc 'result json)))))
         (count (length songs))
         (current-config (current-window-configuration)))
    (setq search-songs-list ())
    (dotimes (index count)
      (let* ((song (get-song-from-tracks songs index))
             (song-name (cdr (assoc 'name song)))
             (song-id (cdr (assoc 'id song)))
             (song-ins (make-instance 'song)))
        (format-song-detail-for-search-result song song-ins)
        (push (cons song-id song-ins) search-songs-list)))
    (setq search-songs-list (reverse-list search-songs-list))
;;; popup window
    (popwin:popup-buffer (get-buffer-create buffer-name-search))
    (switch-to-buffer buffer-name-search)
    (erase-buffer)
    (mode)
    (insert (format-netease-title "Search Results: "
                                  "Press jump-into to listen the song.\nPress add-to-songslist can add to the songs list."))
    (insert "*** Song List:\n")
    (insert (format-playlist-songs-table search-songs-list))))

(defun get-current-playing-artist-songs ()
  "Get current playing song's artist information."
  (interactive)
  (let* ((artist-id (slot-value current-playing-song 'artist-id))
         (json (request artist-details-url
                        (format-args netease-music-artist-details-args (artist-id))))
         (artist-name (cdr (assoc 'name (cdr (assoc 'artist json)))))
         (briefDesc (cdr (assoc 'briefDesc (cdr (assoc 'artist json)))))
         (hot-songs (cdr (assoc 'hotSongs json)))
         (count (length hot-songs))
         (current-config (current-window-configuration)))
    (setq search-songs-list ())
    (dotimes (index count)
      (let* ((song-json (get-song-from-tracks hot-songs index))
             (song-name (cdr (assoc 'name song-json)))
             (song-id (cdr (assoc 'id song-json)))
             (song-ins (make-instance 'song)))
        (setf (slot-value song-ins 'name) (cdr (assoc 'name song-json)))
        (setf (slot-value song-ins 'artist)
              (cdr (assoc 'id
                          (aref (cdr (assoc 'ar song-json)) 0))))
        (setf (slot-value song-ins 'album)
              (cdr (assoc 'name
                          (cdr (assoc 'al song-json)))))
        (setf (slot-value song-ins 'song-id) (cdr (assoc 'id song-json)))
        (setf (slot-value song-ins 'artist-id)
              (cdr (assoc 'id
                          (aref (cdr (assoc 'ar song-json)) 0))))
        (push (cons song-id song-ins) search-songs-list)))
    (popwin:popup-buffer (get-buffer-create buffer-name-search))
    (switch-to-buffer buffer-name-search)
    (erase-buffer)
    (mode)
    (insert (format-netease-title "Artist Description"
                                  briefDesc))
    (insert "*** Artist Best 50 Songs ! \n")
    (insert (format-playlist-songs-table search-songs-list))))

(defun get-current-playing-artist-mvs ()
  "Get current playing artist's mvs."
  (interactive)
  (let* ((artist-id (slot-value current-playing-song 'artist-id))
         (json (request artist-mv-url
                        (format-args netease-music-artist-mv-args (artist-id))))
         (mvs (cdr (assoc 'mvs json)))
         (count (length mvs)))
    (setq mvs-list ())
    (dotimes (index count)
      (let* ((mv-json (aref mvs index))
             (name (cdr (assoc 'name mv-json)))
             (mv-id (cdr (assoc 'id mv-json)))
             (artist-name (cdr (assoc 'name (cdr (assoc 'artist mv-json)))))
             (artist-id (cdr (assoc 'id (cdr (assoc 'artist mv-json)))))
             (publish-time (cdr (assoc 'publish-time mv-json)))
             (mv-ins (make-instance 'mv)))
        (setf (slot-value mv-ins 'name) name)
        (setf (slot-value mv-ins 'mv-id) mv-id)
        (setf (slot-value mv-ins 'artist-name) artist-name)
        (setf (slot-value mv-ins 'artist-id) artist-id)
        (setf (slot-value mv-ins 'publish-time) publish-time)
        (push (cons mv-id mv-ins) mvs-list)
        (setq mvs-list (reverse-list mvs-list))))
    (popwin:popup-buffer (get-buffer-create "netease-music-mv"))
    (switch-to-buffer "netease-music-mv")
    (erase-buffer)
    (mode)
    (insert (format-netease-title "Artist's mv" ""))
    (insert "*** Artist's All mvs \n")
    (insert (format-mvlist-table mvs-list))))

(defun get-lyric (song-id)
  "Return lyric of current song by SONG-ID."
  (let* ((json (request lyric-url
                        (format-args netease-music-lyric-args (song-id))))
         (lrc (cdr (assoc 'lrc json)))
         (lyric (cdr (assoc 'lyric lrc))))
    lyric))

(defun get-song-real-url (id)
  "Return song's real url by song's ID."
  (let* ((json (request song-url
                        (format-args netease-music-song-args (id)))))
    (cdr (assoc 'url (aref (cdr (assoc 'data json)) 0)))))

(defun get-personal-fm ()
  "Get personal f.m. songs."
  (let* ((json (request personal-fm-url ""))
         (data (cdr (assoc 'data json))))
    (setq songs-list ())
    (dotimes (index (length data))
      (let* ((song-json (aref data index))
             (song-name (cdr (assoc 'name song-json)))
             (song-ins (make-instance 'song)))
        (format-song-detail song-json song-ins)
        (push (cons song-name song-ins) songs-list)))))

;;;###autoload
(defun init-frame ()
  "Initial main interface.  When you first login netease-music list all your playlist."
  (interactive)
  (format-user-detail user-id)
  (switch-to-buffer "netease-music")
  (mode)
  (erase-buffer)
  (insert (format-netease-title "Signature:"
                                (find-admin-signature)))
  (get-playlist)
  (insert "\n*** 歌单列表\n")
  (insert (format-playlist-table play-list)))

(defun play-song (song-url)
  "Use player to play song by it's SONG-URL."
  ;; (message song-url)
  (if (and process
           (process-live-p process))
      (kill-process))
  (play song-url))

(defun format-playlist-table (playlist)
  "Format the user's all PLAYLIST."
  (let ((playlist-table ""))
    (dotimes (index (safe-length playlist) playlist-table)
      (setq playlist-table (concat playlist-table
                                   (format "%s\n" (car (elt playlist index))))))))

(defun format-playlist-songs-table (songs)
  "Format the playlist's all SONGS."
  (let ((songs-table ""))
    (dotimes (index (safe-length songs) songs-table)
      (setq songs-table (concat songs-table
                                ;; TODO: change format type
                                (format "[[%s][%s]]\n"
                                        (car (elt songs index)) (slot-value (cdr (elt songs index)) 'name)))))))

(defun format-mvlist-table (mvs)
  "Format the mvs-list's all MVS."
  (let ((mvs-table ""))
    (dotimes (index (safe-length mvs) mvs-table)
      (setq mvs-table (concat mvs-table
                              (format "[[%s][%s]]\n" (car (elt mvs index)) (slot-value (cdr (elt mvs index)) 'name)))))))

(defun find-admin-signature ()
  "Get admin's signature."
  (slot-value admin-ins 'signature))

(defun find-playlist-id (playlist-name)
  "Return playlist id from play-list.
Argument: PLAYLIST-NAME, the playlist's name."
  (setq playlist-ins (assoc-default playlist-name play-list))
  (slot-value playlist-ins 'id))

(defun find-playlist-description (playlist-name)
  "Return playlist description from play-list.
Argument: PLAYLIST-NAME, the playlist's name."
  (setq playlist-ins (assoc-default playlist-name play-list))
  (slot-value playlist-ins 'description))

(defun jump-into-playlist-buffer ()
  "Switch to the playlist buffer whose name is this line's content."
  (interactive)
  (setq playlist-name (get-current-line-content))
  (setq id (find-playlist-id playlist-name))
  (get-buffer-create "netease-music-playlist")
  (get-playlist-detail id)
  (with-current-buffer "netease-music-playlist"
    (erase-buffer)
    (mode)
    (insert (format-netease-title playlist-name
                                  (find-playlist-description playlist-name)))
    (insert "*** Song List:\n")
    (insert (format-playlist-songs-table songs-list))
    (goto-char (point-min)))
  (switch-to-buffer "netease-music-playlist"))

(defun find-song-name (song-id lst)
  "Find song's name which name is SONG-ID from the specific LST."
  (setq song-ins (assoc-default song-id lst))
  (slot-value song-ins 'name))

(defun find-song-id (song-id lst)
  "Find song's id which name is SONG-ID from the specific LST."
  (setq song-ins (assoc-default song-id lst))
  (slot-value song-ins 'song-id))

(defun find-song-album (song-id lst)
  "Find song's album name which name is SONG-ID from the specific LST."
  (setq song-ins (assoc-default song-id lst))
  (slot-value song-ins 'album))

(defun find-song-album-id (song-id lst)
  "Find song's album id which name is SONG-ID from the specific LST."
  (setq song-ins (assoc-default song-id lst))
  (slot-value song-ins 'album-id))

(defun find-song-artist (song-id lst)
  "Find song's artist name which name is SONG-ID from the specific LST."
  (setq song-ins (assoc-default song-id lst))
  (slot-value song-ins 'artist))

(defun find-song-artist-id (song-id lst)
  "Find song's artist id which name is SONG-ID from the specific LST."
  (setq song-ins (assoc-default song-id lst))
  (slot-value song-ins 'artist-id))

(defun jump-into-song-buffer (lst)
  "Switch to the song's buffer whose name is this line's content.
Argument LST: play this song from LST."
  (interactive)
  (let* ((line-content (get-current-line-content))
         (song-id (get-music-id-from-content line-content)))
    (message song-id)
    (play-song-by-id (string-to-number song-id) lst)))

(defun download-and-rewrite-url (album artist song-name song-id download-url)
  "Download the song for SONG-ID, save in ~/Music/ALBUM/ARTIST/SONG-NAME.mp3, from DOWNLOAD-URL."
  (let* ((save-to-mp3-file (format "~/Music/%s/%s/%s.mp3" album artist song-name))
         (save-to-mp3-file-quote (shell-quote-argument save-to-mp3-file))
         (shell-command-str
          (format "netease-music download --album %s --artist %s --song-name %s --song-id %s --url %s"
                  (shell-quote-argument album)
                  (shell-quote-argument artist)
                  (shell-quote-argument song-name)
                  (shell-quote-argument (int-to-string song-id))
                  (shell-quote-argument download-url)))
         (shell-command-result (shell-command-to-string shell-command-str)))
    (if (and (not (string= shell-command-result "")) (file-exists-p shell-command-result))
        shell-command-result
      download-url)))

(defun play-song-by-id (song-id lst)
  "Play a song by the SONG-ID.
Argument LST: play this song from LST."
  (let* ((id (find-song-id song-id lst))
         (song-name (find-song-name song-id lst))
         (artist-id (find-song-artist-id song-id lst))
         (artist (find-song-artist song-id lst))
         (album-id (find-song-album-id song-id lst))
         (album (find-song-album song-id lst))
         (song-real-url (get-song-real-url id))
         (lyric (get-lyric id)))
    (message song-name)
    (get-buffer-create "netease-music-playing")
    (setq current-playing-song (make-instance 'song))
    (format-current-playing-song song-name artist album id artist-id)
    (if (equal song-real-url nil)
        (progn
          (message "Cannot play current song. Don't get the song's real url.")
          (kill-process))
      (progn
        (setq song-real-url (download-and-rewrite-url album artist song-name song-id song-real-url))
        (play-song song-real-url)
        (setq global-mode-string song-name)
        (with-current-buffer "netease-music-playing"
          (erase-buffer)
          (mode)
          (insert (format-netease-title (format "[[https://music.163.com/#/song?id=%d][%s]]" song-id song-name)
                                        (format "Artist: [[https://music.163.com/#/artist?id=%d][%s]]  Album: [[https://music.163.com/#/album?id=%d][%s]]" artist-id artist album-id album)))
          (if lyric lyric (setq lyric "纯音乐"))
          (insert lyric)
          (goto-char (point-min)))))))

(defun move-to-current-song ()
  "Move to current playing song's position."
  (with-current-buffer "netease-music-playlist"
    (goto-char (point-min))
    (search-forward (slot-value current-playing-song 'name))))

(defun jump-into-personal-fm ()
  "Jump into your personal fm songs list."
  (interactive)
  (get-personal-fm)
  (with-current-buffer "netease-music-playlist"
    (erase-buffer)
    (mode)
    (insert (format-netease-title "私人FM"
                                  "你的私人 FM 听完之后再次请求可以获得新的歌曲"))
    (insert "*** Song List:\n")
    (insert (format-playlist-songs-table songs-list))
    (goto-char (point-min))))

(defun jump-into-recommend-songs-playlist ()
  "Switch to the recommend songs playlist buffer."
  (interactive)
  (get-buffer-create "netease-music-playlist")
  (get-recommend-songs)
  (with-current-buffer "netease-music-playlist"
    (erase-buffer)
    (mode)
    (insert (format-netease-title "每日推荐"
                                  "网易云音乐精选每日推荐歌曲"))
    (insert "*** Recommend Songs List:\n")
    (insert (format-playlist-songs-table songs-list))
    (goto-char (point-min))))

(defun play-mv ()
  "Play mv based on current line's content."
  (interactive)
  (let* ((line-content (get-current-line-content))
         (mv-id (netease-music-get-music-id-from-content line-content))
         (mv-ins (assoc-default mv-id mvs-list))
         (mv-url (get-high-value-mv-real-url mv-id)))
    (message mv-url)
    (play-song mv-url)))

(defun get-high-value-mv-real-url (mvid)
  "Get high value mv's real url by MVID."
  (let* ((json (request get-mv-url
                        (format-args netease-music-get-mv-args (mvid))))
         (brs (cdr (assoc 'brs (cdr (assoc 'data json)))))
         (mv-real-url (cdr (nth (- (length brs) 1) brs))))
    mv-real-url))

;;;###autoload
(defun jump-into ()
  "Jump into next buffer based on this line's content."
  (interactive)
  ;; (eval-buffer "music.el")
  (let* ((current-buffer-name (buffer-name)))
    (cond ((equal (get-current-line-content) "私人FM")
           (message "私人FM")
           (jump-into-personal-fm))
          ((equal current-buffer-name "netease-music")
           (message "jump into playlist.")
           (jump-into-playlist-buffer))
          ((equal current-buffer-name "netease-music-playlist")
           (message "jump into song")
           (jump-into-song-buffer songs-list)
           (move-to-current-song))
          ((equal current-buffer-name "netease-music-mv")
           (message "play mv.")
           (play-mv))
          ((equal current-buffer-name "Search Results")
           (message "jump into search-song")
           (jump-into-song-buffer search-songs-list)))))

;;;###autoload
(defun play-loop ()
  (interactive)
  (setq play-loop-flag t))

;;;###autoload
(defun play-prev (jump-n)
  (interactive "p")
  (play-next (- jump-n)))

;;;###autoload
(defun play-start ()
  "Jump to start of this song"
  (interactive)
  (play-next 0))

;;;###autoload
(defun play-next (&optional jump-n)
  "Return next song name in songs-list."
  (interactive "p")
  (when (not jump-n)
    (if play-loop-flag
        (setq jump-n 0)
      (setq jump-n 1)))
  (let* ((current-playing-song-name (slot-value current-playing-song 'name))
         (current-playing-song-id (slot-value current-playing-song 'song-id))
         (next-song-name current-playing-song-name)
         (can-play nil)
         (count (length songs-list))
         (position 0))
    (dotimes (index count next-song-name)
      (let* ((block (nth index songs-list))
             (song-ins (cdr block))
             (song-id (slot-value song-ins 'song-id))
             (song-name (slot-value song-ins 'name)))
        (if (and (equal song-id current-playing-song-id)
                 (< index (- count 1)))
            (progn
              (setq can-play 1)
              (setq position index)))
        (setq next-song-id
              (slot-value (cdr (nth (+ position jump-n) songs-list))
                          'song-id))))
    (setq next-song-name
          (slot-value (cdr (nth (+ position 1) songs-list)) 'name))
    (message next-song-name)
    (if can-play
        (play-song-by-id next-song-id netease-music-songs-list))
    (setq global-mode-string next-song-name)
    (move-to-current-song)))

(defun add-to-songslist (song-ins)
  "Add SONG-INS to songs-list."
  (interactive)
  (let ((id (slot-value song-ins 'song-id)))
    (push (cons id song-ins) songs-list)))

(defun get-current-line-content ()
  "Return current line's content."
  (car (split-string
        (thing-at-point 'line t)
        "\n")))

(defun get-music-id-from-content (line-content)
  "Return current LINE-CONTENT's music id."
  (string-match "[0-9]+" line-content)
  (match-string 0 line-content))

(defun reverse-list (lst)
  "Reverse LST."
  (do ((a lst b)
       (b (cdr lst) (cdr b))
       (c nil a))
      ((atom a) c)
    (rplacd a c)))

(defun i-like-it ()
  "You can add it to your favoriate songs' list if you like it."
  (interactive)
  (let* ((json (request like-url
                        (format-args netease-music-like-args
                                     ((slot-value netease-music-current-playing-song 'song-id)))))
         (code (cdr (assoc 'code json))))
    (if (= 200 code)
        (message "Add to your favorite playlist!")
      (message (format "message code: %s" code)))))

(defun process-live-p (proc)
  "Check netease music PROC is alive."
  (memq (process-status proc)
        '(run open listen connect stop)))

;;;###autoload
(defun play (song-real-url)
  "Play a song by SONG-REAL-URL."
  (unless (and process
               (process-live-p process))
    (setq process
          (start-process "netease-music-proc"
                         nil
                         player
                         (if (string-match player "mplayer")
                             "-slave"
                           "")
                         song-real-url))
    (set-process-sentinel process 'netease-music-proc-sentinel)
    (setq status "playing")))

;;;###autoload
(defun toggle ()
  "Pause song or resume song."
  (interactive)
  (if (string-match status "playing")
      (progn
        (setq status "paused")
        (process-send-string process "pause\n"))
    (if (string-match status "paused")
        (progn
          (setq status "playing")
          (process-send-string process "pause\n")))))

(defun kill-process ()
  "Kill current playing process."
  (when (and process
             (process-live-p process))
    (delete-process process)
    (setq process nil)))

(defun proc-sentinel (proc change)
  "Netease music sentinel for PROC with CHANGE."
  (when (string-match "\\(finished\\|Exiting\\)" change)
    (play-next))))

(provide 'netease-music)
;;; netease-music.el ends here
