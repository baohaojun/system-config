;;; lyrics.el --- Show lyrics                         -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/lyrics.el
;; Package-Version: 20180812.1841
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (seq "2.15"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
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

;; `lyrics.el' interface to download and display songs lyrics.
;;
;; Usage:
;;
;; Add `lyrics.el' somewhere in your `load-path'
;;
;;     M-x lyrics
;;
;; Troubleshooting:
;;
;; + `musixmatch' backend shows a incomplete lyrics
;;
;;   MusixMatch filters requests with an unknown User-Agent header.  Be sure to
;;   check that `url-privacy-level' is not set to "'paranoid".

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (defvar url-http-end-of-headers)
  (defvar url-http-response-status))

(require 'dom)
(require 'seq)
(require 'url-util)


(defgroup lyrics nil
  "Show lyrics."
  :prefix "lyrics-"
  :group 'multimedia)

(defcustom lyrics-directory (expand-file-name "~/.lyrics")
  "Directory where to the lyrics will be stored.

It defaults to `~/.lyrics' since other MPD clients (e.g.  ncmpc)
also use that location."
  :type '(directory :must-match t)
  :group 'lyrics)

(defcustom lyrics-normalize-function 'lyrics-capitalize
  "Function used to normalize lyrics filename."
  :type 'function
  :group 'lyrics)

(defcustom lyrics-backend 'lyrics-lyricswiki
  "Function used to get lyrics.

This function should receive three parameters: Artist, Song,
Buffer (optional), and ultimately call `lyrics-show' to show the
lyrics."
  :type '(radio (function-item lyrics-azlyrics)
                (function-item lyrics-lyricswiki)
                (function-item lyrics-musixmatch)
                (function :tag "Function"))
  :group 'lyrics)

(defcustom lyrics-current-song-function nil
  "Function to fetch the current playing song.

Should return a list of the form: (ARTIST SONG)."
  :type 'function
  :group 'lyrics)

(defface lyrics-face-song
  '((t :inherit font-lock-constant-face))
  "Face for song titles."
  :group 'lyrics)

(defface lyrics-face-artist
  '((t :inherit font-lock-constant-face))
  "Face for song artist."
  :group 'lyrics)

(defface lyrics-face-lyrics
  '((t :inherit default))
  "Face for song lyrics."
  :group 'lyrics)

(defface lyrics-face-item
  '((t :weight bold))
  "Face for lyrics item."
  :group 'lyrics)

(defvar-local lyrics-song nil)
(defvar-local lyrics-artist nil)

(defvar lyrics-song-history nil)
(defvar lyrics-artist-history nil)
(defvar lyrics-buffer-name "*Lyrics*")
(defvar lyrics-newlines-separator 1)

(defconst lyrics-node-tag-ignore '(comment script))

(define-error 'lyrics-error "Unknown lyrics error")
(define-error 'lyrics-not-found "Lyrics not found" 'lyrics-error)
(define-error 'lyrics-backend-error "Lyrics backend error" 'lyrics-error)

(defun lyrics-capitalize (string)
  "Capitalize a STRING."
  ;; XXX: Obviously this is grammatically incomplete https://english.stackexchange.com/a/91
  (cl-labels ((title (str)
                     (concat (upcase (substring str 0 1)) (downcase (substring str 1)))))
    (string-join (mapcar #'title (split-string string)) " ")))

(defun lyrics-clean-blank-lines (string)
  "Clean duplicate blank lines from STRING."
  (replace-regexp-in-string (rx (= 2 "\n" (? space))) "\n" string))

(defun lyrics-cache-filename (artist song)
  "Return a filename used to cache lyrics from a ARTIST SONG."
  (let ((lyrics-file (concat artist " - " song ".txt")))
    (expand-file-name lyrics-file lyrics-directory)))

(defun lyrics-node-text (node &optional separator)
  "Return all the text bits from NODE concatenated by SEPARATOR."
  (string-join (cl-remove-if-not 'stringp (dom-children node)) (or separator " ")))

(defun lyrics-node-texts (node &optional separator)
  "Return all textual data under NODE concatenated with SEPARATOR in-between.

Similar to `dom-texts' but ignores `lyrics-node-tag-ignore' tags."
  (string-join (mapcar
                (lambda (elem)
                  (cond
                   ((stringp elem) elem)
                   ((eq (dom-tag elem) 'br) "\n")
                   ((member (dom-tag elem) lyrics-node-tag-ignore) "")
                   (t (lyrics-node-texts elem separator))))
                (dom-children node))
               (or separator " ")))

(defun lyrics-url-retrieve-parse-xml (buffer)
  "Return the parsed xml from a `url-retrieve' BUFFER response body."
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    (if (fboundp 'libxml-parse-xml-region)
        (libxml-parse-xml-region (1+ (point)) (point-max))
      (signal 'lyrics-error '("Emacs without xml support")))))

(defun lyrics-url-retrieve-parse-html (buffer)
  "Return the parsed html from a `url-retrieve' BUFFER response body."
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    (if (fboundp 'libxml-parse-html-region)
        (libxml-parse-html-region (1+ (point)) (point-max))
      (signal 'lyrics-error '("Emacs without xml support")))))

(defun lyrics-save (artist song lyrics)
  "Save ARTIST SONG LYRICS in `lyrics-directory'."
  (condition-case err
      (with-temp-buffer
        (insert lyrics)
        (write-region nil nil (lyrics-cache-filename artist song) nil 0))
    (file-error (message (error-message-string err)))))

(defun lyrics-read-current-song (&optional ask)
  "Read the artist and song, is ASK is non nil will show a prompt."
  (let (artist song)
    (when (functionp lyrics-current-song-function)
      (with-demoted-errors "Error while loading song/artist with `lyrics-current-song-function': %S"
        (cl-multiple-value-setq (artist song) (funcall lyrics-current-song-function))))
    (if (and (not ask) artist song)
        (list artist song)
      (list (read-string "Artist: " artist 'lyrics-artist-history)
            (read-string "Song: " song 'lyrics-song-history)))))

(defun lyrics-show (artist song lyrics &optional buffer save)
  "Show ARTIST SONG LYRICS in a BUFFER.

If SAVE is non nil will save the lyrics into the `lyrics-directory'."
  (and save (lyrics-save artist song lyrics))
  (with-current-buffer (get-buffer-create (or buffer lyrics-buffer-name))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%s\t\t%s\n" (propertize "Song:" 'face 'lyrics-face-item) (propertize song 'face 'lyrics-face-song))
              (format "%s\t\t%s\n" (propertize "Artist:" 'face 'lyrics-face-item) (propertize artist 'face 'lyrics-face-artist))
              (make-string lyrics-newlines-separator ?\n)
              (propertize lyrics 'face 'lyrics-face-lyrics))
      (goto-char (point-min))
      (lyrics-show-mode)
      (setq lyrics-song song
            lyrics-artist artist)
      (display-buffer (current-buffer)))))

(defun lyrics-show-revert-buffer (_ignore-auto noconfirm)
  "Revert an lyrics buffer.

The function receives two arguments _IGNORE-AUTO and NOCONFIRM,
which are the arguments that `revert-buffer' received."
  (let ((old-pos (point)))
    (when (or noconfirm
              (y-or-n-p (format "Reload lyrics for song \"%s - %s\"? " lyrics-artist lyrics-song)))
      (message "Loading lyrics for song \"%s - %s\"?" lyrics-artist lyrics-song)
      (lyrics lyrics-artist lyrics-song (current-buffer))
      (force-mode-line-update)
      (goto-char old-pos))))

(defvar lyrics-show-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "e") #'lyrics-edit)
    (define-key map (kbd "j") #'next-line)
    (define-key map (kbd "k") #'previous-line)
    (define-key map (kbd "/") #'isearch-forward)
    map)
  "Local keymap for `lyrics-show-mode' buffers.")

(define-derived-mode lyrics-show-mode special-mode "Lyrics"
  "A major mode for showing lyrics.

\\{lyrics-show-mode-map}"
  (setq buffer-auto-save-file-name nil
        truncate-lines t
        buffer-read-only t
        mode-line-buffer-identification
        (list (default-value 'mode-line-buffer-identification)
              " {" 'lyrics-song " - " 'lyrics-artist "}"))
  (setq-local revert-buffer-function #'lyrics-show-revert-buffer))


;;; Lyrics wiki
(defun lyrics-lyricswiki (artist song &optional buffer)
  "Process lyrics from LyricWiki <URL:http://lyrics.wikia.com/>."
  (let ((url (concat "http://lyrics.wikia.com/api.php"
                     "?fmt=xml"
                     "&action=lyrics"
                     "&song=" (url-hexify-string (replace-regexp-in-string " " "_" song))
                     "&artist=" (url-hexify-string (replace-regexp-in-string " " "_" artist)))))
    (url-retrieve url #'lyrics-lyricswiki-api-callback (list artist song buffer))))

(defun lyrics-lyricswiki-api-callback (status artist song &optional buffer)
  "Callback for LyricWiki backend, check if STATUS is erred.

Receives ARTIST, SONG, and the BUFFER to show the lyrics."
  (if (plist-get status :error)
      (message (error-message-string (plist-get status :error)))
    (let* ((tree (lyrics-url-retrieve-parse-xml (current-buffer)))
           (lyrics (cadr (assoc-default 'lyrics (cddr tree))))
           (lyrics-url (cadr (assoc-default 'url (cddr tree)))))
      (if (string= lyrics "Not found")
          (signal 'lyrics-not-found (list artist song))
        (lyrics-lyricswiki-extract lyrics-url artist song buffer)))))

(defun lyrics-lyricswiki-extract (url artist song &optional buffer)
  "Display lyrics lyrics-wiki URL ARTIST SONG in BUFFER."
  (url-retrieve url (lambda (status)
                      (if (plist-get status :error)
                          (message (error-message-string (plist-get status :error)))
                        (let* ((dom (lyrics-url-retrieve-parse-html (current-buffer)))
                               (node (dom-by-class dom "lyricbox"))
                               (lyrics (string-trim (lyrics-clean-blank-lines (lyrics-node-texts node "\n")))))
                          (lyrics-show artist song lyrics buffer 'save))))))


;;; AZLyrics
(defun lyrics-azlyrics (artist song &optional buffer)
  "Process lyrics from AZLyrics <URL:https://www.azlyrics.com/>."
  (url-retrieve (lyrics-azlyrics-url artist song) #'lyrics-azlyrics-page-callback (list artist song buffer)))

(defun lyrics-azlyrics-url (artist song)
  "Return an AZLyrics url for ARTIST SONG."
  (cl-labels ((cleanup (string)
                       (downcase (replace-regexp-in-string "[[:space:][:punct:]]+" "" string))))
    (format "https://www.azlyrics.com/lyrics/%s/%s.html" (cleanup artist) (cleanup song))))

(defun lyrics-azlyrics-page-callback (status artist song &optional buffer)
  "Callback for AZLyrics backend, check if STATUS is erred.

Receives ARTIST, SONG, and the BUFFER to show the lyrics."
  (if-let (err (plist-get status :error))
      (if (= url-http-response-status 404)
          (signal 'lyrics-not-found (list artist song))
        (message (error-message-string err)))
    (let* ((dom (lyrics-url-retrieve-parse-html (current-buffer)))
           (node (seq-find (lambda (node)
                             ;; XXX: text from the first 'div' node which
                             ;; doesn't have any attribute
                             (and (not (stringp node))
                                  (eq (dom-tag node) 'div)
                                  (null (dom-attributes node))))
                           (dom-children (dom-by-class dom "col-xs-12 col-lg-8 text-center"))))
           (lyrics (string-trim (lyrics-clean-blank-lines (lyrics-node-texts node "\n")))))
      (lyrics-show artist song lyrics buffer 'save))))


;;; MusixMatch
(defun lyrics-musixmatch (artist song &optional buffer)
  "Process lyrics from MusicMatch <URL:https://www.musixmatch.com/>."
  (url-retrieve (lyrics-musixmatch-url artist song) #'lyrics-musixmatch-page-callback (list artist song buffer)))

(defun lyrics-musixmatch-url (artist song)
  "Return an url for ARTIST SONG."
  (cl-labels ((cleanup (string)
                       ;; XXX: not "[[:punct:]]" because there are characters should transform to "-"
                       (replace-regexp-in-string "['/[:space:]]+" "-" (string-trim (replace-regexp-in-string "[].,:+!?&()[-]" " " string)))))
    (format "https://www.musixmatch.com/lyrics/%s/%s" (cleanup artist) (cleanup song))))

(defun lyrics-musixmatch-page-callback (status artist song &optional buffer)
  "Callback for MusicMatch backend, check if STATUS is erred.

Receives ARTIST, SONG, and the BUFFER to show the lyrics."
  (if-let (err (plist-get status :error))
      (if (= url-http-response-status 404)
          (signal 'lyrics-not-found (list artist song))
        (message (error-message-string err)))
    (let* ((dom (lyrics-url-retrieve-parse-html (current-buffer)))
           (nodes (dom-by-class dom "mxm-lyrics__content"))
           (lyrics (string-trim (string-join (mapcar #'lyrics-node-texts nodes) "\n\n"))))
      (if (string-blank-p lyrics)
          (signal 'lyrics-not-found (list artist song))
        (lyrics-show artist song lyrics buffer 'save)))))


;;;###autoload
(defun lyrics-edit (artist song)
  "Edit ARTIST SONG LYRICS in `lyrics-directory'."
  (interactive (lyrics-read-current-song current-prefix-arg))
  (find-file (lyrics-cache-filename artist song)))

;;;###autoload
(defun lyrics (artist song &optional buffer)
  "Browse lyrics wiki from ARTIST SONG in BUFFER."
  (interactive (lyrics-read-current-song current-prefix-arg))
  (when (functionp lyrics-normalize-function)
    (setq song (funcall lyrics-normalize-function song)
          artist (funcall lyrics-normalize-function artist)))
  (if (file-exists-p (lyrics-cache-filename artist song))
      (let ((lyrics (with-temp-buffer
                      (insert-file-contents (lyrics-cache-filename artist song))
                      (buffer-string))))
        (lyrics-show artist song lyrics buffer))
    (funcall lyrics-backend artist song buffer)))

(provide 'lyrics)
;;; lyrics.el ends here
