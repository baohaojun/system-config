;; -*- Emacs-Lisp -*-
;;; douban-music-mode.el ---
;; Time-stamp: <2013-05-03 15:52:28 Friday by lzy>

;; Copyright (C) 2013 zhengyu li
;;
;; Author: zhengyu li <lizhengyu419@gmail.com>
;; Keywords: douban fm client for emacs

;; This file is not part of GNU Emacs.

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

;; 2012-05-03:  zhengyu li <lizhengyu419@gmail.com>
;;   the first version

;; Put this file into your load-path and the following into your ~/.emacs:
;;
;; Installation:
;; add the following lines into your configuration file
;;
;;   (autoload 'douban-music "douban-music-mode" nil t)
;;
;;; Code:

(require 'json)
(require 'assoc)
(require 'url-http)

(defgroup douban-music nil
  "douban music group"
  :group 'entertainment)

(defcustom douban-music-mode-map nil
  "douban music map"
  :group 'douban-music)

(defcustom douban-music-buffer-name "Douban Music"
  "douban music buffer name"
  :group 'douban-music)

(defcustom douban-music-cache-directory "~/.emacs.d/DouBanMusic/"
  "cache directory of douban music mode"
  :group 'douban-music)

(defcustom douban-music-track-face '((t (:height 1.2 :foreground "Grey70")))
  "face for douban music tags"
  :group 'douban-music)

(defcustom douban-music-item-face '((t (:height 1.1 :foreground "Steelblue1")))
  "face for douban music tags"
  :group 'douban-music)

(defcustom douban-music-title-face '((t (:height 1.05 :foreground "Yellow")))
  "face for douban music title"
  :group 'douban-music)

(defcustom douban-music-artist-face '((t (:height 1.05 :foreground "Red3")))
  "face for douban music artist"
  :group 'douban-music)

(defcustom douban-music-publish-year-face '((t (:height 1.05 :foreground "Green")))
  "face for douban music publish year"
  :group 'douban-music)

(defconst douban-music-get-channels-url "http://www.douban.com/j/app/radio/channels"
  "douban channel to retrive url.")
(defconst douban-music-get-new-song-list-url "http://www.douban.com/j/app/radio/people?app_name=radio_desktop_win&version=100&channel=%s&type=n"
  "song list to fetch from douban music server.")

(defvar douban-music-channels nil "channels for douban music.")
(defvar douban-music-current-channel 0 "current channel for douban music, default is 0.")
(defvar douban-music-song-list nil "song list for current channel.")
(defvar douban-music-current-song 0 "song currently playing.")
(defvar douban-music-indent1 "  " "1-level indentation")
(defvar douban-music-indent2 "          " "2-level indentation")
(defvar douban-music-indent3 "      " "3-level indentation")

(setq douban-music-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "RET") 'douban-music-goto-current-playing)
        (define-key map "s" 'douban-music-stop)
        (define-key map "g" 'douban-music-refresh)
        (define-key map "j" 'douban-music-goto-current-playing)
        (define-key map "c" 'douban-music-set-channel)
        (define-key map "n" 'douban-music-play-next)
        (define-key map "p" 'douban-music-play-previous)
        (define-key map "q" 'douban-music-bury-buffer)
        (define-key map "x" 'douban-music-quit)
        map))

(defun douban-music-display ()
  (douban-music-get-channels)
  (douban-music-get-new-song-list)
  (douban-music-kill-process)
  (douban-music-play))

(defun douban-music-stop ()
  (interactive)
  (douban-music-kill-process))

(defun douban-music-refresh ()
  (interactive)
  (douban-music-get-new-song-list)
  (douban-music-kill-process)
  (douban-music-play))

(defun douban-music-goto-current-playing ()
  (interactive)
  (douban-music-song-list-update))

(defun douban-music-set-channel (channel-number)
  (interactive "nChannel number:")
  (if (assoc channel-number douban-music-channels)
      (progn
        (setq douban-music-current-channel channel-number)
        (message (format "Change to channel: %s" (cdr (assoc channel-number douban-music-channels))))
        (douban-music-refresh))
    (message "Warnning: not exist channel")))

(defun douban-music-play-next ()
  (interactive)
  (douban-music-kill-process)
  (douban-music-get-next-song)
  (douban-music-play))

(defun douban-music-play-next-refresh ()
  (interactive)
  (let ((previous-song douban-music-current-song))
    (douban-music-kill-process)
    (douban-music-get-next-song)
    (if (> previous-song douban-music-current-song)
        (progn (douban-music-refresh))
      (douban-music-play))))

(defun douban-music-play-previous ()
  (interactive)
  (douban-music-kill-process)
  (douban-music-get-previous-song)
  (douban-music-play))

(defun douban-music-current-song-info ()
  (interactive)
  (princ douban-music-current-song))

(defun douban-music-bury-buffer ()
  (interactive)
  (when (eq major-mode 'douban-music-mode)
    (if (fboundp 'quit-window)
        (quit-window)
      (bury-buffer))))

(defun douban-music-quit ()
  (interactive)
  (when (eq major-mode 'douban-music-mode)
    (douban-music-stop)
    (kill-buffer (current-buffer))))

(defun douban-music-play ()
  (let (song)
    (setq song (elt douban-music-song-list douban-music-current-song))
    (if (not song)
        (error "Get song from song list failed"))
    (douban-music-song-list-update)
    (set-process-filter
     (start-process "douban-music-proc" nil "mpg123" (aget song 'url))
     'douban-music-proc-filter)))

(defun douban-music-proc-filter (proc string)
  (if (string-match "finished" string)
      (progn
        (douban-music-kill-process)
        (douban-music-play-next-refresh))))

(defun douban-music-get-previous-song ()
  (if (null douban-music-song-list)
      (error "song list is null")
    (setq douban-music-current-song (mod (- douban-music-current-song 1)
                                         (length douban-music-song-list)))))

(defun douban-music-get-next-song ()
  (if (null douban-music-song-list)
      (error "song list is null")
    (setq douban-music-current-song (mod (+ douban-music-current-song 1)
                                         (length douban-music-song-list)))))

(defun douban-music-kill-process ()
  (dolist (elt (process-list))
    (if (string-match "douban-music-proc<?[0-9]*>?" (process-name elt))
        (delete-process elt))))

(defun douban-music-get-channels ()
  "Get channels from douban music server"
  (let ((json-buffer (douban-music-send-url douban-music-get-channels-url))
        jason-start json-end jason)
    (with-current-buffer json-buffer
      (goto-char (point-min))
      (if (not (search-forward "channels"))
          (message "get channels failed")
        (setq json-start (line-beginning-position))
        (setq json-end (line-end-position))
        (setq json (cdr (assoc 'channels
                               (json-read-from-string (decode-coding-string
                                                       (buffer-substring-no-properties
                                                        json-start json-end) 'utf-8)))))
        (if (not (vectorp json))
            (error "Invalid data format")
          (setq douban-music-channels nil)
          (dotimes (i (length json))
            (let ((var (aref json i)))
              (setq douban-music-channels
                    (cons
                     (cons (cdr (assoc 'channel_id var))
                           (cdr (assoc 'name var)))
                     douban-music-channels))))
          (setq douban-music-channels
                (sort douban-music-channels
                      (lambda (el1 el2)
                        (< (car el1) (car el2))))))))))

(defun douban-music-get-new-song-list ()
  "Get channels from douban music server"
  (let* ((url (if douban-music-current-channel
                  (format douban-music-get-new-song-list-url douban-music-current-channel)
                (error "douban-music-current-channel is nil")))
         (json-buffer (douban-music-send-url url))
         jason-start json-end jason)
    (with-current-buffer json-buffer
      (goto-char (point-min))
      (if (not (search-forward "song"))
          (message "get song list failed")
        (setq json-start (line-beginning-position))
        (setq json-end (line-end-position))
        (setq json (cdr (assoc 'song
                               (json-read-from-string (decode-coding-string
                                                       (buffer-substring-no-properties json-start json-end)
                                                       'utf-8)))))
        (if (not (vectorp json))
            (error "Invalid data format")
          (setq douban-music-song-list nil)
          (setq douban-music-current-song 0)
          (dotimes (i (length json))
            (let ((var (aref json i)))
              (setq douban-music-song-list
                    (cons var douban-music-song-list))
              (douban-music-download-image-file (aget var 'picture)))))))))

(defun douban-music-song-list-update ()
  (let ((channels "")
        (channel-list douban-music-channels)
        (counter 0)
        song title artist
        public-time image song-info)
    (with-current-buffer
        douban-music-buffer-name
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (concat (propertize "豆瓣" 'face '((t (:height 1.3 :foreground "Grey70"))))
                      (propertize "FM" 'face '((t (:height 1.4 :foreground "ForestGreen"))))
                      (propertize " douban.fm\n\n" 'face '((t (:height 0.8 :foreground "grey70" :))))))
      (insert (propertize "  Channels:\n  ======================================================================================================\n"
                          'face '((t (:foreground "Grey80" :height 1.05)))))
      (while channel-list
        (if (zerop (mod counter 7))
            (progn
              (if (not (string-equal channels ""))
                  (progn
                    (insert channels)
                    (insert "\n")))
              (setq channels "  ")))
        (setq channels (concat channels (concat
                                         (propertize (format "%-3d" (car (car channel-list))) 'face '((t (:foreground "Green"))))
                                         (propertize (format "%-10s " (cdr (car channel-list))) 'face '((t (:foreground "Grey80")))))))
        (setq counter (1+ counter))
        (setq channel-list (cdr channel-list)))
      (if (not (string-equal channels ""))
          (progn
            (insert channels)))
      (insert (propertize "\n  ======================================================================================================\n"
                          'face '((t (:foreground "Grey80" :height 1.05)))))
      (insert "\n")
      (setq song (elt douban-music-song-list douban-music-current-song))
      (if song
          (progn
            (setq image (make-image-file-name (aget song 'picture)))
            (insert douban-music-indent3)
            (douban-music-insert-image image)
            (insert "\n"))
        (error "current song is nil"))
      (insert (concat (propertize "\n Current channel: " 'face douban-music-track-face)
                      (propertize (format "%s\n" (cdr (assoc douban-music-current-channel douban-music-channels))) 'face douban-music-publish-year-face)))
      (dotimes (i (length douban-music-song-list))
        (setq song (elt douban-music-song-list i))
        (setq title (aget song 'title))
        (setq artist (aget song 'artist))
        (setq public-time (aget song 'public_time))
        (setq song-info (concat (propertize (format "\n%sTrack%2d " douban-music-indent1 i)
                                            'face douban-music-track-face)
                                (propertize "Title: " 'face douban-music-item-face)
                                (propertize (format "%s\n" title) 'face douban-music-title-face)
                                (propertize (format "%sArtist: " douban-music-indent2)
                                            'face douban-music-item-face)
                                (propertize (format "%s\n" artist) 'face douban-music-artist-face)
                                (propertize (format "%sPublish Year: " douban-music-indent2)
                                            'face douban-music-item-face)
                                (propertize (format "%s" public-time) 'face douban-music-publish-year-face)))
        (insert song-info))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (search-forward (format "Track%2d" douban-music-current-song))
      (goto-char (line-end-position)))))

(defun douban-music-send-url (url &optional args)
  "Get data from douban music server"
  (let ((url-request-method "GET")
        (url-request-data (mapconcat (lambda (arg)
                                       (concat (url-hexify-string (car arg))
                                               "="
                                               (url-hexify-string (cdr arg))))
                                     args "&")))
    (url-retrieve-synchronously url)))

(defun douban-music-insert-image (image-file)
  (when image-file
    (condition-case err
        (let ((img (create-image image-file)))
          (insert-image img)
          img)
      (error
       (when (file-exists-p image-file)
         (delete-file image-file))
       nil))))

(defun douban-music-download-image-file (url)
  (let ((image-file (make-image-file-name url)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (let ((end (search-forward "\n\n" nil t)))
        (when end
          (delete-region (point-min) end)
          (setq buffer-file-coding-system 'no-conversion)
          (write-region (point-min) (point-max) image-file nil 0)))
      (kill-buffer))
    image-file))

(defun make-image-file-name (url)
  (let ((image-file-name (file-name-nondirectory
                          (url-filename (url-generic-parse-url url)))))
    (expand-file-name image-file-name douban-music-cache-directory)))

;;;###autoload
(defun douban-music ()
  "Play douban music in its own buffer."
  (interactive)
  (cond
   ((buffer-live-p (get-buffer douban-music-buffer-name))
    (set-buffer douban-music-buffer-name))
   (t
    (set-buffer (get-buffer-create douban-music-buffer-name))
    (douban-music-mode)
    (setq buffer-read-only t)
    (setq truncate-lines t)))
  (if (not (file-exists-p douban-music-cache-directory))
      (mkdir douban-music-cache-directory t))
  (douban-music-display)
  (set-buffer-modified-p nil)
  (select-window (display-buffer (current-buffer)))
  (delete-other-windows))

(defun douban-music-mode ()
  "Major mode for controlling the Douban Music buffer.

This buffer used to show douban music play under emacs."
  (kill-all-local-variables)
  (use-local-map douban-music-mode-map)
  (setq major-mode 'douban-music-mode)
  (setq mode-name "Douban-Music")
  (setq buffer-undo-list t)
  (run-hooks 'douban-music-mode-hook))

;;; provide features
(provide 'douban-music-mode)

;;; douban-music-mode.el ends here
