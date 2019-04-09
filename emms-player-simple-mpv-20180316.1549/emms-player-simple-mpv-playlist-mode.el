;;; emms-player-simple-mpv-playlist-mode.el --- Playlist for mpv -*- lexical-binding: t -*-

;; Copyright (C) 2017 momomo5717

;; Author: momomo5717
;; URL: https://github.com/momomo5717/emms-player-simple-mpv

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

;; Setup:
;; (require 'emms-player-simple-mpv-playlist-mode)
;;
;; ;; Some mpv control functions can be added to `emms-player-simple-mpv-playlist-mode-map'.
;; (emms-player-simple-mpv-playlist-mode-setup-keybinds)
;;
;; Usage:
;;
;; M-x emms-player-simple-mpv-playlist-popup
;;
;; `emms-player-simple-mpv-playlist-mode-display-action' will be used
;; for the action argument of `display-buffer'.

;;; Code:
(require 'emms-player-simple-mpv-control-functions)

(defgroup emms-simple-player-mpv-playlist-mode nil
  "Display mpv playlist."
  :group 'emms-simple-player-mpv
  :prefix "emms-simple-player-mpv-playlist-mode-")

(defface emms-simple-player-mpv-playlist-mode-selected-face
  '((t :inherit emms-playlist-selected-face))
  "Face used for the selected entry."
  :group 'emms-simple-player-mpv-playlist-mode)

(defface emms-simple-player-mpv-playlist-mode-entry-face
  '((t :inherit emms-playlist-track-face))
  "Face used for entries."
  :group 'emms-simple-player-mpv-playlist-mode)

(defcustom emms-player-simple-mpv-playlist-mode-display-action
  '((display-buffer-reuse-window
     display-buffer-at-bottom
     display-buffer-pop-up-window)
    (window-height . 0.3))
  "Action for `emms-player-simple-mpv-playlist-popup'.
This will be used for the action arg of `display-buffer'."
  :type (get 'display-buffer-base-action 'custom-type)
  :group 'emms-simple-player-mpv-playlist-mode)

(defcustom emms-player-simple-mpv-playlist-mode-format-function
  #'emms-player-simple-mpv-playlist-mode-default-format-function
  "Function to format an entry.
It takes an entry and the position as arguments.
This will be used for `emms-player-simple-mpv-plm--insert-entry'."
  :type 'function
  :group 'emms-simple-player-mpv-playlist-mode)

(defvar emms-player-simple-mpv-plm--buffer-name " *EMMS mpv Playlist*"
  "Buffer name for mpv playlist.")

(defvar emms-player-simple-mpv-plm--buffer nil
  "Buffer for mpv playlist.")

(defvar emms-player-simple-mpv-plm--mpv-socket nil
  "Store current `emms-player-simple-mpv--socket'.
This will be used as a buffer local variable.")
(make-variable-buffer-local 'emms-player-simple-mpv-plm--mpv-socket)

(defvar emms-player-simple-mpv-plm--wait-response-p nil
  "Wait for the response if non-nil.")

(defun emms-player-simple-mpv-plm--wait-response (&optional id)
  "Set `emms-player-simple-mpv-plm--wait-response-p' to ID.
If ID is nil, `emms-player-simple-mpv--tq-id-counter' will be used.

This function needs to calling before `emms-player-simple-mpv-tq-enqueue'."
  (setq emms-player-simple-mpv-plm--wait-response-p
        (or id emms-player-simple-mpv--tq-id-counter)))

(defun emms-player-simple-mpv-plm--maybe-response-p (id)
  "Return t if ID equals `emms-player-simple-mpv-plm--wait-response-p'.
and set `emms-player-simple-mpv-plm--wait-response-p' to nil.
If ID is an alist, request_id value will be used."
  (when (or (eq emms-player-simple-mpv-plm--wait-response-p id)
            (and (listp id)
                 (eq emms-player-simple-mpv-plm--wait-response-p
                     (cdr (assq 'request_id id)))))
    (setq emms-player-simple-mpv-plm--wait-response-p nil)
    t))

(defvar emms-player-simple-mpv-plm--last-playlist []
  "Last mpv playlist.")

(defvar emms-player-simple-mpv-plm--last-length nil)

(defun emms-player-simple-mpv-plm--last-length (&optional resetp)
  "Get the last playlist length.
Reset it if RESETP is no-nil."
  (if (or resetp (null emms-player-simple-mpv-plm--last-length))
      (setq emms-player-simple-mpv-plm--last-length
            (length emms-player-simple-mpv-plm--last-playlist))
    emms-player-simple-mpv-plm--last-length))

(defvar emms-player-simple-mpv-plm--last-cpos nil
  "The current entry position of the last playlist.")

(defun emms-player-simple-mpv-plm--last-cpos (&optional resetp)
  "Get the current entry position of the last playlist.
Reset it if RESETP is no-nil."
  (if (or resetp (null emms-player-simple-mpv-plm--last-cpos))
      (setq emms-player-simple-mpv-plm--last-cpos
            (cl-position t emms-player-simple-mpv-plm--last-playlist
                         :key (lambda (e) (cdr (assq 'current e)))))
    emms-player-simple-mpv-plm--last-cpos))

(defun emms-player-simple-mpv-plm--set-last-cpos (pos)
  "Set `emms-player-simple-mpv-plm--last-cpos' to POS."
  (let* ((cpos (emms-player-simple-mpv-plm--last-cpos))
         (playlist emms-player-simple-mpv-plm--last-playlist)
         (entry1 (aref playlist cpos))
         (entry2 (aref playlist pos)))
    (aset playlist cpos (dolist (key '(current playing) entry1)
                          (setq entry1 (assq-delete-all key entry1))))
    (aset playlist pos `((current . t) (playing . t) ,@entry2))
    (setq emms-player-simple-mpv-plm--last-cpos pos)))

(defun emms-player-simple-mpv-plm--get-buffer (&optional buffer)
  "Get `emms-player-simple-mpv-plm--buffer-name' buffer.
If BUFFER is no-nil, it will be used."
  (setq emms-player-simple-mpv-plm--buffer
        (or buffer
            (and (buffer-live-p emms-player-simple-mpv-plm--buffer)
                 emms-player-simple-mpv-plm--buffer)
            (get-buffer-create emms-player-simple-mpv-plm--buffer-name))))

(defun emms-player-simple-mpv-playlist-mode-default-format-function (entry position)
  "Return a concatenated string of ENTRY and POSITION."
  (format "%03d: %s"
          position
          (cdr (or (assq 'title entry)
                   (assq 'filename entry)))))

(defun emms-player-simple-mpv-plm--insert-entry (entry position)
  "Insert ENTRY with POSITION."
  (let ((inhibit-read-only t))
    (insert (propertize
             (concat
              (funcall emms-player-simple-mpv-playlist-mode-format-function entry position)
              "\n")
             'mpv-playlist-entry entry
             'mpv-playlist-entry-pos position
             'face (if (eq (cdr (assq 'current entry)) t)
                       'emms-simple-player-mpv-playlist-mode-selected-face
                     'emms-simple-player-mpv-playlist-mode-entry-face)))))

(defun emms-player-simple-mpv-plm--insert-playlist (playlist)
  "Insert PLAYLIST and return point of the current entry."
  (cl-loop with current = nil
           for pos from 0
           for entry across playlist do
           (when (assq 'current entry) (setq current (point)))
           (emms-player-simple-mpv-plm--insert-entry entry pos)
           finally return current))

(defun emms-player-simple-mpv-plm--entry-pos-at (&optional point)
  "Get mpv-playlist-entry-pos property at POINT."
  (get-text-property (or point (point)) 'mpv-playlist-entry-pos))

(defun emms-player-simple-mpv-plm--entry-at (&optional point)
  "Get entry at POINT."
  (get-text-property (or point (point)) 'mpv-playlist-entry))

(defun emms-player-simple-mpv-plm--filename-at (&optional point)
  "Get filename of the entry at POINT."
  (cdr (assq 'filename (emms-player-simple-mpv-plm--entry-at point))))

(defun emms-player-simple-mpv-plm--title-at (&optional point)
  "Get title of the entry at POINT."
  (cdr (assq 'title (emms-player-simple-mpv-plm--entry-at point))))

(defun emms-player-simple-mpv-plm--current-at (&optional point)
  "Get current of the entry at POINT."
  (cdr (assq 'current (emms-player-simple-mpv-plm--entry-at point))))

(defun emms-player-simple-mpv-plm--playing-at (&optional point)
  "Get playing of the entry at POINT."
  (cdr (assq 'playing (emms-player-simple-mpv-plm--entry-at point))))

(defun emms-player-simple-mpv-plm--get-nth-point (n)
  "Return point of the N th entry."
  (with-current-buffer (emms-player-simple-mpv-plm--get-buffer)
    (save-excursion
      (goto-char (point-min))
      (forward-line n)
      (point))))

(defun emms-player-simple-mpv-plm--run-with-playlist (fn)
  "`run-with-timer' FN with playlist."
  (emms-player-simple-mpv--playlist
   nil nil
   (lambda (playlist)
     (setq emms-player-simple-mpv-plm--last-playlist playlist)
     (setq emms-player-simple-mpv-plm--last-length nil)
     (setq emms-player-simple-mpv-plm--last-cpos nil)
     (run-with-timer 0 nil fn playlist))))

(defun emms-player-simple-mpv-plm--reload (playlist &optional goto-currentp)
  "Reload PLAYLIST and return point of the current entry.
Go to the current entry if GOTO-CURRENTP is non-nil."
  (with-current-buffer (emms-player-simple-mpv-plm--get-buffer)
    (let* ((inhibit-read-only t)
           (point (point))
           (selected-winp (eq (current-buffer)
                              (window-buffer (selected-window))))
           (win-start (when selected-winp (window-start)))
           current)
      (erase-buffer)
      (emms-player-simple-mpv-playlist-mode)
      (setq current (emms-player-simple-mpv-plm--insert-playlist playlist))
      (goto-char (if goto-currentp (if current current (point-min)) point))
      (when win-start
        (if goto-currentp
            (recenter)
          (set-window-start (selected-window) win-start t)))
      current)))

(defun emms-player-simple-mpv-plm--refresh (playlist)
  "`erase-buffer' and `insert' PLAYLIST."
  (emms-player-simple-mpv-plm--reload playlist t))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-reload ()
  "Reload mpv playlist buffer."
  (interactive)
  (emms-player-simple-mpv-plm--run-with-playlist
   #'emms-player-simple-mpv-plm--reload))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-refresh ()
  "Refreash mpv playlist buffer."
  (interactive)
  (emms-player-simple-mpv-plm--run-with-playlist
   #'emms-player-simple-mpv-plm--refresh))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-goto-nth (n)
  "Go to the N the entry."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number
            (format "Entry number (0-%d): "
                    (1- (emms-player-simple-mpv-plm--last-length)))))))
  (let ((pt (emms-player-simple-mpv-plm--get-nth-point n)))
    (when pt
      (or (use-region-p) (push-mark))
      (goto-char pt)
      (recenter))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-goto-current ()
  "Go to the current entry."
  (interactive)
  (let ((cpos (emms-player-simple-mpv-plm--last-cpos)))
    (when cpos (emms-player-simple-mpv-playlist-mode-goto-nth cpos))))

(defun emms-player-simple-mpv-plm--update-playlist-pos (pos &optional gotop)
  "Set the POS entry to the current entry for `emms-player-simple-mpv-plm--buffer'.
Go to the begging of the POS if GOTOP is non-nil."
  (with-current-buffer (emms-player-simple-mpv-plm--get-buffer)
    (let* ((inhibit-read-only t)
           (playlist emms-player-simple-mpv-plm--last-playlist)
           (cpos (emms-player-simple-mpv-plm--last-cpos))
           (face1 'emms-simple-player-mpv-playlist-mode-entry-face)
           (face2 'emms-simple-player-mpv-playlist-mode-selected-face)
           (dls (list (- cpos (or (emms-player-simple-mpv-plm--entry-pos-at)
                                  (and (eobp)
                                       (emms-player-simple-mpv-plm--last-length))))
                      (- pos cpos)))
           start)
      (save-excursion
        (emms-player-simple-mpv-plm--set-last-cpos pos)
        (cl-loop for (i . face) in `((,cpos . ,face1) (,pos . ,face2))
                 for dl in dls do
                 (forward-line dl)
                 (setq start (point))
                 (add-text-properties
                  start (next-single-char-property-change start 'mpv-playlist-entry-pos)
                  `(face ,face mpv-playlist-entry ,(aref playlist i)))))
      (when gotop (goto-char start)))))

;;;###autoload
(defun emms-player-simple-mpv-plm-update-playlist-pos (pos)
  "Update playlist-pos(POS) for `emms-player-simple-mpv-plm--buffer'."
  (when (and (buffer-live-p emms-player-simple-mpv-plm--buffer)
             (eq (buffer-local-value 'emms-player-simple-mpv-plm--mpv-socket
                                     emms-player-simple-mpv-plm--buffer)
                 emms-player-simple-mpv--socket)
             (> (emms-player-simple-mpv-plm--last-length) 1))
    (unless (eq pos (emms-player-simple-mpv-plm--last-cpos))
      (emms-player-simple-mpv-plm--update-playlist-pos pos))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-play-at (&optional point)
  "Set playlist-pos to the entry at POINT."
  (interactive)
  (when (eq emms-player-simple-mpv-plm--mpv-socket emms-player-simple-mpv--socket)
    (let ((pos (emms-player-simple-mpv-plm--entry-pos-at (or point (point))))
          (response-id emms-player-simple-mpv--tq-id-counter))
      (when (and pos (not emms-player-simple-mpv-plm--wait-response-p))
        (emms-player-simple-mpv-plm--wait-response response-id)
        (emms-player-simple-mpv-set_property
         "playlist-pos" pos
         :fn (lambda (pos)
               (when (emms-player-simple-mpv-plm--maybe-response-p response-id)
                 (emms-player-simple-mpv-plm-update-playlist-pos pos)))
         :msg nil
         :err-msg (format "set_property playlist-pos %s" pos))))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-remove-at (&optional point)
  "Remove the entry at POINT."
  (interactive)
  (when (eq emms-player-simple-mpv-plm--mpv-socket emms-player-simple-mpv--socket)
    (let ((pos (emms-player-simple-mpv-plm--entry-pos-at (or point (point)))))
      (when pos
        (forward-line 0)
        (emms-player-simple-mpv-tq-enqueue
         `("playlist-remove" ,pos) nil
         (lambda (_ ans-ls)
           (if (emms-player-simple-mpv-tq-success-p ans-ls)
               (emms-player-simple-mpv-playlist-mode-reload)
             (message "mpv playlist-remove %s : %s" pos
                      (cdr (assq 'error ans-ls))))))))))

(defun emms-player-simple-mpv-plm--with-playlist-move (fn index1 index2)
  "Run FN with INDEX1 and INDEX2.
Display error message if FN is nil."
  (unless emms-player-simple-mpv-plm--wait-response-p
    (emms-player-simple-mpv-plm--wait-response)
    (emms-player-simple-mpv-tq-enqueue
     (list "playlist-move" index1 index2) nil
     (lambda (_ ans-ls)
       (when (emms-player-simple-mpv-plm--maybe-response-p ans-ls)
         (if (and fn (emms-player-simple-mpv-tq-success-p ans-ls))
             (funcall fn index1 index2)
           (message "mpv playlist-move %s %s : %s"
                    index1 index2 (cdr (assq 'error ans-ls)))))))))

(defun emms-player-simple-mpv-plm--move-slide (pos1 pos2 &optional up)
  "Slide and insert POS1 entry after POS2 entry if UP is nil.
or Slide and insert POS2 entry before POS1 entry, if UP is non-nil.
and Return point which was at POS1 entry."
  (unless (< pos2 (emms-player-simple-mpv-plm--last-length))
    (error "Invalid value: pos2 or emms-player-simple-mpv-plm--last-length"))
  (with-current-buffer (emms-player-simple-mpv-plm--get-buffer)
    (save-excursion
      (save-restriction
        (forward-line (- pos1 (emms-player-simple-mpv-plm--entry-pos-at)))
        (let* ((inhibit-read-only t)
               (playlist emms-player-simple-mpv-plm--last-playlist)
               (entry1 (aref playlist pos1))
               (entry2 (aref playlist pos2))
               (start (point))
               (dpos (- pos2 pos1)))
          (forward-line (1+ dpos))
          (narrow-to-region start (point))
          (if up
              ;; move-up: pos1 e0 e2 e3 pos2 => pos2 pos1 e0 e2 e3
              (cl-loop for i downfrom pos2 above pos1 do
                       (aset playlist i (aref playlist (1- i)))
                       finally do (aset playlist pos1 entry2))
            ;; move-down: pos1 e0 e2 e3 pos2 => e0 e2 e3 pos2 pos1
            (cl-loop for i from pos1 below pos2 do
                     (aset playlist i (aref playlist (1+ i)))
                     finally do (aset playlist pos2 entry1)))
          (aset playlist (if up pos1 pos2) (if up entry2 entry1))
          (delete-region (point-min) (point-max))
          (goto-char (point-min))
          (cl-loop for i from pos1 to pos2 do
                   (emms-player-simple-mpv-plm--insert-entry (aref playlist i) i))
          start)))))

(defun emms-player-simple-mpv-plm--move-up (index1 index2)
  "Move INDEX1 to INDEX2."
  (let ((pos1 index1)
        (pos2 (if (>= index1 index2) index2 (1- index2))))
    (with-current-buffer (emms-player-simple-mpv-plm--get-buffer)
      (goto-char (emms-player-simple-mpv-plm--move-slide
                  (min pos1 pos2) (max pos1 pos2) (>= index1 index2)))
      (when (< pos1 pos2) (forward-line (- pos2 pos1))))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-move-up (n)
  "Move up the entry at the point N times."
  (interactive "p")
  (when (and (eq emms-player-simple-mpv-plm--mpv-socket emms-player-simple-mpv--socket)
             (null emms-player-simple-mpv-plm--wait-response-p))
    (if (< n 0) (emms-player-simple-mpv-playlist-mode-move-down (abs n))
      (let* ((pos (emms-player-simple-mpv-plm--entry-pos-at (point)))
             (pos2 (max (- pos (if (> n 1) n 1)) 0)))
        (when (and pos (> pos 0))
          (emms-player-simple-mpv-plm--with-playlist-move
           #'emms-player-simple-mpv-plm--move-up
           pos pos2))))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-move-down (n)
  "Move down the entry at the point N times."
  (interactive "p")
  (when (and (eq emms-player-simple-mpv-plm--mpv-socket emms-player-simple-mpv--socket)
             (null emms-player-simple-mpv-plm--wait-response-p))
    (if (< n 0) (emms-player-simple-mpv-playlist-mode-move-up (abs n))
      (let* ((pos (emms-player-simple-mpv-plm--entry-pos-at (point)))
             (pos2 (min (+ pos (if (> n 1) n 1))
                        (1- (emms-player-simple-mpv-plm--last-length)))))
        (when (and pos (not (eq pos pos2)))
          (emms-player-simple-mpv-plm--with-playlist-move
           #'emms-player-simple-mpv-plm--move-up
           pos (1+ pos2)))))))

(defun emms-player-simple-mpv-plm--next (&optional prevp)
  "Run playlist-next and update.
Run playlist-prev and update if PREVP is non-nil."
  (let ((com (if prevp "playlist-prev" "playlist-next")))
    (unless emms-player-simple-mpv-plm--wait-response-p
      (emms-player-simple-mpv-plm--wait-response)
      (emms-player-simple-mpv-tq-enqueue
       (list com)
       com
       (lambda (com ans-ls)
         (when (emms-player-simple-mpv-plm--maybe-response-p ans-ls)
           (if (emms-player-simple-mpv-tq-success-p ans-ls)
               (emms-player-simple-mpv-plm--update-playlist-pos
                (+ (if prevp -1 1) (emms-player-simple-mpv-plm--last-cpos)) t)
             (message "mpv %s : %s" com
                      (cdr (assq 'error ans-ls))))))))))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-next ()
  "Run playlist-next and reload."
  (interactive)
  (emms-player-simple-mpv-plm--next))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-prev ()
  "Run playlist-prev and reload."
  (interactive)
  (emms-player-simple-mpv-plm--next t))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-shuffle ()
  "Shuffle playlist."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("playlist-shuffle") nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (emms-player-simple-mpv-playlist-mode-refresh)
       (message "mpv playlist-shuffle : %s"
                (cdr (assq 'error ans-ls)))))))

(defun emms-player-simple-mpv-plm--shuffle-restart ()
  "Set playlist-pos to 0 and refresh."
  (emms-player-simple-mpv-set_property
   "playlist-pos" 0
   :fn (lambda (_) (emms-player-simple-mpv-playlist-mode-refresh))
   :msg nil
   :err-msg "set_property playlist-pos 0"))

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-shuffle-restart ()
  "Shuffle playlist and Set playlist-pos to 0."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("playlist-shuffle") nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (emms-player-simple-mpv-plm--shuffle-restart)
       (message "mpv playlist-shuffle : %s"
                (cdr (assq 'error ans-ls)))))))

(defvar emms-player-simple-mpv-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'emms-player-simple-mpv-playlist-mode-play-at)
    (define-key map (kbd "d") 'emms-player-simple-mpv-playlist-mode-remove-at)
    (define-key map (kbd "g") 'emms-player-simple-mpv-playlist-mode-reload)
    (define-key map (kbd "<M-up>") 'emms-player-simple-mpv-playlist-mode-move-up)
    (define-key map (kbd "<M-down>") 'emms-player-simple-mpv-playlist-mode-move-down)
    (define-key map (kbd ">") 'emms-player-simple-mpv-playlist-mode-next)
    (define-key map (kbd "<") 'emms-player-simple-mpv-playlist-mode-prev)
    (define-key map (kbd "r") 'emms-player-simple-mpv-playlist-mode-shuffle)
    (define-key map (kbd "R") 'emms-player-simple-mpv-playlist-mode-shuffle-restart)
    (define-key map (kbd ".") 'emms-player-simple-mpv-playlist-mode-refresh)
    (define-key map (kbd "c") 'emms-player-simple-mpv-playlist-mode-goto-current)
    map)
  "Keymap used in EMMS MPV playlist.")

;;;###autoload
(defun emms-player-simple-mpv-playlist-mode-setup-keybinds ()
  "Set some control functions to `emms-player-simple-mpv-playlist-mode-map'."
  (let ((map emms-player-simple-mpv-playlist-mode-map))
    (define-key map (kbd "m") 'emms-player-simple-mpv-mute)
    (define-key map (kbd "p") 'emms-pause)
    (define-key map (kbd "<left>")    (lambda () (interactive) (emms-seek -5)))
    (define-key map (kbd "<right>")   (lambda () (interactive) (emms-seek 5)))
    (define-key map (kbd "S-<left>")  (lambda () (interactive) (emms-seek -1)))
    (define-key map (kbd "S-<right>") (lambda () (interactive) (emms-seek 1)))
    (define-key map (kbd "[") 'emms-player-simple-mpv-speed-decrease)
    (define-key map (kbd "]") 'emms-player-simple-mpv-speed-increase)
    (define-key map (kbd "{") 'emms-player-simple-mpv-speed-halve)
    (define-key map (kbd "}") 'emms-player-simple-mpv-speed-double)
    (define-key map (kbd "<backspace>") 'emms-player-simple-mpv-speed-normal)
    (define-key map (kbd "T") 'emms-player-simple-mpv-ontop)
    (define-key map (kbd "f") 'emms-player-simple-mpv-fullscreen)
    (define-key map (kbd "9") 'emms-volume-lower)
    (define-key map (kbd "0") 'emms-volume-raise)
    (define-key map (kbd "l") 'emms-player-simple-mpv-ab-loop)))

;;;###autoload
(defun emms-player-simple-mpv-playlist-popup (&optional action frame)
  "Popup mpv playlist buffer.
ACTION and FRAME will be used as arguments for `display-buffer'.
If ACTION is nil,
`emms-player-simple-mpv-playlist-mode-display-action' will be uesd."
  (interactive)
  (emms-player-simple-mpv-plm--run-with-playlist
   (lambda (playlist)
     (let ((current (emms-player-simple-mpv-plm--reload playlist))
           (win (display-buffer
                 (emms-player-simple-mpv-plm--get-buffer)
                 (or action
                     emms-player-simple-mpv-playlist-mode-display-action)
                 frame)))
       (select-window win)
       (goto-char current)
       (recenter)))))

;;;###autoload
(define-derived-mode emms-player-simple-mpv-playlist-mode special-mode
  "EMMS mpv playlist"
  "Major mode for displaying mpv playlist.

\\{emms-player-simple-mpv-playlist-mode-map}"

  (buffer-disable-undo)
  (setq-local emms-player-simple-mpv-plm--mpv-socket
              emms-player-simple-mpv--socket)
  (setq-local truncate-lines t)
  (setq emms-player-simple-mpv-plm--wait-response-p nil)
  (add-hook 'emms-player-simple-mpv-tq-event-playlist-pos-functions
            'emms-player-simple-mpv-plm-update-playlist-pos))

(provide 'emms-player-simple-mpv-playlist-mode)
;;; emms-player-simple-mpv-playlist-mode.el ends here
