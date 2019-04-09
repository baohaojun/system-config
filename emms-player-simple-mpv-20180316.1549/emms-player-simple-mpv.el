;;; emms-player-simple-mpv.el --- An extension of emms-player-simple.el for mpv JSON IPC  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 momomo5717

;; Keywords: emms, mpv
;; Version: 0.4.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (emms "4.0"))
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
;;
;; This is an extension of emms-player-simple.el for mpv JSON IPC.
;; It provides macros and functions for defining emms simple players of mpv.
;; emms-player-simple-mpv-control-functions.el provides other functions to control mpv.
;;
;; Further information is available from:
;; https://github.com/momomo5717/emms-player-simple-mpv
;;
;;
;; Other Requirements:
;;
;;   + mpv v0.10.0 or later
;;   + Unix Sockets
;;
;; Setup:
;;
;; (require 'emms-player-simple-mpv)
;; ;; This plugin provides control functions (e.g. ab-loop, speed, fullscreen).
;; (require 'emms-player-simple-mpv-control-functions)
;;
;; Usage:
;;
;; ;; An example of setting like emms-player-mplayer.el
;; ;; `emms-player-my-mpv' is defined in this case.
;; (define-emms-simple-player-mpv my-mpv '(file url streamlist playlist)
;;     (concat "\\`\\(http[s]?\\|mms\\)://\\|"
;;             (apply #'emms-player-simple-regexp
;;                    "aac" "pls" "m3u"
;;                    emms-player-base-format-list))
;;     "mpv" "--no-terminal" "--force-window=no" "--audio-display=no")
;;
;; (emms-player-simple-mpv-add-to-converters
;;  'emms-player-my-mpv "." '(playlist)
;;  (lambda (track-name) (format "--playlist=%s" track-name)))
;;
;; (add-to-list 'emms-player-list 'emms-player-my-mpv)
;;
;; ;; Playing YouTube playlist in reverse order.
;; `emms-player-my-mpv-ytpl-reverse' will be defined in this case.
;; (define-emms-simple-player-mpv my-mpv-ytpl-reverse '(url)
;;   "\\`http[s]://www\\.youtube\\.com/playlist\\?list="
;;   "mpv" "--no-terminal" "--force-window=no" "--audio-display=no"
;;   "--ytdl" "--ytdl-raw-options=playlist-reverse=")
;;
;; (add-to-list 'emms-player-list 'emms-player-my-mpv-ytpl-reverse)
;;
;; ;; M-x emms-player-simple-mpv-playlist-popup can display playlist
;;
;; The following example configuration files are available:
;;
;;   + emms-player-simple-mpv-e.g.time-display.el
;;   + emms-player-simple-mpv-e.g.playlist-fname.el
;;   + emms-player-simple-mpv-e.g.hydra.el

;;; Code:

(require 'emms-player-simple)
(require 'emms-volume)
(require 'cl-lib)
(require 'json)
(require 'tq)
(require 'later-do)

(defconst emms-player-simple-mpv-version "0.4.0")

(defgroup emms-simple-player-mpv nil
  "An extension of emms-simple-player.el."
  :group 'emms-player
  :prefix "emms-simple-player-mpv-")

(defcustom emms-player-simple-mpv-ipc-option-name nil
  "IPC option name.
Renamed --input-unix-socket to --input-ipc-server since mpv v0.17.0"
  :group 'emms-simple-player-mpv
  :type '(choice (const :tag "Set automatically" nil)
                 (const :tag "mpv v0.17.0 or later" "--input-ipc-server")
                 (const :tag "mpv v0.10.0 to v0.16.0 " "--input-unix-socket")))

;;;###autoload
(defun emms-player-simple-mpv-get-version ()
  "Return mpv version."
  (with-temp-buffer
    (unless (zerop (call-process "mpv" nil t "--verison"))
      (error "Failed to get mpv version"))
    (goto-char (point-min))
    (if (re-search-forward
         "^mpv\\s-+\\(\\([0-9]+\\.\\)\\{2\\}[0-9]+\\)[^0-9]" nil t)
        (match-string-no-properties 1)
      (error "Failed to get mpv version"))))

(defun emms-player-simple-mpv-get-ipc-option-name ()
  "Return ipc option name."
  (if (version< (emms-player-simple-mpv-get-version) "0.17.0")
      "--input-unix-socket"
    "--input-ipc-server"))

(defcustom emms-player-simple-mpv-ipc-dir nil
  "Directory name for IPC.
If nil, `temporary-file-directory' will be used."
  :group 'emms-simple-player-mpv
  :type '(choice (const :tag "Use `temporary-file-directory'." nil)
                 (directory :tag "Directory name.")))

(defvar emms-player-simple-mpv-ipc-fname-prefix "mpv--socket"
  "File name prefix for IPC.
This variable will be used with `make-temp-name'.")

(defcustom emms-player-simple-mpv-use-volume-change-function-p t
  "If non-nil, `emms-player-simple-mpv-volume-change' is used as `emms-volume-change-function'."
  :group 'emms-simple-player-mpv
  :type 'boolean)

(defvar emms-player-simple-mpv-default-volume-function emms-volume-change-function
  "Set emms-volume-change-function for buckup.")

(defcustom emms-player-simple-mpv-use-start-tq-error-message-p t
  "If non-nil, display error message when failed to start tq process."
  :group 'emms-simple-player-mpv
  :type 'boolean)

(defcustom emms-player-simple-mpv-tq-event-pause-hook nil
  "Normal hook run when TQ process receives \"pause\" from mpv."
  :group 'emms-simple-player-mpv
  :type 'hook)

(defcustom emms-player-simple-mpv-tq-event-unpause-hook nil
  "Normal hook run when TQ process receives \"unpause\" from mpv."
  :group 'emms-simple-player-mpv
  :type 'hook)

(defcustom emms-player-simple-mpv-tq-event-playback-restart-hook nil
  "Normal hook run when TQ process receives \"playback-restart\" from mpv."
  :group 'emms-simple-player-mpv
  :type 'hook)

(defcustom emms-player-simple-mpv-tq-event-filename-functions nil
  "Abnormal hook run with one argument which is filename."
  :group 'emms-simple-player-mpv
  :type 'hook)

(defcustom emms-player-simple-mpv-tq-event-volume-functions nil
  "Abnormal hook run with one argument which is volume."
  :group 'emms-simple-player-mpv
  :type 'hook)

(defvar emms-player-simple-mpv-last-volume nil
  "Last volume value.")

(add-hook 'emms-player-simple-mpv-tq-event-volume-functions
          (lambda (vol) (setq emms-player-simple-mpv-last-volume vol)))

(defcustom emms-player-simple-mpv-tq-event-speed-functions nil
  "Abnormal hook run with one argument which is speed."
  :group 'emms-simple-player-mpv
  :type 'hook)

(defvar emms-player-simple-mpv-last-speed nil
  "Last speed value.")

(add-hook 'emms-player-simple-mpv-tq-event-speed-functions
          (lambda (speed) (setq emms-player-simple-mpv-last-speed speed)))

(defcustom emms-player-simple-mpv-tq-event-duration-functions nil
  "Abnormal hook run with one argument which is duration."
  :group 'emms-simple-player-mpv
  :type 'hook)

(define-obsolete-variable-alias 'emms-player-simple-mpv-tq-event-length-functions
  'emms-player-simple-mpv-tq-event-duration-functions
  "20170930")

(defcustom emms-player-simple-mpv-tq-event-playlist-pos-functions nil
  "Abnormal hook run with one argument which is playlist-pos."
  :group 'emms-simple-player-mpv
  :type 'hook)

(defcustom emms-player-simple-mpv-tq-event-property-change-functions-alist
  (list '("filename" . emms-player-simple-mpv-tq-event-filename-functions)
        '("volume" . emms-player-simple-mpv-tq-event-volume-functions)
        '("speed" . emms-player-simple-mpv-tq-event-speed-functions)
        '("duration" . emms-player-simple-mpv-tq-event-duration-functions)
        '("playlist-pos" . emms-player-simple-mpv-tq-event-playlist-pos-functions))
  "Alist of property name and abnormal hook.
Abnormal hook run with one argument for data
when TQ process receives \"property-change\" from mpv."
  :group 'emms-simple-player-mpv
  :type '(alist :key-type string :value-type symbol))

(defcustom emms-player-simple-mpv-keep-properties
  (list '("volume"
          emms-player-simple-mpv-keep-volume-available-p
          emms-player-simple-mpv-last-volume)
        '("speed"
          emms-player-simple-mpv-keep-speed-available-p
          emms-player-simple-mpv-last-speed))
  "Alist of property name , function and symbol which has the last value.
The function takes no arguments and returns boolean."
  :group 'emms-simple-player-mpv
  :type '(alist :key-type string (group function symbol)))

(define-minor-mode emms-player-simple-mpv-keep-volume-mode
  "Last volume value is used when new track starts."
  :group 'emms-simple-player-mpv
  :global t)

(defun emms-player-simple-mpv-keep-volume-available-p ()
  "Return t if keep-volume-mode is t and last-volume is available."
  (and emms-player-simple-mpv-keep-volume-mode
       (emms-player-simple-mpv-last-volume-available-p)))

(defun emms-player-simple-mpv-last-volume-available-p ()
  "Retrun t if `emms-player-simple-mpv-last-volume' is available."
  (and (numberp emms-player-simple-mpv-last-volume)
       (<= 0 emms-player-simple-mpv-last-volume)))

(define-minor-mode emms-player-simple-mpv-keep-speed-mode
  "Last speed value is used when new track starts."
  :group 'emms-simple-player-mpv
  :global t)

(defun emms-player-simple-mpv-keep-speed-available-p ()
  "Return t if keep-speed-mode is t and last-speed is available.
Only track type of file is available."
  (when (eq (emms-track-type (emms-playlist-current-selected-track))
            'file)
    (and emms-player-simple-mpv-keep-speed-mode
         (emms-player-simple-mpv-last-speed-available-p))))

(defun emms-player-simple-mpv-last-speed-available-p ()
  "Retrun t if `emms-player-simple-mpv-last-speed' is available."
  (and (numberp emms-player-simple-mpv-last-speed)
       (<= 0.01 emms-player-simple-mpv-last-speed)
       (<= emms-player-simple-mpv-last-speed 100)))

;;;###autoload
(defmacro define-emms-simple-player-mpv (name types regex command &rest args)
  "Extension of `define-emms-simple-player' for mpv JSON IPC."
  (let ((group         (intern (format "emms-player-%s"              name)))
        (command-name  (intern (format "emms-player-%s-command-name" name)))
        (parameters    (intern (format "emms-player-%s-parameters"   name)))
        (player-name   (intern (format "emms-player-%s"              name)))
        (start         (intern (format "emms-player-%s-start"        name)))
        (stop          (intern (format "emms-player-%s-stop"         name)))
        (playablep     (intern (format "emms-player-%s-playable-p"   name))))
  `(progn
     (defgroup ,group nil
       ,(format "EMMS player for %s." command)
       :group 'emms-player
       :prefix ,(format "emms-player-%s-" name))
     (defcustom ,command-name ,command
       ,(format "*The command name of %s." command)
       :type  'string
       :group ',group)
     (defcustom ,parameters ',args
       ,(format "*The arguments to `%s'." command-name)
       :type  '(repeat string)
       :group ',group)
     (defcustom ,player-name (emms-player ',start ',stop ',playablep)
       "*A player for EMMS."
       :type '(cons symbol alist)
       :group ',group)
     (emms-player-set ,player-name 'regex   ,regex)
     (emms-player-set ,player-name 'pause   'emms-player-simple-mpv-pause)
     (emms-player-set ,player-name 'resume  'emms-player-simple-mpv-unpause)
     (emms-player-set ,player-name 'seek    'emms-player-simple-mpv-seek)
     (emms-player-set ,player-name 'seek-to 'emms-player-simple-mpv-seek-to)
     (emms-player-set ,player-name 'mpv-track-name-converters '())
     (emms-player-set ,player-name 'mpv-start-process-function
                      'emms-player-simple-mpv-default-start-process)
     (defun ,start (track)
       "Start the player process."
       (emms-player-simple-mpv-start track
                                     ,player-name
                                     ,command-name
                                     ,parameters))
     (defun ,stop ()
       "Stop the player process."
       (emms-player-simple-stop))
     (defun ,playablep (track)
       "Return non-nil when we can play this track."
       (and (executable-find ,command-name)
            (memq (emms-track-type track) ,types)
            (string-match (emms-player-get ,player-name 'regex)
                          (emms-track-name track)))))))

;; Utility for tq

(defvar emms-player-simple-mpv--tq nil
  "TQ process.")

(defvar emms-player-simple-mpv--tq-id-counter 0
  "Counter for request_id.")

(defvar emms-player-simple-mpv--tq-hash (make-hash-table)
  "Key: request_id, Value: \(closure . fn).")

(defvar emms-player-simple-mpv-tq-process-name
  "emms-player-simple-mpv-tq-process")

(defvar emms-player-simple-mpv--socket
  (expand-file-name (make-temp-name emms-player-simple-mpv-ipc-fname-prefix)
                    (or emms-player-simple-mpv-ipc-dir temporary-file-directory)))

(defun emms-player-simple-mpv--socket ()
  (setq emms-player-simple-mpv--socket
        (expand-file-name (make-temp-name emms-player-simple-mpv-ipc-fname-prefix)
                          (or emms-player-simple-mpv-ipc-dir temporary-file-directory))))

(defun emms-player-simple-mpv--tq-create ()
  (setq emms-player-simple-mpv--tq-id-counter 0)
  (setq emms-player-simple-mpv--tq-hash (make-hash-table))
  (tq-create (make-network-process
              :name emms-player-simple-mpv-tq-process-name
              :family 'local
              :service emms-player-simple-mpv--socket)))

(defun emms-player-simple-mpv--tq-close ()
  (when emms-player-simple-mpv--tq
    (tq-close emms-player-simple-mpv--tq)
    (setq emms-player-simple-mpv--tq nil))
  (when (file-exists-p emms-player-simple-mpv--socket)
    (ignore-errors (delete-file emms-player-simple-mpv--socket))))

(add-hook 'emms-player-stopped-hook  'emms-player-simple-mpv--tq-close)
(add-hook 'emms-player-finished-hook 'emms-player-simple-mpv--tq-close)

(defun emms-player-simple-mpv--socket-filter (_proc string)
  (emms-player-simple-mpv--tq-filter emms-player-simple-mpv--tq string))

(defun emms-player-simple-mpv--tq-filter (tq string)
  "Append STRING to the TQ's buffer; then process the new data. See tq.el."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert string)
        (when (and (eobp) (bolp))
         (emms-player-simple-mpv--tq-process-buffer tq))))))

(defun emms-player-simple-mpv--tq-process-buffer (tq)
  "Check TQ's buffer at the head of the queue.
See `tq-process-buffer'."
  (let ((buffer (tq-buffer tq)))
    (while (and (buffer-live-p buffer) (set-buffer buffer) (> (buffer-size) 0))
      (goto-char (point-min))
      (let* ((answer-ls (ignore-errors (json-read)))
             (point (if (re-search-forward "{" nil t)
                        (goto-char (match-beginning 0))
                      (point)))
             (request_id (emms-player-simple-mpv-tq-assq-v 'request_id answer-ls))
             (hash-v (when request_id (gethash request_id emms-player-simple-mpv--tq-hash)))
             (closure (car hash-v))
             (fn (cdr hash-v))
             (event (and (listp answer-ls)
                         (emms-player-simple-mpv-tq-assq-v 'event answer-ls))))
        (delete-region (point-min)
                       (if (ignore-errors (json-read)) point (point-max)))
        (if event (emms-player-simple-mpv--tq-event-action event answer-ls)
          (unwind-protect
              (when fn (condition-case err
                           (funcall fn closure answer-ls)
                         (error (message "Error: mpv tq-process-buffer : %s"
                                         (error-message-string err)))))
            (remhash request_id emms-player-simple-mpv--tq-hash)))))))

(defun emms-player-simple-mpv-playing-p ()
  "Return t when `emms-player-simple-mpv--tq' process is open."
  (let ((process (tq-process emms-player-simple-mpv--tq)))
    (when process
     (eq (process-status process) 'open))))

;;;###autoload
(defun emms-player-simple-mpv-tq-clear ()
  "Clear tq-enque if it remains."
  (interactive)
  (when emms-player-simple-mpv--tq
    (let ((buffer (tq-buffer emms-player-simple-mpv--tq)))
      (setcar emms-player-simple-mpv--tq nil)
      (setq emms-player-simple-mpv--tq-id-counter 0)
      (setq emms-player-simple-mpv--tq-hash (make-hash-table))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (delete-region (point-min) (point-max)))))))

(defun emms-player-simple-mpv--tq-make-command (com-ls id)
  "Build JSON command from COM-LS and request ID.
`emms-player-simple-mpv--tq-id-counter' will be used as request_id."
  (concat (json-encode `(("command" . ,com-ls) ("request_id" . ,id)))
          "\n"))

;;;###autoload
(defun emms-player-simple-mpv-tq-enqueue (com-ls closure fn)
  "Work like `tq-enqueue' except for using a hash table.
and return the request_id.
COM-LS is a list of a command name and params.
CLOSURE will be used as a first arg for FN.
FN will take CLOSURE and a parsed json object \(alist) after receiving a reply."
  (when (emms-player-simple-mpv-playing-p)
    (puthash emms-player-simple-mpv--tq-id-counter
             (cons closure fn) emms-player-simple-mpv--tq-hash)
    (process-send-string (tq-process emms-player-simple-mpv--tq)
                         (funcall #'emms-player-simple-mpv--tq-make-command
                                  com-ls emms-player-simple-mpv--tq-id-counter))
    (1- (cl-incf emms-player-simple-mpv--tq-id-counter))))

(defun emms-player-simple-mpv-tq-success-p (ans)
  "Check command response from ANS."
  (let ((err-msg
         (if (atom (caar ans))
             (cdr (assq 'error ans)) ;; For decoded JSON obj
           (cl-loop for obj in ans   ;; For decoded JSON obj list
                    for msg = (cdr (assq 'error obj))
                    when msg return msg))))
    (and (stringp err-msg)
         (string= err-msg "success"))))

(defun emms-player-simple-mpv-tq-assq (key ans)
  "Return the association for KEY in ANS."
  (if (atom (caar ans))
      (assq key ans)        ;; For decoded JSON obj
    (cl-loop for obj in ans ;; For decoded JSON obj list
             for assoc = (assq key obj)
             when assoc return assoc)))

(defun emms-player-simple-mpv-tq-assq-v (key ans)
  "Return a value of the association for KEY in ANS."
  (cdr (emms-player-simple-mpv-tq-assq key ans)))

;;;###autoload
(cl-defun emms-player-simple-mpv-tq-data-message
    (form &key (fn #'identity) (err-form form))
  "Return function to display a data message by FORM.
FORM can include a format specification for data.
:FN takes data as an argument.
:ERR-FORM can include a format specification %s."
  (lambda (_ ans-ls)
    (if (emms-player-simple-mpv-tq-success-p ans-ls)
        (let ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
          (if data (message form (funcall fn data))
            (message "mpv : nothing data message")))
      (message err-form (emms-player-simple-mpv-tq-assq-v 'error ans-ls)))))

;;;###autoload
(defun emms-player-simple-mpv-tq-error-message (form)
  "Return function to display an error message by FORM.
FORM can include a format specification %s."
  (lambda (_ ans-ls)
    (let ((err (emms-player-simple-mpv-tq-assq-v 'error ans-ls)))
      (if err (message form err)
        (message "mpv : nothing error message")))))

;; Event action
(defun emms-player-simple-mpv--tq-event-action (event ans-ls)
  "Action for the EVENT from mpv.
If event is \"property-change\", ANS-LS is used."
  (when (stringp event)
   (cond
    ((string= event "playback-restart")
     (emms-player-simple-mpv--tq-event-action-playback-restart))
    ((string= event "property-change")
     (emms-player-simple-mpv--tq-event-action-property-change ans-ls)))))

;; Event for playback-restart
(defvar emms-player-simple-mpv--sent-observe_property-p) ; Suppress a warning message

(defsubst emms-player-simple-mpv--tq-event-action-playback-restart-dummy ()
  "Dummy action for getting observe_property's message (for mpv v.0.11)."
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "volume") nil
   (lambda (_ ans-ls)
     (when (emms-player-simple-mpv-tq-success-p ans-ls)
       (emms-player-simple-mpv-tq-enqueue
        `("set_property" "volume"
          ,(emms-player-simple-mpv-tq-assq-v 'data ans-ls)) nil
        (lambda (_ __)))))))

(defun emms-player-simple-mpv--tq-event-action-playback-restart ()
  "Event action for playback-restart."
  (unless emms-player-simple-mpv--sent-observe_property-p
    (emms-player-simple-mpv--send-observe_property)
    (emms-player-simple-mpv--tq-event-action-playback-restart-dummy))
  (condition-case err
      (run-hooks 'emms-player-simple-mpv-tq-event-playback-restart-hook)
    (error (message "Error : mpv evet hook for playback-restart : %s"
                    (error-message-string err)))))

;; Event for pause
(defun emms-player-simple-mpv--tq-event-action-pause ()
  "Event action for pause."
  (setq emms-player-paused-p t)
  (run-hooks 'emms-player-paused-hook)
  (condition-case err
      (run-hooks 'emms-player-simple-mpv-tq-event-pause-hook)
    (error (message "Error : mpv evet hook for pause : %s"
                    (error-message-string err)))))

;; Event for unpause
(defun emms-player-simple-mpv--tq-event-action-unpause ()
  "Event action for unpause."
  (setq emms-player-paused-p nil)
  (run-hooks 'emms-player-paused-hook)
  (condition-case err
      (run-hooks 'emms-player-simple-mpv-tq-event-unpause-hook)
    (error (message "Error : mpv evet hook for unpause : %s"
                    (error-message-string err)))))

;; Event for property-change
(defun emms-player-simple-mpv--tq-event-action-property-change (ans-ls)
  "Event action for property-change which is from ANS-LS."
  (let ((name (emms-player-simple-mpv-tq-assq-v 'name ans-ls) )
        (data (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
    (when (stringp name)
     (cond
      ((string= name "pause")
       (if (eq data t) (emms-player-simple-mpv--tq-event-action-pause)
         (emms-player-simple-mpv--tq-event-action-unpause)))
      (t (emms-player-simple-mpv--run-tq-event-property-change-functions name data))))))

(defvar emms-player-simple-mpv--observe_property-name-als nil
  "Alist of property name and id.")

(defvar emms-player-simple-mpv--observe_property-id-counter
  (let ((count 0)) (lambda () (cl-incf count)))
    "Return new property id number.")

(defun emms-player-simple-mpv--observe_property-id-counter (&optional initializep)
  "Make property id counter."
  (if initializep
      (setq emms-player-simple-mpv--observe_property-id-counter
            (let ((count 0)) (lambda () (cl-incf count))))
    (funcall emms-player-simple-mpv--observe_property-id-counter)))

(defvar emms-player-simple-mpv--sent-observe_property-p nil
  "If non-nil, `emms-player-simple-mpv--send-observe_property' has done.")

(defun emms-player-simple-mpv--send-observe_property ()
  "Send observe_property for initialization."
  (emms-player-simple-mpv-observe_property "pause")
  (cl-loop for (name . _)
           in emms-player-simple-mpv-tq-event-property-change-functions-alist do
           (emms-player-simple-mpv-observe_property name))
  (setq emms-player-simple-mpv--sent-observe_property-p t))

(defun emms-player-simple-mpv--run-tq-event-property-change-functions (name data)
  "Event action for NAME.
DATA is used for the argument of name's abnormal hook.
`emms-player-simple-mpv-tq-event-property-change-functions-alist' is used."
  (let ((functions (assoc-default name emms-player-simple-mpv-tq-event-property-change-functions-alist)))
    (condition-case err
        (when functions (run-hook-with-args functions data))
      (error (message "Error : mpv evet hook for %s : %s"
                      name (error-message-string err))))))

;; Functions to start mpv

;;;###autoload
(defun emms-player-simple-mpv-add-to-converters (player regexp types fn &optional appendp)
  "Add a converter to PLAYER's mpv-track-name-converters like `add-to-list'.
Converter is  \(list REGEXP TYPES FN\).
If APPENDP is no-nil,add converter to last.
TYPES is type list or t.
FN takes track-name as an argument."
  (let ((converters (emms-player-get player 'mpv-track-name-converters))
        (converter (list regexp types fn)))
    (unless (cl-find converter converters :test #'equal)
      (emms-player-set player 'mpv-track-name-converters
                       (if appendp
                           (nconc converters (list converter))
                         (cons converter converters))))
    (emms-player-get player 'mpv-track-name-converters)))

;;;###autoload
(defun emms-player-simple-mpv-remove-converter (player regexp)
  "Remove the converter from PLAYER's mpv-track-name-converters which has REGEXP."
  (let ((converters (emms-player-get player 'mpv-track-name-converters)))
    (emms-player-set player 'mpv-track-name-converters
                     (cl-delete regexp converters :key #'car :test #'equal))
    (emms-player-get player 'mpv-track-name-converters)))

(defun emms-player-simple-mpv--track-to-input-form (track track-name-converters)
  "Convert TRACK to mpv input form by TRACK-NAME-CONVERTERS."
  (let* ((track-name (emms-track-name track))
         (track-type (emms-track-type track))
         (converter
          (cl-loop for (regexp types fn) in track-name-converters
                   when (and (string-match-p regexp track-name)
                             (or (eq types t) (memq track-type types)))
                   return fn)))
    (if converter (funcall converter track-name) track-name)))

(defun emms-player-simple-mpv--start-tq-error-message (params input-form)
  "Error message when tq-process fails to start."
  (message "Failed to start mpv--tq. Check parameters or input form.\n%s%s\n%s%s"
           "    " (mapconcat #'identity  params " ") "    " input-form))

(defun emms-player-simple-mpv-default-start-process
    (cmdname params input-form _track)
  "Default function for mpv-start-process-function."
  (apply  #'start-process
          emms-player-simple-process-name
          nil
          cmdname
          `(,@params ,input-form)))

(defun emms-player-simple-mpv--start-initialize ()
  "Initalize golobal variables before making a process."
  (emms-player-simple-mpv--tq-close)
  (emms-player-simple-mpv--observe_property-id-counter t)
  (setq emms-player-simple-mpv--observe_property-name-als nil
        emms-player-simple-mpv--sent-observe_property-p nil))

(defun emms-player-simple-mpv--add-keep-property-params (params)
  "Add `emms-player-simple-mpv-keep-properties' to PARAMS."
  (when emms-player-simple-mpv-keep-properties
    (setq params (cl-copy-list params))
    (cl-loop for (name availablep val-name) in emms-player-simple-mpv-keep-properties
             when (funcall availablep) do
             (setq params
                   (cons (format "--%s=%s" name (symbol-value val-name))
                         (cl-delete-if (lambda (param)
                                         (string-match (format "\\`--%s=" name)
                                                       param))
                                       params)))))
  params)

(defvar emms-player-simple-mpv--connect-socket-timer nil)

(defvar emms-player-simple-mpv--connect-socket-timer-interval 0.05)

(defvar emms-player-simple-mpv--connect-socket-timeout 300.0)

(defun emms-player-simple-mpv--connect-socket-cancel-timer ()
  "Cancel `emms-player-simple-mpv--connect-socket-timer'."
  (when emms-player-simple-mpv--connect-socket-timer
    (cancel-timer emms-player-simple-mpv--connect-socket-timer)
    (setq emms-player-simple-mpv--connect-socket-timer nil)))

(defun emms-player-simple-mpv--set-tq-socket ()
  "Set `emms-player-simple-mpv--tq' to a new tq."
  (setq emms-player-simple-mpv--tq (emms-player-simple-mpv--tq-create))
  (set-process-filter (tq-process emms-player-simple-mpv--tq)
                      'emms-player-simple-mpv--socket-filter)
  (when emms-player-simple-mpv-use-volume-change-function-p
    (emms-player-simple-mpv--set-volume-change-function)))

(defun emms-player-simple-mpv--connect-socket (process)
  "Try to connect `emms-player-simple-mpv--socket' which PROCESS made."
  (emms-player-simple-mpv--connect-socket-cancel-timer)
  (let ((counter 0.0)
        (timeout emms-player-simple-mpv--connect-socket-timeout)
        (interval emms-player-simple-mpv--connect-socket-timer-interval)
        (dir (file-name-directory emms-player-simple-mpv--socket)))
    (unless (file-exists-p dir)
      (error "Failed to find the directory: %s" dir))
    (setq emms-player-simple-mpv--connect-socket-timer
          (run-at-time
           interval interval
           (lambda ()
             (cl-incf counter interval)
             (cond
              ((> counter timeout)
               (emms-player-simple-mpv--connect-socket-cancel-timer)
               (message "Timeout: failed to find the socket file: %s"
                        emms-player-simple-mpv--socket))
              ((not (and (processp process) (eq (process-status process) 'run)))
               (emms-player-simple-mpv--connect-socket-cancel-timer))
              ((file-exists-p emms-player-simple-mpv--socket)
               (emms-player-simple-mpv--connect-socket-cancel-timer)
               (condition-case err
                   (emms-player-simple-mpv--set-tq-socket)
                 (error (message "%s" (error-message-string err)))))))))))

;;;###autoload
(defun emms-player-simple-mpv-start (track player cmdname params)
  "Emulate `emms-player-simple-start' but the first arg."
  (unless emms-player-simple-mpv-ipc-option-name
    (setq emms-player-simple-mpv-ipc-option-name
          (emms-player-simple-mpv-get-ipc-option-name)))
  (emms-player-simple-mpv--start-initialize)
  (let* ((input-socket
          (format "%s=%s" emms-player-simple-mpv-ipc-option-name
                  (emms-player-simple-mpv--socket)))
         (input-form
          (emms-player-simple-mpv--track-to-input-form
           track (emms-player-get player 'mpv-track-name-converters)))
         (params (emms-player-simple-mpv--add-keep-property-params params))
         (process
          (funcall (emms-player-get player 'mpv-start-process-function)
                   cmdname `(,input-socket ,@params)
                   input-form track)))
    (set-process-sentinel process 'emms-player-simple-sentinel)
    (emms-player-started player)
    (setq emms-player-paused-p t)
    (run-hooks 'emms-player-paused-hook)
    (emms-player-simple-mpv--connect-socket process)))

;; Functions to control mpv

;;;###autoload
(defun emms-player-simple-mpv-observe_property (name)
  "Set observe_property of NAME."
  (let ((id (emms-player-simple-mpv--observe_property-id-counter)))
    (unless (assoc name emms-player-simple-mpv--observe_property-name-als)
      (emms-player-simple-mpv-tq-enqueue
       (list "observe_property" id name) nil
       (lambda (_ ans-ls)
         (if (emms-player-simple-mpv-tq-success-p ans-ls)
             (push (cons name id) emms-player-simple-mpv--observe_property-name-als)
           (message "mpv : Failed to set \"observe_property\" of %s" name)))))))

(defun emms-player-simple-mpv--set_property-1
    (com property value spec msg err-msg fn)
  "Helper function for emms-player-simple-mpv-set_property\(_string\)."
  (emms-player-simple-mpv-tq-enqueue
   (list com property value) nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (if msg
             (message (format "mpv %s : %s" msg spec)
                      (if fn (funcall fn value) value))
           (when fn (funcall fn value)))
       (when err-msg
         (message "mpv %s : %s" err-msg (cdr (assq 'error ans-ls))))))))

;;;###autoload
(cl-defun emms-player-simple-mpv-set_property
    (property value &key (spec "%s") (msg property) (err-msg property) (fn #'identity))
  "Set PROPERTY to VALUE via set_property.
:SPEC is a format specification for VALUE.
:MSG is displayed when command succeeds. If nil, it will be ignored.
:ERR-MSG is displayed when command fails. If nil, it will be ignored.
:FN takes VALUE as an argument. Its returned value will be used for :SPEC if :MSG is non-nil."
  (emms-player-simple-mpv--set_property-1
   "set_property" property value spec msg err-msg fn))

;;;###autoload
(cl-defun emms-player-simple-mpv-set_property_string
    (property value &key (spec "%s") (msg property) (err-msg property) (fn #'identity))
  "Set PROPERTY to VALUE via property_string.
:SPEC is a format specification for VALUE.
:MSG is displayed when command succeeds. If nil, it will be ignored.
:ERR-MSG is displayed when command fails. If nil, it will be ignored.
:FN takes VALUE as an argument. Its returned value will be used for :SPEC if :MSG is non-nil."
  (emms-player-simple-mpv--set_property-1
   "set_property_string" property value spec msg err-msg fn))

(defsubst emms-player-simple-mpv--time-string (sec)
  "SEC to \"%02h:%02m:%02s\"."
  (let* ((h (floor sec 3600))
         (m (floor (- sec (* 3600 h)) 60))
         (s (- sec (* 60 (+ (* 60 h) m)))))
    (format "%02d:%02d:%02d" h m s)))

;; pause

;;;###autoload
(defun emms-player-simple-mpv-pause ()
  "Pause."
  (emms-player-simple-mpv-set_property_string
   "pause" "yes" :spec "success"))

;;;###autoload
(defun emms-player-simple-mpv-unpause ()
  "Unpause."
  (emms-player-simple-mpv-set_property_string
   "pause" "no" :spec "success" :msg "unpause" :err-msg "unpause"))

;; seek

(defun emms-player-simple-mpv--seek-1 (als ans-ls)
  "Helper funcion for `emms-player-simple-mpv-seek'."
  ;; als  : ((sec . n0) (len . n1))
  ;; data : time-pos
  (if (emms-player-simple-mpv-tq-success-p ans-ls)
      (let* ((sec (cdr (assq 'sec als)))
             (len (cdr (assq 'len als)))
             (data  (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
             (data+ (+ sec data))
             (next-sec (cond
                        ((< data+ 0) 0)
                        ((> data+ len) len)
                        (t data+)))
             (time (emms-player-simple-mpv--time-string next-sec)))
        (emms-player-simple-mpv-tq-enqueue
         (list "seek" next-sec "absolute")
         (format "mpv seek %s : %s" (if (>= sec 0) ">>" "<<") time)
         (lambda (form ans-ls)
           (if (emms-player-simple-mpv-tq-success-p ans-ls)
               (message form)
             (message "mpv seek : error")))))
    (message "mpv seek : error")))

(defun emms-player-simple-mpv--seek-2 (sec)
  "Helper funcion for `emms-player-simple-mpv-seek'.
For a track which does not have duration property."
  (emms-player-simple-mpv-tq-enqueue
   (list "seek" sec "relative")
   nil
   (emms-player-simple-mpv-tq-error-message
    (format "mpv seek %s %+d : %%s" (if (>= sec 0) ">>" "<<") sec))))

;;;###autoload
(defun emms-player-simple-mpv-seek (sec)
  "Seek by SEC."
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "duration")
   sec
   (lambda (sec ans-ls)
     (let ((successp (emms-player-simple-mpv-tq-success-p ans-ls))
           (data (emms-player-simple-mpv-tq-assq-v 'data ans-ls)))
       (if (and successp (numberp data) (> data 0.0))
           (emms-player-simple-mpv-tq-enqueue
            '("get_property" "time-pos")
            `((sec . ,sec) (len . ,data))
            'emms-player-simple-mpv--seek-1)
         (emms-player-simple-mpv--seek-2 sec))))))

;;;###autoload
(defun emms-player-simple-mpv-seek-to (sec)
  "Seek to SEC."
  (interactive "nmpv seek to (sec) : ")
  (emms-player-simple-mpv-tq-enqueue
   (list "seek" sec "absolute")
   sec
   (lambda (sec ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (message "mpv seek to : %s" (emms-player-simple-mpv--time-string sec))
       (message "mpv seek to : error")))))

;; volume

(defun emms-player-simple-mpv--volume-change-1 (v ans-ls)
  "Set volume plus V in `emms-player-simple-mpv-volume-change'.
ANS-LS includes data value."
  (if (emms-player-simple-mpv-tq-success-p ans-ls)
      (let* ((data (emms-player-simple-mpv-tq-assq-v 'data ans-ls))
             (data+ (round (+ data v)))
             (vol (if (< data+ 0) 0 data+)))
        (emms-player-simple-mpv-set_property
         "volume" vol :err-msg (format ": set volume to %s" vol)))
    (message "mpv volume : error")))

;;;###autoload
(defun emms-player-simple-mpv-volume-change (v)
  "Change volume by V."
  (emms-player-simple-mpv-tq-enqueue
   (list "get_property" "volume")
   v 'emms-player-simple-mpv--volume-change-1))

(defun emms-player-simple-mpv--set-default-volume-change-function ()
  "Set default volume change function to `emms-volume-change-function'."
  (let ((default-volume-function
          (get 'emms-player-simple-mpv-volume-change
               :default-volume-change-function)))
    (if  (null default-volume-function)
        (setq emms-volume-change-function emms-player-simple-mpv-default-volume-function)
      (setq emms-volume-change-function default-volume-function)
      (put 'emms-player-simple-mpv-volume-change
           :default-volume-change-function nil)))
  (remove-hook 'emms-player-stopped-hook
               'emms-player-simple-mpv--set-default-volume-change-function)
  (remove-hook 'emms-player-finished-hook
               'emms-player-simple-mpv--set-default-volume-change-function))

(defun emms-player-simple-mpv--set-volume-change-function ()
  "Set `emms-player-simple-mpv-volume-change' to `emms-volume-change-function'."
  (put 'emms-player-simple-mpv-volume-change
       :default-volume-change-function  emms-volume-change-function)
  (setq emms-volume-change-function 'emms-player-simple-mpv-volume-change)
  (add-hook 'emms-player-stopped-hook
            'emms-player-simple-mpv--set-default-volume-change-function)
  (add-hook 'emms-player-finished-hook
            'emms-player-simple-mpv--set-default-volume-change-function))

(provide 'emms-player-simple-mpv)
;;; emms-player-simple-mpv.el ends here
