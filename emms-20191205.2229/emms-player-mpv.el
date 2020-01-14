;;; emms-player-mpv.el --- mpv support for EMMS
;;
;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Authors: Mike Kazantsev <mk.fraggod@gmail.com>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; This code provides EMMS backend for using mpv player.
;;
;; It works in one of two modes, depending on `emms-player-mpv-ipc-method'
;; customizable value or installed mpv version:
;;
;;  - Using long-running mpv instance and JSON IPC interface to switch tracks
;;    and receive player feedback/metadata - for mpv 0.7.0 2014-10-16 and later.
;;
;;  - Starting new mpv instance for each track, using its exit
;;    as "next track" signal and --input-file interface for pause/seek.
;;    Used as a fallback for any older mpv versions (supported in all of them).
;;
;; In default configuration, mpv will read its configuration files
;; (see its manpage for locations), and can display window for
;; video, subtitles, album-art or audio visualization.
;;
;; Useful `emms-player-mpv-parameters' tweaks:
;;
;;  - Ignore config file(s): (add-to-list 'emms-player-mpv-parameters "--no-config")
;;  - Disable vo window: (add-to-list 'emms-player-mpv-parameters "--vo=null")
;;  - Show simple cqt visualizer window:
;;      (add-to-list 'emms-player-mpv-parameters
;;        "--lavfi-complex=[aid1]asplit[ao][a]; [a]showcqt[vo]")
;;
;; See "M-x customize-group emms-player-mpv" and mpv manpage for more options.
;;
;; See `emms-player-mpv-event-connect-hook' and `emms-player-mpv-event-functions',
;; as well as `emms-player-mpv-ipc-req-send' for handling more mpv events,
;; processing more playback info and metadata from it, as well as extending
;; control over its vast functionality.
;;

;;; Code:


(require 'emms)
(require 'emms-player-simple)
(require 'json)
(require 'cl-lib)


(defcustom emms-player-mpv
  (emms-player
   #'emms-player-mpv-start
   #'emms-player-mpv-stop
   #'emms-player-mpv-playable-p)
  "*Parameters for mpv player."
  :type '(cons symbol alist)
  :group 'emms-player-mpv)

(emms-player-set emms-player-mpv 'regex
                 (apply #'emms-player-simple-regexp emms-player-base-format-list))

(defcustom emms-player-mpv-command-name "mpv"
  "mpv binary to use. Can be absolute path or just binary name."
  :type 'file
  :group 'emms-player-mpv)

(defcustom emms-player-mpv-parameters
  '("--quiet" "--really-quiet" "--no-audio-display")
  "Extra command-line arguments for started mpv process(es).
Either a list of strings or function returning such list.
Extra arguments --idle and --input-file/--input-ipc-server
are added automatically, depending on mpv version.
Note that unless --no-config option is specified here,
mpv will also use options from its configuration files.
For mpv binary path, see `emms-player-mpv-command-name'."
  :type '(choice (repeat :tag "List of mpv arguments" string)
                 function)
  :group 'emms-player-mpv)

(defcustom emms-player-mpv-environment ()
  "List of extra environment variables (\"VAR=value\" strings) to pass on to mpv process.
These are added on top of `process-environment' by default.
Adding nil as an element to this list will discard emacs
`process-environment' and only pass variables that are specified in the list."
  :type '(repeat (choice string (const :tag "Start from blank environment" nil)))
  :group 'emms-player-mpv)

(defcustom emms-player-mpv-ipc-method nil
  "Switch for which IPC method to use with mpv.
Possible symbols: detect, ipc-server, unix-socket, file.
Defaults to nil value, which will cause `emms-player-mpv-ipc-detect'
to pick one based on mpv --version output.
Using JSON-IPC variants (ipc-server and unix-socket) enables
support for various feedback and metadata options from mpv."
  :type '(choice
          (const :tag "Auto-detect from mpv --version" nil)
          (const :tag "Use --input-ipc-server JSON IPC (v0.17.0 2016-04-11)" ipc-server)
          (const :tag "Use --input-unix-socket JSON IPC (v0.7.0 2014-10-16)" unix-socket)
          (const :tag "Use --input-file FIFO (any mpv version)" file))
  :group 'emms-player-mpv)

(defcustom emms-player-mpv-ipc-socket
  (concat (file-name-as-directory emms-directory)
          "mpv-ipc.sock")
  "Unix IPC socket or FIFO to use with mpv --input-* options,
depending on `emms-player-mpv-ipc-method' value and/or mpv version."
  :type 'file
  :group 'emms-player-mpv)

(defvar emms-player-mpv-ipc-proc nil) ; to avoid warnings while keeping useful defs at the top

(defcustom emms-player-mpv-update-duration t
  "Update track duration when played by mpv.
Uses `emms-player-mpv-event-functions' hook."
  :type 'boolean
  :set (lambda (sym value)
         (run-at-time 0.1 nil
                      (lambda (value)
                        (if value
                            (add-hook
                             'emms-player-mpv-event-functions
                             #'emms-player-mpv-info-duration-event-func)
                          (remove-hook
                           'emms-player-mpv-event-functions
                           #'emms-player-mpv-info-duration-event-func)))
                      value))
  :group 'emms-player-mpv)

(defcustom emms-player-mpv-update-metadata nil
  "Update track info (artist, album, name, etc) from mpv events, when it is played.
This allows to dynamically update stream info from ICY tags, for example.
Uses `emms-player-mpv-event-connect-hook' and `emms-player-mpv-event-functions' hooks."
  :type 'boolean
  :set (lambda (sym value)
         (run-at-time 0.1 nil
                      (lambda (value)
                        (if value
                            (progn
                              (add-hook
                               'emms-player-mpv-event-connect-hook
                               #'emms-player-mpv-info-meta-connect-func)
                              (add-hook
                               'emms-player-mpv-event-functions
                               #'emms-player-mpv-info-meta-event-func)
                              (when (process-live-p emms-player-mpv-ipc-proc)
                                (emms-player-mpv-info-meta-connect-func)))
                          (progn
                            (remove-hook
                             'emms-player-mpv-event-connect-hook
                             #'emms-player-mpv-info-meta-connect-func)
                            (remove-hook
                             'emms-player-mpv-event-functions
                             #'emms-player-mpv-info-meta-event-func))))
                      value))
  :group 'emms-player-mpv)


(defvar emms-player-mpv-proc nil
  "Running mpv process, controlled over --input-ipc-server/--input-file sockets.")

(defvar emms-player-mpv-proc-kill-delay 5
  "Delay until SIGKILL gets sent to `emms-player-mpv-proc',
if it refuses to exit cleanly on `emms-player-mpv-proc-stop'.")


(defvar emms-player-mpv-ipc-proc nil
  "Unix socket process that communicates with running `emms-player-mpv-proc' instance.")

(defvar emms-player-mpv-ipc-buffer " *emms-player-mpv-ipc*"
  "Buffer to associate with `emms-player-mpv-ipc-proc' socket/pipe process.")

(defvar emms-player-mpv-ipc-connect-timer nil
  "Timer for connection attempts to JSON IPC unix socket.")
(defvar emms-player-mpv-ipc-connect-delays
  '(0.1 0.1 0.1 0.1 0.1 0.1 0.2 0.2 0.3 0.3 0.5 1.0 1.0 2.0)
  "List of delays before initiating socket connection for new mpv process.")

(defvar emms-player-mpv-ipc-connect-command nil
  "JSON command for `emms-player-mpv-ipc-sentinel' to run as soon as it connects to mpv.
I.e. last command that either initiated connection or was used while connecting to mpv.
Set by `emms-player-mpv-start' and such,
cleared once it gets sent by `emms-player-mpv-ipc-sentinel'.")

(defvar emms-player-mpv-ipc-id 1
  "Auto-incremented value sent in JSON requests for request_id and observe_property id's.
Use `emms-player-mpv-ipc-id-get' to get and increment this value, instead of using it directly.
Wraps-around upon reaching `emms-player-mpv-ipc-id-max' (unlikely to ever happen).")

(defvar emms-player-mpv-ipc-id-max (expt 2 30)
  "Max value for `emms-player-mpv-ipc-id' to wrap around after.
Should be fine with both mpv and emacs, and probably never reached anyway.")

(defvar emms-player-mpv-ipc-req-table nil
  "Auto-initialized hash table of outstanding API req_ids to their handler funcs.")

(defvar emms-player-mpv-ipc-stop-command nil
  "Internal flag to track when stop command starts/finishes before next loadfile.
Set to either nil, t or playback start function to call on end-file event after stop command.
This is a workaround for mpv-0.30+ behavior, when 'stop + loadfile' only runs 'stop'.")


(defvar emms-player-mpv-event-connect-hook nil
  "Normal hook run right after establishing new JSON IPC
connection to mpv instance and before `emms-player-mpv-ipc-connect-command'	if any.
Best place to send any observe_property, request_log_messages, enable_event commands.
Use `emms-player-mpv-ipc-id-get' to get unique id values for these.
See also `emms-player-mpv-event-functions'.")

(defvar emms-player-mpv-event-functions nil
  "List of functions to call for each event emitted from JSON IPC.
One argument is passed to each function - JSON line,
as sent by mpv and decoded by `json-read-from-string'.
See also `emms-player-mpv-event-connect-hook'.")


(defvar emms-player-mpv-stopped nil
  "Non-nil if playback was stopped by call from emms.
Similar to `emms-player-stopped-p', but set for future async events,
to indicate that playback should stop instead of switching to next track.")

(defvar emms-player-mpv-idle-timer (timer-create)
  "Timer to delay `emms-player-stopped' when mpv unexpectedly goes idle.")

(defvar emms-player-mpv-idle-delay 0.5
  "Delay before issuing `emms-player-stopped' when mpv unexpectedly goes idle.")


(defvar emms-player-mpv-ipc-conn-emacs-26.1-workaround
  (and (= emacs-major-version 26)
       (= emacs-minor-version 1))
  "Non-nil to enable workaround for issue #31901 in emacs 26.1.
Emacs 26.1 fails to indicate missing socket file error for unix socket network processes
that were started with :nowait t, so blocking connections are used there instead.")


;; ----- helpers

(defvar emms-player-mpv-debug nil
  "Enable to print sent/received JSON lines and process
start/stop events to *Messages* buffer using `emms-player-mpv-debug-msg'.")

(defvar emms-player-mpv-debug-ts-offset nil
  "Timestamp offset for `emms-player-mpv-debug-msg'.
Set on first use, with intent to both shorten and obfuscate time in logs.")

(defun emms-player-mpv-debug-trim (s)
  (if (stringp s)
      (replace-regexp-in-string "\\(^[ \t\n\r]+\\|[ \t\n\r]+$\\)" "" s t t)
    s))

(defun emms-player-mpv-debug-msg (tpl-or-msg &rest tpl-values)
  "Print debug message to *Messages* if `emms-player-mpv-debug' is non-nil.
Message is only formatted if TPL-VALUES is non-empty.
Strips whitespace from start/end of TPL-OR-MSG and strings in TPL-VALUES."
  (when emms-player-mpv-debug
    (setq
     tpl-or-msg (emms-player-mpv-debug-trim tpl-or-msg)
     tpl-values (seq-map #'emms-player-mpv-debug-trim tpl-values))
    (unless tpl-values
      (setq tpl-or-msg (replace-regexp-in-string "%" "%%" tpl-or-msg t t)))
    (let ((ts (float-time)))
      (unless emms-player-mpv-debug-ts-offset (setq emms-player-mpv-debug-ts-offset ts))
      (apply 'message
             (concat "emms-player-mpv %.1f " tpl-or-msg)
             (- ts emms-player-mpv-debug-ts-offset)
             tpl-values))))

(defun emms-player-mpv-ipc-fifo-p ()
  "Returns non-nil if --input-file fifo should be used.
Runs `emms-player-mpv-ipc-detect' to detect/set `emms-player-mpv-ipc-method' if necessary."
  (unless emms-player-mpv-ipc-method
    (setq emms-player-mpv-ipc-method
          (emms-player-mpv-ipc-detect emms-player-mpv-command-name)))
  (eq emms-player-mpv-ipc-method 'file))

(defun emms-player-mpv-ipc-detect (cmd)
  "Run mpv --version and return symbol for best IPC method supported.
CMD should be either name of mpv binary to use or full path to it.
Return values correspond to `emms-player-mpv-ipc-method' options.
Error is signaled if mpv binary fails to run."
  (with-temp-buffer
    (let ((exit-code (call-process cmd nil '(t t)
                                   nil "--version")))
      (unless (zerop exit-code)
        (insert (format "----- process exited with code %d -----" exit-code))
        (error (format "Failed to run mpv binary [%s]:\n%s" cmd (buffer-string))))
      (goto-char (point-min))
      (pcase
          (if (re-search-forward "^mpv\\s-+\\(\\([0-9]+\\.?\\)+\\)" nil t 1)
              (mapconcat (lambda (n)
                           (format "%03d" n))
                         (seq-map 'string-to-number
                                  (split-string (match-string-no-properties 1)
                                                "\\." t))
                         ".")
            "000.000.000")
        ((pred (string> "000.006.999"))
         'file)
        ((pred (string> "000.016.999"))
         'unix-socket)
        (- 'ipc-server)))))


;; ----- mpv process

(defun emms-player-mpv-proc-playing-p (&optional proc)
  "Return whether playback in PROC or `emms-player-mpv-proc' is started,
which is distinct from 'start-command sent' and 'process is running' states.
Used to signal emms via `emms-player-started' and `emms-player-stopped' calls."
  (let ((proc (or proc emms-player-mpv-proc)))
    (and proc (process-get proc 'mpv-playing))))

(defun emms-player-mpv-proc-playing (state &optional proc)
  "Set process mpv-playing state flag for `emms-player-mpv-proc-playing-p'."
  (let ((proc (or proc emms-player-mpv-proc)))
    (when proc (process-put proc 'mpv-playing state))))

(defun emms-player-mpv-proc-symbol-id (sym &optional proc)
  "Get unique process-specific id integer for SYM or nil if it was already requested."
  (let
      ((proc (or proc emms-player-mpv-proc))
       (sym-id (intern (concat "mpv-sym-" (symbol-name sym)))))
    (unless (process-get proc sym-id)
      (let ((id (emms-player-mpv-ipc-id-get)))
        (process-put proc sym-id id)
        id))))

(defun emms-player-mpv-proc-init-fifo (path &optional mode)
  "Create named pipe (fifo) socket for mpv --input-file PATH, if not exists already.
Optional MODE should be 12-bit octal integer, e.g. #o600 (safe default).
Signals error if mkfifo exits with non-zero code."
  (let ((attrs (file-attributes path)))
    (when
        (and attrs (not (string-prefix-p "p" (nth 8 attrs))))
      (delete-file path)
      (setq attrs nil))
    (unless attrs
      (unless
          (zerop (call-process "mkfifo" nil nil nil
                               (format "--mode=%o" (or mode #o600))
                               path))
        (error (format "Failed to run mkfifo for mpv --input-file path: %s" path))))))

(defun emms-player-mpv-proc-sentinel (proc ev)
  (let
      ((status (process-status proc))
       (playing (emms-player-mpv-proc-playing-p proc)))
    (emms-player-mpv-debug-msg
     "proc[%s]: %s (status=%s, playing=%s)" proc ev status playing)
    (when (and (memq status '(exit signal))
               playing)
      (emms-player-stopped))))

(defun emms-player-mpv-proc-init (&rest media-args)
  "initialize new mpv process as `emms-player-mpv-proc'.
MEDIA-ARGS are used instead of --idle, if specified."
  (emms-player-mpv-proc-stop)
  (unless (file-directory-p (file-name-directory emms-player-mpv-ipc-socket))
    (make-directory (file-name-directory emms-player-mpv-ipc-socket)))
  (when (emms-player-mpv-ipc-fifo-p)
    (emms-player-mpv-proc-init-fifo emms-player-mpv-ipc-socket))
  (let*
      ((argv emms-player-mpv-parameters)
       (argv (append
              (list emms-player-mpv-command-name)
              (if (functionp argv)
                  (funcall argv)
                argv)
              (list (format "--input-%s=%s"
                            emms-player-mpv-ipc-method emms-player-mpv-ipc-socket))
              (or media-args '("--idle"))))
       (env emms-player-mpv-environment)
       (process-environment (append
                             (unless (seq-some 'not env)
                               process-environment)
                             (seq-filter 'identity env))))
    (setq emms-player-mpv-proc
          (make-process :name "emms-player-mpv"
                        :buffer nil :command argv :noquery t :sentinel #'emms-player-mpv-proc-sentinel))
    (when (emms-player-mpv-ipc-fifo-p)
      (emms-player-mpv-proc-playing t))
    (emms-player-mpv-debug-msg "proc[%s]: start %s" emms-player-mpv-proc argv)))

(defun emms-player-mpv-proc-stop ()
  "Stop running `emms-player-mpv-proc' instance via SIGINT, if any.
`delete-process' (SIGKILL) timer is started if `emms-player-mpv-proc-kill-delay' is non-nil."
  (when emms-player-mpv-proc
    (let ((proc emms-player-mpv-proc))
      (emms-player-mpv-debug-msg "proc[%s]: stop" proc)
      (if (not (process-live-p proc))
          (delete-process proc)
        (emms-player-mpv-proc-playing nil proc)
        (interrupt-process proc)
        (when emms-player-mpv-proc-kill-delay
          (run-at-time
           emms-player-mpv-proc-kill-delay nil
           (lambda (proc)
             (delete-process proc))
           proc))))
    (setq emms-player-mpv-proc nil)))


;; ----- IPC socket/fifo

(defun emms-player-mpv-ipc-sentinel (proc ev)
  (emms-player-mpv-debug-msg "ipc[%s]: %s" proc ev)
  (when (memq (process-status proc)
              '(open run))
    (run-hooks 'emms-player-mpv-event-connect-hook)
    (when emms-player-mpv-ipc-connect-command
      (let ((cmd emms-player-mpv-ipc-connect-command))
        (setq emms-player-mpv-ipc-connect-command nil)
        (emms-player-mpv-ipc-req-send cmd nil proc)))))

(defun emms-player-mpv-ipc-filter (proc s)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point)
                       (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert s)
          (set-marker (process-mark proc)
                      (point)))
        (if moving (goto-char (process-mark proc))))
      ;; Process/remove all complete lines of json, if any
      (let ((p0 (point-min)))
        (while
            (progn
              (goto-char p0)
              (end-of-line)
              (equal (following-char)
                     ?\n))
          (let*
              ((p1 (point))
               (json (buffer-substring p0 p1)))
            (delete-region p0 (+ p1 1))
            (emms-player-mpv-ipc-recv json)))))))

(defun emms-player-mpv-ipc-connect (delays)
  "Make IPC connection attempt, rescheduling if there's no socket by (car DELAYS).
(cdr DELAYS) gets passed to next connection attempt,
so it can be rescheduled further until function runs out of DELAYS values.
Sets `emms-player-mpv-ipc-proc' value to resulting process on success."
  ;; Note - emacs handles missing unix socket files in different ways between versions:
  ;;  emacs <26 returns nil, emacs 26.1 leaves process in a stuck 'open
  ;;  state (see issue #31901), emacs 26.2+ sets 'file-missing status.
  ;;  None of these cases call sentinel function, so status must also be checked here.
  (emms-player-mpv-debug-msg "ipc: connect-delay %s" (car delays))
  (let ((use-nowait (not emms-player-mpv-ipc-conn-emacs-26.1-workaround)))
    (setq emms-player-mpv-ipc-proc
          (condition-case nil
              (make-network-process
               :name "emms-player-mpv-ipc"
               :family 'local
               :service emms-player-mpv-ipc-socket
               :nowait use-nowait
               :coding '(utf-8 . utf-8)
               :buffer (get-buffer-create emms-player-mpv-ipc-buffer)
               :noquery t
               :filter #'emms-player-mpv-ipc-filter
               :sentinel #'emms-player-mpv-ipc-sentinel)
            (file-error nil)))
    (unless (process-live-p emms-player-mpv-ipc-proc)
      (setq emms-player-mpv-ipc-proc nil))
    (when (and emms-player-mpv-ipc-proc (not use-nowait))
      (emms-player-mpv-ipc-sentinel emms-player-mpv-ipc-proc 'open)))
  (when (and (not emms-player-mpv-ipc-proc)
             delays)
    (run-at-time (car delays)
                 nil #'emms-player-mpv-ipc-connect (cdr delays))))

(defun emms-player-mpv-ipc-connect-fifo ()
  "Set `emms-player-mpv-ipc-proc' to process wrapper for
writing to a named pipe (fifo) file/node or signal error."
  (setq emms-player-mpv-ipc-proc
        (start-process-shell-command "emms-player-mpv-input-file" nil
                                     (format "cat > \"%s\"" (shell-quote-argument emms-player-mpv-ipc-socket))))
  (set-process-query-on-exit-flag emms-player-mpv-ipc-proc nil)
  (unless emms-player-mpv-ipc-proc (error (format
                                           "Failed to start cat-pipe to fifo: %s" emms-player-mpv-ipc-socket)))
  (when emms-player-mpv-ipc-connect-command
    (let ((cmd emms-player-mpv-ipc-connect-command))
      (setq emms-player-mpv-ipc-connect-command nil)
      (emms-player-mpv-ipc-fifo-cmd cmd emms-player-mpv-ipc-proc))))

(defun emms-player-mpv-ipc-init ()
  "Initialize new mpv ipc socket/file process and associated state."
  (emms-player-mpv-ipc-stop)
  (emms-player-mpv-debug-msg "ipc: init")
  (if (emms-player-mpv-ipc-fifo-p)
      (emms-player-mpv-ipc-connect-fifo)
    (when emms-player-mpv-ipc-connect-timer (cancel-timer emms-player-mpv-ipc-connect-timer))
    (with-current-buffer (get-buffer-create emms-player-mpv-ipc-buffer)
      (erase-buffer))
    (setq
     emms-player-mpv-ipc-id 1
     emms-player-mpv-ipc-req-table nil
     emms-player-mpv-ipc-connect-timer nil
     emms-player-mpv-ipc-connect-timer
     (run-at-time (car emms-player-mpv-ipc-connect-delays)
                  nil
                  #'emms-player-mpv-ipc-connect (cdr emms-player-mpv-ipc-connect-delays)))))

(defun emms-player-mpv-ipc-stop ()
  (when emms-player-mpv-ipc-proc
    (emms-player-mpv-debug-msg "ipc: stop")
    (delete-process emms-player-mpv-ipc-proc)
    (setq emms-player-mpv-ipc-proc nil)))

(defun emms-player-mpv-ipc ()
  "Return open IPC socket/fifo process or nil, (re-)starting mpv/connection if necessary.
Return nil when starting async process/connection, and any follow-up
command should be stored to `emms-player-mpv-ipc-connect-command' in this case."
  (unless
      ;; Don't start idle processes for fifo - just ignore all ipc requests there
      (and (not (process-live-p emms-player-mpv-proc))
           (emms-player-mpv-ipc-fifo-p))
    (unless (process-live-p emms-player-mpv-proc)
      (emms-player-mpv-proc-init))
    (unless (process-live-p emms-player-mpv-ipc-proc)
      (emms-player-mpv-ipc-init))
    (and
     emms-player-mpv-ipc-proc
     (memq (process-status emms-player-mpv-ipc-proc)
           '(open run))
     emms-player-mpv-ipc-proc)))


;; ----- IPC protocol

(defun emms-player-mpv-ipc-id-get ()
  "Get new connection-unique id value, tracked via `emms-player-mpv-ipc-id'."
  (let ((ipc-id emms-player-mpv-ipc-id))
    (setq emms-player-mpv-ipc-id
          (if (< emms-player-mpv-ipc-id emms-player-mpv-ipc-id-max)
              (1+ emms-player-mpv-ipc-id)
            1))
    ipc-id))

(defun emms-player-mpv-ipc-req-send (cmd &optional handler proc)
  "Send JSON IPC request and assign HANDLER to response for it, if any.
CMD value is encoded via `json-encode'.
HANDLER func will be called with decoded response JSON as (handler data err),
where ERR will be either nil on \"success\", 'connection-error or whatever is in JSON.
If HANDLER is nil, default `emms-player-mpv-ipc-req-error-printer'
will be used to at least log errors.
PROC can be specified to avoid `emms-player-mpv-ipc' call (e.g. from sentinel/filter funcs)."
  (let
      ((req-id (emms-player-mpv-ipc-id-get))
       (req-proc (or proc (emms-player-mpv-ipc)))
       (handler (or handler #'emms-player-mpv-ipc-req-error-printer)))
    (unless emms-player-mpv-ipc-req-table
      (setq emms-player-mpv-ipc-req-table (make-hash-table)))
    (let ((json (concat (json-encode (list :command cmd :request_id req-id))
                        "\n")))
      (emms-player-mpv-debug-msg "json >> %s" json)
      (condition-case err
          ;; On any disconnect, assume that mpv process is to blame and force restart.
          (process-send-string req-proc json)
        (error
         (emms-player-mpv-proc-stop)
         (funcall handler nil 'connection-error)
         (setq handler nil))))
    (when handler (puthash req-id handler emms-player-mpv-ipc-req-table))))

(defun emms-player-mpv-ipc-req-resolve (req-id data err)
  "Run handler-func for specified req-id."
  (when emms-player-mpv-ipc-req-table
    (let
        ((handler (gethash req-id emms-player-mpv-ipc-req-table))
         (err (if (string= err "success")
                  nil err)))
      (remhash req-id emms-player-mpv-ipc-req-table)
      (when handler (funcall handler data err)))))

(defun emms-player-mpv-ipc-req-error-printer (data err)
  "Simple default `emms-player-mpv-ipc-req-send' handler to log errors, if any."
  (when err (message "emms-player-mpv ipc-error: %s" err)))

(defun emms-player-mpv-ipc-recv (json)
  "Handler for all JSON lines from mpv process.
Only used with JSON IPC, never called with --input-file as there's no feedback there."
  (emms-player-mpv-debug-msg "json << %s" json)
  (let*
      ((json-data (json-read-from-string json))
       (req-id (alist-get 'request_id json-data))
       (ev (alist-get 'event json-data)))
    (when req-id
      ;; Response to command
      (emms-player-mpv-ipc-req-resolve req-id
                                       (alist-get 'data json-data)
                                       (alist-get 'error json-data)))
    (when ev
      ;; mpv event
      (emms-player-mpv-event-handler json-data)
      (run-hook-with-args 'emms-player-mpv-event-functions json-data))))

(defun emms-player-mpv-ipc-fifo-cmd (cmd &optional proc)
  "Send --input-file command string for older mpv versions.
PROC can be specified to avoid `emms-player-mpv-ipc' call."
  (let
      ((proc (or proc (emms-player-mpv-ipc)))
       (cmd-line (concat (mapconcat (lambda (s)
                                      (format "%s" s))
                                    cmd " ")
                         "\n")))
    (emms-player-mpv-debug-msg "fifo >> %s" cmd-line)
    (process-send-string proc cmd-line)))

(defun emms-player-mpv-observe-property (sym)
  "Send mpv observe_property command for property identified by SYM.
Only sends command once per process, removing any
potential duplication if used for same properties from different functions."
  (let ((id (emms-player-mpv-proc-symbol-id sym)))
    (when id (emms-player-mpv-ipc-req-send `(observe_property ,id ,sym)))))

(defun emms-player-mpv-event-idle ()
  "Delayed check for switching tracks when mpv goes idle for no good reason."
  (emms-player-mpv-debug-msg "idle-check (stopped=%s)" emms-player-mpv-stopped)
  (unless emms-player-mpv-stopped (emms-player-stopped)))

(defun emms-player-mpv-event-handler (json-data)
  "Handler for supported mpv events, including property changes.
Called before `emms-player-mpv-event-functions' and does same thing as these hooks."
  (pcase (alist-get 'event json-data)
    ("playback-restart"
     ;; Separate emms-player-mpv-proc-playing state is used for emms started/stopped signals,
     ;;	because start-file/end-file are also emitted after track-change and for playlists,
     ;;	and don't correspond to actual playback state.
     (unless (emms-player-mpv-proc-playing-p)
       (emms-player-mpv-proc-playing t)
       (emms-player-started emms-player-mpv)))
    ("end-file"
     (when (emms-player-mpv-proc-playing-p)
       (emms-player-mpv-proc-playing nil)
       (emms-player-stopped))
     (when emms-player-mpv-ipc-stop-command
       (unless (eq emms-player-mpv-ipc-stop-command t)
         (funcall emms-player-mpv-ipc-stop-command))
       (setq emms-player-mpv-ipc-stop-command nil)))
    ("idle"
     ;; Can mean any kind of error before or during playback.
     ;; Example can be access/format error, resulting in start+end without playback-restart.
     (cancel-timer emms-player-mpv-idle-timer)
     (setq
      emms-player-mpv-idle-timer
      (run-at-time emms-player-mpv-idle-delay nil #'emms-player-mpv-event-idle)
      emms-player-mpv-ipc-stop-command nil))
    ("start-file" (cancel-timer emms-player-mpv-idle-timer))))


;; ----- Metadata update hooks

(defun emms-player-mpv-info-meta-connect-func ()
  "Hook function for `emms-player-mpv-event-connect-hook' to update metadata from mpv."
  (emms-player-mpv-observe-property 'metadata))

(defun emms-player-mpv-info-meta-event-func (json-data)
  "Hook function for `emms-player-mpv-event-functions' to update metadata from mpv."
  (when
      (and
       (string= (alist-get 'event json-data)
                "property-change")
       (string= (alist-get 'name json-data)
                "metadata"))
    (let ((info-alist (alist-get 'data json-data)))
      (when info-alist (emms-player-mpv-info-meta-update-track info-alist)))))

(defun emms-player-mpv-info-meta-update-track (info-alist &optional track)
  "Update TRACK with mpv metadata from INFO-ALIST.
`emms-playlist-current-selected-track' is used by default."
  (mapc
   (lambda (cc)
     (setcar cc (intern (downcase (symbol-name (car cc))))))
   info-alist)
  (cl-macrolet
      ((key (k)
            `(alist-get ',k info-alist))
       (set-track-info (track &rest body)
                       (cons 'progn
                             (cl-loop for (k v)
                                      on body by 'cddr collect
                                      `(let ((value ,v))
                                         (when value
                                           (emms-track-set ,track ',(intern (format "info-%s" k))
                                                           value)))))))
    (unless track (setq track (emms-playlist-current-selected-track)))
    (set-track-info track
                    title (or (key title)
                              (key icy-title))
                    artist (or (key artist)
                               (key album_artist)
                               (key icy-name))
                    album (key album)
                    tracknumber (key track)
                    year (key date)
                    genre (key genre)
                    note (key comment))
    (emms-track-updated track)))

(defun emms-player-mpv-info-duration-event-func (json-data)
  "Hook function for `emms-player-mpv-event-functions' to update track duration from mpv."
  (when
      (string= (alist-get 'event json-data)
               "playback-restart")
    (emms-player-mpv-info-duration-check)))

(defun emms-player-mpv-info-duration-check ()
  "Check whether current mpv track has reliable duration info and request it."
  (emms-player-mpv-ipc-req-send '(get_property stream-end)
                                (lambda (pts-end err)
                                  (if err
                                      (unless (and (stringp err)
                                                   (string= err "property unavailable"))
                                        (emms-player-mpv-ipc-req-error-printer pts-end err))
                                    (when pts-end
                                      (emms-player-mpv-ipc-req-send '(get_property duration)
                                                                    #'emms-player-mpv-info-duration-handler))))))

(defun emms-player-mpv-info-duration-handler (duration err)
  "Duration property request handler to update it for current emms track."
  (if err
      (emms-player-mpv-debug-msg "duration-req-error: %s" err)
    ;; Duration can be nil or 0 for network streams, depending on version/stream
    (when (and (numberp duration)
               (> duration 0))
      (let
          ((duration (round duration))
           (track (emms-playlist-current-selected-track)))
        (emms-track-set track 'info-playing-time duration)
        (emms-track-set track 'info-playing-time-min (/ duration 60))
        (emms-track-set track 'info-playing-time-sec (% duration 60))))))


;; ----- High-level EMMS interface

(defun emms-player-mpv-cmd (cmd &optional handler)
  "Send mpv command to process/connection if both are running,
or otherwise schedule start/connect and set
`emms-player-mpv-ipc-start-track' for `emms-player-mpv-ipc-sentinel'."
  (setq emms-player-mpv-ipc-connect-command nil)
  (let ((proc (emms-player-mpv-ipc)))
    (if proc
        (if (emms-player-mpv-ipc-fifo-p)
            (emms-player-mpv-ipc-fifo-cmd cmd proc)
          (emms-player-mpv-ipc-req-send cmd handler proc))
      (setq emms-player-mpv-ipc-connect-command cmd))))

(defmacro emms-player-mpv-cmd-prog (cmd &rest handler-body)
  "Macro around `emms-player-mpv-cmd' that creates
handler callback (see `emms-player-mpv-ipc-req-send') from HANDLER-BODY forms,
which have following bindings:
- mpv-cmd for CMD.
- mpv-data for response data (decoded json, nil if none).
- mpv-error for response error (nil if no error, decoded json or 'connection-error)."
  `(emms-player-mpv-cmd ,cmd (apply-partially
                              (lambda (mpv-cmd mpv-data mpv-error)
                                ,@handler-body)
                              ,cmd)))


(defun emms-player-mpv-playable-p (track)
  (memq (emms-track-type track)
        '(file url streamlist playlist)))

(defun emms-player-mpv-start-error-handler (mpv-cmd mpv-data mpv-error)
  "Playback-restart error handler for `emms-player-mpv-cmd',
to restart/reconnect-to mpv and re-run MPV-CMD,
if there was any issue when trying to start it initially."
  (if (eq mpv-error 'connection-error)
      ;; Reconnect and restart playback if current connection fails (e.g. mpv crash)
      (emms-player-mpv-cmd-prog
       (emms-player-mpv-cmd mpv-cmd)
       (emms-player-mpv-cmd `(set pause no)))
    (emms-player-mpv-cmd `(set pause no))))

(defun emms-player-mpv-start (track)
  (setq emms-player-mpv-stopped nil)
  (emms-player-mpv-proc-playing nil)
  (let
      ((track-name (emms-track-get track 'name))
       (track-is-playlist (memq (emms-track-get track 'type)
                                '(streamlist playlist))))
    (if (emms-player-mpv-ipc-fifo-p)
        (progn
          ;; ipc-stop is to clear any buffered commands
          (emms-player-mpv-ipc-stop)
          (emms-player-mpv-proc-init (if track-is-playlist "--playlist" "--")
                                     track-name)
          (emms-player-started emms-player-mpv))
      (let*
          ((start-cmd (list (if track-is-playlist 'loadlist 'loadfile)
                            track-name 'replace))
           (start-func `(lambda ()
                          (emms-player-mpv-cmd ',start-cmd
                                               (apply-partially 'emms-player-mpv-start-error-handler ',start-cmd)))))
        (if emms-player-mpv-ipc-stop-command
            (setq emms-player-mpv-ipc-stop-command start-func)
          (funcall start-func))))))

(defun emms-player-mpv-stop ()
  (setq
   emms-player-mpv-stopped t
   emms-player-mpv-ipc-stop-command t)
  (emms-player-mpv-proc-playing nil)
  (emms-player-mpv-cmd `(stop))
  (emms-player-stopped))


(defun emms-player-mpv-pause ()
  (emms-player-mpv-cmd `(set pause yes)))

(defun emms-player-mpv-resume ()
  (emms-player-mpv-cmd `(set pause no)))

(defun emms-player-mpv-seek (sec)
  (emms-player-mpv-cmd `(seek ,sec relative)))

(defun emms-player-mpv-seek-to (sec)
  (emms-player-mpv-cmd `(seek ,sec absolute)))

(emms-player-set emms-player-mpv 'pause #'emms-player-mpv-pause)
(emms-player-set emms-player-mpv 'resume #'emms-player-mpv-resume)
(emms-player-set emms-player-mpv 'seek #'emms-player-mpv-seek)
(emms-player-set emms-player-mpv 'seek-to #'emms-player-mpv-seek-to)


(provide 'emms-player-mpv)
;;; emms-player-mpv.el ends here
