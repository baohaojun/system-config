;;; clang-server.el --- Auto Completion source by libclang for GNU Emacs -*- lexical-binding: t; -*-

;;; last updated : 2018/07/09.20:57:00

;; Copyright (C) 2010       Brian Jiang
;; Copyright (C) 2012       Taylan Ulrich Bayirli/Kammer
;; Copyright (C) 2013       Golevka
;; Copyright (C) 2013-2018  yaruopooner
;; 


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:



(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'pp))
(eval-when-compile (require 'json))


;; client version
(defconst clang-server-version "2.1.1")




;;;
;;; Server Variables
;;;

;; clang-server binary type
(defvar clang-server-type 'release
  "clang-server binary type
`release'  : release build version
`debug'    : debug build version (server develop only)
`test'     : feature test version (server develop only)")


;; server binary launch options(client side)
(defvar clang-server-pipe-buffer-size nil
  "This value is use shadowing for variable value of `w32-pipe-buffer-size' when execute 'start-process' for server.
Ideal value is 4kB over. see help of `w32-pipe-buffer-size'.
If the value is nil, will be use the largest value of `clang-server-stdin-buffer-size' or `clang-server-stdout-buffer-size'.")

(eval-when-compile
  (unless (boundp 'w32-pipe-buffer-size)
    (defvar w32-pipe-buffer-size nil
      "Dummy variable for prevent byte compile warning. (The environment without windows-nt.)")))


;; server binary launch options(server side)
(defvar clang-server-stdin-buffer-size nil
  "STDIN buffer size of the server. value range is 1 - 5 MB. 
If the value is nil, will be allocated 1MB.
The value is specified in megabytes.
e.g. 
(setq clang-server-stdin-buffer-size 1) is 1MB.")

(defvar clang-server-stdout-buffer-size nil
  "STDOUT buffer size of the server. value range is 1 - 5 MB. 
If the value is nil, will be allocated 1MB.
The value is specified in MB.
e.g. 
(setq clang-server-stdout-buffer-size 1) is 1MB.")

(defvar clang-server-input-data-type 's-expression
  "The server receive(STDIN) data type.
`s-expression' : s-expression format (default)
`json'         : json format")

(defvar clang-server-output-data-type 's-expression
  "The server send(STDOUT) data type.
`s-expression' : s-expression format (default)
`json'         : json format")

(defvar clang-server-logfile nil
  "IPC records output file.(for debug)")


;; server binaries property list
(defconst clang-server--binaries '(release "clang-server"
                                   debug   "clang-server-debug"
                                   test    "clang-server-test"))


;; server binary require version
(defconst clang-server--require-version '(2 0 0)
  "(MAJOR MINOR MAINTENANCE)")


;; server process details
(defcustom clang-server--executable nil
  "Location of clang-server executable."
  :group 'clang-server
  :type 'file)


(defconst clang-server--process-name "Clang-Server")

(defconst clang-server--process-buffer-name "*Clang-Server*")

(defvar clang-server--process nil)
(defvar clang-server--status 'idle
  "clang-server status
`idle'          : job is nothing
`receive'       : receiving command sent result
`transaction'   : transaction execute to received command result
`shutdown'      : shutdown server")


;; clang-server behaviors
(defvar clang-server-translation-unit-flags "CXTranslationUnit_DetailedPreprocessingRecord|CXTranslationUnit_Incomplete|CXTranslationUnit_PrecompiledPreamble|CXTranslationUnit_CacheCompletionResults|CXTranslationUnit_IncludeBriefCommentsInCodeCompletion|CXTranslationUnit_CreatePreambleOnFirstParse"
  "CXTranslationUnit Flags. 
for Server behavior.
The value sets flag-name strings or flag-name combined strings.
Separator is `|'.
`CXTranslationUnit_DetailedPreprocessingRecord'            : Required if you want jump to macro declaration, inclusion-file.
`CXTranslationUnit_Incomplete'                             : for PCH
`CXTranslationUnit_PrecompiledPreamble'                    : Increase completion performance.
`CXTranslationUnit_CacheCompletionResults'                 : Increase completion performance.
`CXTranslationUnit_ForSerialization'                       :  
`CXTranslationUnit_CXXChainedPCH'                          :  
`CXTranslationUnit_SkipFunctionBodies'                     :  
`CXTranslationUnit_IncludeBriefCommentsInCodeCompletion'   : Required if you want to brief-comment of completion.
`CXTranslationUnit_CreatePreambleOnFirstParse'             : Increase completion performance.
`CXTranslationUnit_KeepGoing'                              : 
`CXTranslationUnit_SingleFileParse'                        : ")

(defvar clang-server-complete-at-flags "CXCodeComplete_IncludeMacros|CXCodeComplete_IncludeCodePatterns|CXCodeComplete_IncludeBriefComments"
  "CXCodeComplete Flags. 
for Server behavior.
The value sets flag-name strings or flag-name combined strings.
Separator is `|'.
`CXCodeComplete_IncludeMacros'                             :
`CXCodeComplete_IncludeCodePatterns'                       :
`CXCodeComplete_IncludeBriefComments'                      : You need to set `CXTranslationUnit_IncludeBriefCommentsInCodeCompletion' in `clang-server-translation-unit-flags'.

This flags is same as below clang command line options. 
-code-completion-macros
-code-completion-patterns
-code-completion-brief-comments")

(defvar clang-server-complete-results-limit 0
  "acceptable number of result candidate. 
for Server behavior.
`clang-server-complete-results-limit' == 0 : accept all candidates.
`clang-server-complete-results-limit' != 0 : if number of result candidates greater than `clang-server-complete-results-limit', discard all candidates.")


;; client behaviors
(defvar clang-server-session-establishing-buffers-finalize-hooks nil
  "Normal hook for per buffer of `clang-server-session-establishing-buffers'.
That is run before server shutdown when execute clang-server-finalize.
Hook is execution after switch to each buffer.")


(defvar clang-server-finalize-hooks nil
  "Normal hook that is run before shutdown when execute clang-server-finalize.")


(defvar clang-server-tmp-pch-automatic-cleanup-p (eq system-type 'windows-nt)
  "automatically cleanup for generated temporary precompiled headers.")


(defvar clang-server-automatic-recovery-p t
  "automatically recover server when command queue reached limitation.")




;;;
;;; Session Variables
;;;

(defvar-local clang-server--session-name nil)

(defvar clang-server-session-establishing-buffers nil
  "This is a list of buffers establishing a session with clang-server process.")


;; CFLAGS builder behaviors
(defvar-local clang-server-language-option-function nil
  "Function to return the language type for option -x.")

(defvar-local clang-server-prefix-header nil
  "The prefix header to pass to the Clang executable.")


;; clang-server session behavior
(defvar-local clang-server-cflags nil
  "Extra flags to pass to the Clang executable.
This variable will typically contain include paths, 
e.g., (\"-I~/MyProject\" \"-I.\" \"-D _RUNTIME_DEBUG\" \"-D _M_IX86_FP=0\").")




;;;
;;; Transaction Variables
;;;

(defvar clang-server--transaction-id 0)

(defvar clang-server--transaction-hash (make-hash-table :test #'eq))
(defvar clang-server--transaction-limit 10)


(defvar clang-server--command-result-data nil)


(defconst clang-server--packet-encoder/decoder-infos '(s-expression
                                                       (:encoder
                                                        clang-server--encode-s-expression-packet
                                                        :decoder
                                                        clang-server--decode-s-expression-packet)
                                                       json
                                                       (:encoder
                                                        clang-server--encode-json-packet
                                                        :decoder
                                                        clang-server--decode-json-packet)))


(defvar clang-server--packet-encoder nil
  "Specify the function to be used encoding.
Automatic set from value of `clang-server-input-data-type'.
#'clang-server--encode-s-expression-packet
#'clang-server--encode-json-packet")

(defvar clang-server--packet-decoder nil
  "Specify the function to be used decoding.
Automatic set from value of `clang-server-output-data-type'.
#'clang-server--decode-s-expression-packet
#'clang-server--decode-json-packet")




;; transaction send packet debug
(defconst clang-server--debug-log-buffer-name "*Clang-Log*")
(defvar clang-server-debug-log-buffer-p nil)
(defvar clang-server-debug-log-buffer-size (* 50 1024))


;; transaction performance profiler debug
(defvar clang-server-debug-profiler-p nil)
(defvar clang-server--debug-profiler-hash (make-hash-table :test #'eq))
(defvar clang-server--debug-profiler-display-marks '((:transaction-register :packet-receive)
                                                     (:packet-receive :packet-decode)
                                                     (:packet-decode :transaction-receiver)
                                                     (:transaction-register :transaction-receiver)))




;;;
;;; Primitive Functions
;;;

;; server launch option builder
(defun clang-server--build-launch-options ()
  (append 
   (when clang-server-stdin-buffer-size
     (list "--stdin-buffer-size" (format "%d" clang-server-stdin-buffer-size)))
   (when clang-server-stdout-buffer-size
     (list "--stdout-buffer-size" (format "%d" clang-server-stdout-buffer-size)))
   (when clang-server-input-data-type
     (list "--input-data" (format "%S" clang-server-input-data-type)))
   (when clang-server-output-data-type
     (list "--output-data" (format "%S" clang-server-output-data-type)))
   (when clang-server-logfile
     (list "--logfile" (format "%s" clang-server-logfile)))))


;; CFLAGS builders
(defsubst clang-server--build-language-option ()
  (or (and clang-server-language-option-function
           (funcall clang-server-language-option-function))
      (cond ((eq major-mode 'c++-mode)
             "c++")
            ((eq major-mode 'c-mode)
             "c")
            ((eq major-mode 'objc-mode)
             (cond ((string= "m" (file-name-extension (buffer-file-name)))
                    "objective-c")
                   (t
                    "objective-c++")))
            (t
             "c++"))))


;; CFLAGS builders
(defsubst clang-server--build-complete-cflags ()
  (append '("-cc1" "-fsyntax-only")
          (list "-x" (clang-server--build-language-option))
          clang-server-cflags
          (when (stringp clang-server-prefix-header)
            (list "-include-pch" (expand-file-name clang-server-prefix-header)))))




;;;
;;; source code utilities
;;;

;; (defsubst clang-server--get-column-bytes ()
;;   (1+ (length (encode-coding-string (buffer-substring-no-properties (line-beginning-position) (point)) 'binary))))


(defsubst clang-server--column-number-at-pos (point)
  (save-excursion
    (goto-char point)
    (1+ (length (encode-coding-string (buffer-substring-no-properties (line-beginning-position) point) 'binary)))))


(defsubst clang-server--get-buffer-bytes ()
  (1- (position-bytes (point-max))))


(defmacro clang-server--with-widening (&rest body)
  (declare (indent 0) (debug t))
  `(save-restriction
     (widen)
     (progn ,@body)))


(defun clang-server--get-source-code ()
  (clang-server--with-widening
    (let ((source-buffuer (current-buffer))
          (cs (coding-system-change-eol-conversion buffer-file-coding-system 'unix)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer source-buffuer
            (decode-coding-region (point-min) (point-max) cs temp-buffer)))

        (buffer-substring-no-properties (point-min) (point-max))))))


;; (defmacro clang-server--with-running-server (&rest body)
;;   (declare (indent 0) (debug t))
;;   (when (eq (process-status clang-server--process) 'run)
;;     `(progn ,@body)))




;;;
;;; performance profiler functions for IPC
;;;

;; (defsubst clang-server--mark-and-register-profiler (transaction-id mark-property)
;;   (when clang-server-debug-profiler-p
;;     (setf (gethash transaction-id clang-server--debug-profiler-hash) (append (gethash transaction-id clang-server--debug-profiler-hash) `(,mark-property ,(float-time))))))

(defmacro clang-server--mark-and-register-profiler (transaction-id mark-property)
  `(when clang-server-debug-profiler-p
     (setf (gethash ,transaction-id clang-server--debug-profiler-hash) (append (gethash ,transaction-id clang-server--debug-profiler-hash) (list ,mark-property (float-time))))))


;; (defsubst clang-server--mark-profiler (profile-plist mark-property)
;;   (when clang-server-debug-profiler-p
;;     (set profile-plist (append (symbol-value profile-plist) `(,mark-property ,(float-time))))))

(defmacro clang-server--mark-profiler (profile-plist mark-property)
  `(when clang-server-debug-profiler-p
     (setq ,profile-plist (append ,profile-plist (list ,mark-property (float-time))))))


;; (defsubst clang-server--append-profiler (transaction-id profile-plist)
;;   (when clang-server-debug-profiler-p
;;     (setf (gethash transaction-id clang-server--debug-profiler-hash) (append (gethash transaction-id clang-server--debug-profiler-hash) profile-plist))))

(defmacro clang-server--append-profiler (transaction-id profile-plist)
  `(when clang-server-debug-profiler-p
     (setf (gethash ,transaction-id clang-server--debug-profiler-hash) (append (gethash ,transaction-id clang-server--debug-profiler-hash) ,profile-plist))))


(defun clang-server--display-profiler (transaction-id)
  (when clang-server-debug-profiler-p
    (let ((plist (gethash transaction-id clang-server--debug-profiler-hash)))
      (when plist
        (message "client : Performance Profiler : transaction-id : %d" transaction-id)
        (message "client :  [ mark-begin                => mark-end                  ] elapsed-time(s)")
        (message "client : -----------------------------------------------------------------------")
        (cl-dolist (begin-end clang-server--debug-profiler-display-marks)
          (let* ((begin (nth 0 begin-end))
                 (end (nth 1 begin-end))
                 (begin-time (plist-get plist begin))
                 (end-time (plist-get plist end)))
            (when (and begin-time end-time)
              (message "client :  [ %-25s => %-25s ] %8.3f" (symbol-name begin) (symbol-name end) (- end-time begin-time)))))))))


(defun clang-server--display-server-profiler (profiles)
  (message "server : Sampled Profiles")
  (message "server :  scope-name                               : elapsed-time(ms)")
  (message "server : -----------------------------------------------------------------------")
  ;; type of profiles is vector
  (mapc (lambda (profile)
          (let ((name (plist-get profile :Name))
                (elapsed-time (plist-get profile :ElapsedTime)))
            (message "server :  %-40s : %8.3f" name elapsed-time)))
        profiles)
  ;; (message "server : server side profiles")
  ;; (message "%S" profiles)
  )




;;;
;;; transaction functions for IPC
;;;

(defsubst clang-server--register-transaction (transaction)
  ;; (message "clang-server--register-transaction : %s" transaction)
  (clang-server--mark-and-register-profiler clang-server--transaction-id :transaction-register)
  (puthash clang-server--transaction-id transaction clang-server--transaction-hash))


(defsubst clang-server--unregister-transaction (transaction-id)
  (let ((transaction (gethash transaction-id clang-server--transaction-hash)))
    (when transaction
      (remhash transaction-id clang-server--transaction-hash))
    transaction))


(defsubst clang-server--count-transaction ()
  (hash-table-count clang-server--transaction-hash))


(defsubst clang-server--query-transaction (transaction-id)
  (gethash transaction-id clang-server--transaction-hash))


(defsubst clang-server--clear-transaction ()
  (clrhash clang-server--transaction-hash))


(defsubst clang-server-request-transaction (sender-function receiver-function args)
  (if (< (clang-server--count-transaction) clang-server--transaction-limit)
      (progn
        (when receiver-function
          (clang-server--register-transaction `(:receiver ,receiver-function :sender ,sender-function :args ,args)))
        (funcall sender-function args))

    ;; This is recovery logic.
    (message "clang-server : The number of requests of the transaction reached the limit.")
    (when clang-server-automatic-recovery-p
      (clang-server--clear-transaction)
      ;; Send command
      (clang-server-get-server-specification)
      ;; Process response wait(as with thread preemption point)
      (sleep-for 0.1)
      ;; When process response is not received, I suppose that server became to deadlock.
      (if (= (clang-server--count-transaction) 0)
          (message "clang-server : clear transaction requests.")
        (clang-server-reboot)))))




;;;
;;; send primitive functions for IPC
;;;

(defsubst clang-server--process-send-string (string)
  (when clang-server-debug-log-buffer-p
    (let ((log-buffer (get-buffer-create clang-server--debug-log-buffer-name)))
      (when log-buffer
        (with-current-buffer log-buffer
          (when (and clang-server-debug-log-buffer-size (> (buffer-size) clang-server-debug-log-buffer-size))
            (erase-buffer))

          (goto-char (point-max))
          (pp (encode-coding-string string 'binary) log-buffer)
          (insert "\n")))))

  (process-send-string clang-server--process string))




;;;
;;; encoder/decoder for Packet
;;;

(defsubst clang-server--encode-plane-text-packet (data)
  data)

(defsubst clang-server--decode-plane-text-packet (data)
  data)


(defsubst clang-server--encode-s-expression-packet (data)
  (format "%S" data))
  ;; (clang-server--mark-and-register-profiler clang-server--transaction-id :packet-encode)
  ;; (let ((pp-escape-newlines nil))
  ;;   (pp-to-string data)))

(defsubst clang-server--decode-s-expression-packet (data)
  (read data))


(defsubst clang-server--encode-json-packet (data)
  (let* ((json-object-type 'plist)
         (json-array-type 'vector))
    (json-encode data))
  ;; (clang-server--mark-and-register-profiler clang-server--transaction-id :packet-encode)
  )

(defsubst clang-server--decode-json-packet (data)
  (let* ((json-object-type 'plist)
         (json-array-type 'vector))
    (json-read-from-string data)))




;;;
;;; command packet utilities
;;;

(defsubst clang-server--create-command-context (command-plist)
  ;; (message "clang-server--create-command-context : transaction-id %d" clang-server--transaction-id)
  (let* ((header `(:RequestId ,clang-server--transaction-id))
         (context (append header command-plist (and clang-server-debug-profiler-p '(:IsProfile t)))))
    ;; Caution:
    ;; The reference and change of transaction-id must be completed before transmitting the packet.
    ;; The reason is that there is a possibility that references and changes to the transaction-id 
    ;; may be made in the background at the time of packet transmission, resulting in a state like a thread data race.
    (setq clang-server--transaction-id (1+ clang-server--transaction-id))
    context))

(defsubst clang-server--send-command-packet (context)
  (let* ((packet-object (funcall clang-server--packet-encoder context))
         (packet-size (length packet-object))
         (send-object (concat (format "PacketSize:%d\n" packet-size) packet-object)))
    (clang-server--process-send-string send-object)))


;; immediate create and send
(defsubst clang-server--send-command (&rest command-plist)
  (let ((context (clang-server--create-command-context command-plist))
        ;; (clang-server-debug-log-buffer-p t) ; for debug
        )
    (clang-server--send-command-packet context)))




;;;
;;; server command sender functions for IPC
;;;

(defun clang-server--send-specification-command (&optional _args)
  (clang-server--send-command :CommandType "Server"
                              :CommandName "GET_SPECIFICATION"))


(defun clang-server--send-clang-parameters-command (&optional _args)
  (clang-server--send-command :CommandType "Server"
                              :CommandName "SET_CLANG_PARAMETERS"
                              :TranslationUnitFlags clang-server-translation-unit-flags
                              :CompleteAtFlags clang-server-complete-at-flags
                              :CompleteResultsLimit clang-server-complete-results-limit))


(defun clang-server--send-create-session-command (&optional _args)
  (clang-server--with-widening
    (clang-server--send-command :CommandType "Server"
                                :CommandName "CREATE_SESSION"
                                :SessionName clang-server--session-name
                                :CFLAGS (clang-server--build-complete-cflags)
                                :SourceCode (clang-server--get-source-code))))


(defun clang-server--send-delete-session-command (&optional _args)
  (clang-server--send-command :CommandType "Server"
                              :CommandName "DELETE_SESSION"
                              :SessionName clang-server--session-name))


(defun clang-server--send-reset-command (&optional _args)
  (clang-server--send-command :CommandType "Server"
                              :CommandName "RESET"))


(defun clang-server--send-shutdown-command (&optional _args)
  (when (eq (process-status clang-server--process) 'run)
    (clang-server--send-command :CommandType "Server"
                                :CommandName "SHUTDOWN")))


(defun clang-server-send-suspend-command (&optional _args)
  (clang-server--send-command :CommandType "Session"
                              :CommandName "SUSPEND"
                              :SessionName clang-server--session-name))


(defun clang-server-send-resume-command (&optional _args)
  (clang-server--send-command :CommandType "Session"
                              :CommandName "RESUME"
                              :SessionName clang-server--session-name))


(defun clang-server--send-cflags-command (&optional _args)
  (if (listp clang-server-cflags)
      (clang-server--with-widening
        (clang-server--send-command :CommandType "Session"
                                    :CommandName "SET_CFLAGS"
                                    :SessionName clang-server--session-name
                                    :CFLAGS (clang-server--build-complete-cflags)
                                    :SourceCode (clang-server--get-source-code)))
    (message "`clang-server-cflags' should be a list of strings")))


(defun clang-server--send-reparse-command (&optional _args)
  (clang-server--with-widening
    (clang-server--send-command :CommandType "Session"
                                :CommandName "REPARSE"
                                :SessionName clang-server--session-name
                                :SourceCode (clang-server--get-source-code))))


(defun clang-server-send-completion-command (&optional args)
  (clang-server--with-widening
    (clang-server--send-command :CommandType "Session"
                                :CommandName "COMPLETION"
                                :SessionName clang-server--session-name
                                :Line (line-number-at-pos (plist-get args :start-point))
                                :Column (clang-server--column-number-at-pos (plist-get args :start-point))
                                :SourceCode (clang-server--get-source-code))))


(defun clang-server-send-diagnostics-command (&optional _args)
  (clang-server--with-widening
    (clang-server--send-command :CommandType "Session"
                                :CommandName "SYNTAXCHECK"
                                :SessionName clang-server--session-name
                                :SourceCode (clang-server--get-source-code))))


(defun clang-server-send-inclusion-command (&optional _args)
  (clang-server--with-widening
    (clang-server--send-command :CommandType "Session"
                                :CommandName "INCLUSION"
                                :SessionName clang-server--session-name
                                :Line (line-number-at-pos (point))
                                :Column (clang-server--column-number-at-pos (point))
                                :SourceCode (clang-server--get-source-code))))


(defun clang-server-send-definition-command (&optional _args)
  (clang-server--with-widening
    (clang-server--send-command :CommandType "Session"
                                :CommandName "DEFINITION"
                                :SessionName clang-server--session-name
                                :Line (line-number-at-pos (point))
                                :Column (clang-server--column-number-at-pos (point))
                                :SourceCode (clang-server--get-source-code))))


(defun clang-server-send-declaration-command (&optional _args)
  (clang-server--with-widening
    (clang-server--send-command :CommandType "Session"
                                :CommandName "DECLARATION"
                                :SessionName clang-server--session-name
                                :Line (line-number-at-pos (point))
                                :Column (clang-server--column-number-at-pos (point))
                                :SourceCode (clang-server--get-source-code))))


(defun clang-server-send-smart-jump-command (&optional _args)
  (clang-server--with-widening
    (clang-server--send-command :CommandType "Session"
                                :CommandName "SMARTJUMP"
                                :SessionName clang-server--session-name
                                :Line (line-number-at-pos (point))
                                :Column (clang-server--column-number-at-pos (point))
                                :SourceCode (clang-server--get-source-code))))




;;;
;;; Receive clang-server responses common filter (receive response by command)
;;;

(defun clang-server--process-filter (process output)
  ;; IPC packet receive phase.
  (setq clang-server--status 'receive)

  (let ((receive-buffer (process-buffer process))
        (receive-buffer-marker (process-mark process))
        profile-plist)

    ;; check the receive buffer allocation
    (unless receive-buffer
      (when (setq receive-buffer (get-buffer-create clang-server--process-buffer-name))
        (set-process-buffer process receive-buffer)
        (with-current-buffer receive-buffer
          (set-marker receive-buffer-marker (point-min-marker)))))

    ;; Output of the server is appended to receive buffer.
    (when receive-buffer
      (with-current-buffer receive-buffer
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char receive-buffer-marker)
          (insert output)
          (set-marker receive-buffer-marker (point)))
        (goto-char receive-buffer-marker)))

    ;; Check the server response termination.('$' is packet termination character.)
    (when (string= (substring output -1 nil) "$")
      (when (> (clang-server--count-transaction) 0)
        ;; IPC packet decode phase.
        (setq clang-server--status 'transaction)
        (clang-server--mark-profiler profile-plist :packet-receive)
        (setq clang-server--command-result-data (clang-server--decode-received-packet receive-buffer))
        (clang-server--mark-profiler profile-plist :packet-decode)

        (let* ((transaction-id (plist-get clang-server--command-result-data :RequestId))
               (command-error (plist-get clang-server--command-result-data :Error))
               (profiles (plist-get clang-server--command-result-data :Profiles))
               (transaction (clang-server--unregister-transaction transaction-id)))

          (when command-error
            ;; server side error.
            (message "clang-server : server command error! : %s" command-error))

          (unless transaction
            (message "clang-server : transaction not found! : %S" transaction-id))

          (when (and transaction (not command-error))
            ;; client side execution phase.

            ;; setup transaction
            (let (;;(transaction-buffer (plist-get transaction :buffer))
                  (transaction-receiver (plist-get transaction :receiver))
                  (transaction-args (plist-get transaction :args)))

              ;; execute transaction receiver.
              (unless (ignore-errors
                        (funcall transaction-receiver clang-server--command-result-data transaction-args)
                        t)
                (message "clang-server : transaction(%d) receiver function execute error! : %s" transaction-id transaction)))
            ;; clear current result data.
            ;; (setq clang-server--command-result-data nil)
            (clang-server--mark-profiler profile-plist :transaction-receiver)
            (clang-server--append-profiler transaction-id profile-plist)
            (clang-server--display-profiler transaction-id)
            (when profiles
              (clang-server--display-server-profiler profiles))
            )))

      ;; clear receive-buffer for next packet.
      (with-current-buffer receive-buffer
        (erase-buffer))
      (setq clang-server--status 'idle))))


(defun clang-server--decode-received-packet (buffer)
  "Result value is property-list(s-expression) that converted from packet."
  (with-current-buffer buffer
    ;; (1- (point-max)) is exclude packet termination character.
    (funcall clang-server--packet-decoder (buffer-substring-no-properties (point-min) (1- (point-max))))))




;;;
;;; sender function for IPC
;;;

(defun clang-server-get-server-specification ()
  (interactive)

  (when clang-server--process
    (clang-server-request-transaction #'clang-server--send-specification-command #'clang-server--receive-server-specification nil)))


(defun clang-server--receive-server-specification (data _args)
  (let ((results (plist-get data :Results)))
    (message "clang-server : server-specification %S" results)))




;;;
;;; The session control functions
;;;

(defun clang-server-activate-session ()
  "Create session for current buffer."

  (unless clang-server--session-name
    (setq clang-server--session-name (buffer-file-name))
    (push (current-buffer) clang-server-session-establishing-buffers)

    (clang-server--send-create-session-command)
    t))


(defun clang-server-deactivate-session ()
  "Delete created session for current buffer."

  (when clang-server--session-name
    (when (clang-server-live-p)
      (clang-server--send-delete-session-command))

    (setq clang-server-session-establishing-buffers (delete (current-buffer) clang-server-session-establishing-buffers))
    (setq clang-server--session-name nil)
    t))


(defun clang-server-reparse-buffer ()
  "Reparse source code of current buffer."

  (when clang-server--session-name
    (clang-server--send-reparse-command)))


(defun clang-server-update-cflags ()
  "Update CFLAGS of current buffer."
  (interactive)

  (when clang-server--session-name
    ;; (message "clang-server-update-cflags %s" clang-server--session-name)
    (clang-server--send-cflags-command)))


(defun clang-server-set-cflags ()
  "Set `clang-server-cflags' interactively."
  (interactive)

  (setq clang-server-cflags (split-string (read-string "New cflags: ")))
  (clang-server-update-cflags))


(defun clang-server-set-cflags-from-shell-command ()
  "Set `clang-server-cflags' to a shell command's output.
  set new cflags for clang-server from shell command output"
  (interactive)

  (setq clang-server-cflags
        (split-string
         (shell-command-to-string
          (read-shell-command "Shell command: " nil nil
                              (and buffer-file-name
                                   (file-relative-name buffer-file-name))))))
  (clang-server-update-cflags))


(defun clang-server-set-prefix-header (prefix-header)
  "Set `clang-server-prefix-header' interactively."
  (interactive
   (let ((default (car (directory-files "." t "\\([^.]h\\|[^h]\\).pch\\'" t))))
     (list
      (read-file-name (concat "Clang prefix header (currently " (or clang-server-prefix-header "nil") "): ")
                      (when default (file-name-directory default))
                      default nil (when default (file-name-nondirectory default))))))

  (cond
   ((string-match "^[\s\t]*$" prefix-header)
    (setq clang-server-prefix-header nil))
   (t
    (setq clang-server-prefix-header prefix-header))))




;;;
;;; The server control functions
;;;

(defun clang-server--clean-tmp-pch ()
  "Clean up temporary precompiled headers."

  (cl-dolist (pch-file (directory-files temporary-file-directory t "preamble-.*\\.pch$" t))
    (ignore-errors
      (delete-file pch-file)
      t)))



(defsubst clang-server-live-p ()
  (process-live-p clang-server--process))


(defun clang-server-launch ()
  (interactive)

  (when (and clang-server--executable (not clang-server--process))
    (let ((process-connection-type nil)
          (process-adaptive-read-buffering nil)
          (w32-pipe-buffer-size (or clang-server-pipe-buffer-size (* (max (or clang-server-stdin-buffer-size 1) (or clang-server-stdout-buffer-size 1)) 1024 1024)))
          (coding-system-for-write 'binary))

      (setq clang-server--process
            (apply #'start-process
                   clang-server--process-name clang-server--process-buffer-name
                   clang-server--executable (clang-server--build-launch-options))))

    (if clang-server--process
        (progn
          ;; transaction initialize
          (setq clang-server--status 'idle)
          (clang-server--clear-transaction)

          ;; packet encoder/decoder configuration
          (setq clang-server--packet-encoder (plist-get (plist-get clang-server--packet-encoder/decoder-infos clang-server-input-data-type) :encoder))
          (setq clang-server--packet-decoder (plist-get (plist-get clang-server--packet-encoder/decoder-infos clang-server-output-data-type) :decoder))

          ;; process configuration
          (set-process-coding-system clang-server--process
                                     (coding-system-change-eol-conversion buffer-file-coding-system nil)
                                     'binary)
          (set-process-filter clang-server--process #'clang-server--process-filter)
          (set-process-query-on-exit-flag clang-server--process nil)

          ;; server configuration
          (clang-server--send-clang-parameters-command)
          t)
      (display-warning 'clang-server "clang-server launch failed.")
      nil)))


(defun clang-server-shutdown ()
  (interactive)

  (when (clang-server-live-p)
    (clang-server--send-shutdown-command))

  (setq clang-server--status 'shutdown)

  (setq clang-server--process nil)

  (when (get-buffer clang-server--process-buffer-name)
    (kill-buffer clang-server--process-buffer-name))

  t)


(defun clang-server-update-clang-parameters ()
  (interactive)

  (when clang-server--process
    (clang-server--send-clang-parameters-command)
    t))


(defun clang-server-reset ()
  (interactive)

  (let ((buffers clang-server-session-establishing-buffers))
    (cl-dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (clang-server-deactivate-session)))))

  (when (clang-server-live-p)
    (clang-server--send-reset-command))
  (setq clang-server-session-establishing-buffers nil)
  t)


(cl-defun clang-server-reboot ()
  (interactive)

  (let ((buffers clang-server-session-establishing-buffers))
    (clang-server-reset)

    (unless (clang-server-shutdown)
      (message "clang-server : reboot failed.")
      (cl-return-from clang-server-reboot nil))

    (unless (clang-server-launch)
      (message "clang-server : reboot failed.")
      (cl-return-from clang-server-reboot nil))

    (cl-dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (clang-server-activate-session)))))

  (message "clang-server : reboot success.")
  t)




(defun clang-server--check-require-version-p ()
  (let ((result (shell-command-to-string (format "%s --version" clang-server--executable))))
    (when (string-match "server version \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" result)
      (let ((major (string-to-number (match-string 1 result)))
            (minor (string-to-number (match-string 2 result)))
            (maintenance (string-to-number (match-string 3 result)))
            (rq-major (nth 0 clang-server--require-version))
            (rq-minor (nth 1 clang-server--require-version))
            (rq-maintenance (nth 2 clang-server--require-version)))
        (or (> major rq-major) (and (= major rq-major) (or (> minor rq-minor) (and (= minor rq-minor) (>= maintenance rq-maintenance)))))))))


(cl-defun clang-server-initialize ()
  (interactive)

  ;; check environment
  (when (and (eq system-type 'windows-nt) (boundp 'w32-pipe-read-delay) (> w32-pipe-read-delay 0))
    (display-warning 'clang-server "Please set the appropriate value for `w32-pipe-read-delay'. Because a pipe delay value is large value. Ideal value is 0. see help of `w32-pipe-read-delay'."))

  ;; decide server binary
  (unless clang-server--executable
    (setq clang-server--executable (executable-find (or (plist-get clang-server--binaries clang-server-type) "")))

    ;; check executable
    (if clang-server--executable
        ;; check version
        (unless (clang-server--check-require-version-p)
          (setq clang-server--executable nil)
          (display-warning 'clang-server (format "clang-server binary is old. please replace new binary. require version is %S over." clang-server--require-version))
          (cl-return-from clang-server-initialize nil))
      ;; not found
      (progn
        (display-warning 'clang-server "clang-server binary not found.")
        (cl-return-from clang-server-initialize nil))))

  ;; launch server
  ;; (message "clang-server-initialize")
  (clang-server-launch))


(defun clang-server-finalize ()
  (interactive)

  ;; (message "clang-server-finalize")
  (unless
      (ignore-errors
        (let ((buffers clang-server-session-establishing-buffers))
          (cl-dolist (buffer buffers)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (run-hooks 'clang-server-session-establishing-buffers-finalize-hooks)))))
        (run-hooks 'clang-server-finalize-hooks)
        t)
    (message "clang-server hooks error!"))

  (clang-server-shutdown)

  (setq clang-server--executable nil)

  (when clang-server-tmp-pch-automatic-cleanup-p
    (clang-server--clean-tmp-pch))

  t)






(provide 'clang-server)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; clang-server.el ends here
