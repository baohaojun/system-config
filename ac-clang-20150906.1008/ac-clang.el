;;; ac-clang.el --- Auto Completion source by libclang for GNU Emacs -*- lexical-binding: t; -*-

;;; last updated : 2015/09/05.04:13:04

;; Copyright (C) 2010       Brian Jiang
;; Copyright (C) 2012       Taylan Ulrich Bayirli/Kammer
;; Copyright (C) 2013       Golevka
;; Copyright (C) 2013-2015  yaruopooner
;; 
;; Original Authors: Brian Jiang <brianjcj@gmail.com>
;;                   Golevka [https://github.com/Golevka]
;;                   Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;                   Many others
;; Author: yaruopooner [https://github.com/yaruopooner]
;; URL: https://github.com/yaruopooner/ac-clang
;; Keywords: completion, convenience, intellisense
;; Version: 1.6.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (auto-complete "1.4.0") (pos-tip "0.4.6") (yasnippet "0.8.0"))


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


;;; Commentary:
;; 
;; * INTRODUCTION:
;;   This program fork from auto-complete-clang-async.el
;;   ac-clang provide code completion and arguments expand.
;;   This program consists of the client(elisp) and server(binary).
;;   The server is executable file, and a self-build is necessary.
;;   The server achieve code completion using libclang of LLVM.
;; 
;; * FEATURES:
;;   - Basic(same auto-complete-clang-async)
;;     Code Completion by libclang.
;;     Auto Completion support. 
;;     Uses a "completion server" process to utilize libclang.
;;     C/C++/Objective-C mode support.
;;     Jump to definition or declaration. return from jumped location. 
;;     Jump is an on-the-fly that doesn't use the tag file.
;;     Also provides flymake syntax checking.
;;     A few bugfix and refactoring.
;;    
;;   - Extension
;;     "completion server" process is 1 process per Emacs. (original version is per buffer)
;;     Template Method Parameters expand support. 
;;     Manual Completion support. 
;;     libclang CXTranslationUnit Flags support. 
;;     libclang CXCodeComplete Flags support. 
;;     Multibyte support. 
;;     Debug Logger Buffer support. 
;;     Jump to inclusion-file. return from jumped location. 
;;     more a few modified. (client & server)
;;    
;;   - Optional
;;     CMake support.
;;     clang-server.exe and libclang.dll built with Microsoft Visual Studio 2015/2013.
;;     x86_64 Machine Architecture + Windows Platform support. (Visual Studio Predefined Macros)
;; 
;; * EASY INSTALLATION(Windows Only):
;;   - Visual C++ Redistributable Packages for Visual Studio 2015/2013
;;     Must be installed if don't have a Visual Studio 2015/2013.
;; 
;;     - 2015
;;       [http://www.microsoft.com/download/details.aspx?id=48145]
;;     - 2013
;;       [http://www.microsoft.com/download/details.aspx?id=40784]
;;    
;;   - Completion Server Program
;;     Built with Microsoft Visual Studio 2015/2013.
;;     [https://github.com/yaruopooner/ac-clang/releases]
;;     1. download clang-server.zip
;;     2. clang-server.exe and libclang.dll is expected to be available in the PATH or in Emacs' exec-path.
;;    
;; * STANDARD INSTALLATION(Linux, Windows):
;;   Generate a Unix Makefile or a Visual Studio Project by CMake.
;; 
;;   - Self-Build step
;;     1. LLVM
;;        checkout, apply patch, generate project, build
;;        It is recommended that you use this shell.
;;        [https://github.com/yaruopooner/llvm-build-shells.git]
;; 
;;     2. Clang Server
;;        generate project, build
;; 
;;     see clang-server's reference manual.
;;     ac-clang/clang-server/readme.org
;; 
;; * NOTICE:
;;   - LLVM libclang.[dll, so, ...]
;;     This binary is not official binary.
;;     Because offical libclang has mmap lock problem.
;;     Applied a patch to LLVM's source code in order to solve this problem.
;; 
;;     see clang-server's reference manual.
;;     ac-clang/clang-server/readme.org
;; 


;; Usage:
;; * DETAILED MANUAL:
;;   For more information and detailed usage, refer to the project page:
;;   [https://github.com/yaruopooner/ac-clang]
;; 
;; * SETUP:
;;   (require 'ac-clang)
;; 
;;   (setq w32-pipe-read-delay 0)          ;; <- Windows Only
;; 
;;   (when (ac-clang-initialize)
;;     (add-hook 'c-mode-common-hook '(lambda ()
;;                                      (setq ac-clang-cflags CFLAGS)
;;                                      (ac-clang-activate-after-modify))))
;; 
;; * DEFAULT KEYBIND
;;   - start auto completion
;;     code completion & arguments expand
;;     `.` `->` `::`
;;   - start manual completion
;;     code completion & arguments expand
;;     `<tab>`
;;   - jump to inclusion-file, definition, declaration / return from it
;;     this is nestable jump.
;;     `M-.` / `M-,`
;; 

;;; Code:



(require 'cl-lib)
(require 'auto-complete)
(require 'pos-tip)
(require 'yasnippet)
(require 'flymake)




(defconst ac-clang-version "1.6.0")
(defconst ac-clang-libclang-version nil)


;;;
;;; for Server vars
;;;


;; clang-server binary type
(defvar ac-clang-server-type 'release
  "clang-server binary type
`release'  : release build version
`debug'    : debug build version (server develop only)
`x86_64'   : (obsolete. It will be removed in the future.) 64bit release build version
`x86_64d'  : (obsolete. It will be removed in the future.) 64bit debug build version (server develop only)
`x86_32'   : (obsolete. It will be removed in the future.) 32bit release build version
`x86_32d'  : (obsolete. It will be removed in the future.) 32bit debug build version (server develop only)
")


;; clang-server launch option values
(defvar ac-clang-server-stdin-buffer-size nil
  "STDIN buffer size. value range is 1 - 5 MB. 
If the value is nil, will be allocated 1MB.
The value is specified in MB.")

(defvar ac-clang-server-stdout-buffer-size nil
  "STDOUT buffer size. value range is 1 - 5 MB. 
If the value is nil, will be allocated 1MB.
The value is specified in MB.")

(defvar ac-clang-server-logfile nil
  "IPC records output file.(for debug)")


;; server binaries property list
(defconst ac-clang--server-binaries '(release "clang-server"
                                      debug   "clang-server-debug"))

(defconst ac-clang--server-obsolete-binaries '(x86_64  "clang-server-x86_64"
                                               x86_64d "clang-server-x86_64d"
                                               x86_32  "clang-server-x86_32"
                                               x86_32d "clang-server-x86_32d"))


;; server process details
(defcustom ac-clang--server-executable nil
  "Location of clang-server executable."
  :group 'auto-complete
  :type 'file)


(defconst ac-clang--process-name "Clang-Server")

(defconst ac-clang--process-buffer-name "*Clang-Server*")
(defconst ac-clang--completion-buffer-name "*Clang-Completion*")
(defconst ac-clang--diagnostics-buffer-name "*Clang-Diagnostics*")

(defvar ac-clang--server-process nil)
(defvar ac-clang--status 'idle
  "clang-server status
`idle'          : job is nothing
`receive'       : receiving command sent result
`transaction'   : transaction execute to received command result
`shutdown'      : shutdown server
  ")


(defvar ac-clang--activate-buffers nil)


;; server debug
(defconst ac-clang--debug-log-buffer-name "*Clang-Log*")
(defvar ac-clang-debug-log-buffer-p nil)
(defvar ac-clang-debug-log-buffer-size (* 1024 50))


;; clang-server behaviors
(defvar ac-clang-clang-translation-unit-flags "CXTranslationUnit_DetailedPreprocessingRecord|CXTranslationUnit_PrecompiledPreamble|CXTranslationUnit_CacheCompletionResults"
  "CXTranslationUnit Flags. 
for Server behavior.
The value sets flag-name strings or flag-name combined strings.
Separator is `|'.
`CXTranslationUnit_DetailedPreprocessingRecord'            : Required if you want jump to macro declaration, inclusion-file.
`CXTranslationUnit_Incomplete'                             :  
`CXTranslationUnit_PrecompiledPreamble'                    : Increase completion performance.
`CXTranslationUnit_CacheCompletionResults'                 : Increase completion performance.
`CXTranslationUnit_ForSerialization'                       :  
`CXTranslationUnit_CXXChainedPCH'                          :  
`CXTranslationUnit_SkipFunctionBodies'                     :  
`CXTranslationUnit_IncludeBriefCommentsInCodeCompletion'   : Required if you want to brief-comment of completion.
")

(defvar ac-clang-clang-complete-at-flags "CXCodeComplete_IncludeMacros"
  "CXCodeComplete Flags. 
for Server behavior.
The value sets flag-name strings or flag-name combined strings.
Separator is `|'.
`CXCodeComplete_IncludeMacros'
`CXCodeComplete_IncludeCodePatterns'
`CXCodeComplete_IncludeBriefComments'
")

(defvar ac-clang-clang-complete-results-limit 0
  "acceptable number of result candidate. 
for Server behavior.
ac-clang-clang-complete-results-limit == 0 : accept all candidates.
ac-clang-clang-complete-results-limit != 0 : if number of result candidates greater than ac-clang-clang-complete-results-limit, discard all candidates.
")


;; client behaviors
(defvar ac-clang-tmp-pch-automatic-cleanup-p (eq system-type 'windows-nt)
  "automatically cleanup for generated temporary precompiled headers.")


(defvar ac-clang-server-automatic-recovery-p t
  "automatically recover server when command queue reached limitation.")



;;;
;;; for auto-complete vars
;;;

;; clang-server response filter pattern for auto-complete candidates
(defconst ac-clang--completion-pattern "^COMPLETION: \\(%s[^\s\n:]*\\)\\(?: : \\)*\\(.*$\\)")

;; auto-complete behaviors
(defvar ac-clang-async-autocompletion-automatically-p t
  "If autocompletion is automatically triggered when you type `.', `->', `::'")

(defvar ac-clang-async-autocompletion-manualtrigger-key "<tab>")



;; auto-complete faces
(defface ac-clang-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for clang candidate"
  :group 'auto-complete)

(defface ac-clang-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the clang selected candidate."
  :group 'auto-complete)



;;;
;;; for Session vars
;;;

(defvar-local ac-clang--activate-p nil)

(defvar-local ac-clang--session-name nil)

;; for patch
(defvar-local ac-clang--suspend-p nil)


;; auto-complete ac-sources backup
(defvar-local ac-clang--ac-sources-backup nil)


;; auto-complete candidates and completion start-point
(defvar-local ac-clang--candidates nil)
(defvar-local ac-clang--start-point nil)
(defvar-local ac-clang--template-candidates nil)
(defvar-local ac-clang--template-start-point nil)


;; CFLAGS build behaviors
(defvar-local ac-clang-language-option-function nil
  "Function to return the language type for option -x.")

(defvar-local ac-clang-prefix-header nil
  "The prefix header to pass to the Clang executable.")


;; clang-server session behavior
(defvar-local ac-clang-cflags nil
  "Extra flags to pass to the Clang executable.
This variable will typically contain include paths, e.g., (\"-I~/MyProject\" \"-I.\").")


(defvar ac-clang--jump-stack nil
  "The jump stack (keeps track of jumps via jump-inclusion, jump-definition, jump-declaration, jump-smart)") 




;;;
;;; primitive functions
;;;

;; server launch option builder
(defun ac-clang--build-server-launch-options ()
  (append 
   (when ac-clang-server-stdin-buffer-size
     (list "--stdin-buffer-size" (format "%d" ac-clang-server-stdin-buffer-size)))
   (when ac-clang-server-stdout-buffer-size
     (list "--stdout-buffer-size" (format "%d" ac-clang-server-stdout-buffer-size)))
   (when ac-clang-server-logfile
     (list "--logfile" (format "%s" ac-clang-server-logfile)))))


;; CFLAGS builders
(defsubst ac-clang--language-option ()
  (or (and ac-clang-language-option-function
           (funcall ac-clang-language-option-function))
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


(defsubst ac-clang--build-complete-cflags ()
  (append '("-cc1" "-fsyntax-only")
          (list "-x" (ac-clang--language-option))
          ac-clang-cflags
          (when (stringp ac-clang-prefix-header)
            (list "-include-pch" (expand-file-name ac-clang-prefix-header)))))



(defsubst ac-clang--get-column-bytes ()
  (1+ (length (encode-coding-string (buffer-substring-no-properties (line-beginning-position) (point)) 'binary))))


(defsubst ac-clang--get-buffer-bytes ()
  (1- (position-bytes (point-max))))


(defsubst ac-clang--create-position-string (point)
  (save-excursion
    (goto-char point)
    (format "line:%d\ncolumn:%d\n" (line-number-at-pos) (ac-clang--get-column-bytes))))


(defmacro ac-clang--with-widening (&rest body)
  (declare (indent 0) (debug t))
  `(save-restriction
     (widen)
     (progn ,@body)))


;; (defmacro ac-clang--with-running-server (&rest body)
;;   (declare (indent 0) (debug t))
;;   (when (eq (process-status ac-clang--server-process) 'run)
;;     `(progn ,@body)))



;;;
;;; transaction command functions for IPC
;;;

(defvar ac-clang--server-command-queue nil)
(defvar ac-clang--server-command-queue-limit 4)


(defsubst ac-clang--request-command (sender-function receive-buffer receiver-function args)
  (if (< (length ac-clang--server-command-queue) ac-clang--server-command-queue-limit)
      (progn
        (when (and receive-buffer receiver-function)
          (ac-clang--enqueue-command `(:buffer ,receive-buffer :receiver ,receiver-function :sender ,sender-function :args ,args)))
        (funcall sender-function args))
    (message "ac-clang : The number of requests of the command queue reached the limit.")
    ;; This is recovery logic.
    (when ac-clang-server-automatic-recovery-p
      (ac-clang--clear-command-queue)
      ;; Send message
      (ac-clang-get-server-specification)
      ;; Process response wait(as with thread preemption point)
      (sleep-for 0.1)
      ;; When process response is not received, I suppose that server became to deadlock.
      (if (= (length ac-clang--server-command-queue) 0)
          (message "ac-clang : clear server command queue.")
        (ac-clang-reboot-server)))))


(defsubst ac-clang--enqueue-command (command)
  (if ac-clang--server-command-queue
      (nconc ac-clang--server-command-queue (list command))
    (setq ac-clang--server-command-queue (list command))))


(defsubst ac-clang--dequeue-command ()
  (let ((command ac-clang--server-command-queue))
    (setq ac-clang--server-command-queue (cdr command))
    (car command)))


(defsubst ac-clang--get-top-command ()
  (car ac-clang--server-command-queue))


(defsubst ac-clang--clear-command-queue ()
  (setq ac-clang--server-command-queue nil))



;;;
;;; sender primitive functions for IPC
;;;

(defsubst ac-clang--process-send-string (string)
  (process-send-string ac-clang--server-process string)

  (when ac-clang-debug-log-buffer-p
    (let ((log-buffer (get-buffer-create ac-clang--debug-log-buffer-name)))
      (when log-buffer
        (with-current-buffer log-buffer
          (when (and ac-clang-debug-log-buffer-size (> (buffer-size) ac-clang-debug-log-buffer-size))
            (erase-buffer))

          (goto-char (point-max))
          (pp (encode-coding-string string 'binary) log-buffer)
          (insert "\n"))))))


(defsubst ac-clang--process-send-region (start end)
  (process-send-region ac-clang--server-process start end))


(defun ac-clang--send-set-clang-parameters ()
  (ac-clang--process-send-string (format "translation_unit_flags:%s\n" ac-clang-clang-translation-unit-flags))
  (ac-clang--process-send-string (format "complete_at_flags:%s\n" ac-clang-clang-complete-at-flags))
  (ac-clang--process-send-string (format "complete_results_limit:%d\n" ac-clang-clang-complete-results-limit)))


(defun ac-clang--send-cflags ()
  ;; send message head and num_cflags
  (ac-clang--process-send-string (format "num_cflags:%d\n" (length (ac-clang--build-complete-cflags))))

  (let (cflags)
    ;; create CFLAGS strings
    (mapc
     (lambda (arg)
       (setq cflags (concat cflags (format "%s\n" arg))))
     (ac-clang--build-complete-cflags))
    ;; send cflags
    (ac-clang--process-send-string cflags)))


(defun ac-clang--send-source-code ()
  (ac-clang--with-widening
    (let ((source-buffuer (current-buffer))
          (cs (coding-system-change-eol-conversion buffer-file-coding-system 'unix)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer source-buffuer
            (decode-coding-region (point-min) (point-max) cs temp-buffer)))

        (ac-clang--process-send-string (format "source_length:%d\n" (ac-clang--get-buffer-bytes)))
        ;; (ac-clang--process-send-region (point-min) (point-max))
        (ac-clang--process-send-string (buffer-substring-no-properties (point-min) (point-max)))
        (ac-clang--process-send-string "\n\n")))))


;; (defun ac-clang--send-source-code ()
;;   (ac-clang--with-widening
;;     (ac-clang--process-send-string (format "source_length:%d\n" (ac-clang--get-buffer-bytes)))
;;     (ac-clang--process-send-region (point-min) (point-max))
;;     (ac-clang--process-send-string "\n\n")))


(defsubst ac-clang--send-command (command-type command-name &optional session-name)
  (let ((command (format "command_type:%s\ncommand_name:%s\n" command-type command-name)))
    (when session-name
      (setq command (concat command (format "session_name:%s\n" session-name))))
    (ac-clang--process-send-string command)))



;;;
;;; sender command request functions for IPC
;;;

(defun ac-clang--send-server-specification-request (&optional _args)
  (ac-clang--send-command "Server" "GET_SPECIFICATION"))


(defun ac-clang--send-clang-parameters-request (&optional _args)
  (ac-clang--send-command "Server" "SET_CLANG_PARAMETERS")
  (ac-clang--send-set-clang-parameters))


(defun ac-clang--send-create-session-request (&optional _args)
  (ac-clang--with-widening
    (ac-clang--send-command "Server" "CREATE_SESSION" ac-clang--session-name)
    (ac-clang--send-cflags)
    (ac-clang--send-source-code)))


(defun ac-clang--send-delete-session-request (&optional _args)
  (ac-clang--send-command "Server" "DELETE_SESSION" ac-clang--session-name))


(defun ac-clang--send-reset-server-request (&optional _args)
  (ac-clang--send-command "Server" "RESET"))


(defun ac-clang--send-shutdown-request (&optional _args)
  (when (eq (process-status ac-clang--server-process) 'run)
    (ac-clang--send-command "Server" "SHUTDOWN")))


(defun ac-clang--send-suspend-request (&optional _args)
  (ac-clang--send-command "Session" "SUSPEND" ac-clang--session-name))


(defun ac-clang--send-resume-request (&optional _args)
  (ac-clang--send-command "Session" "RESUME" ac-clang--session-name))


(defun ac-clang--send-cflags-request (&optional _args)
  (if (listp ac-clang-cflags)
      (ac-clang--with-widening
        (ac-clang--send-command "Session" "SET_CFLAGS" ac-clang--session-name)
        (ac-clang--send-cflags)
        (ac-clang--send-source-code))
    (message "`ac-clang-cflags' should be a list of strings")))


(defun ac-clang--send-reparse-request (&optional _args)
  (ac-clang--with-widening
    (ac-clang--send-command "Session" "SET_SOURCECODE" ac-clang--session-name)
    (ac-clang--send-source-code)
    (ac-clang--send-command "Session" "REPARSE" ac-clang--session-name)))


(defun ac-clang--send-completion-request (&optional args)
  (ac-clang--with-widening
    (ac-clang--send-command "Session" "COMPLETION" ac-clang--session-name)
    (ac-clang--process-send-string (ac-clang--create-position-string (plist-get args :start-point)))
    (ac-clang--send-source-code)))


(defun ac-clang--send-diagnostics-request (&optional _args)
  (ac-clang--with-widening
    (ac-clang--send-command "Session" "SYNTAXCHECK" ac-clang--session-name)
    (ac-clang--send-source-code)))


(defun ac-clang--send-inclusion-request (&optional _args)
  (ac-clang--with-widening
    (ac-clang--send-command "Session" "INCLUSION" ac-clang--session-name)
    (ac-clang--process-send-string (ac-clang--create-position-string (point)))
    (ac-clang--send-source-code)))


(defun ac-clang--send-definition-request (&optional _args)
  (ac-clang--with-widening
    (ac-clang--send-command "Session" "DEFINITION" ac-clang--session-name)
    (ac-clang--process-send-string (ac-clang--create-position-string (point)))
    (ac-clang--send-source-code)))


(defun ac-clang--send-declaration-request (&optional _args)
  (ac-clang--with-widening
    (ac-clang--send-command "Session" "DECLARATION" ac-clang--session-name)
    (ac-clang--process-send-string (ac-clang--create-position-string (point)))
    (ac-clang--send-source-code)))


(defun ac-clang--send-smart-jump-request (&optional _args)
  (ac-clang--with-widening
    (ac-clang--send-command "Session" "SMARTJUMP" ac-clang--session-name)
    (ac-clang--process-send-string (ac-clang--create-position-string (point)))
    (ac-clang--send-source-code)))




;;;
;;; Receive clang-server responses common filter (receive response by command)
;;;

(defvar ac-clang--transaction-context nil)
(defvar ac-clang--transaction-context-buffer-name nil)
(defvar ac-clang--transaction-context-buffer nil)
(defvar ac-clang--transaction-context-buffer-marker nil)
(defvar ac-clang--transaction-context-receiver nil)
(defvar ac-clang--transaction-context-args nil)


(defun ac-clang--process-filter (process output)
  ;; command parse for context
  (unless ac-clang--transaction-context
    (if (setq ac-clang--transaction-context (ac-clang--dequeue-command))
        ;; setup context
        (progn
          (setq ac-clang--transaction-context-buffer-name (plist-get ac-clang--transaction-context :buffer)
                ac-clang--transaction-context-receiver (plist-get ac-clang--transaction-context :receiver)
                ac-clang--transaction-context-args (plist-get ac-clang--transaction-context :args))
          (setq ac-clang--status 'receive)
          (when ac-clang--transaction-context-buffer-name
            (setq ac-clang--transaction-context-buffer (get-buffer-create ac-clang--transaction-context-buffer-name))
            (with-current-buffer ac-clang--transaction-context-buffer
              (unless (local-variable-p 'ac-clang--transaction-context-buffer-marker)
                ;; The buffer created just now.
                (set (make-local-variable 'ac-clang--transaction-context-buffer-marker) (point-min-marker)))
              (erase-buffer))))
      (progn
        (setq ac-clang--transaction-context-buffer (process-buffer process))
        (setq ac-clang--transaction-context-buffer-marker (process-mark process)))))

  ;; Output of the server is appended to context buffer.
  (when ac-clang--transaction-context-buffer
    (ac-clang--append-to-transaction-context-buffer output))

  ;; Check the server response termination.
  (when (and ac-clang--transaction-context (string= (substring output -1 nil) "$"))
    ;; execute context receiver.
    (setq ac-clang--status 'transaction)
    (unless (ignore-errors
              (funcall ac-clang--transaction-context-receiver ac-clang--transaction-context-buffer output ac-clang--transaction-context-args)
              t)
      (message "ac-clang : receiver function error!"))
    ;; clear current context.
    (setq ac-clang--transaction-context nil
          ac-clang--transaction-context-buffer-name nil
          ac-clang--transaction-context-buffer nil
          ac-clang--transaction-context-receiver nil
          ac-clang--transaction-context-args nil)
    (setq ac-clang--status 'idle)))


(defun ac-clang--append-to-transaction-context-buffer (output)
  "Append output to the transaction context buffer."
  (with-current-buffer ac-clang--transaction-context-buffer
    (save-excursion
      ;; Insert the text, advancing the process marker.
      (goto-char ac-clang--transaction-context-buffer-marker)
      (insert output)
      (set-marker ac-clang--transaction-context-buffer-marker (point)))
    (goto-char ac-clang--transaction-context-buffer-marker)))




;;;
;;; receive clang-server responses. 
;;; build completion candidates and fire auto-complete.
;;;

(defun ac-clang--build-completion-candidates (buffer start-word)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((pattern (format ac-clang--completion-pattern (regexp-quote start-word)))
          candidates
          candidate
          declaration
          (prev-candidate ""))
      (while (re-search-forward pattern nil t)
        (setq candidate (match-string-no-properties 1))
        (unless (string= "Pattern" candidate)
          (setq declaration (match-string-no-properties 2))

          (if (string= candidate prev-candidate)
              (progn
                (when declaration
                  (setq candidate (propertize candidate 'ac-clang--detail (concat (get-text-property 0 'ac-clang--detail (car candidates)) "\n" declaration)))
                  (setf (car candidates) candidate)))
            (setq prev-candidate candidate)
            (when declaration
              (setq candidate (propertize candidate 'ac-clang--detail declaration)))
            (push candidate candidates))))
      candidates)))


(defun ac-clang--receive-completion (buffer _output args)
  (setq ac-clang--candidates (ac-clang--build-completion-candidates buffer (plist-get args :start-word)))
  (setq ac-clang--start-point (plist-get args :start-point))

  ;; (setq ac-show-menu t)
  ;; (ac-start :force-init t)
  ;; (ac-update))
  (ac-complete-clang-async))



(defun ac-clang--get-autotrigger-start-point (&optional point)
  (unless point
    (setq point (point)))
  (let ((c (char-before point)))
    (when (or 
           ;; '.'
           (eq ?. c)
           ;; '->'
           (and (eq ?> c)
                (eq ?- (char-before (1- point))))
           ;; '::'
           (and (eq ?: c)
                (eq ?: (char-before (1- point)))))
      point)))


(defun ac-clang--get-manualtrigger-start-point ()
  (let* ((symbol-point (ac-prefix-symbol))
         (point (or symbol-point (point)))
         (c (char-before point)))
    (when (or 
           (ac-clang--get-autotrigger-start-point point)
           ;; ' ' for manual completion
           (eq ?\s c))
      point)))


(defsubst ac-clang--async-completion (start-point)
  (when start-point
    (ac-clang--request-command
     'ac-clang--send-completion-request
     ac-clang--completion-buffer-name
     'ac-clang--receive-completion
     (list :start-word (buffer-substring-no-properties start-point (point)) :start-point start-point))))


(defun ac-clang-async-autocomplete-autotrigger ()
  (interactive)

  (self-insert-command 1)
  (when ac-clang-async-autocompletion-automatically-p
    (ac-clang--async-completion (ac-clang--get-autotrigger-start-point))))


(defun ac-clang-async-autocomplete-manualtrigger ()
  (interactive)

  (ac-clang--async-completion (ac-clang--get-manualtrigger-start-point)))




;;;
;;; auto-complete ac-source build functions
;;;

(defsubst ac-clang--candidates ()
  ac-clang--candidates)


(defsubst ac-clang--prefix ()
  ac-clang--start-point)


(defsubst ac-clang--clean-document (s)
  (when s
    (setq s (replace-regexp-in-string "<#\\|#>\\|\\[#" "" s))
    (setq s (replace-regexp-in-string "#\\]" " " s)))
  s)


(defsubst ac-clang--in-string/comment ()
  "Return non-nil if point is in a literal (a comment or string)."
  (nth 8 (syntax-ppss)))


(defun ac-clang--action ()
  (interactive)

  ;; (ac-last-quick-help)
  (let* ((func-name (regexp-quote (substring-no-properties (cdr ac-last-completion))))
         (c/c++-pattern (format "\\(?:^.*%s\\)\\([<(].*)\\)" func-name))
         (objc-pattern (format "\\(?:^.*%s\\)\\(:.*\\)" func-name))
         (detail (get-text-property 0 'ac-clang--detail (cdr ac-last-completion)))
         (help (ac-clang--clean-document detail))
         (declarations (split-string detail "\n"))
         args
         (ret-t "")
         ret-f
         candidates)

    ;; parse function or method overload declarations
    (cl-dolist (declaration declarations)
      ;; function result type
      (when (string-match "\\[#\\(.*\\)#\\]" declaration)
        (setq ret-t (match-string 1 declaration)))
      ;; remove result type
      (setq declaration (replace-regexp-in-string "\\[#.*?#\\]" "" declaration))

      ;; parse arguments
      (cond (;; C/C++ standard argument
             (string-match c/c++-pattern declaration)
             (setq args (match-string 1 declaration))
             (push (propertize (ac-clang--clean-document args) 'ac-clang--detail ret-t 'ac-clang--args args) candidates)
             ;; default argument
             (when (string-match "\{#" args)
               (setq args (replace-regexp-in-string "\{#.*#\}" "" args))
               (push (propertize (ac-clang--clean-document args) 'ac-clang--detail ret-t 'ac-clang--args args) candidates))
             ;; variadic argument
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize (ac-clang--clean-document args) 'ac-clang--detail ret-t 'ac-clang--args args) candidates)))

            (;; check whether it is a function ptr
             (string-match "^\\([^(]*\\)(\\*)\\((.*)\\)" ret-t)
             (setq ret-f (match-string 1 ret-t)
                   args (match-string 2 ret-t))
             (push (propertize args 'ac-clang--detail ret-f 'ac-clang--args "") candidates)
             ;; variadic argument
             (when (string-match ", \\.\\.\\." args)
               (setq args (replace-regexp-in-string ", \\.\\.\\." "" args))
               (push (propertize args 'ac-clang--detail ret-f 'ac-clang--args "") candidates)))

            (;; Objective-C/C++ argument
             (string-match objc-pattern declaration)
             (setq args (match-string 1 declaration))
             (push (propertize (ac-clang--clean-document args) 'ac-clang--detail ret-t 'ac-clang--args args) candidates))))

    (cond (candidates
           (setq candidates (delete-dups candidates))
           (setq candidates (nreverse candidates))
           (setq ac-clang--template-candidates candidates)
           (setq ac-clang--template-start-point (point))
           (ac-complete-clang-template)

           (unless (cdr candidates) ;; unless length > 1
             (message (replace-regexp-in-string "\n" "   ;    " help))))
          (t
           (message (replace-regexp-in-string "\n" "   ;    " help))))))


(defun ac-clang--document (item)
  (if (stringp item)
      (let (s)
        (setq s (get-text-property 0 'ac-clang--detail item))
        (ac-clang--clean-document s)))
  ;; (popup-item-property item 'ac-clang--detail)
  )



(ac-define-source clang-async
  '((candidates     . ac-clang--candidates)
    (candidate-face . ac-clang-candidate-face)
    (selection-face . ac-clang-selection-face)
    (prefix         . ac-clang--prefix)
    (requires       . 0)
    (action         . ac-clang--action)
    (document       . ac-clang--document)
    (cache)
    (symbol         . "c")))



(defun ac-clang--same-count-in-string (c1 c2 s)
  (let ((count 0)
        (cur 0)
        (end (length s))
        c)
    (while (< cur end)
      (setq c (aref s cur))
      (cond ((eq c1 c)
             (setq count (1+ count)))
            ((eq c2 c)
             (setq count (1- count))))
      (setq cur (1+ cur)))
    (= count 0)))


(defun ac-clang--split-args (s)
  (let ((sl (split-string s ", *")))
    (cond ((string-match "<\\|(" s)
           (let (res
                 (pre "")
                 subs)
             (while sl
               (setq subs (pop sl))
               (unless (string= pre "")
                 (setq subs (concat pre ", " subs))
                 (setq pre ""))
               (cond ((and (ac-clang--same-count-in-string ?\< ?\> subs)
                           (ac-clang--same-count-in-string ?\( ?\) subs))
                      ;; (cond ((ac-clang--same-count-in-string ?\< ?\> subs)
                      (push subs res))
                     (t
                      (setq pre subs))))
             (nreverse res)))
          (t
           sl))))


(defsubst ac-clang--template-candidates ()
  ac-clang--template-candidates)


(defsubst ac-clang--template-prefix ()
  ac-clang--template-start-point)


(defun ac-clang--template-action ()
  (interactive)

  (when ac-clang--template-start-point
    (let ((point (point))
          sl 
          (snp "")
          (s (get-text-property 0 'ac-clang--args (cdr ac-last-completion))))
      (cond (;; function ptr call
             (string= s "")
             (setq s (cdr ac-last-completion))
             (setq s (replace-regexp-in-string "^(\\|)$" "" s))
             (setq sl (ac-clang--split-args s))
             (cl-dolist (arg sl)
               (setq snp (concat snp ", ${" arg "}")))
             (yas-expand-snippet (concat "("  (substring snp 2) ")") ac-clang--template-start-point point))
            (;; function args
             t
             (unless (string= s "()")
               (setq s (replace-regexp-in-string "{#" "" s))
               (setq s (replace-regexp-in-string "#}" "" s))
               (setq s (replace-regexp-in-string "<#" "${" s))
               (setq s (replace-regexp-in-string "#>" "}" s))
               (setq s (replace-regexp-in-string ", \\.\\.\\." "}, ${..." s))
               (yas-expand-snippet s ac-clang--template-start-point point)))))))


;; This source shall only be used internally.
(ac-define-source clang-template
  '((candidates . ac-clang--template-candidates)
    (prefix     . ac-clang--template-prefix)
    (requires   . 0)
    (action     . ac-clang--template-action)
    (document   . ac-clang--document)
    (cache)
    (symbol     . "t")))





;;;
;;; receive clang-server responses. 
;;; syntax checking with flymake
;;;

(defun ac-clang--receive-diagnostics (buffer _output _args)
  (let (result-texts)
    (with-current-buffer buffer
      (flymake-log 3 "received %d byte(s) of output from process %d" (ac-clang--get-buffer-bytes) (process-id ac-clang--server-process))
      (setq result-texts (buffer-substring-no-properties (point-min) (point-max))))
    (flymake-parse-output-and-residual result-texts))

  (flymake-parse-residual)
  (setq flymake-err-info flymake-new-err-info)
  (setq flymake-new-err-info nil)
  (setq flymake-err-info (flymake-fix-line-numbers flymake-err-info 1 (count-lines (point-min) (point-max))))
  (flymake-delete-own-overlays)
  (flymake-highlight-err-lines flymake-err-info))


(defun ac-clang-diagnostics ()
  (interactive)

  (if ac-clang--suspend-p
      (ac-clang-resume)
    (ac-clang-activate))

  (ac-clang--request-command 'ac-clang--send-diagnostics-request ac-clang--diagnostics-buffer-name 'ac-clang--receive-diagnostics nil))




;;;
;;; receive clang-server responses. 
;;; jump declaration/definition/smart-jump
;;;

(defun ac-clang--receive-jump (_buffer output _arg)
  (unless (eq (aref output 0) ?$)
    (let* ((parsed (split-string-and-unquote output))
           (filename (pop parsed))
           (line (string-to-number (pop parsed)))
           (column (1- (string-to-number (pop parsed))))
           (new-loc (list filename line column))
           (current-loc (list (buffer-file-name) (line-number-at-pos) (current-column))))
      (when (not (equal current-loc new-loc))
        (push current-loc ac-clang--jump-stack)
        (ac-clang--jump new-loc)))))


(defun ac-clang--jump (location)
  (let* ((filename (pop location))
         (line (pop location))
         (column (pop location)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))


(defun ac-clang-jump-back ()
  (interactive)

  (when ac-clang--jump-stack
    (ac-clang--jump (pop ac-clang--jump-stack))))


(defun ac-clang-jump-inclusion ()
  (interactive)

  (if ac-clang--suspend-p
      (ac-clang-resume)
    (ac-clang-activate))

  (ac-clang--request-command 'ac-clang--send-inclusion-request ac-clang--process-buffer-name 'ac-clang--receive-jump nil))


(defun ac-clang-jump-definition ()
  (interactive)

  (if ac-clang--suspend-p
      (ac-clang-resume)
    (ac-clang-activate))

  (ac-clang--request-command 'ac-clang--send-definition-request ac-clang--process-buffer-name 'ac-clang--receive-jump nil))


(defun ac-clang-jump-declaration ()
  (interactive)

  (if ac-clang--suspend-p
      (ac-clang-resume)
    (ac-clang-activate))

  (ac-clang--request-command 'ac-clang--send-declaration-request ac-clang--process-buffer-name 'ac-clang--receive-jump nil))


(defun ac-clang-jump-smart ()
  (interactive)

  (if ac-clang--suspend-p
      (ac-clang-resume)
    (ac-clang-activate))

  (ac-clang--request-command 'ac-clang--send-smart-jump-request ac-clang--process-buffer-name 'ac-clang--receive-jump nil))




;;;
;;; sender function for IPC
;;;

(defun ac-clang-get-server-specification ()
  (interactive)

  (when ac-clang--server-process
    (ac-clang--request-command 'ac-clang--send-server-specification-request ac-clang--process-buffer-name '(lambda (_buffer _output _args)) nil)))



;;;
;;; The session control functions
;;;

(defun ac-clang-activate ()
  (interactive)

  (remove-hook 'first-change-hook 'ac-clang-activate t)

  (unless ac-clang--activate-p
    ;; (if ac-clang--activate-buffers
    ;;  (ac-clang-update-cflags)
    ;;   (ac-clang-initialize))

    (setq ac-clang--activate-p t)
    (setq ac-clang--session-name (buffer-file-name))
    (setq ac-clang--suspend-p nil)
    (setq ac-clang--ac-sources-backup ac-sources)
    (setq ac-sources '(ac-source-clang-async))
    (push (current-buffer) ac-clang--activate-buffers)

    (ac-clang--send-create-session-request)

    (local-set-key (kbd ".") 'ac-clang-async-autocomplete-autotrigger)
    (local-set-key (kbd ">") 'ac-clang-async-autocomplete-autotrigger)
    (local-set-key (kbd ":") 'ac-clang-async-autocomplete-autotrigger)
    (local-set-key (kbd ac-clang-async-autocompletion-manualtrigger-key) 'ac-clang-async-autocomplete-manualtrigger)

    (add-hook 'before-save-hook 'ac-clang-suspend nil t)
    ;; (add-hook 'after-save-hook 'ac-clang-deactivate nil t)
    ;; (add-hook 'first-change-hook 'ac-clang-activate nil t)
    ;; (add-hook 'before-save-hook 'ac-clang-reparse-buffer nil t)
    ;; (add-hook 'after-save-hook 'ac-clang-reparse-buffer nil t)
    (add-hook 'before-revert-hook 'ac-clang-deactivate nil t)
    (add-hook 'kill-buffer-hook 'ac-clang-deactivate nil t)))


(defun ac-clang-deactivate ()
  (interactive)

  (when ac-clang--activate-p
    (remove-hook 'before-save-hook 'ac-clang-suspend t)
    (remove-hook 'first-change-hook 'ac-clang-resume t)
    ;; (remove-hook 'before-save-hook 'ac-clang-reparse-buffer t)
    ;; (remove-hook 'after-save-hook 'ac-clang-reparse-buffer t)
    (remove-hook 'before-revert-hook 'ac-clang-deactivate t)
    (remove-hook 'kill-buffer-hook 'ac-clang-deactivate t)

    (ac-clang--send-delete-session-request)

    (pop ac-clang--activate-buffers)
    (setq ac-sources ac-clang--ac-sources-backup)
    (setq ac-clang--ac-sources-backup nil)
    (setq ac-clang--suspend-p nil)
    (setq ac-clang--session-name nil)
    (setq ac-clang--activate-p nil)

    ;; (unless ac-clang--activate-buffers
    ;;   (ac-clang-finalize))
    ))


(defun ac-clang-activate-after-modify ()
  (interactive)

  (if (buffer-modified-p)
      (ac-clang-activate)
    (add-hook 'first-change-hook 'ac-clang-activate nil t)))


(defun ac-clang-suspend ()
  (when (and ac-clang--activate-p (not ac-clang--suspend-p))
    (setq ac-clang--suspend-p t)
    (ac-clang--send-suspend-request)
    (add-hook 'first-change-hook 'ac-clang-resume nil t)))


(defun ac-clang-resume ()
  (when (and ac-clang--activate-p ac-clang--suspend-p)
    (setq ac-clang--suspend-p nil)
    (remove-hook 'first-change-hook 'ac-clang-resume t)
    (ac-clang--send-resume-request)))


(defun ac-clang-reparse-buffer ()
  (when ac-clang--server-process
    (ac-clang--send-reparse-request)))


(defun ac-clang-update-cflags ()
  (interactive)

  (when ac-clang--activate-p
    ;; (message "ac-clang-update-cflags %s" ac-clang--session-name)
    (ac-clang--send-cflags-request)))


(defun ac-clang-set-cflags ()
  "Set `ac-clang-cflags' interactively."
  (interactive)

  (setq ac-clang-cflags (split-string (read-string "New cflags: ")))
  (ac-clang-update-cflags))


(defun ac-clang-set-cflags-from-shell-command ()
  "Set `ac-clang-cflags' to a shell command's output.
  set new cflags for ac-clang from shell command output"
  (interactive)

  (setq ac-clang-cflags
        (split-string
         (shell-command-to-string
          (read-shell-command "Shell command: " nil nil
                              (and buffer-file-name
                                   (file-relative-name buffer-file-name))))))
  (ac-clang-update-cflags))


(defun ac-clang-set-prefix-header (prefix-header)
  "Set `ac-clang-prefix-header' interactively."
  (interactive
   (let ((default (car (directory-files "." t "\\([^.]h\\|[^h]\\).pch\\'" t))))
     (list
      (read-file-name (concat "Clang prefix header (currently " (or ac-clang-prefix-header "nil") "): ")
                      (when default (file-name-directory default))
                      default nil (when default (file-name-nondirectory default))))))

  (cond
   ((string-match "^[\s\t]*$" prefix-header)
    (setq ac-clang-prefix-header nil))
   (t
    (setq ac-clang-prefix-header prefix-header))))




;;;
;;; The server control functions
;;;


(defun ac-clang--clean-tmp-pch ()
  "Clean up temporary precompiled headers."

  (cl-dolist (pch-file (directory-files temporary-file-directory t "preamble-.*\\.pch$" t))
    (ignore-errors
      (delete-file pch-file)
      t)))



(defun ac-clang-launch-server ()
  (interactive)

  (when (and ac-clang--server-executable (not ac-clang--server-process))
    (let ((process-connection-type nil)
          (coding-system-for-write 'binary))
      (setq ac-clang--server-process
            (apply 'start-process
                   ac-clang--process-name ac-clang--process-buffer-name
                   ac-clang--server-executable (ac-clang--build-server-launch-options))))

    (if ac-clang--server-process
        (progn
          (setq ac-clang--status 'idle)
          (ac-clang--clear-command-queue)

          (set-process-coding-system ac-clang--server-process
                                     (coding-system-change-eol-conversion buffer-file-coding-system nil)
                                     'binary)
          (set-process-filter ac-clang--server-process 'ac-clang--process-filter)
          (set-process-query-on-exit-flag ac-clang--server-process nil)

          (ac-clang--send-clang-parameters-request)
          t)
      (display-warning 'ac-clang "clang-server launch failed.")
      nil)))


(defun ac-clang-shutdown-server ()
  (interactive)

  (when ac-clang--server-process
    (ac-clang--send-shutdown-request)

    (setq ac-clang--status 'shutdown)

    (setq ac-clang--server-process nil)
    t))


(defun ac-clang-update-clang-parameters ()
  (interactive)

  (when ac-clang--server-process
    (ac-clang--send-clang-parameters-request)
    t))


(defun ac-clang-reset-server ()
  (interactive)

  (when ac-clang--server-process
    (cl-dolist (buffer ac-clang--activate-buffers)
      (with-current-buffer buffer 
        (ac-clang-deactivate)))
    (ac-clang--send-reset-server-request)))


(cl-defun ac-clang-reboot-server ()
  (interactive)

  (let ((buffers ac-clang--activate-buffers))
    (ac-clang-reset-server)

    (unless (ac-clang-shutdown-server)
      (message "ac-clang : reboot server failed.")
      (cl-return-from ac-clang-reset-server nil))

    (unless (ac-clang-launch-server)
      (message "ac-clang : reboot server failed.")
      (cl-return-from ac-clang-reset-server nil))

    (cl-dolist (buffer buffers)
      (with-current-buffer buffer
        (ac-clang-activate))))

  (message "ac-clang : reboot server success.")
  t)




(defun ac-clang-initialize ()
  (interactive)

  ;; server binary decide
  (unless ac-clang--server-executable
    (setq ac-clang--server-executable (executable-find (or (plist-get ac-clang--server-binaries ac-clang-server-type) ""))))

  ;; check obsolete
  (unless ac-clang--server-executable
    (when (setq ac-clang--server-executable (executable-find (or (plist-get ac-clang--server-obsolete-binaries ac-clang-server-type) "")))
      (display-warning 'ac-clang "The clang-server which you are using is obsolete. please replace to the new binary.")))

  ;; (message "ac-clang-initialize")
  (if ac-clang--server-executable
      (when (ac-clang-launch-server)
        ;; Optional keybindings
        (define-key ac-mode-map (kbd "M-.") 'ac-clang-jump-smart)
        (define-key ac-mode-map (kbd "M-,") 'ac-clang-jump-back)
        ;; (define-key ac-mode-map (kbd "C-c `") 'ac-clang-diagnostics)) 

        (add-hook 'kill-emacs-hook 'ac-clang-finalize)

        (when (and (eq system-type 'windows-nt) (boundp 'w32-pipe-read-delay) (> w32-pipe-read-delay 0))
          (display-warning 'ac-clang "Please set the appropriate value for `w32-pipe-read-delay'. Because a pipe delay value is large value. Ideal value is 0. see help of `w32-pipe-read-delay'."))

        t)
    (display-warning 'ac-clang "clang-server binary not found.")
    nil))


(defun ac-clang-finalize ()
  (interactive)

  ;; (message "ac-clang-finalize")
  (when (ac-clang-shutdown-server)
    (define-key ac-mode-map (kbd "M-.") nil)
    (define-key ac-mode-map (kbd "M-,") nil)

    (setq ac-clang--server-executable nil)

    (when ac-clang-tmp-pch-automatic-cleanup-p
      (ac-clang--clean-tmp-pch))

    t))





(provide 'ac-clang)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; ac-clang.el ends here
