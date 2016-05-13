;; bhj-run-application.jl -- prompt for an application and run it

;; Copyright (C) 2007 Sven Schoenung <sven.schoenung@gmail.com>
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; bhj-run-application is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; bhj-run-application is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Sawfish, the program this script is written for; see the
;; file COPYING.  If not, write to  the Free Software Foundation,
;; 675 Mass Ave, Cambridge, MA 02139, USA.

;; Version: 0.1
;;  - Initial release
;;
;; Version: 0.2
;;  - Custom path support
;;  - Application exclusion support

(require 'bhj.prompt)
(require 'rep.io.files)

(defgroup bhj-run-application "Run application"
  :group misc)

(defcustom bhj-run-application:x-position nil
  "X:  Position: \\w"
  :type (choice (nil "Center")
                (left "Left")
                (right "Right"))
  :group (misc bhj-run-application))

(defcustom bhj-run-application:x-offset 0
  "X:  Offset: \\w"
  :type (number 0)
  :group (misc bhj-run-application)
  :depends bhj-run-application:x-position)

(defcustom bhj-run-application:y-position nil
  "Y:  Position: \\w"
  :type (choice (nil "Center")
                (top "Top")
                (bottom "Bottom"))
  :group (misc bhj-run-application))

(defcustom bhj-run-application:y-offset 0
  "Y:  Offset: \\w"
  :type (number 0)
  :group (misc bhj-run-application)
  :depends bhj-run-application:y-position)

(defcustom bhj-run-application:use-custom-path nil
  "Use custom PATH: "
  :type boolean
  :group (misc bhj-run-application))

(defcustom bhj-run-application:custom-path (getenv "PATH")
  " (colon seperated list)"
  :type string
  :group (misc bhj-run-application)
  :depends bhj-run-application:use-custom-path)

(defcustom bhj-run-application:use-application-exclude t
  "Exclude applications: "
  :type boolean
  :group (misc bhj-run-application))

(defcustom bhj-run-application:application-exclude "^\\."
  " (regular expression)"
  :type string
  :group (misc bhj-run-application)
  :depends bhj-run-application:use-application-exclude)

;; FIXME: This could probably be better ...
(define (bhj-prompt-application-executable file dir)
  (let ((f (expand-file-name file dir)))
    (and (not (file-directory-p f))
         (string-match "x" (file-modes-as-string f)))))

(define (bhj-prompt-application-duplicate file dirs)
  (delete-if-not (lambda (dir)
                   (file-exists-p (expand-file-name file dir)))
                 dirs))

(define (bhj-prompt-mismatch-application str dir file dirs)
  (or (not (string-head-eq file str))
      (bhj-prompt-application-duplicate file dirs)
      (not (bhj-prompt-application-executable file dir))
      (and bhj-run-application:use-application-exclude
           (string-match bhj-run-application:application-exclude file))))

(define (bhj-prompt-application-path)
  (let ((path (if bhj-run-application:use-custom-path
                  bhj-run-application:custom-path
                (getenv "PATH")))
        (path-list '())
        (dir "")
        (start 0)
        (end 0))
    (while (< start (length path))
      (setq end (if (string-match ":" path start)
                    (match-start) (length path)))
      (setq dir (substring path start end))
      (when (file-exists-p dir)
        (setq path-list (append path-list (list dir))))
      (setq start (1+ end)))
    path-list))

(define (bhj-prompt-complete-application str)
  (let* ((output-stream (make-string-output-stream))
         (process (make-process output-stream))
         (lines nil))
    (apply #'call-process
           process
           "/dev/null"
           (concat (getenv "HOME") "/system-config/bin/skeleton_compgen_word.pl")
           "-d"
           "\\n"
           "-f"
           "~/.cache/system-config/.bash_history.bak"
           "--"
           (string-split "\\s+" str))
    (setq lines (string-split "\n" (get-output-stream-string output-stream)))
    (if (and (= 2 (length lines))
             (string-equal (cadr lines) ""))
        (list (car lines))
      lines)))

(define (bhj-prompt-for-application #!optional title start default)
  "Prompt for an application in $PATH"
  (unless (stringp title)
    (setq title "Enter application:"))
  (let* ((bhj-prompt-completion-fun bhj-prompt-complete-application)
         (str (bhj-prompt title start)))
    (when (and (string= str "") default)
      (setq str default))
    str))

(define (bhj-run-application)
  "Prompt for an application and run it"
  (setq bhj-prompt-window-position
        (cons (case bhj-run-application:x-position
                ('left bhj-run-application:x-offset)
                ('right (- -1 bhj-run-application:x-offset))
                (nil nil))
              (case bhj-run-application:y-position
                ('top bhj-run-application:y-offset)
                ('bottom (- -1 bhj-run-application:y-offset))
                (nil nil))))
  (system (format nil "%s &" (string-replace "&\\s*$" "" (bhj-prompt-for-application "Run application: ")))))

(define-command 'bhj-run-application bhj-run-application)
