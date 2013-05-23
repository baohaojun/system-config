;; run-application.jl -- prompt for an application and run it

;; Copyright (C) 2007 Sven Schoenung <sven.schoenung@gmail.com>
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; run-application is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; run-application is distributed in the hope that it will be useful, 
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

(require 'sawfish.wm.util.prompt)
(require 'rep.io.files)

(defgroup run-application "Run application"
  :group misc)

(defcustom run-application:x-position nil
  "X:  Position: \\w"
  :type (choice (nil "Center")
		(left "Left")
		(right "Right"))
  :group (misc run-application))

(defcustom run-application:x-offset 0
  "X:  Offset: \\w"
  :type (number 0)
  :group (misc run-application)
  :depends run-application:x-position)

(defcustom run-application:y-position nil
  "Y:  Position: \\w"
  :type (choice (nil "Center")
		(top "Top")
		(bottom "Bottom"))
  :group (misc run-application))

(defcustom run-application:y-offset 0
  "Y:  Offset: \\w"
  :type (number 0)
  :group (misc run-application)
  :depends run-application:y-position)

(defcustom run-application:use-custom-path nil
  "Use custom PATH: "
  :type boolean
  :group (misc run-application))

(defcustom run-application:custom-path (getenv "PATH")
  " (colon seperated list)"
  :type string
  :group (misc run-application)
  :depends run-application:use-custom-path)

(defcustom run-application:use-application-exclude t
  "Exclude applications: "
  :type boolean
  :group (misc run-application))

(defcustom run-application:application-exclude "^\\."
  " (regular expression)"
  :type string
  :group (misc run-application)
  :depends run-application:use-application-exclude)

;; FIXME: This could probably be better ...
(define (prompt-application-executable file dir)
   (let ((f (expand-file-name file dir)))
    (and (not (file-directory-p f))
	 (string-match "x" (file-modes-as-string f)))))

(define (prompt-application-duplicate file dirs)
  (delete-if-not (lambda (dir)
		   (file-exists-p (expand-file-name file dir)))
		 dirs))

(define (prompt-mismatch-application str dir file dirs)
  (or (not (string-head-eq file str))
      (prompt-application-duplicate file dirs)
      (not (prompt-application-executable file dir))
      (and run-application:use-application-exclude
	   (string-match run-application:application-exclude file))))

(define (prompt-application-path)
  (let ((path (if run-application:use-custom-path
		  run-application:custom-path
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

(define (prompt-complete-application str)
  (let ((path (prompt-application-path)))
    (apply #'nconc 
	   (mapcar (lambda (dir)
		     (let ((dirs (cdr (member dir path))))
		       (delete-if 
			 (lambda (file) 
			   (prompt-mismatch-application str dir file dirs))
			 (directory-files dir))))
		   path))))

(define (prompt-for-application #!optional title start default)
  "Prompt for an application in $PATH"
  (unless (stringp title)
    (setq title "Enter application:"))
  (let* ((prompt-completion-fun prompt-complete-application)
	 (str (prompt title start)))
    (when (and (string= str "") default)
      (setq str default))
    str))

(define (run-application)
  "Prompt for an application and run it"
  (setq prompt-window-position 
	(cons (case run-application:x-position
		('left run-application:x-offset)
		('right (- -1 run-application:x-offset))
		(nil nil))
	      (case run-application:y-position
		('top run-application:y-offset)
		('bottom (- -1 run-application:y-offset))
		(nil nil))))
  (system (format nil "%s &" (prompt-for-application "Run application: "))))

(define-command 'run-application run-application)
