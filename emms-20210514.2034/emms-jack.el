;;; emms-jack.el --- Jack Audio Connection Kit support  -*- lexical-binding: t; -*-

;; Copyright (C) 2005-2021  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: multimedia, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; JACK is a low-latency audio server, written for POSIX conformant
;; operating systems such as GNU/Linux and Apple's OS X. It can connect a
;; number of different applications to an audio device, as well as
;; allowing them to share audio between themselves. Its clients can run in
;; their own processes (ie. as normal applications), or they can run
;; within the JACK server (ie. as a "plugin").
;;
;; JACK was designed from the ground up for professional audio work, and
;; its design focuses on two key areas: synchronous execution of all
;; clients, and low latency operation.
;;
;; jack.el provides a fascility for starting jackd from within Emacs.
;; It also povides convenience functions for prompting the user for
;; jack client and port names in the minibuffer, as well as the
;; functions `jack-connect' and `jack-disconnect' which can be used to
;; rearrange jack port wiring with a minimum of keystrokes.

;;; Code:

(require 'emms-compat)

(defgroup emms-jack ()
  "Jack Audio Connection Kit"
  :group 'processes)

(defcustom emms-jack-rc '("~/.jackdrc" "/etc/jackd.conf")
  "JACK run control paths."
  :type 'repeat)

(defcustom emms-jack-use-jack-rc t
  "If non-nil, try to retrieve jack startup arguments from run control files
listed in `jack-rc'.  If no rc file is found or this variable is set
to nil, use the Emacs variables to build the startup args."
  :type 'boolean)

(defcustom emms-jack-program (executable-find "jackd")
  "JACK executable path."
  :type 'file)

(defcustom emms-jack-sample-rate 44100
  "Default sampling rate for JACK."
  :type 'integer)

(defcustom emms-jack-period-size 128
  "Period size to use when launching new JACK process."
  :type 'integer)

(defcustom emms-jack-alsa-device nil
  "ALSA soundcard to use."
  :type '(choice (const :tag "Ask" nil) string))

(defun emms-jack-read-alsa-device ()
  "Read an ALSA device name using the minibuffer."
  (let (cards)
    (with-temp-buffer
      (insert-file-contents "/proc/asound/cards")
      (while (not (eobp))
	(if (looking-at "^\\([0-9]\\) \\[.+\\]: \\(.+\\)\n +\\(.*\\)$")
	    (setq cards (append (list (cons (match-string 3) (match-string 1))) cards)))
	(forward-line 1)))
    (concat "hw:" (cdr (assoc (completing-read "Card: " cards nil t) cards)))))

(defun emms-jack-alsa-device ()
  (or emms-jack-alsa-device (emms-jack-read-alsa-device)))

(defcustom emms-jack-output-buffer-name "*JACK output*"
  "Output buffer name."
  :type 'string)

(defun emms-jack-args ()
  "Return a list of startup arguments to use.
First element is the executable path."
  (or (and emms-jack-use-jack-rc
	   (catch 'rc-found
	     (let ((files (mapcar #'expand-file-name emms-jack-rc)))
	       (while files
		 (if (file-exists-p (car files))
		     (with-temp-buffer
		       (insert-file-contents (car files))
		       (when (> (buffer-size) 0)
			 (throw 'rc-found
				(split-string (buffer-string) "[\n \t]+")))))
		 (setq files (cdr files))))
	     nil))
      (list emms-jack-program
	    "-v"
	    "-R"
	    "-dalsa"
	    (format "-d%s" (emms-jack-alsa-device))
	    (format "-r%d" emms-jack-sample-rate)
	    (format "-p%d" emms-jack-period-size))))

(defcustom emms-jack-set-rtlimits t
  "Use set_rtlimits (if available) to gain realtime priorities if -R
is given in jackd command-line."
  :type 'boolean)

(defcustom emms-jack-set-rtlimits-program (executable-find "set_rtlimits")
  "Path to set_rtlimits."
  :type 'file)

(defun emms-jack-maybe-rtlimits (args)
  (if (and emms-jack-set-rtlimits
	   (or (member "-R" args) (member "--realtime" args))
	   (file-exists-p emms-jack-set-rtlimits-program))
      (append (list emms-jack-set-rtlimits-program "-r") args)
    args))

(defvar emms-jack-process nil)

(defvar emms-jack-load 0)

(defvar emms-jack-max-usecs 0)

(defvar emms-jack-spare 0)

(defun emms-jack-output-buffer ()
  (or (get-buffer emms-jack-output-buffer-name)
      (with-current-buffer (get-buffer-create emms-jack-output-buffer-name)
	(setq major-mode 'emms-jack-mode
	      mode-name "JACK"
	      mode-line-format (copy-tree mode-line-format))
	(setcar (nthcdr 16 mode-line-format)
		`(:eval (format "load:%.2f" emms-jack-load)))
	(add-hook 'kill-buffer-hook #'emms-jack-kill nil t)
	(current-buffer))))

(defvar emms-jack-xruns nil)

(defun emms-jack-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	(save-match-data
	  (if (string-match "^load = \\([^ ]+\\) max usecs: \\([^,]+\\), spare = \\(.+\\)$" string)
	      (setq emms-jack-load (string-to-number (match-string 1 string))
		    emms-jack-max-usecs (string-to-number (match-string 2 string))
		    emms-jack-spare (string-to-number (match-string 3 string)))
	    (if (string-match "^**** alsa_pcm: xrun of at least \\([^ ]+\\) msecs$" string)
		(push (string-to-number (match-string 1 string)) emms-jack-xruns)
	      (goto-char (process-mark proc))
	      (insert string)
	      (set-marker (process-mark proc) (point))))))
      (when moving (goto-char (process-mark proc))))))

(defun emms-jack-running-p ()
  (and emms-jack-process (processp emms-jack-process)
       (eq (process-status emms-jack-process) 'run)))

(defcustom emms-jack-started-hook nil
  "Hook run when `emms-jack-start' successfully started a new JACK intance."
  :type 'hook)

(defun emms-jack-start ()
  "Start the JACK process."
  (interactive)
  (if (emms-jack-running-p) (error "JACK already running")
    (setq emms-jack-process
	  (apply #'start-process "jack" (emms-jack-output-buffer)
		 (emms-jack-maybe-rtlimits (emms-jack-args))))
    (set-process-filter emms-jack-process #'emms-jack-filter)
    (run-hooks 'emms-jack-started-hook)
    (switch-to-buffer (emms-jack-output-buffer))))

(defun emms-jack-kill ()
  "Kill the currently running JACK process."
  (interactive)
  (when (emms-jack-running-p) (delete-process emms-jack-process))
  (setq emms-jack-process nil))

(defun emms-jack-restart ()
  "Restart JACK."
  (interactive)
  (if (emms-jack-running-p) (emms-jack-kill))
  (sit-for 0)
  (emms-jack-start))

(defun emms-jack-list ()
  "Retrieve a list of JACK clients/ports."
  (with-temp-buffer
    (call-process "jack_lsp" nil t nil "-cpl")
    (goto-char (point-min))
    (let (result current-port)
      (while (not (eobp))
	(cond
	 ((looking-at "^\\([^ \t:]+\\):\\(.+\\)$")
	  (let ((program (match-string 1))
		(port (match-string 2)))
	    (if (assoc program result)
		(setcdr (assoc program result)
			(append (cdr (assoc program result)) (list (setq current-port (list port)))))
	      (setq result
		    (append (list (list program (setq current-port (list port)))) result)))))
	 ((looking-at "^   \\([^ \t:]+\\):\\(.+\\)$")
	  (if (assoc 'connections (cdr current-port))
	      (setcdr (assoc 'connections (cdr current-port))
		      (append (cdr (assoc 'connections current-port))
			      (list (list (match-string 1) (match-string 2)))))
	    (setcdr current-port
		    (append (list (list 'connections (list (match-string 1) (match-string 2)))) (cdr current-port)))))
	 ((looking-at "^\tproperties: \\(.+\\),$")
	  (setcdr current-port
		  (append (list (append (list 'properties) (mapcar #'intern (split-string (match-string 1) ",")))) (cdr current-port)))))
	(forward-line 1))
      result)))
	  
(defun emms-jack-ports (program)
  (cdr (assoc program (emms-jack-list))))

(defun emms-jack-get-port-connections (program port)
  (cdr (assoc 'connections (cdr (assoc port (emms-jack-ports program))))))

(defun emms-jack-get-port-properties (program port)
  (cdr (assoc 'properties (cdr (assoc port (emms-jack-ports program))))))

(defun emms-jack-get-direction (program port)
  (let ((props (emms-jack-get-port-properties program port)))
    (or (car (member 'output props))
	(car (member 'input props))
	(error "Neither input nor output port"))))
      
(defun emms-jack-read-program (prompt &optional predicate)
  (let ((progs (if (functionp predicate)
		   (emms-remove-if-not predicate (emms-jack-list))
		 (emms-jack-list))))
    (unless progs (error "No matching JACK clients found"))
    (if (< (length progs) 2) (caar progs)
      (completing-read prompt progs nil t))))

(defun emms-jack-unique-port-name (strings)
  (let ((start "")
	(maxlen (apply #'min (mapcar #'length strings))))
    (while (and (< (length start) maxlen)
		(catch 'not-ok
		  (let ((nextchar (substring (car strings) (length start) (1+ (length start)))))
		    (mapc (lambda (str)
			    (unless (string= (concat start nextchar) (substring str 0 (1+ (length start))))
			      (throw 'not-ok nil)))
			  strings)
		    t)))
      (setq start (substring (car strings) 0 (1+ (length start)))))
    start))

(defun emms-jack-read-port (program prompt &optional predicate)
  (let ((ports (if (functionp predicate)
		   (emms-remove-if-not predicate (emms-jack-ports program))
		 (emms-jack-ports program))))
    (if (< (length ports) 2) (caar ports)
      (completing-read prompt ports nil t
                       (emms-jack-unique-port-name (mapcar #'car ports))))))

(defun emms-jack-connect (from-program from-port to-program to-port)
  "Connect FROM-PROGRAM's output port FROM-PORT to TO-PROGRAM's input port
TO-PORT.
If called interactively, the direction does not matter."
  (interactive
   (let* ((prog (emms-jack-read-program "Connect: "))
	  (port (emms-jack-read-port prog (format "Connect %s port: " prog)))
	  (to-type (if (eq (emms-jack-get-direction prog port) 'input) 'output 'input))
	  (to-prog (emms-jack-read-program
		 (format "Connect %s port %s to: " prog port)
		 (lambda (prog)
		   (emms-find-if (lambda (port)
				   (member to-type (assoc 'properties
							  (cdr port))))
				 (cdr prog)))))
	  (to-port (emms-jack-read-port
		    to-prog
		    (format "Connect %s port %s to %s port: " prog port to-prog)
		    (lambda (port)
		      (member to-type (cdr (assoc 'properties (cdr port))))))))
     (if (eq to-type 'input)
	 (list prog port to-prog to-port)
       (list to-prog to-port prog port))))
  (let ((result (call-process "jack_connect" nil nil nil
			      (format "%s:%s" from-program from-port)
			      (format "%s:%s"  to-program to-port))))
    (if (= result 0)
	(message "JACK: Connected %s:%s to %s:%s"
		 from-program from-port to-program to-port))))

(defun emms-jack-disconnect (from-program from-port to-program to-port)
  "Disconnect FROM-PROGRAM's output port FROM-PORT from TO-PROGRAM's
input port TO-PORT.
If called interactively, the direction is not relevant."
  (interactive
   (let* ((prog (emms-jack-read-program
		 "Disconnect: "
		 (lambda (prog)
		   (emms-find-if (lambda (port) (assoc 'connections (cdr port)))
				 (cdr prog)))))
	  (port (emms-jack-read-port prog
		 (format "Disconnect %s port: " prog)
		 (lambda (port)
		   (assoc 'connections (cdr port)))))
	  (connections (emms-jack-get-port-connections prog port))
	  (from (list prog port))
	  (to (if (< (length connections) 2)
		  (car connections)
		(let* ((to-progs (let (result)
				   (mapc (lambda (conn)
					   (if (not (member (car conn) result))
					       (setq result
						     (append (list (car conn))
							     result))))
					 connections)
				   (mapcar #'list result)))
		       (to-prog (if (< (length to-progs) 2)
				    (caar to-progs)
				  (completing-read
				   (format "Disconnect %s port %s from: "
					   prog port) to-progs nil t))))
		  (setq connections (emms-remove-if-not
				     (lambda (conn)
				       (string= (car conn) to-prog))
				     connections))
		  (if (< (length connections) 2)
		      (car connections)
		    (let ((to-port (completing-read
				    (format "Disconnect %s port %s from %s port: "
					    prog port to-prog)
				    (mapcar #'cdr connections) nil t)))
		      (list to-prog to-port)))))))
     (if (eq (emms-jack-get-direction prog port) 'output)
	 (append from to)
       (append to from))))
  (let ((result (call-process "jack_disconnect" nil nil nil
			      (format "%s:%s" from-program from-port)
			      (format "%s:%s"  to-program to-port))))
    (if (= result 0)
	(message "JACK: Disconnected %s:%s from %s:%s"
		 from-program from-port to-program to-port))))

(provide 'emms-jack)
;;; emms-jack.el ends here
