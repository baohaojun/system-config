;;; matlab-netshell.el --- Control MATLAB from a network port.
;;
;; Copyright (C) 2019 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@gnu.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Form a back-channale for Emacs to chat with a running MATLAB.
;; Allows you to edit for and with a MATLAB sessions, even if it is not in a
;; matlab-shell buffer.

(require 'matlab)

;;; Code:
(declare-function matlab-shell-active-p "matlab-shell" ())
(declare-function matlab-shell--get-emacsclient-command "matlab-shell" ())
(declare-function matlab-shell-mode-gud-enable-bindings "matlab-shell-gud" ())

(defvar matlab-netshell-listen-port 32475
  "Port used for the Emacs server listening for MATLAB connections.")

(defvar matlab-netshell-server-name "*MATLAB netshell*"
  "Name used for the Netshell server.")

(defvar matlab-netshell-clients nil
  "List of clients created from the MATLAB netshell server.")

;;;###autoload
(defun matlab-netshell-server-active-p ()
  "Return non-nil if there is an active MATLAB netshell server."
  (let ((buff (get-buffer matlab-netshell-server-name)))
    (when (and buff (get-buffer-process buff))
      t)))

;;;###autoload
(defun matlab-netshell-server-start ()
    "Start the MATLAB netshell server."
    (interactive)
    (make-network-process :name matlab-netshell-server-name
			  :buffer matlab-netshell-server-name :family 'ipv4 :host 'local
			  :service matlab-netshell-listen-port
			  :filter #'matlab-netshell-filter
			  :sentinel #'matlab-netshell-sentinel
			  :server t)
    (setq matlab-netshell-clients nil)
    

    )

(defun matlab-netshell-client ()
  "Return a netshell client."
  (car matlab-netshell-clients))

(defun matlab-netshell-server-stop nil
  "Stop the MATLAB Netshell server."
  (interactive)
  (dolist (C matlab-netshell-clients)
    (delete-process C))
  (setq matlab-netshell-clients nil)
  (delete-process matlab-netshell-server-name)
  )

(defvar matlab-netshell-acc ""
  "Text Accumulator for MATLAB's netshell.")
(make-variable-buffer-local 'matlab-netshell-acc)

(defun matlab-netshell-filter (proc string)
  "Filter used for MATLAB Netshell processes.
PROC is the TCP connection that produced STRING."
  ;; Accumulate from the process
  (setq matlab-netshell-acc (concat matlab-netshell-acc string))
  ;; Wait for a NULL command terminator.
  (while (string-match "\0" matlab-netshell-acc)
    (let* ((cmdstr (substring matlab-netshell-acc 0 (match-beginning 0))))
      ;; Trim off our new command from accumulator.
      (setq matlab-netshell-acc (substring matlab-netshell-acc (match-end 0)))

      ;; Is this a test packet?  If so, we're done.
      (when (not (string= cmdstr ""))
      
	;; Find the newline.
	(unless (string-match "\n" cmdstr)
	  (message "Unable to find command in MATLAB command.  Ignoring."))
	
	(let ((cmd (substring cmdstr 0 (match-beginning 0)))
	      (data
	       (let ((me (match-end 0)))
		 (if (> (length cmdstr) me)
		     (substring cmdstr me)
		   ""))))
	  (matlab-netshell-execute-command proc cmd data)
	  )))))
  
(defun matlab-netshell-execute-command (proc cmd data)
  "For MATLAB associated with PROC, execute CMD with DATA.
The CMD is a request from MATLAB to do something in Emacs.
A common command might be to display data to the user as a
response from some Emacs based request."
  ;; Log the command
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (when (not (= (current-column) 0))
      (insert "\n"))
    (insert "Command: [" cmd "]\n")
    (when (not (string= data ""))
      (insert "Data: :" data "\n"))
    )
  ;; Interpret the command.
  (cond ((string= "init" cmd)
	 ;; Make sure GUD bindings are available, but do so in
	 ;; the netshell buffer so when gud bundings run, they
	 ;; don't stomp on C-c matlab-mode bindings.
	 (with-current-buffer (process-buffer proc)
	   (matlab-shell-mode-gud-enable-bindings))
	 ;; Send info about emacs client
	 (when (not (matlab-shell-active-p))
	   (let* ((ecc (matlab-shell--get-emacsclient-command))
	 	  (ecca (if ecc (format "emacs.set('clientcmd', '%s');" ecc))))
	     (when ecc
	       (matlab-netshell-eval ecca))))
	 (message "MATLAB connection initialized.")
	 )
	((string= "ack" cmd)
	 (message "Ack recieved.  Send ACK back.")
	 (matlab-netshell-send "nowledge" ""))
	((string= "nowledge" cmd)
	 (message "Acknowledgement recieved."))
	((string= "output" cmd)
	 (message "Ouput: %S" data))
	((string= "error" cmd)
	 (message "MATLAB Error: %s" data))
	((string= "eval" cmd)
	 ;; (message "MATLAB Evaluating remote request")
	 (let ((forms (read data)))
	   (eval forms)))
	(t
	 (message "Unknown command from matlab: %S" cmd)
	 )))

(defun matlab-netshell-sentinel (proc msg)
  "Sentinel used for MATLAB Netshell processes.
Identify when a connection is lost, and close down services.
PROC is the TCP stream which generated MSG."
  (cond ((string-match "^open from " msg)
	 ;; New connection - set it up.
	 (setq matlab-netshell-clients (cons proc matlab-netshell-clients))
	 (let ((newbuff (get-buffer-create (process-name proc))))
	   (set-process-buffer proc newbuff))
	 (message "MATLAB Has connected!"))
  
	((string= msg "connection broken by remote peer\n")
	 (setq matlab-netshell-clients (delq proc matlab-netshell-clients))
	 (message (format "MATLAB has dropped its connecction")))

	(t
	 (message "Unhandled event."))))

(defun matlab-netshell-send(cmd data)
  "Send a command CMD to MATLAB shell connection with DATA."
  (let ((C (car matlab-netshell-clients)))
    (if C
	(process-send-string C (concat cmd "\n" data "\0"))
      (error "No MATLAB network connection to send to"))))

(defun matlab-netshell-eval (mcode)
  "Send MCODE to the active MATLAB shell connection to eval."
  (interactive "sMCode: ")
  (let ((C (car matlab-netshell-clients)))
    (if C
	(process-send-string C (concat "eval\n" mcode "\0"))
      (error "No MATLAB network connection to send to"))))

(defun matlab-netshell-evalc (mcode)
  "Send MCODE to the active MATLAB shell connection to eval."
  (interactive "sMCode: ")
  (let ((C (car matlab-netshell-clients)))
    (if C
	(process-send-string C (concat "evalc\n" mcode "\0"))
      (error "No MATLAB network connection to send to"))))

(defun matlab-netshell-ack ()
  "Send an ACK to MATLAB to see if it can respond."
  (interactive)
  (matlab-netshell-send "ack" ""))
  

;;(matlab-netshell-server-start)
;;(sleep-for 300)
;;(matlab-netshell-server-stop)


(provide 'matlab-netshell)

;;; matlab-netshell.el ends here
