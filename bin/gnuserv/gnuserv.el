;;; gnuserv.el

;; Lisp Interface code between GNU Emacs and gnuserv.
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software.
;; Copying is permitted under those conditions described by the GNU
;; General Public License.
;;
;; Copyright (C) 1989-1994  Free Software Foundation, Inc.
;;
;; Author: Andy Norman (ange@hplb.hpl.hp.com) based on
;;         'lisp/server.el' from the 18.52 GNU Emacs distribution.
;;
;; Please mail bugs and suggestions to the author at the above address.
;;
;; Updated for XEmacs, GNU Emacs 19 and Epoch V4 to use multiple frames
;; by Bob Weiner, <weiner@mot.com>, 1/20/94.  (Still works with Emacs V18, too.)
;;    Modified 'server-process-filter' to use \^D as end of request terminator
;;      as gnuclient and gnudoit have been modified to send.  This permits
;;      multi-line requests.
;;    Modified 'server-make-window-visible' to work with multiple frames.
;;    Modified 'server-find-file' to display in a separate frame when possible.
;;    Modified 'server-edit' to delete newly created frame when used to
;;      terminate an edit and to signal an error if called within a
;;      non-server-edit buffer.
;; Bob Weiner, <weiner@mot.com>, 5/9/94.
;;    Added 'server-done-function' variable.  Made default value 'kill-buffer'
;;    instead of 'bury-buffer' as in original gnuserv.el.
;;
;; Darrell Kindred <dkindred+@cmu.edu> May/1994
;; Updated to allow multi-line return values:
;;    - output to gnuserv is "m/n:xxx" where m is the client number,
;;      n is the length of the data, and xxx is the data itself, followed 
;;      by newline
;;
;; Arup Mukherjee <arup+@cmu.edu> May/1994
;; Updated for XEmacs 19.10, and others:
;;    - use find-file-other-screen if present
;;    - new variable gnuserv-frame can be set to a frame or screen which is 
;;      is used for all edited files. 
;;    - check to see if server.el is already loaded and complain if it is, since
;;      gnuserv.el can't coexist with server.el
;;    - rename server-start to gnuserv-start, although server-start remains as
;;      an alias. This allows gnuserv-start to be autoloaded from gnuserv.el
;;    - changed server-get-buffer to take into account that in newer emacsen,
;;      get buffer returns nil on deleted buffers.
;;    - only try to create/delete frames or screens if window-system is non-nil 
;;      (otherwise things don't work w/ emacs19 on a dumb terminal)
;;
;; Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se> 2004-07-24
;; Updated to allow for three modes of frame handling when gnuserv shall open a file:
;;      1) Open the file in current active frame in current window, DEFAULT (old)
;;      2) Create a new frames and open the file there (old)
;;      3) Check if the file is the "active" file in any frame, then display this frame (new)
;;      Customized by variable server-reuseframe in group w32-feeling
;;      
;; Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se> 2005-01-17
;;      Renamed all server-* to gnuserv-*. See readme.nt.
;;
;; Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se> 2005-01-29
;;      Added aliases for old server-* functions called from gnuclient.


(defconst gnuserv-rcs-header-id "$Header: gnuserv.el,v 2.1 95/02/16 12:00:16 arup alpha $")


;; server.el and gnuserv.el can't coexist because of conflicting defvar's and
;; function names. 

(if (and (boundp 'gnuserv-buffer-clients)
	 (not (featurep 'gnuserv)))
    (error "Can't run gnuserv because server.el appears to be loaded already"))

(defvar gnuserv-frame nil
  "*If non-nil, the frame to be used to display all edited files.
If nil, then a new frame is created for each file edited.
This variable has no effect in XEmacs versions older than 19.9.")

(defvar gnuserv-done-function 'kill-buffer 
  "*A function of one argument, a buffer, which removes the buffer after editing.
Functions such as 'kill-buffer' and 'bury-buffer' are good values.")

(defvar gnuserv-program "gnuserv"
  "*The program to use as the edit server")

(defvar gnuserv-process nil 
  "The current server process")

(defvar gnuserv-string ""
  "The last input string from the server")

(defvar gnuserv-current-client nil
  "The client we are currently talking to")

(defvar gnuserv-clients nil
  "List of current server clients.
Each element is (CLIENTID BUFFER...) where CLIENTID is an integer
that can be given to the server process to identify a client.
When a buffer is killed, it is removed from this list.")

(defvar gnuserv-buffer-clients nil
  "List of client ids for clients requesting editing of the current buffer.")

(make-variable-buffer-local 'gnuserv-buffer-clients)
(setq-default gnuserv-buffer-clients nil)
(or (assq 'gnuserv-buffer-clients minor-mode-alist)
    (setq minor-mode-alist (cons '(gnuserv-buffer-clients " Server") 
				 minor-mode-alist)))

(defun gnuserv-log (string)
  "If a *server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*server*")
      (save-excursion
	(set-buffer "*server*")
	(goto-char (point-max))
	(insert (current-time-string) " " string)
	(or (bolp) (newline)))))


(defun gnuserv-sentinel (proc msg)
  (cond ((eq (process-status proc) 'exit)
	 (gnuserv-log (message "Server subprocess exited")))
	((eq (process-status proc) 'signal)
	 (gnuserv-log (message "Server subprocess killed")))))


(defun gnuserv-process-display-error (string)
  "Whenever a gnuserv error is reported, display it in a pop-up window."
  (let ((cur (selected-window))
	(pop-up-windows t))
    (pop-to-buffer (get-buffer-create "*server*"))
    (set-window-start (selected-window) (point))
    (gnuserv-log string)
    (select-window cur)))


(defun gnuserv-process-filter (proc string)
  "Process client gnuserv requests to execute Emacs commands."
  (setq gnuserv-string (concat gnuserv-string string))
  (if (string-match "\^D$" gnuserv-string) ; requests end with ctrl-D
      (if (string-match "^[0-9]+" gnuserv-string) ; client request id
	  (progn
	    (gnuserv-log gnuserv-string)
	    (let ((header (read-from-string gnuserv-string)))
	      (setq gnuserv-current-client (car header))
	      ;;(gnuserv-log (message "gnuserv-current-client=%s" gnuserv-current-client))
	      ;;(gnuserv-log (message "to eval=%s" (car (read-from-string gnuserv-string (cdr header)))))
	      (condition-case oops
		  (eval (car (read-from-string gnuserv-string 
					       (cdr header))))
		(error (setq gnuserv-string "")
		       (gnuserv-write-to-client gnuserv-current-client oops)
		       (setq gnuserv-current-client nil)
		       (signal (car oops) (cdr oops)))
		(quit (setq gnuserv-string "")
		      (gnuserv-write-to-client gnuserv-current-client oops)
		      (setq gnuserv-current-client nil)
		      (signal 'quit nil)))
	      (setq gnuserv-string "")))
	(progn				;error string from server
	  (gnuserv-process-display-error gnuserv-string)
	  (setq gnuserv-string "")))))


(defun gnuserv-release-outstanding-buffers ()
  "Release all buffers that have clients waiting for them to be finished."
  (interactive)
  (while gnuserv-clients
    (let ((buffer (nth 1 (car gnuserv-clients)))) ; for all buffers...
      (gnuserv-buffer-done buffer)))) ; destructively modifies gnuserv-clients

;;;###autoload
(defun gnuserv-start (&optional leave-dead)
  "Allow this Emacs process to be a server for client processes.
This starts a server communications subprocess through which
client \"editors\" (gnuclient and gnudoit) can send editing commands to 
this Emacs job. See the gnuserv(1) manual page for more details.

Prefix arg means just kill any existing server communications subprocess."
  (interactive "P")
  ;; kill it dead!
  (if gnuserv-process
      (progn
	(gnuserv-release-outstanding-buffers)
	(set-process-sentinel gnuserv-process nil)
	(condition-case ()
	    (delete-process gnuserv-process)
	  (error nil))))
  ;; If we already had a server, clear out associated status.
  (if leave-dead
      nil
    (if gnuserv-process
	(gnuserv-log (message "Restarting server")))
    (setq gnuserv-string "")
    (setq gnuserv-current-client nil)
    (let ((process-connection-type t))
      (setq gnuserv-process 
	    (start-process "server" nil gnuserv-program)))
    (set-process-sentinel gnuserv-process 'gnuserv-sentinel)
    (set-process-filter gnuserv-process 'gnuserv-process-filter)
    (process-kill-without-query gnuserv-process)))

;; make gnuserv-start an alias to gnuserv-start, for backward compatibility
(fset 'server-start (function gnuserv-start))


(defun gnuserv-write-to-client (client form)
  "Write the given form to the given client via the server process."
  (if (and client
	   (eq (process-status gnuserv-process) 'run))
      (let* ((result (format "%s" form))
	     (s      (format "%s/%d:%s\n" client (length result) result)))
	(gnuserv-log s)
	(process-send-string gnuserv-process s))))

(defun gnuserv-eval (form)
  "Evaluate form and return result to client."
  (gnuserv-write-to-client gnuserv-current-client (eval form))
  (setq gnuserv-current-client nil))
(defalias 'server-eval 'gnuserv-eval)


(defun gnuserv-eval-quickly (form)
  "Let client know that we've received the request, but eval the form
afterwards in order to not keep the client waiting."
  (gnuserv-write-to-client gnuserv-current-client nil)
  (setq gnuserv-current-client nil)
  (eval form))
(defalias 'server-eval-quickly 'gnuserv-eval-quickly)


(defun gnuserv-make-window-visible ()
  "Try to make this window even more visible."
  (and (boundp 'window-system)
       (boundp 'window-system-version)
       (or (memq window-system '(win32 w32))
	   (and (eq window-system 'x)
		(eq window-system-version 11)))
       (cond ((fboundp 'raise-frame)
	      (raise-frame (selected-frame)))
	     ((fboundp 'deiconify-screen)
	      (deiconify-screen (selected-screen))
	      (raise-screen (selected-screen)))
	     ((fboundp 'mapraised-screen)
	      (mapraised-screen))
	     ((fboundp 'x-remap-window)
	      (x-remap-window)
	      ;; give window chance to re-display text
	      (accept-process-output)))))


(defun gnuserv-find-file (file)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists."
;;;(gnuserv-log (format "gnuserv.el:gnuserv-find-file(%s)" file)) ; Lennart
;;;(gnuserv-log (format "gnuserv.el:gnuserv-frame=%s" gnuserv-frame)) ; Lennart


  ;;(let ((gnuserv-frame (eval gnuserv-frame)))
  (let ((gnuserv-frame (gnuserv-reuseframe)))

    (let ((obuf (get-file-buffer file)))
      (if (and obuf (set-buffer obuf))
	  (if (file-exists-p file)
	      (if (or (not (verify-visited-file-modtime obuf))
		      (buffer-modified-p obuf))
		  (revert-buffer t nil))
	    (if (y-or-n-p
		 (concat "File no longer exists: "
			 file
			 ", write buffer to file? "))
		(write-file file))))
      (cond ((and window-system
		  gnuserv-frame (fboundp 'frame-live-p)	;; v19 & Xemacs 19.12+
		  (frame-live-p gnuserv-frame))
	     (select-frame gnuserv-frame)
	     (find-file file))

	    ((and window-system
		  gnuserv-frame (fboundp 'live-screen-p) ;; XEmacs 19.9+
		  (live-screen-p gnuserv-frame))
	     (select-screen gnuserv-frame)          
	     (find-file file))
	  
	    ((and window-system
		  (fboundp 'select-frame)) ;; v19 & XEmacs 19.12+
	     (select-frame (make-frame))
	     (find-file file))

	    ((and window-system
		  (fboundp 'select-screen) ;; XEmacs 19.10+
		  (fboundp 'make-screen))
	     (select-screen (make-screen))
	     (find-file file))
	  
	    ((and (eq window-system 'x)	;; XEmacs 19.9-
		  (fboundp 'select-screen)
		  (fboundp 'x-create-screen))
	     (select-screen (x-create-screen nil))
	     (find-file file))

	    ((and window-system
		  (fboundp 'create-screen)) ;; epoch
	     (if (screenp gnuserv-frame)
		 (progn (select-screen gnuserv-frame)
			(find-file file))
	       (select-screen (create-screen (find-file-noselect file)))))

	    (t (find-file file))))) ;; emacs18+
  )

(defun gnuserv-edit-files-quickly (list)
  "For each (line-number . file) pair in LIST, edit the file at line-number.
Unlike (gnuserv-edit-files), no information is saved about clients waiting on
edits to this buffer."
  (gnuserv-write-to-client gnuserv-current-client nil)
  (setq gnuserv-current-client nil)
  (while list
    (let ((line (car (car list)))
	  (path (cdr (car list))))
      (gnuserv-find-file path)
      (gnuserv-make-window-visible)
      (goto-line line))
    (setq list (cdr list))))
(defalias 'server-edit-files-quickly 'gnuserv-edit-files-quickly)


(defun gnuserv-edit-files (list)
  "For each (line-number . file) pair in LIST, edit the file at line-number.
Save enough information for (gnuserv-kill-buffer) to inform the client when
the edit is finished."
  (while list
    (let ((line (car (car list)))
	  (path (cdr (car list))))
      (gnuserv-find-file path)
      (gnuserv-make-window-visible)
      (let ((old-clients (assq gnuserv-current-client gnuserv-clients))
	    (buffer (current-buffer)))
	(goto-line line)
	(setq gnuserv-buffer-clients
	      (cons gnuserv-current-client gnuserv-buffer-clients))
	(if old-clients		  ;client already waiting for buffers?
	    (nconc old-clients (list buffer)) ;yes -- append this one as well
	  (setq gnuserv-clients		;nope -- make a new record
		(cons (list gnuserv-current-client buffer)
		      gnuserv-clients)))))
    (setq list (cdr list)))
  (message (substitute-command-keys
	    (if (and (boundp 'infodock-version) window-system)
		"Type {\\[gnuserv-edit]} or select Frame/Delete to finish edit."
	      "When done with a buffer, type \\[gnuserv-edit]."))))
(defalias 'server-edit-files 'gnuserv-edit-files)


(defun gnuserv-get-buffer (buffer)
  "One arg, a BUFFER or a buffer name.  Return the buffer object even if killed.
Signal an error if there is no record of BUFFER."
  (if (null buffer)
      (current-buffer)
    (let ((buf (get-buffer buffer)))
      (if (null buf)
	  (if (bufferp buffer)
	      buffer
	    (if (stringp buffer)
		(error "No buffer named %s" buffer)
	      (error "Invalid buffer argument")))
	buf))))

(defun gnuserv-kill-buffer (buffer)
  "Kill the BUFFER.  The argument may be a buffer object or buffer name. 
NOTE: This function has been enhanced to allow for remote editing
in the following way:

If the buffer is waited upon by one or more clients, and a client is
not waiting for other buffers to be killed, then the client is told
that the buffer has been killed."
  (interactive "bKill buffer ")
  (setq buffer (gnuserv-get-buffer buffer))
  (if (buffer-name buffer)
      (save-excursion
	(set-buffer buffer)
	(let ((old-clients gnuserv-clients))
	  (gnuserv-real-kill-buffer buffer) ;try to kill it
	  (if (buffer-name buffer)	;succeeded in killing?
	      nil			;nope
	    (while old-clients
	      (let ((client (car old-clients)))
		(delq buffer client)
		(if (cdr client)	;pending buffers?
		    nil			;yep
		  (gnuserv-write-to-client (car client) nil) ;nope, tell client
		  (setq gnuserv-clients (delq client gnuserv-clients))))
	      (setq old-clients (cdr old-clients))))))))


(defun gnuserv-kill-all-local-variables ()
  "Eliminate all the buffer-local variable values of the current buffer.
This buffer will then see the default values of all variables.
NOTE: This function has been modified to ignore the variable 
gnuserv-buffer-clients."
  (let ((clients gnuserv-buffer-clients))
    (gnuserv-real-kill-all-local-variables)
    (if clients
	(setq gnuserv-buffer-clients clients))))


(or (fboundp 'gnuserv-real-kill-buffer)
    (fset 'gnuserv-real-kill-buffer (symbol-function 'kill-buffer)))

(fset 'kill-buffer 'gnuserv-kill-buffer)

(or (fboundp 'gnuserv-real-kill-all-local-variables)
    (fset 'gnuserv-real-kill-all-local-variables
	  (symbol-function 'kill-all-local-variables)))

(fset 'kill-all-local-variables 'gnuserv-kill-all-local-variables)


(defun gnuserv-buffer-done (buffer)
  "Mark BUFFER as \"done\" for its client(s).
Buries the buffer, and returns another server buffer as a suggestion for the
new current buffer."
  (let ((next-buffer nil)
	(old-clients gnuserv-clients))
    (while old-clients
      (let ((client (car old-clients)))
	(or next-buffer 
	    (setq next-buffer (nth 1 (memq buffer client))))
	(delq buffer client)
	;; If client now has no pending buffers,
	;; tell it that it is done, and forget it entirely.
	(if (cdr client)
	    nil
	  (gnuserv-write-to-client (car client) nil)
	  (setq gnuserv-clients (delq client gnuserv-clients))))
      (setq old-clients (cdr old-clients)))
    (if (buffer-name buffer)
	(save-excursion
	  (set-buffer buffer)
	  (setq gnuserv-buffer-clients nil)))
    (funcall gnuserv-done-function buffer)
    next-buffer))


(defun mh-draft-p (buffer)
  "Return non-nil if this BUFFER is an mh <draft> file. Since MH deletes 
draft *BEFORE* it is edited, the server treats them specially."
  ;; This may not be appropriately robust for all cases.
  (string= (buffer-name buffer) "draft"))


(defun gnuserv-done ()
  "Offer to save current buffer and mark it as \"done\" for clients.
Also bury it, and return a suggested new current buffer."
  (let ((buffer (current-buffer)))
    (if gnuserv-buffer-clients
	(progn
 	  (if (mh-draft-p buffer)
 	      (progn (save-buffer)
		     (write-region (point-min) (point-max)
				   (concat buffer-file-name "~"))
		     (kill-buffer buffer))
	    (if (and (buffer-modified-p)
		     (y-or-n-p (concat "Save file " buffer-file-name "? ")))
		(save-buffer buffer)))
	  (gnuserv-buffer-done buffer)))))


(defun gnuserv-edit (&optional arg)
  "Switch to next server editing buffer and mark current one as \"done\".
If a server buffer is current, it is marked \"done\" and optionally saved.
MH <draft> files are always saved and backed up, no questions asked.
When all of a client's buffers are marked as \"done\", the client is notified.

If invoked with a prefix argument, or if there is no server process running, 
starts server process and that is all.  Invoked by \\[gnuserv-edit]."
  (interactive "P")
  ;;(let ((file) (gnuserv-frame (eval gnuserv-frame)))
  (let ((gnuserv-frame (gnuserv-reuseframe)))
    (if (or arg
	    (not gnuserv-process)
	    (memq (process-status gnuserv-process) '(signal exit)))
	(gnuserv-start nil)
      (if gnuserv-buffer-clients
	  (progn (gnuserv-switch-buffer (gnuserv-done))
		 (cond ((or (not window-system)
			    (and gnuserv-frame 
				 (or (and (fboundp 'frame-live-p)
					  (frame-live-p gnuserv-frame))
				     (and (fboundp 'live-screen-p)
					  (live-screen-p gnuserv-frame))
				     (and (fboundp 'create-screen)
					  (screenp gnuserv-frame)))))
			()) ;; do nothing
		       ((fboundp 'delete-frame)
			(delete-frame (selected-frame) t))
		       ((fboundp 'delete-screen)
			(delete-screen))))
	(error 
	 "(gnuserv-edit): Use only on buffers created by external programs.")
	))))

(defun gnuserv-switch-buffer (next-buffer)
  "Switch to NEXT-BUFFER if a live buffer, otherwise switch to another buffer
with gnuserv clients. If no such buffer is available, simply choose another 
one."
  (if next-buffer
      (if (and (bufferp next-buffer)
	       (buffer-name next-buffer))
	  (switch-to-buffer next-buffer)
	;; If NEXT-BUFFER is a dead buffer,
	;; remove the server records for it
	;; and try the next surviving server buffer.
	(gnuserv-switch-buffer
	 (gnuserv-buffer-done next-buffer)))
    (if gnuserv-clients
	(gnuserv-switch-buffer (nth 1 (car gnuserv-clients)))
      (switch-to-buffer (other-buffer)))))

(global-set-key "\C-x#" 'gnuserv-edit)



					; Added by Lennart Borgman 2004-07
(defun gnuserv-find-files-frame()
  "Try to find frame where file is displayed in active buffer. Return nil if not found."
					;(gnuserv-log (format "************************ find-files-frame"))
  (let (
	(frame-list-holder (frame-list))
	(use-frame))
    (while (and frame-list-holder (not use-frame))
      (let ((f1 (car frame-list-holder)))
	(setq frame-list-holder (cdr frame-list-holder))
					;(gnuserv-log (format "Frame name: %s" (frame-parameter f1 'name)))
	(let ((b1 (car (frame-parameter f1 'buffer-list))))
					;(gnuserv-log (format "    buffer-file-name: %s" (buffer-file-name b1)))
					;(gnuserv-log (format "    file=%s" file))
	  (if (string= (buffer-file-name b1) file)
	      (progn
					;(gnuserv-log (format "**** HERE ****"))
		(setq use-frame f1))
	    ))))
    use-frame))

;; Lennart
(defcustom gnuserv-reuseframe 'gnuserv-always-reuse-frame
  "Controls how frames should be handled when Emacs opens a file.
Note: 'frame' is Emacs terminology for what otherwise in MS Windows is
called 'window'"
  :tag "Frame reusing when opening files"
  :type '(choice
	  (const :tag "Open files in same frame"
		 gnuserv-always-reuse-frame)
	  (const :tag "Open files in new frame"
		 nil)
	  (const :tag "Try to find frame where file is active"
		 gnuserv-find-files-frame)
	  )
  :group 'w32
  )

;; Lennart
(defun gnuserv-reuseframe()
  "Find out which frame to use.
Customized by `gnuserv-reuseframe'."
  (cond
   ((eq gnuserv-reuseframe 'gnuserv-always-reuse-frame) (selected-frame))
   ((eq gnuserv-reuseframe nil) nil)
   ((eq gnuserv-reuseframe 'gnuserv-find-files-frame) (gnuserv-find-files-frame))))


(provide 'gnuserv)

;;; gnuserv.el ends here
