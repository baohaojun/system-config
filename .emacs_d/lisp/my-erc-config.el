(defface erc-header-line-disconnected
  '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC has been disconnected.")

(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
	  (t 'erc-header-line-disconnected))))
(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)

(setq erc-log-channels-directory "~/.emacs.d/logs/")
(setq erc-save-buffer-on-part nil)
(setq erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(setq erc-hide-list '("MODE"))

(defvar bitlbee-password (auth-source-user-or-password "password" "localhost" 6667))
(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
  (when (and (string= "localhost" erc-session-server)
	     (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
				   (erc-default-target)
				   bitlbee-password))))
(add-hook 'erc-join-hook 'bitlbee-identify)

(defun bitlbee-connect ()
  (interactive)
  (save-window-excursion
    (when (get-buffer "&bitlbee")
      (switch-to-buffer "&bitlbee")
      (erc-message "PRIVMSG" (concat (erc-default-target) " identify " bitlbee-password))
      (erc-message "PRIVMSG" (concat (erc-default-target) " account on")))))

(setq erc-keywords '((".*Online.*" (:foreground "green"))
		     (".*Busy" (:foreground "red"))
		     (".*Away" (:foreground "red"))
		     (".*Idle" (:foreground "orange"))
		     ))

(defun erc-notify-on-msg (msg)
  (let ((msg (substring-no-properties msg)))
  (when (and (not (string-match "^<root>" msg))
	     (string-match "^<.*>" msg))
    (shell-command (concat "bhj-notify erc " 
			   (shell-quote-argument msg))))))

(add-hook 'erc-insert-pre-hook 'erc-notify-on-msg)

(defun erc-ignore-unimportant (msg)
  (if (or (string-match "*** localhost has changed mode for &bitlbee to" msg)
	  (string-match "Account already online\\|Trying to get all accounts" msg)
	  (string-match "You're already logged in" msg)
	  (string-match "has quit: Leaving\\|has joined channel" msg)
	  (string-match "Unknown error while loading configuration" msg))
      (setq erc-insert-this nil)))
(add-hook 'erc-insert-pre-hook 'erc-ignore-unimportant) ; order is important

(erc :server "localhost" :port "6667" :nick "bhj" :password bitlbee-password)
