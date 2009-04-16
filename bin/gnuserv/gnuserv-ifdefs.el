;;; gnuserv-ifdefs.el --- hide-ifdef-mode definitions for editing gnuserv C source.

;; Author: Lennart Borgman

;;; Commentary:
;; 
;; This is for hide-ifdef-mode. To use it load this file BEFORE
;; opening the C source code file (since the variable hide-ifdef-env
;; is buffer local) and then use M-x hide-ifdefs and M-x show-ifdefs.

;;; History:
;; 

;;; Code:

(defun gnuserv-ifdef-w32()
  "Set default `hide-ifdef-env' for win32."
  (interactive)
  (set-default 'hide-ifdef-env
      '(
	(WIN32 . t)
	(_WANT_SEND_RECV . t)
	(HAVE_BROKEN_INET_ADDR . nil)
	(INTERNET_DOMAIN_SOCKETS . t)
	(UNIX_DOMAIN_SOCKETS . nil)
	(SYSV_IPC . nil)
	(HIDE_UNIX_SOCKET . nil)
	(EOPNOTSUPP . nil)
	(_LOCAL_HOST_ACCESS 1 . t)
	)))

(defun gnuserv-ifdef()
  "Set default `hide-ifdef-env' for your current environment."
  (cond ((memq system-type '(emx win32 w32 windows-nt))
	 (gnuserv-ifdef-w32))
	(t (error "No hide-ifdev-env setup for you current environment."))  
	))

;; Set it when loading
(gnuserv-ifdef)

(provide 'gnuserv-ifdefs)

;;; gnuserv-ifdefs.el ends here
