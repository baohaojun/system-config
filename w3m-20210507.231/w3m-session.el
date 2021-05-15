;;; w3m-session.el --- Functions to operate session of w3m

;; Copyright (C) 2001-2003, 2005-2013, 2017-2021
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Hideyuki SHIRAI <shirai@meadowy.org>
;; Keywords: w3m, WWW, hypermedia
;; Homepage: http://emacs-w3m.namazu.org/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; w3m-session.el is an add-on program of emacs-w3m to save, load,
;; delete, and rename sessions or their selected tabs.

;; You need not load or restore an entire session.  You can examine and
;; manage the contents (individual tabs) of any session, including
;; deleting, renaming, or loading them one at a time.

;; A session is any collection of buffers (tabs) open at any given
;; time.  Be aware, however, that unfortunately much of the code and
;; documentation uses the terms session and buffer interchangeably.
;; There is also a term 'session-group' which is identical to a
;; session which has more than buffer.  Yes, this does need to be
;; refactored!

;; Session management does its best to automatically keep track of the
;; current collection of w3m buffers so that should a catastrophic
;; crash occur, the entire session can be restored.  Upon restarting
;; emacs-w3m after a crash, the user will be prompted whether to
;; recover the prior session.  Additionally, the user may at any time
;; manually save the current session, or restore the tabs of a prior
;; session.  Note that restoring a prior session is not destructive; it
;; only adds new tabs into the current session for each tab of the
;; saved session.

;; Session information is also saved when one quits emacs-w3m using
;; the `w3m-quit' command, bound by default to `Q'.

;; The session information for all sessions are stored as a single
;; elisp object, by default in file ~/.w3m/.session.  Tab content is
;; NOT stored.  What is stored for each tab is the url, title, cursor
;; position, and tab history.  Even so, this can eventually become a
;; monstrously large data structure, and probably needlessly, if one
;; accumulates large history records for each tab.

;;;; Typical usage and tutorial

;; One enters session management by evaluating `w3m-session-select'
;; (M-s).  Typically, there will be at least one session automatically
;; present, either labeled as a crash recovery session or an
;; automatically saved one.  Next to each session name will be a number
;; in brackets representing the number of tab (buffers) in the
;; session.  There will also be column entries for the session
;; timestamp, and summary URL entries.  One exits session management
;; with either `q' or `C-g'.

;; From any emacs-w3m buffer one may save a session by evaluating
;; `w3m-session-save' (M-S).

;; From within session management, one can rename (`r') or delete
;; (`d') any session. Sessions can also be merged ('m'). Selecting a
;; session using the RET key will load ALL of that session's tabs. You
;; can examine the details of a session by pressing `M-s' (the same
;; key binding you used to enter the session management). At this
;; point, you can rename, delete, or load any individual tab. Note
;; that the first entry on the list gives you a second chance to load
;; ALL the tabs. Quitting returns you to the session list.

;; All changes are saved to disk immediately.


;;; Code:

(require 'w3m-util)

(defvar w3m-async-exec)
(defvar w3m-current-title)
(defvar w3m-current-url)
(defvar w3m-history)
(defvar w3m-history-flat)
(defvar w3m-language)
(defvar w3m-mode-map)
(defvar w3m-profile-directory)

(declare-function w3m--setup-popup-window "w3m" (toggle buffer-name nomsg))
(declare-function w3m-goto-url-new-session "w3m"
		  (url &optional reload charset post-data referer background))
(declare-function w3m-history-slimmed-history-flat "w3m-hist")
(declare-function w3m-history-tree "w3m-hist" (&optional newpos))
(declare-function w3m-load-list "w3m" (file &optional coding-system))
(declare-function w3m-message "w3m" (&rest args))
(declare-function w3m-save-list "w3m"
		  (file list &optional coding-system escape-ctl-chars))

(autoload 'seq-position "seq")

(defvar w3m-session-group-open nil
  "Which session-group is open.

If a session-group is currently open, i.e., when displaying a
list of buffers for an individual session, this should be set
to the session (session-group) number.

There is a legacy terminology problem that needs to be addressed
here.  The documentation and symbol names currently confuse
'sessions', 'buffers`, and 'session-groups'.  A 'session-group'
is identical to a 'session' that has more than one 'buffer'.")

(defcustom w3m-session-file
  (expand-file-name ".sessions" w3m-profile-directory)
  "File name to keep sessions."
  :group 'w3m
  :type 'file)

(defcustom w3m-session-autosave t
  "Non-nil means save automatically when w3m quit."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-session-deleted-save t
  "Non-nil means save deleted sessions."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-session-crash-recovery t
  "If non-nil, recover session by auto-saved one when emacs-w3m crashes."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-session-time-format
  (if (equal "Japanese" w3m-language)
      "%Y年%m月%d日(%a) %H:%M"
    "%Y-%m-%d (%a) %H:%M")
  "Format of saved time."
  :group 'w3m
  :type 'string)

(defcustom w3m-session-automatic-title
  (if (equal "Japanese" w3m-language)
      "自動保存"
    "Automatic saved sessions")
  "String of title to save session automatically."
  :group 'w3m
  :type 'string)

(defcustom w3m-session-deleted-title
  (if (equal "Japanese" w3m-language)
      "削除セッション"
    "Removed sessions")
  "String of title to save session when buffer delete."
  :group 'w3m
  :type 'string)

(defcustom w3m-session-crash-recovery-title
  (if (equal "Japanese" w3m-language)
      "クラッシュ回復"
    "Crash recovery sessions")
  "String of title to save session to use for crash recovering."
  :group 'w3m
  :type 'string)

(defcustom w3m-session-deleted-keep-number 5
  "Number to keep sessions when buffers delete."
  :group 'w3m
  :type 'integer)

(defcustom w3m-session-automatic-keep-number 5
  "Number to keep sessions automatically."
  :group 'w3m
  :type 'integer)

(defcustom w3m-session-unknown-title "<Unknown Title>"
  "String of title to use when title is not specified."
  :group 'w3m
  :type 'string)

(defcustom w3m-session-load-last-sessions nil
  "Whether to reload the most recent session when emacs-w3m
starts."
  :group 'w3m
  :type
  '(radio
    (const :format "Reload the last session automatically\n" t)
    (const :format "Ask whether to reload the last session\n" ask)
    (const :format "Never reload the last session automatically" nil)))

(defcustom w3m-session-load-crashed-sessions 'ask
  "Whether to reload a crashed session when emacs-w3m starts.
This is used when emacs-w3m determines that the most recent session crashed."
  :group 'w3m
  :type
  '(radio
    (const :format "Reload the crashed session automatically\n" t)
    (const :format "Ask whether to reload the crashed session\n" ask)
    (const :format "Never reload the crashed session automatically" nil)))

(defface w3m-session-select
  `((((class color) (background light) (type nil))
     (:foreground "black"))
    (((class color) (background dark) (type nil))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "dark blue"))
    (((class color) (background dark))
     (:foreground "white"))
    (t nil))
  "Face of w3m-session."
  :group 'w3m)

(defface w3m-session-selected
  `((((class color) (background light) (type nil))
     (:foreground "blue" :bold t :underline t))
    (((class color) (background dark) (type nil))
     (:foreground "cyan" :bold t :underline t))
    (((class color) (background light))
     (:foreground "dark blue" :bold t :underline t))
    (((class color) (background dark))
     (:foreground "white" :bold t :underline t))
    (t (:bold t :underline t)))
  "Face of selected w3m-session."
  :group 'w3m)

(defun w3m-session-history-to-save ()
  "Return a copy of `w3m-history-flat' without current page data."
  (let ((pos (cadar w3m-history)))
    (apply
     'append
     (mapcar (lambda (x)
	       (unless (equal (nth 2 x) pos)
		 (list x)))
	     (copy-sequence (w3m-history-slimmed-history-flat))))))

;; format of session file.
;; '((sessiontitle1 time1 ((url11 pos11 hflat11 urltitle11)
;;                         (url12 pos12 hflat12 urltitle12) ...) current1)
;;   ...

(defmacro w3m-session-ignore-errors (&rest forms)
  "Run FORMS.  Remove `w3m-session-file' and quit if any error happens."
  `(condition-case err
       (progn ,@forms)
     (error
      (if (and (file-exists-p w3m-session-file)
	       (yes-or-no-p (format
			     "An error was found in \"%s\"; may we remove it? "
			     (abbreviate-file-name w3m-session-file))))
	  (progn
	    (delete-file w3m-session-file)
	    (run-at-time 0.1 nil #'message
			 "\"%s\" has been removed; try again."
			 (abbreviate-file-name w3m-session-file))
	    (keyboard-quit))
	(signal (car err) (cdr err))))))

;;;###autoload
(defun w3m-session-save ()
  "Save the current session (all currently open emacs-w3m buffers).

The user will be prompted for a name for the saved session.  The
saved session information will include, for each currently open
emacs-w3m buffer: the current url and page title, and the
buffer's url history."
  (interactive)
  (w3m-session-ignore-errors
   (let ((sessions (w3m-load-list w3m-session-file))
	 (bufs (w3m-list-buffers))
	 (prompt "New session title: ")
	 (cnum 0)
	 (i 0)
	 otitle title titles urls len buf cbuf)
     (mapc (lambda (x)
	     (setq titles (cons (cons (car x) (car x)) titles)))
	   sessions)
     (setq otitle (or w3m-current-title
		      (with-current-buffer (car bufs)
			w3m-current-title)))
     (setq titles (cons (cons otitle otitle) titles))
     (while (not title)
       (setq title (completing-read prompt titles nil nil otitle nil otitle))
       (if (or (string= title "")
	       (and (assoc title sessions)
		    (not (y-or-n-p (format "\"%s\" exists.  Overwrite? "
					   title)))))
	   (setq title nil)))
     (setq cbuf (current-buffer))
     (save-current-buffer
       (while (setq buf (car bufs))
	 (setq bufs (cdr bufs))
	 (set-buffer buf)
	 (when w3m-current-url
	   (when (eq cbuf (current-buffer))
	     (setq cnum i))
	   (setq i (1+ i))
	   (setq urls (cons (list w3m-current-url
				  (copy-sequence (cadar w3m-history))
				  (w3m-session-history-to-save)
				  w3m-current-title)
			    urls)))))
     (if (not urls)
	 (message "%s: no buffers saved...done" title)
       (setq len (length urls))
       (setq urls (nreverse urls))
       (when (assoc title sessions)
	 (setq sessions (delq (assoc title sessions) sessions)))
       (setq sessions (cons (list title (current-time) urls cnum) sessions))
       (w3m-save-list w3m-session-file sessions)
       (message "%s: %d buffer%s saved...done" title len (if (= len 1) "" "s"))
       (when (and (setq buf (get-buffer " *w3m-session select*"))
		  (get-buffer-window buf 'visible))
	 (save-selected-window (w3m-session-select)))))))

(defun w3m-session-automatic-save ()
  "Save list of displayed session automatically."
  (when w3m-session-autosave
    (w3m-session-ignore-errors
     (let ((sessions (w3m-load-list w3m-session-file))
	   (bufs (w3m-list-buffers))
	   (title (concat w3m-session-automatic-title "-1"))
	   (titleregex (concat "\\`"
			       (regexp-quote w3m-session-automatic-title)
			       "-[0-9]+\\'"))
	   (cnum 0)
	   (i 0)
	   urls buf cbuf session
	   tmp tmptitle tmptime tmpurls)
       (when bufs
	 (setq cbuf (current-buffer))
	 (save-current-buffer
	   (while (setq buf (car bufs))
	     (setq bufs (cdr bufs))
	     (set-buffer buf)
	     (when w3m-current-url
	       (when (eq cbuf (current-buffer))
		 (setq cnum i))
	       (setq i (1+ i))
	       (setq urls (cons (list w3m-current-url
				      (copy-sequence (caar w3m-history))
				      (w3m-session-history-to-save)
				      w3m-current-title)
				urls)))))
	 (when urls
	   (setq i 2)
	   (while (setq session (car sessions))
	     (setq sessions (cdr sessions))
	     (if (string-match titleregex (nth 0 session))
		 (when (<= i w3m-session-automatic-keep-number)
		   (setq tmptitle (format (concat w3m-session-automatic-title
						  "-%d") i))
		   (setq tmptime (nth 1 session))
		   (setq tmpurls (nth 2 session))
		   (setq tmp (cons (list tmptitle tmptime tmpurls nil) tmp))
		   (setq i (1+ i)))
	       (setq tmp (cons session tmp))))
	   (setq sessions (nreverse tmp))
	   (setq urls (nreverse urls))
	   (setq sessions (cons (list title (current-time) urls cnum)
				sessions))
	   (w3m-save-list w3m-session-file sessions)))))))

(defun w3m-session-deleted-save (buffers)
  "Save list of deleted session."
  (when w3m-session-deleted-save
    (w3m-session-ignore-errors
     (let ((sessions (w3m-load-list w3m-session-file))
	   (title (concat w3m-session-deleted-title "-1"))
	   (titleregex (concat "\\`"
			       (regexp-quote w3m-session-deleted-title)
			       "-[0-9]+\\'"))
	   (bufs (copy-sequence buffers))
	   (i 2)
	   urls buf session
	   tmp tmptitle tmptime tmpurls)
       (when bufs
	 (setq bufs (sort bufs 'w3m-buffer-name-lessp))
	 (save-current-buffer
	   (while (setq buf (car bufs))
	     (setq bufs (cdr bufs))
	     (set-buffer buf)
	     (when w3m-current-url
	       (setq urls (cons (list w3m-current-url
				      (copy-sequence (caar w3m-history))
				      (w3m-session-history-to-save)
				      w3m-current-title)
				urls)))))
	 (when urls
	   (while (setq session (car sessions))
	     (setq sessions (cdr sessions))
	     (if (string-match titleregex (nth 0 session))
		 (when (<= i w3m-session-deleted-keep-number)
		   (setq tmptitle (format (concat w3m-session-deleted-title
						  "-%d") i))
		   (setq tmptime (nth 1 session))
		   (setq tmpurls (nth 2 session))
		   (setq tmp (cons (list tmptitle tmptime tmpurls nil) tmp))
		   (setq i (1+ i)))
	       (setq tmp (cons session tmp))))
	   (setq sessions (nreverse tmp))
	   (setq urls (nreverse urls))
	   (setq sessions (cons (list title (current-time) urls nil) sessions))
	   (w3m-save-list w3m-session-file sessions)))))))

(defun w3m-session-crash-recovery-save ()
  "Save list of displayed session."
  (when w3m-session-crash-recovery
    (w3m-session-ignore-errors
     (let ((sessions (w3m-load-list w3m-session-file))
	   (bufs (w3m-list-buffers))
	   (title w3m-session-crash-recovery-title)
	   urls buf tmp)
       (when bufs
	 (save-current-buffer
	   (while (setq buf (car bufs))
	     (setq bufs (cdr bufs))
	     (set-buffer buf)
	     (when w3m-current-url
	       (setq urls (cons (list w3m-current-url
				      (copy-sequence (caar w3m-history))
				      (w3m-session-history-to-save)
				      w3m-current-title)
				urls)))))
	 (when urls
	   (setq urls (nreverse urls))
	   (setq tmp (assoc title sessions))
	   (when tmp (setq sessions (delq tmp sessions)))
	   (setq sessions (cons (list title (current-time) urls nil) sessions))
	   (w3m-save-list w3m-session-file sessions)))))))

;;;###autoload
(defun w3m-session-crash-recovery-remove ()
  "Remove crash recovery session set."
  (when w3m-session-crash-recovery
    (w3m-session-ignore-errors
     (let* ((sessions (w3m-load-list w3m-session-file))
	    (item (assoc w3m-session-crash-recovery-title sessions)))
       (when item
	 (setq sessions (delq item sessions))
	 (w3m-save-list w3m-session-file sessions))))))

(defvar w3m-session-select-mode-map nil)
(unless w3m-session-select-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'w3m-session-select-quit)
    (define-key map "Q" 'w3m-session-select-quit)
    (define-key map "\C-g" 'w3m-session-select-quit)
    (define-key map "\C-m" 'w3m-session-select-select)
    (define-key map "\M-s" 'w3m-session-select-open-session-group)
    (define-key map "c" 'w3m-session-select-copy)
    (define-key map "C" 'w3m-session-select-copy)
    (define-key map "d" 'w3m-session-select-delete)
    (define-key map "D" 'w3m-session-select-delete)
    (define-key map "s" 'w3m-session-select-save)
    (define-key map "S" 'w3m-session-select-save)
    (define-key map "r" 'w3m-session-select-rename)
    (define-key map "R" 'w3m-session-select-rename)
    (define-key map "m" 'w3m-session-select-merge)
    (define-key map "M" 'w3m-session-select-merge)
    (define-key map "n" 'w3m-session-select-next)
    (define-key map "j" 'w3m-session-select-next)
    (define-key map "\C-n" 'w3m-session-select-next)
    (define-key map [down] 'w3m-session-select-next)
    (define-key map "p" 'w3m-session-select-previous)
    (define-key map "k" 'w3m-session-select-previous)
    (define-key map "\C-p" 'w3m-session-select-previous)
    (define-key map [up] 'w3m-session-select-previous)
    (setq w3m-session-select-mode-map map)))

;;; Local variables
(defvar w3m-session-select-sessions nil
  "Buffer-local copy of session list.")
(make-variable-buffer-local 'w3m-session-select-sessions)

(defun w3m-session-select-mode (&optional sessions)
  "Major mode for selecting emacs-w3m session.

\\<w3m-session-select-mode-map>
\\[w3m-session-select-select]	Select the session.
\\[w3m-session-select-open-session-group]	Open the session group.
\\[w3m-session-select-copy]	Copy the session.
\\[w3m-session-select-delete]	Delete the session.
\\[w3m-session-select-rename]	Rename the session.
\\[w3m-session-select-merge]	Merge the session into another one.
\\[w3m-session-select-save]	Save the session.
\\[w3m-session-select-next]	Move the point to the next session.
\\[w3m-session-select-previous]	Move the point to the previous session.
\\[w3m-session-select-quit]	Exit selecting session.
"
  (w3m-session-ignore-errors
   (let ((sessions (or sessions
		       (w3m-load-list w3m-session-file))))
     (buffer-disable-undo)
     (setq mode-name "w3m session"
	   truncate-lines t
	   buffer-read-only nil
	   major-mode 'w3m-session-select-mode
	   w3m-session-select-sessions sessions
	   buffer-quit-function 'w3m-session-select-quit
	   buffer-read-only t)
     (setq w3m-session-group-open nil)
     (use-local-map w3m-session-select-mode-map)
     (w3m-session-select-list-all-sessions)
     (add-hook 'pre-command-hook 'w3m--session-update-faces t t)
     (add-hook 'post-command-hook 'w3m--session-update-faces t t))))

(defun w3m--session-update-faces ()
  "A hook function for `w3m-session-select' buffers.
Meant for use  with  `pre-command-hook' and `post-command-hook'."
  (let ((beg (line-beginning-position))
	(inhibit-read-only t))
    (put-text-property
     beg (next-single-property-change beg 'w3m-session-number)
     'face (if (equal (get-text-property beg 'face) 'w3m-session-select)
	       'w3m-session-selected
	     'w3m-session-select))))

(defun w3m-session-select-list-all-sessions ()
  "List all saved sessions."
  (let ((sessions w3m-session-select-sessions)
	(num 0)
	(max 0)
	(inhibit-read-only t)
	title titles time times wid pos)
    (if (not sessions)
	(progn
	  (message "No saved session")
	  (w3m-session-select-quit))
      (mapc (lambda (x)
	      (setq title (format "%s[%d]" (nth 0 x) (length (nth 2 x))))
	      (setq wid (string-width title))
	      (when (> wid max)
		(setq max wid))
	      (setq titles (cons title titles))
	      (setq times (cons (format-time-string w3m-session-time-format
						    (nth 1 x))
				times)))
	    sessions)
      (setq titles (nreverse titles))
      (setq times (nreverse times))
      (setq max (+ max 2))
      (erase-buffer)
      (while (and (setq title (car titles))
		  (setq time (car times)))
	(setq titles (cdr titles))
	(setq times (cdr times))
	(setq pos (point))
	(insert title)
	(add-text-properties pos (point)
			     `(face w3m-session-select
				    w3m-session-number ,num))
	(setq num (1+ num))
	(insert (make-string (- max (string-width title)) ? ))
	(insert time "\n"))
      (delete-char -1)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))))

(defun w3m-session-select-list-session-group (arg)
  "List all buffers (i.e., tabs) within a session.

The list can be acted upon similarly to a session list, i.e.,
entries can be individually deleted, renamed, or opened as a new
buffer in the current session."
  (let ((session (nth 2 (nth arg w3m-session-select-sessions)))
	(num 0)
	(max 0)
	(inhibit-read-only t)
	title url wid
	titles urls pos)
    (when session
      (mapc (lambda (x)
	      (setq title
		    (format "%s" (or (nth 3 x) w3m-session-unknown-title)))
	      (setq wid (string-width title))
	      (when (> wid max)
		(setq max wid))
	      (setq titles (cons title titles))
	      (setq urls (cons (nth 0 x)
			       urls)))
	    session)
      (setq titles (nreverse titles))
      (setq urls (nreverse urls))
      (setq max (+ max 2))
      (erase-buffer)
      (insert "Select session:\n\n")
      (setq pos (point))
      (insert "Open all sessions")
      (add-text-properties pos (point)
			   `(face w3m-session-selected
				  w3m-session-number ,arg))
      (insert "\n")
      (while (and (setq title (car titles))
		  (setq url (car urls)))
	(setq titles (cdr titles))
	(setq urls (cdr urls))
	(setq pos (point))
	(insert title)
	(add-text-properties pos (point)
			     `(face w3m-session-select
				    w3m-session-number ,(cons arg num)))
	(setq num (1+ num))
	(insert (make-string (- max (string-width title)) ? ))
	(insert url "\n"))
      (goto-char (point-min))
      (goto-char (next-single-property-change
		  (point) 'w3m-session-number)))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

(defun w3m-session-select-next (&optional arg)
  "Move the point to the next session."
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((target (1+ (mod (1- (+ arg (line-number-at-pos (point))))
			 (line-number-at-pos (point-max))))))
    (goto-char (point-min))
    (forward-line (1- target))
    (set-buffer-modified-p nil)))

(defun w3m-session-select-previous (&optional arg)
  "Move the point to the previous session."
  (interactive "p")
  (w3m-session-select-next (- arg)))

(defun w3m-session-select-quit ()
  "Exit from w3m session select mode."
  (interactive)
  (if w3m-session-group-open
      (let ((num w3m-session-group-open))
	(setq w3m-session-group-open nil)
	(w3m-session-select-list-all-sessions)
	(forward-line num))
    (let ((buffer (current-buffer)))
      (or (one-window-p) (delete-window))
      (kill-buffer buffer))))

(defun w3m-session-select-select ()
  "Select the session."
  (interactive)
  (beginning-of-line)
  (let* ((num (get-text-property
	       (point) 'w3m-session-number))
	 (item (if (consp num)
		   (nth (cdr num)
			(caddr (nth (car num)
				    w3m-session-select-sessions)))
		 (nth num w3m-session-select-sessions)))
	 (session (if (consp num)
		      (list (or (cadddr item) w3m-session-unknown-title)
			    nil
			    (list item)
			    nil)
		    item)))
    (w3m-session-goto-session session)))

(defun w3m-session-select-open-session-group (&optional arg)
  "Open the session group."
  (interactive)
  (beginning-of-line)
  (let ((num (or arg (get-text-property (point) 'w3m-session-number)))
	wheight)
    (if (consp num)
	(message "This is not a session group.")
      (setq w3m-session-group-open num)
      (setq wheight
	    (max (+ (length (caddr (nth num w3m-session-select-sessions))) 6)
		 window-min-height))
      (condition-case nil
	  (enlarge-window (- wheight (window-height)))
	(error nil))
      (w3m-session-select-list-session-group num))))

(defun w3m-session-select-save ()
  "Save the session."
  (interactive)
  (when (y-or-n-p "Save this session? ")
    (w3m-session-select-quit)
    (w3m-session-save)
    (w3m-session-select)))

(defun w3m-session-select-rename ()
  "Rename this session."
  (interactive)
  (beginning-of-line)
  (let ((num (get-text-property
	      (point) 'w3m-session-number))
	(sessions w3m-session-select-sessions))
    (w3m-session-rename sessions num)
    (if (not w3m-session-group-open)
	(w3m-session-select num)
      (w3m-session-select-open-session-group w3m-session-group-open))))

(defun w3m-session-select-delete ()
  "Delete an entry (either a session or a buffer)."
  (interactive)
  (when (y-or-n-p "Delete this entry? ")
    (beginning-of-line)
    (let ((num (get-text-property
		(point) 'w3m-session-number))
	  (sessions w3m-session-select-sessions))
      (w3m-session-delete sessions num)
      (if (not w3m-session-group-open)
	  (w3m-session-select (min (if (consp num) (car num) num)
				   (1- (length sessions))))
	(w3m-session-select-open-session-group w3m-session-group-open)
	(forward-line (min (1+ (cdr num))
			   (- (line-number-at-pos (point-max)) 4)))))))

(defun w3m-session-select-copy ()
  "Copy the currently selected session."
  (interactive)
  (beginning-of-line)
  (let ((sessions w3m-session-select-sessions)
	(default-prompt "Name for new session: ")
	(source-number (get-text-property (point) 'w3m-session-number))
	prompt source-session target-session otitle title buf)
    (setq prompt default-prompt)
    (if (not (integerp source-number))
	(error "Only for sessions, not their elements.")
      (setq source-session (nth source-number sessions))
      (setq target-session source-session))
    (setq otitle (car source-session))
    (while (not title)
      ;; A devious way to emulate INITIAL-INPUT that is deprecated
      ;; (see the docstring for `read-from-minibuffer').
      (let ((minibuffer-setup-hook (lambda nil (insert otitle))))
	(setq title (read-from-minibuffer prompt nil nil nil nil otitle)))
      (cond
       ((string= title "")
	(setq title nil
	      prompt default-prompt))
       ((string= title otitle)
	(setq prompt (concat title
			     " is same as original title (C-g to abort): ")
	      title nil))
       ((assoc title sessions)
	(if (not (y-or-n-p (format "\"%s\" exists.  Overwrite? " title)))
	    (setq prompt default-prompt
		  title nil)
	  (setq sessions (delq (assoc title sessions) sessions))))))
    (setq sessions (cons (list title
			       (current-time)
			       (nth 2 source-session)
			       (nth 3 source-session))
			 sessions))
    (w3m-save-list w3m-session-file sessions)
    (w3m-message "Session %s copied to %s." otitle title)
    (when (and (setq buf (get-buffer " *w3m-session select*"))
	       (get-buffer-window buf 'visible))
      (save-selected-window (w3m-session-select)))))

(defun w3m-session-select-merge ()
  "Copy the elements of the selected session into another one.

The user will be prompted for the receiving session. If point is
on a single element within a session, then only that element will
be copied."
  (interactive)
  (beginning-of-line)
  (let ((sessions w3m-session-select-sessions)
	(prompt "Merge into session: ")
	(source-number (get-text-property (point) 'w3m-session-number))
	source-session source-element source-time titles target)
    (if (integerp source-number)
	(setq source-session (nth source-number sessions))
      (setq source-session (nth (car source-number) sessions))
      (setq source-element (nth (cdr source-session) (nth 2 source-session))))
    (mapc (lambda (x) (push (car x) titles))
	  sessions)
    (setq titles
	  (delete
	   (car source-session)
	   titles))
    (setq target (assoc (completing-read prompt titles nil t) sessions))
    (setq source-time (nth 1 source-session))
    (when (time-less-p (nth 1 target) source-time)
      (setf (nth 1 target) source-time))
    (setf (nth 2 target)
	  (delete-dups ;; This will allow multiple entries for the same
	   ;; URL, but with different details, eg. form selections and
	   ;; cursor positions. It may be better to delete more broadly,
	   ;; any entry with a duplicate URL.
	   (nconc (nth 2 target)
		  (or source-element
		      (nth 2 source-session)))))
    (w3m-save-list w3m-session-file sessions)
    (if (not w3m-session-group-open)
	(w3m-session-select (min source-number (1- (length sessions))))
      (w3m-session-select-open-session-group w3m-session-group-open)
      (forward-line (min (cdr source-number)
			 (- (line-number-at-pos (point-max)) 4))))))

;;;###autoload
(defun w3m-session-select (&optional n toggle nomsg)
  "Select session from session list.
Position point at N-th session if N is given.  With the
prefix-argument, toggles the position of the popup window between
being below or beside the main window."
  (interactive (list nil current-prefix-arg nil))
  (w3m-session-ignore-errors
   (let ((sessions (w3m-load-list w3m-session-file))
	 (showbuf " *w3m-session select*")
	 window)
     (if sessions
	 (progn
	   (w3m--setup-popup-window toggle showbuf nomsg)
	   (w3m-session-select-mode sessions)
	   (when n (w3m-session-select-next n)))
       ;; else, ie. (not sessions)
       (when (setq showbuf (get-buffer showbuf))
	 (when (setq window (prog1 (get-buffer-window showbuf t)
			      (kill-buffer showbuf)))
	   (save-selected-window
	     (select-window window)
	     (or (one-window-p t t) (delete-window window)))))
       (message "No saved session")))))

(defun w3m-session-goto-session (session)
  "Create buffer(s) for the selected SESSION url(s).

If point is selecting a session, then a buffer will be created for each
element (url) of that session that does not already exist. If point is
selecting a session element, then only a single buffer for that
url will be created, only if it does not already exist."
  (let* ((title (nth 0 session))
	 (urls (nth 2 session))
	 (cnum (nth 3 session))
	 (snum (count-lines (point-min) (line-beginning-position)))
	 (i 0)
	 (session-buf (current-buffer))
	 (session-win (selected-window))
	 (w3m-urls ; checking for duplicates
	  (mapcar (lambda(x)
		    (with-current-buffer x
		      (cons w3m-current-url x)))
		  (w3m-list-buffers)))
	 (cwin (or (catch 'window-to-use
		     (save-current-buffer
		       (dolist (win (window-list))
			 (set-buffer (window-buffer win))
			 (when (derived-mode-p 'w3m-mode)
			   (throw 'window-to-use win)))))
		   ;; There is no w3m-mode window.
		   session-win))
	 url cbuf buf pos history url-title
	 w3m-pop-up-windows w3m-pop-up-frames)
    (when (not cwin)
      (error "No visible w3m window found"))
    (with-selected-window cwin
      (w3m-message "Session goto(%s)..." title)
      (while (setq url (pop urls))
	(unless (stringp url)
	  (setq pos       (nth 1 url)
		history   (nth 2 url)
		url-title (nth 3 url)
		url       (nth 0 url)))
	(cond
	 ((setq cbuf (cdr (assoc url w3m-urls))) t)
	 (t ; ie. (not (assoc url w3m-urls))
	  (w3m-goto-url-new-session url nil nil nil nil t) ;; background
	  (setq buf (car (last (w3m-list-buffers))))
	  (when (or (and (numberp cnum) (= cnum i))
		    (and (not cnum) (= i 0)))
	    (setq cbuf buf))
	  (when (and buf pos history)
	    (set-buffer buf)
	    (push (list url (list :title url-title) pos) history) ;; current
	    (setq w3m-history-flat history)
	    (w3m-history-tree pos))
	  (setq i (1+ i))))))
    (unless (get-buffer-window session-buf)
      (w3m-session-select snum))
    (when cbuf
      (set-window-buffer cwin cbuf))
    (w3m-message "Session goto(%s)...done" title)))

(defun w3m-session-rename (sessions num)
  "Rename an entry (either a session or a buffer).
Rename session number NUM of the SESSIONS data structure, when NUM is
an integer.  NUM may also be a cons cell, for which the car is
a session number and the cdr is a buffer entry (i.e., a tab) within
that session.  In that case rename the buffer entry."
  (let* ((default-prompt "Enter new session title (C-g to abort): ")
	 (prompt default-prompt)
	 overwrite
	 title
	 (group (if (consp num) (nth 2 (nth (car num) sessions)) nil))
	 (tmp  (if group (nth (cdr num) group) (nth num sessions)))
	 (otitle (if (consp num) (nth 2 (cdr tmp)) (car tmp))))
    (while (not title)
      ;; A devious way to emulate INITIAL-CONTENTS that is deprecated
      ;; (see the docstring for `read-from-minibuffer').
      (let ((minibuffer-setup-hook (lambda nil (insert otitle))))
	(setq title (read-from-minibuffer prompt nil nil nil nil otitle)))
      (cond
       ((string= title "")
	(setq title nil
	      prompt default-prompt))
       ((string= title otitle)
	(setq prompt (concat title
			     " is same as original title (C-g to abort): ")
	      title nil))
       ((and (not group) (assoc title sessions))
	(if (not (y-or-n-p (format "\"%s\" exists.  Overwrite? " title)))
	    (setq prompt default-prompt
		  title nil)
	  (setq sessions (delq (assoc title sessions) sessions))
	  (setq num (seq-position sessions (assoc otitle sessions)))))
       ((and group (member title (mapcar (lambda (x) (car (last x))) group)))
	;; handle *buffer* rename (overwrite existing)
	;; within a session ("session-group")
	(setq prompt "\
Not yet supported.  Manually delete the other entry, or try again. "
	      title nil))))
    ;; in this case, wrapper must decrement its copy of num
    ;; BB_2018-02-15: I don't understand that comment
    (cond
     (group ;; handle *buffer* rename within a session ("session-group")
      (setf (nth 2 (cdr tmp)) title)
      (setf (nth (cdr num) group) tmp)
      (setf (nth 2 (nth (car num) sessions)) group))
     ;; BB_2018-02-15: The good news is that this seems to be working;
     ;; the bad news is that an examination of the .sessions file
     ;; reveals a format difference, in that the original buffer name
     ;; was encoded with text properties, like so:
     ;;     #("w3m_home_page.html" 0 1 (idx 0))
     ;; and the replacement is just a string.  The text property may
     ;; just be cruft for the purpose of this file, but I'm not sure.
     (t
      (setcar tmp title)
      (setcar (nthcdr num sessions) tmp)))
    (w3m-save-list w3m-session-file sessions)))

(defun w3m-session-delete (sessions num)
  "Delete an entry (either a session or a buffer).
Delete from the SESSIONS data structure the session number NUM, when
NUM is an integer.  NUM may also be a cons cell, for which the car is
a session number and the cdr is a buffer entry (i.e., a tab) within
that session.  In that case delete the buffer entry."
  (if (consp num)
      (let* ((item (nth 2 (nth (car num) sessions)))
	     (tmp (delq (nth (cdr num) item) item)))
	(if (not (zerop (length tmp)))
	    (setf (nth 2 (nth (car num) sessions)) tmp)
	  (setq sessions (delq (nth (car num) sessions) sessions))
	  (setq w3m-session-group-open nil)))
    (setq sessions (delq (nth num sessions) sessions)))
  (if sessions
      (w3m-save-list w3m-session-file sessions)
    (let ((file (expand-file-name w3m-session-file)))
      (when (and (file-exists-p file) (file-writable-p file))
	(delete-file file)))))

(defvar w3m-session-menu-items
  `([,(w3m-make-menu-item "新しいセッションを作る..."
			  "Create New Session...")
     w3m-goto-new-session-url t]
    [,(w3m-make-menu-item "このセッションを複製する" "Copy This Session")
     w3m-copy-buffer w3m-current-url]
    "----" ;; separator
    [,(w3m-make-menu-item "前のセッションに移動する"
			  "Move Previous Session")
     w3m-previous-buffer
     (> (safe-length (w3m-list-buffers)) 1)]
    [,(w3m-make-menu-item "次のセッションに移動する" "Move Next Session")
     w3m-next-buffer
     (> (safe-length (w3m-list-buffers)) 1)]
    "----" ;; separator
    [,(w3m-make-menu-item "このセッションを閉じる" "Close This Session")
     w3m-delete-buffer
     (> (safe-length (w3m-list-buffers)) 1)]
    [,(w3m-make-menu-item "他のセッションを閉じる" "Close Other Sessions")
     w3m-delete-other-buffers
     (> (safe-length (w3m-list-buffers)) 1)]
    [,(w3m-make-menu-item "現在のセッションを保存する"
			  "Save Displayed Sessions")
     w3m-session-save t]
    [,(w3m-make-menu-item "セッションを選択する" "Select Sessions")
     w3m-session-select t])
  "List of the session menu items.")

;;;###autoload
(defun w3m-setup-session-menu ()
  "Setup w3m session items in menubar."
  (unless (lookup-key w3m-mode-map [menu-bar Session])
    (easy-menu-define w3m-session-menu w3m-mode-map "" '("Session"))
    (w3m-easy-menu-add w3m-session-menu)
    (add-hook 'menu-bar-update-hook 'w3m-session-menubar-update)))

(defvar w3m-session-menu-items-pre nil)
(defvar w3m-session-menu-items-time nil)

(defun w3m-session-menubar-update ()
  "Update w3m session menubar."
  (when (and (eq major-mode 'w3m-mode) menu-bar-mode)
    (let ((items w3m-session-menu-items)
	  (pages (w3m-session-make-menu-items)))
      (easy-menu-define w3m-session-menu w3m-mode-map
	"The menu kepmap for the emacs-w3m session."
	(cons "Session" (if pages
			    (append items '("----") pages)
			  items))))))

(defun w3m-session-file-modtime ()
  "Return the modification time of the session file `w3m-session-file'.
The value is a list of two time values `(HIGH LOW)' if the session
file exists, otherwise nil."
  (nth 5 (file-attributes w3m-session-file)))

(defun w3m-session-make-item (item) item)

(defun w3m-session-make-menu-items ()
  "Create w3m session menu items."
  (if (and w3m-session-menu-items-pre
	   w3m-session-menu-items-time
	   (equal w3m-session-menu-items-time
		  (w3m-session-file-modtime)))
      w3m-session-menu-items-pre
    (w3m-session-ignore-errors
     (let ((sessions (w3m-load-list w3m-session-file)))
       (setq w3m-session-menu-items-time (w3m-session-file-modtime))
       (setq w3m-session-menu-items-pre
	     (and sessions
		  (mapcar
		   (lambda (entry)
		     (cons (car entry)
			   (cons (vector "Open all sessions"
					 `(w3m-session-goto-session
					   (quote ,entry)))
				 (mapcar
				  (lambda (item)
				    (let ((title
					   (or (nth 3 item)
					       w3m-session-unknown-title)))
				      (vector
				       title
				       `(w3m-session-goto-session
					 (quote
					  ,(list title
						 nil
						 (list item)
						 nil))))))
				  (nth 2 entry)))))
		   sessions)))))))

;;;###autoload
(defun w3m-session-last-autosave-session ()
  (when w3m-session-load-last-sessions
    (w3m-session-ignore-errors
     (let ((item
	    (let ((sessions (w3m-load-list w3m-session-file))
		  (n 1) x)
	      (catch 'loop
		(while t
		  (if (< w3m-session-automatic-keep-number n)
		      (throw 'loop nil)
		    (setq x (assoc (format "%s-%d"
					   w3m-session-automatic-title n)
				   sessions))
		    (when x (throw 'loop x)))
		  (setq n (1+ n)))))))
       (when (and item
		  (or (and (eq w3m-session-load-last-sessions 'ask)
			   (y-or-n-p "Load the last sessions? "))
		      w3m-session-load-last-sessions))
	 item)))))

;;;###autoload
(defun w3m-session-last-crashed-session ()
  (when (and w3m-session-crash-recovery w3m-session-load-crashed-sessions)
    (w3m-session-ignore-errors
     (let ((item (assoc w3m-session-crash-recovery-title
			(w3m-load-list w3m-session-file))))
       (when (and item
		  (or (and (eq w3m-session-load-crashed-sessions 'ask)
			   (y-or-n-p "Load the crashed sessions? "))
		      (eq w3m-session-load-crashed-sessions t)))
	 item)))))

(add-hook 'w3m-arrived-shutdown-functions 'w3m-session-crash-recovery-remove t)
(add-hook 'w3m-arrived-shutdown-functions 'w3m-session-automatic-save t)

(provide 'w3m-session)

;;; w3m-session.el ends here
