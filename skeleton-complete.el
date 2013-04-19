;;; skeleton-complete.el --- Dynamically expand expressions by provided skeleton (flex matching)

;; Copyright (C) 2013 Bao Haojun

;; Author: Bao Haojun <baohaojun@gmail.com>
;; Maintainer: Bao Haojun <baohaojun@gmail.com>
;; Created: 15th April 2013
;; Keywords: abbrev
;; Version: 0.0.20130419
;; URL: https://github.com/baohaojun/skeleton-complete

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; For more information see the readme at:
;; https://github.com/baohaojun/skeleton-complete

;;; Code:

(require 'ecomplete)

(global-set-key [(meta g)(return)] 'skeleton-display-abbrev)

(defun skeleton-display-abbrev ()
  "Display the next possible abbrev for the text before point."
  (interactive)
  (when (looking-back "\\w\\|_" 1)
    (let* ((end (point))
           (start (save-excursion
                    (search-backward-regexp "\\(\\_<.*?\\)")))
           (word (when start (buffer-substring-no-properties start end)))
           (match (when (and word
                             (not (zerop (length word))))
                    (skeleton-display-matches word))))
      (when match
        (delete-region start end)
        (insert match)))))

(defvar skeleton-regexp-completion-history nil)

(global-set-key [(meta s) (return)] 'skeleton-easy-regexp-display-abbrev)
(global-set-key [(meta g) ?x] 'skeleton-easy-regexp-display-abbrev)

(defun skeleton-easy-regexp-display-abbrev ()
  "Simplify writing the regexp. If we are looking back at, for
e.g., \"sthe='\", the regexp for completion should be
\"s.*?t.*?h.*?e.*?=.*?'\". That is, fill in a \".*?\" pattern
between each 2 characters.

If the region is active, the original pattern is built from what
is in the region.

If the region is not active, the original pattern should be
found by looking backwards for the second word boundary \\b. So
that if we are looking back at:

   [{)}& aonehua naoehu[)+{*

we will get the pattern \"naoehu[)+{\"
"

  (interactive)
  (let (old-regexp new-regexp search-start search-end)
    (if mark-active
	(setq old-regexp (buffer-substring-no-properties (region-beginning) (region-end))
	      search-start (region-beginning)
	      search-end (region-end))
      (let* ((back-limit (line-beginning-position)))
	(push-mark (point))
	(activate-mark)

	(while (and (looking-back "\\s ") (< back-limit (point)))
	  (backward-char))

	(while (and (not (looking-back "\\s ")) (< back-limit (point)))
	  (backward-char))
	(setq search-start (region-beginning)
	      search-end (region-end)
	      old-regexp (buffer-substring-no-properties search-start search-end))))
    
    (let* ((re-list (string-to-list old-regexp))
	   (after-first-char nil)
	   (is-first-char t)
	   (meta-chars (string-to-list "^$*?[]+"))
	   new-re-list
	   char)

      (while re-list
        (setq char (car re-list)
              re-list (cdr re-list))

        (if after-first-char
	    (setq new-re-list (append (nreverse (string-to-list ".*?")) new-re-list))
	  (setq after-first-char t))
        
        (if (eq char ?.)
	    (if is-first-char
		(setq new-re-list (append (nreverse (string-to-list "\\.")) new-re-list))
	      (setq new-re-list (append (nreverse (string-to-list "\\W")) new-re-list)))
	  (when (member char meta-chars)
	    (setq new-re-list (cons ?\\ new-re-list)))
	  (setq new-re-list (cons char new-re-list)))

	(setq is-first-char nil))

      (setq new-regexp (apply 'string (nreverse new-re-list))))
    (skeleton-regexp-display-abbrev new-regexp)))

(defun skeleton-regexp-display-abbrev (regexp)
  "Display the possible abbrevs for the regexp."
  (interactive 
   (progn
     (list 
      (read-shell-command "Get matches with regexp: "
                                      (grep-tag-default)
                                      'skeleton-regexp-completion-history
                                      nil))))
  (let ((match (when (not (zerop (length regexp)))
                 (skeleton-regexp-display-matches regexp))))
    (when match
      (when (and transient-mark-mode mark-active
                 (/= (point) (mark)))
        (delete-region (point) (mark)))
      (insert match))))
  
(defmacro skeleton-max-mini-lines ()
  `(if (floatp max-mini-window-height)
       (truncate (* (frame-height) max-mini-window-height))
     max-mini-window-height))

(defun skeleton-highlight-match-line (matches line max-line-num)
  (let* ((max-lines (skeleton-max-mini-lines))
        (max-lines-1 (1- max-lines))
        (max-lines-2 (1- max-lines-1)))
    (cond
     ((< max-line-num max-lines)
      (ecomplete-highlight-match-line matches line))
     (t
      (let* ((min-disp (* max-lines-1 (/ line max-lines-1)))
             (max-disp (min max-line-num (+ (* max-lines-1 (/ line max-lines-1)) max-lines-2))) 
             (line (% line max-lines-1))
             (matches 
              (with-temp-buffer
                (insert matches)
                (goto-line (1+ min-disp)) 
                (beginning-of-line)
                (concat
                 (buffer-substring-no-properties 
                  (point) 
                  (progn
                    (goto-line (1+ max-disp))
                    (end-of-line)
                    (point))) 
                 (format "\nmin: %d, max: %d, total: %d" min-disp max-disp max-line-num)))))
        (ecomplete-highlight-match-line matches line))))))

(defun skeleton-display-matches (word)
  (skeleton-general-display-matches (delete word (nreverse (skeleton-get-matches-order word)))))

(defun skeleton-regexp-display-matches (regexp)
  (skeleton-general-display-matches (delete "" (nreverse (skeleton-regexp-get-matches regexp)))))

(defun skeleton-general-display-matches (strlist)
  (let* ((matches (concat 
                   (mapconcat 'identity (delete-dups strlist) "\n")
                   "\n"))
	 (line 0)
	 (max-line-num (when matches (- (length (split-string matches "\n")) 2)))
	 (message-log-max nil)
	 command highlight)
    (if (not matches)
	(progn
	  (message "No skeleton matches")
	  nil)
      (if (= max-line-num 0)
          (nth line (split-string matches "\n"))
	(setq highlight (skeleton-highlight-match-line matches line max-line-num)) 
	(while (not (memq (setq command (read-event highlight)) '(? return)))
	  (cond
	   ((or (eq command ?\M-n)
                (eq command ?\C-n))
	    (setq line (% (1+ line) (1+ max-line-num))))
	   ((or (eq command ?\M-p)
                (eq command ?\C-p))
	    (setq line (% (+ max-line-num line) (1+ max-line-num)))))
	  (setq highlight (skeleton-highlight-match-line matches line max-line-num)))
	(when (eq command 'return)
	  (nth line (split-string matches "\n")))))))

(defun skeleton-regexp-get-matches (re &optional no-recur)
  (let ((list (skeleton-regexp-get-matches-internal re no-recur)))
    (if (and (boundp 'old-regexp)
	     (stringp old-regexp))
	(delete old-regexp list)
      list)))

(defun skeleton-regexp-get-matches-internal (re &optional no-recur)
  "Display the possible completions for the regexp."
  (let ((strlist-before nil)
        (strlist-after nil)
        (strlist nil)
        (current-pos (point))
        (re (replace-regexp-in-string "\\*\\*" "\\(\\w\\|_\\)*" re t t)))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward re nil t)
        (let ((mb (match-beginning 0))
              (me (match-end 0)))
          (goto-char mb)

	  ;;; mb can not be in the middle of a word, if so, it is considered a
	  ;;; bad match.
          (unless (or (and (looking-at "\\w")
			   (looking-back "\\w")
					; 你好*ma should provide a good match as "ma", not "你好ma"
			   (not (looking-at "\\b")))
		      (and (boundp 'search-start) ; the found string is over our searching pattern
			   (boundp 'search-end)
			   (= me search-end)))
	    ;;; me should also not be in the middle of a word, if so, we should
	    ;;; find the end of the word.
	    (save-excursion
	      (goto-char me)
	      (when (and (looking-at "\\w")
			 (looking-back "\\w"))
		(re-search-forward "\\b" nil t)
		(setq me (point))))
            (let ((substr (buffer-substring-no-properties mb me)))
              (if (< (point) current-pos)
                  (setq strlist-before (cons substr strlist-before))
                (setq strlist-after (cons substr strlist-after)))))
          (goto-char (1+ mb)))))
    (setq strlist-before  (delete-dups strlist-before)
          strlist-after (delete-dups (nreverse strlist-after)))
    (while (and strlist-before strlist-after)
      (setq strlist (cons (car strlist-before) strlist)
            strlist (cons (car strlist-after) strlist)
            strlist-before (cdr strlist-before)
            strlist-after (cdr strlist-after)))
    (setq strlist (append (nreverse strlist-after) (nreverse strlist-before) strlist))
    ;;get matches from other buffers
    (if no-recur
	strlist
      (let* ((buf-old (current-buffer)))
	(save-excursion
	  (mapcar (lambda (buf)
		    (with-current-buffer buf ;;next let's recursive call
		      (setq strlist (append (skeleton-regexp-get-matches re t) strlist))))
		  (delete buf-old
			  (mapcar (lambda (w)
				    (window-buffer w))
				  (window-list))))
	  strlist))
      (if strlist
	  strlist
	(let* ((buf-old (current-buffer)))
	  (save-excursion
	    (mapcar (lambda (buf)
		      (with-current-buffer buf ;;next let's recursive call
			(unless strlist
			  (setq strlist (append (skeleton-regexp-get-matches re t) strlist)))))
		    (delete buf-old
			    (buffer-list)))
	    strlist))
	))))

(defun skeleton-get-matches-order (skeleton &optional no-recur)
  "Get all the words that contains every character of the
SKELETON from the current buffer The words are ordered such that
words closer to the (point) appear first"
  (let ((skeleton-re (mapconcat 'string skeleton ".*"))
        (strlist-before nil)
        (strlist-after nil)
        (strlist nil)
        (current-pos (point)))
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (if (setq endpt (re-search-forward "\\(\\_<.*?\\_>\\)" nil t))
            (let ((substr (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
              (when (string-match skeleton-re substr)
                (if (< (point) current-pos)
                    (setq strlist-before (cons substr strlist-before))
                  (setq strlist-after (cons substr strlist-after)))))
          (end-of-buffer)))) 
    (setq strlist-before  (delete-dups strlist-before)
          strlist-after (delete-dups (nreverse strlist-after)))
    (while (and strlist-before strlist-after)
      (setq strlist (cons (car strlist-before) strlist)
            strlist (cons (car strlist-after) strlist)
            strlist-before (cdr strlist-before)
            strlist-after (cdr strlist-after)))
    (setq strlist (append (nreverse strlist-after) (nreverse strlist-before) strlist))

    (if no-recur
	strlist
      (let* ((buf-old (current-buffer)))
	(save-excursion
	  (mapcar (lambda (buf)
		    (with-current-buffer buf ;;next let's recursive call
		      (setq strlist (append (skeleton-get-matches-order skeleton t) strlist))))
		  (delete buf-old
			  (mapcar (lambda (w)
				    (window-buffer w))
				  (window-list))))
	  strlist))
      (if (and strlist (cdr strlist))
	  strlist
	(let* ((buf-old (current-buffer)))
	  (save-excursion
	    (mapcar (lambda (buf)
		      (with-current-buffer buf ;;next let's recursive call
			(unless (and strlist (cdr strlist))
			  (setq strlist (delete-dups (append (skeleton-get-matches-order skeleton t) strlist))))))
		    (delete buf-old (buffer-list)))
	    strlist))))))

(provide 'skeleton-complete)

;;; skeleton-complete.el ends here
