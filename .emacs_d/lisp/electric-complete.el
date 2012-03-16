(provide 'electric-complete)
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

(defvar regexp-completion-history nil)

(global-set-key [(meta s) (return)] 'easy-regexp-display-abbrev)

(defun easy-regexp-display-abbrev ()
  "Simplify writing the regexp. If we are looking back at, for
e.g., \"sthe='\", the regexp for completion should be
\"s.*?t.*?h.*?e.*?=.*?'\". That is, fill in a \".*?\" pattern
between each 2 characters.

The original pattern should be looked backwards for the second
word boundary \\b. So that if we are looking back at:

   [{)}*& aonehua naoehu[)+{*

we will get the pattern \"naoehu[)+{*\"
"
  (interactive)
  (let* (reg-part1
         reg-part2 
         regexp 
         len1
         len2
         (oldcur (point))
	 (back-limit 0))
    (when mark-active
      (setq oldcur (max (mark) (point))
	   back-limit (min (mark) (point)))
      (goto-char oldcur))
    (push-mark (point))
    (activate-mark)
    (while (not (looking-back "\\w"))
      (backward-char))
    (setq reg-part2 (buffer-substring-no-properties (point) oldcur)
          oldcur (point))
    (while (and (looking-back "\\w\\|\\.") (< back-limit (point)))
      (backward-char))
    (setq reg-part1 (buffer-substring-no-properties (point) oldcur)
          regexp (concat reg-part1 reg-part2)
          len1 (length reg-part1)
          len2 (length reg-part2))
    
    (let* (last-char-w-p
          (re-list (string-to-list regexp))
          (nth 0)
          (new-re-list nil))

      (while re-list
        (setq char (car re-list)
              re-list (cdr re-list))

        (when  (> nth 0)
          (setq new-re-list (append (nreverse (string-to-list ".*?")) new-re-list)))
        
        (if (eq char ?.)
            (setq new-re-list (append (nreverse (string-to-list "\\W")) new-re-list))
          (case (char-syntax char)
            (?w (setq new-re-list (cons char new-re-list)))
            (t (when (member char (string-to-list "^$*?[]+"))
                 (setq new-re-list (cons ?\\ new-re-list)))
               (setq new-re-list (cons char new-re-list)))))
        (setq nth (1+ nth)))
      (setq regexp (apply 'string (nreverse new-re-list))))
    (regexp-display-abbrev regexp)))

(defun regexp-display-abbrev (regexp)
  "Display the possible abbrevs for the regexp."
  (interactive 
   (progn
     (list 
      (read-shell-command "Get matches with regexp: "
                                      (grep-tag-default)
                                      'regexp-completion-history
                                      nil))))
  (let ((match (when (not (zerop (length regexp)))
                 (regexp-display-matches regexp))))
    (when match
      (when (and transient-mark-mode mark-active
                 (/= (point) (mark)))
        (delete-region (point) (mark)))
      (insert match))))
  
(defmacro max-mini-lines ()
  `(if (floatp max-mini-window-height)
       (truncate (* (frame-height) max-mini-window-height))
     max-mini-window-height))

(defun skeleton-highlight-match-line (matches line max-line-num)
  (let* ((max-lines (max-mini-lines))
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
  (general-display-matches (delete word (nreverse (skeleton-get-matches-order word)))))

(defun regexp-display-matches (regexp)
  (general-display-matches (delete "" (nreverse (regexp-get-matches regexp)))))

(defun general-display-matches (strlist)
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


(defun regexp-get-matches (re)
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
          (unless
	      (and (looking-at "\\w")
		   (looking-back "\\w"))
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
    (if current-prefix-arg
        (let ((buf-old (current-buffer))
              (current-prefix-arg nil)
              (buffer-regexp (read-shell-command "What other buffers to search: ")))
          (save-excursion
            (mapcar (lambda (buf)
                          (with-current-buffer buf ;;next let's recursive call
                            (setq strlist (append (regexp-get-matches re) strlist))))
                    (delete-if-not
                     (lambda (x)
                       (string-match-p buffer-regexp (buffer-name x)))
                     (delete buf-old
                             (buffer-list))))
                strlist))
      strlist)))

(defun skeleton-get-matches-order (skeleton)
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
    (when current-prefix-arg
      (save-excursion
        (let ((buffer-old (current-buffer))
              (buffer (general-display-matches
                       (mapcar 'buffer-name (buffer-list))))
              (current-prefix-arg nil))
          (with-current-buffer buffer
            (unless (eq (current-buffer) buffer-old)
              (setq strlist (append (skeleton-get-matches-order skeleton) strlist)))))))
    strlist))

