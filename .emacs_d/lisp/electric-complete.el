(provide 'electric-complete)
(require 'ecomplete)
(global-set-key [(control meta /)] 'skeleton-display-abbrev)
(global-set-key [(control return)] 'skeleton-display-abbrev)


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

(global-set-key [(meta return)] 'easy-regexp-display-abbrev)

(defun easy-regexp-display-abbrev (regexp)
  "Simplify writing the regexp. Some thing like \"sthe.\" will be generated as 
\"s.*?t.*?h.*?e\\W\", the rule is, for each 2 \\w character, fill in a \".*?\" pattern
and the original pattern should be looked for backwards with \\w and \\."
  (interactive
   (progn
     (list
      (let ((oldcur (point)))
        (push-mark (point))
        (activate-mark)
        (while (not (looking-back "\\w\\|\\."))
          (backward-char))
        (while (looking-back "\\w\\|\\.")
          (backward-char))
        (buffer-substring-no-properties (point) oldcur)))))
  (let (last-char-w-p
        (re-list (string-to-list regexp))
        (new-re-list nil))
    (while re-list
      (setq char (car re-list)
            re-list (cdr re-list))
      (case (char-syntax char)
        (?w (when last-char-w-p
              (setq new-re-list (append (nreverse (string-to-list ".*?")) new-re-list)))
            (setq last-char-w-p t
                  new-re-list (cons char new-re-list)))
        (t
         (setq last-char-w-p nil)
         (case char
             (?. (setq new-re-list (append (nreverse (string-to-list "\\W")) new-re-list)))
             (t (when (member char (string-to-list "^$*?[]+"))
                    (setq new-re-list (cons ?\\ new-re-list)))
                (setq new-re-list (cons char new-re-list)))))))
    (setq regexp (apply 'string (nreverse new-re-list))))
  (regexp-display-abbrev regexp))

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
  

(defun skeleton-highlight-match-line (matches line max-line-num)
  (cond
   ((< max-line-num 10)
    (ecomplete-highlight-match-line matches line))
   (t
    (let* ((min-disp (* 9 (/ line 9)))
          (max-disp (min max-line-num (+ (* 9 (/ line 9)) 8))) 
          (line (% line 9))
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
              (format "\nmin: %d, max: %d, total: %d\n" min-disp max-disp max-line-num)))))
      (ecomplete-highlight-match-line matches line)))))

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
          (unless (and (looking-at "\\w")
                       (looking-back "\\w"))
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

