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

(global-set-key [(meta return)] 'regexp-display-abbrev)
(defun regexp-display-abbrev (regexp)
  "Display the possible abbrevs for the regexp."
  (interactive 
   (progn
     (list (read-shell-command "Get matches with regexp: "
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
	   ((eq command ?\M-n)
	    (setq line (% (1+ line) (1+ max-line-num))))
	   ((eq command ?\M-p)
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
      (while (not (eobp))
        (if (setq endpt (re-search-forward re nil t))
            (let ((substr (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
              (if (< (point) current-pos)
                  (setq strlist-before (cons substr strlist-before))
                (setq strlist-after (cons substr strlist-after))))
          (end-of-buffer))))
    (setq strlist-before  (delete-dups strlist-before)
          strlist-after (delete-dups (nreverse strlist-after)))
    (while (and strlist-before strlist-after)
      (setq strlist (cons (car strlist-before) strlist)
            strlist (cons (car strlist-after) strlist)
            strlist-before (cdr strlist-before)
            strlist-after (cdr strlist-after)))
    (setq strlist (append strlist-after strlist-before strlist))
    ;;get matches from other buffers
    (let ((buf-old (current-buffer)))
      (save-excursion
        (with-temp-buffer
          (let ((buf-tmp (current-buffer)))
            (mapcar (lambda (x)
                      (set-buffer x)
                      (save-excursion
                        (save-match-data
                          (goto-char (point-min))
                          (while (re-search-forward re nil t)
                            (let ((substr (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
                              (save-excursion
                                (set-buffer buf-tmp)
                                (insert (format "%s\n" substr))))))))
                    (delete buf-tmp (delete buf-old (buffer-list))))
            (set-buffer buf-tmp)
            (append (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")
                    strlist)))))))

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
      (setq strlist (cons (car strlist-after) strlist)
            strlist (cons (car strlist-before) strlist)
            strlist-after (cdr strlist-after)
            strlist-before (cdr strlist-before)))
    (setq strlist (append strlist-after strlist-before strlist))
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
