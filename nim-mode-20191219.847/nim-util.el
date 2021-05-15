;;; nim-util.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:
(require 'nim-syntax)

(cl-defun nim-log (&rest msg-and-rest)
  (apply `((lambda () (lwarn 'nim :debug ,@msg-and-rest)))))

(cl-defun nim-log-err (&rest msg-and-rest)
  (apply `((lambda () (lwarn 'nim :error ,@msg-and-rest)))))

(defun nim-util-goto-line (line-number)
  "Move point to LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

(defun nim-util-forward-comment (&optional direction)
  "Nim mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (nim-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

(defun nim-util-backward-stmt ()
  "Move point backward to the beginning of the current statement.
Point is moved to the beginning of the first symbol that is
either the first on a line or the first after a
semicolon.  Balanced parentheses, strings and comments are
skipped."
  (let ((level (nth 0 (syntax-ppss))))
    (save-restriction
      ;; narrow to surrounding parentheses
      (nim-util-narrow-to-paren)
      (while (progn
               (if (re-search-backward "[,;]" (line-beginning-position) t)
                   (forward-char)
                 (beginning-of-line))
               (let ((state (syntax-ppss)))
                 (and
                  (or (> (nth 0 state) level)
                      (nim-syntax-comment-or-string-p state)
                      (save-match-data
                        (looking-at (nim-rx (* space) (group operator))))
                      (not (looking-at (nim-rx (* space) (group symbol-name)))))
                  (not (bobp))
                  (prog1 t (backward-char))))))
      (and (match-beginning 1)
           (goto-char (match-beginning 1))))))

(defun nim-util-narrow-to-paren ()
  "Narrow buffer to content of enclosing parentheses.
Returns non-nil if and only if there are enclosing parentheses."
  (save-excursion
    (condition-case nil
        (prog1 t
          (narrow-to-region (progn
                              (backward-up-list)
                              (1+ (point)))
                            (progn
                              (forward-list)
                              (1- (point)))))
      (scan-error nil))))

(defun nim-util-real-current-column ()
  "Return the current column without narrowing."
  (+ (current-column)
     (if (= (line-beginning-position) (point-min))
         (save-excursion
           (goto-char (point-min))
           (save-restriction
             (widen)
             (current-column)))
       0)))

(defun nim-util-real-current-indentation ()
  "Return the indentation without narrowing."
  (+ (current-indentation)
     (if (= (line-beginning-position) (point-min))
         (save-excursion
           (goto-char (point-min))
           (save-restriction
             (widen)
             (current-column)))
       0)))

;; Stolen from org-mode
(defun nim-util-clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone and defaults
to \"^nim-\"."
  (mapc
   (lambda (pair)
     (and (symbolp (car pair))
          (string-match (or regexp "^nim-")
                        (symbol-name (car pair)))
          (set (make-local-variable (car pair))
               (cdr pair))))
   (buffer-local-variables from-buffer)))

(defun nim-util-comint-last-prompt ()
  "Return comint last prompt overlay start and end.
This is for compatibility with Emacs < 24.4."
  (cond ((bound-and-true-p comint-last-prompt-overlay)
         (cons (overlay-start comint-last-prompt-overlay)
               (overlay-end comint-last-prompt-overlay)))
        ((bound-and-true-p comint-last-prompt)
         comint-last-prompt)
        (t nil)))

(defun nim-util-list-directories (directory &optional predicate max-depth)
  "List DIRECTORY subdirs, filtered by PREDICATE and limited by MAX-DEPTH.
Argument PREDICATE defaults to `identity' and must be a function
that takes one argument (a full path) and returns non-nil for
allowed files.  When optional argument MAX-DEPTH is non-nil, stop
searching when depth is reached, else don't limit."
  (let* ((dir (expand-file-name directory))
         (dir-length (length dir))
         (predicate (or predicate #'identity))
         (to-scan (list dir))
         (tally nil))
    (while to-scan
      (let ((current-dir (car to-scan)))
        (when (funcall predicate current-dir)
          (setq tally (cons current-dir tally)))
        (setq to-scan (append (cdr to-scan)
                              (nim-util-list-files
                               current-dir #'file-directory-p)
                              nil))
        (when (and max-depth
                   (<= max-depth
                       (length (split-string
                                (substring current-dir dir-length)
                                "/\\|\\\\" t))))
          (setq to-scan nil))))
    (nreverse tally)))

(defun nim-util-list-files (dir &optional predicate)
  "List files in DIR, filtering with PREDICATE.
Argument PREDICATE defaults to `identity' and must be a function
that takes one argument (a full path) and returns non-nil for
allowed files."
  (let ((dir-name (file-name-as-directory dir)))
    (apply #'nconc
           (mapcar (lambda (file-name)
                     (let ((full-file-name (expand-file-name file-name dir-name)))
                       (when (and
                              (not (member file-name '("." "..")))
                              (funcall (or predicate #'identity) full-file-name))
                         (list full-file-name))))
                   (directory-files dir-name)))))

(defun nim-util-list-packages (dir &optional max-depth)
  "List packages in DIR, limited by MAX-DEPTH.
When optional argument MAX-DEPTH is non-nil, stop searching when
depth is reached, else don't limit."
  (let* ((dir (expand-file-name dir))
         (parent-dir (file-name-directory
                      (directory-file-name
                       (file-name-directory
                        (file-name-as-directory dir)))))
         (subpath-length (length parent-dir)))
    (mapcar
     (lambda (file-name)
       (replace-regexp-in-string
        (rx (or ?\\ ?/)) "." (substring file-name subpath-length)))
     (nim-util-list-directories
      (directory-file-name dir)
      (lambda (dir)
        (file-exists-p (expand-file-name "__init__.py" dir)))
      max-depth))))

(defun nim-util-popn (lst n)
  "Return LST first N elements.
N should be an integer, when negative its opposite is used.
When N is bigger than the length of LST, the list is
returned as is."
  (let* ((n (min (abs n)))
         (len (length lst))
         (acc))
    (if (> n len)
        lst
      (while (< 0 n)
        (setq acc (cons (car lst) acc)
              lst (cdr lst)
              n (1- n)))
      (reverse acc))))

(defun nim-util-strip-string (string)
  "Strip STRING whitespace and newlines from end and beginning."
  (replace-regexp-in-string
   (rx (or (: string-start (* (any whitespace ?\r ?\n)))
           (: (* (any whitespace ?\r ?\n)) string-end)))
   ""
   string))

(defun nim-util-valid-regexp-p (regexp)
  "Return non-nil if REGEXP is valid."
  (ignore-errors (string-match regexp "") t))

;; electric-pair-mode ;;
(defun nim-electric-pair-string-delimiter ()
  (when (and electric-pair-mode
             (memq last-command-event '(?\" ?\'))
             (let ((count 0))
               (while (eq (char-before (- (point) count)) last-command-event)
                 (cl-incf count))
               (= count 3))
             (eq (char-after) last-command-event))
    (save-excursion (insert (make-string 2 last-command-event)))))

(provide 'nim-util)
;;; nim-util.el ends here
