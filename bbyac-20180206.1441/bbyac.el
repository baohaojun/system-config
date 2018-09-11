;;; bbyac.el --- Type a little Bit, and Bang! You Are Completed.

;; Copyright (C) 2015 Bao Haojun

;; Author: Bao Haojun <baohaojun@gmail.com>
;; Maintainer: Bao Haojun <baohaojun@gmail.com>
;; Created: 28th January 2015
;; Package-Requires: ((browse-kill-ring "1.3") (cl-lib "0.5"))
;; Keywords: abbrev
;; Version: 0.0.20150128
;; URL: https://github.com/baohaojun/bbyac

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

;; The Bit Bang You Are Completed package is Yet Another Completion
;; method.

;; Say you have a Lisp variable named
;; this-is-a-very-long-variable-for-example, in the buffer in which
;; this name is already typed, you can type tvvf and then press M-g
;; <return> to complete it.  You can type tvx or even tx for the same
;; effect, if you like.

;; There are also other completing methods, for e.g., to complete
;; arbitrary strings, whole lines, sexp and paragraphs. The last two
;; is not as useful as the others.

;; For more information see the readme at:
;; https://github.com/baohaojun/bbyac

;;; Code:

(require 'ecomplete)
(require 'cl-lib)
(require 'thingatpt)
(require 'browse-kill-ring)

(defvar bbyac--major-mode-buffer-hash
  (make-hash-table)
  "The major mode -> buffer hash table.")

(defvar bbyac--source-file
  load-file-name
  "The bbyac source file.")

(defvar bbyac--source-dir
  (file-name-directory (file-truename bbyac--source-file))
  "The bbyac source dir.")

(defgroup bbyac nil
  "Type a little Bit, and Bang! You Are Completed."
  :group 'abbrev)

(defcustom bbyac-max-chars 80
  "Maximum chars in any single match to use ecomplete.

If exceeded, will use other schemes (such as browse-kill-ring) to
select from the matches.

If you don't like the ecomplete style or think ecomplete is
unreliable, change this to a very small value, such as 1 to force browse-kill-ring."
  :type 'integer
  :group 'bbyac)

(defcustom bbyac-user-config-dir
  (expand-file-name "~/.config/bbyac")
  "The directory to find user's bbyac config files."
  :type 'string
  :group 'bbyac)

(defvar bbyac--start
  nil
  "The start of the bit that is to be expanded.

This var should be set when you are extracting the bit, and
it is used when the expansion of the bit is to be inserted.")

(defvar bbyac--end
  nil
  "The end of the bit that is to be expanded.

See `bbyac--start'.")

(defvar bbyac--the-bit
  nil
  "The bit that is extracted with a bit extracter.")

(defvar bbyac--contains-upcase
  nil
  "Whether the bit contains upcase char.")

(defun bbyac--contains-upcase-p (str)
  "Return non-nil if STR contain upcase chars."
  (let ((case-fold-search nil))
    (string-match-p "[[:upper:]]" str)))

(defun bbyac--clear-vars ()
  "Clear the vars used by this package."
  (setq bbyac--start nil
        bbyac--end nil
        bbyac--the-bit nil
        bbyac--contains-upcase nil))

(defun bbyac--interleave (l1 l2)
  "Interleave the elements from the 2 lists L1 and L2."
  (let (result)
    (while (and l1 l2)
      (setq result (cons (car l1) result) ; consing like this makes strlist in bad order
            result (cons (car l2) result)
            l1 (cdr l1)
            l2 (cdr l2)))
    (append (nreverse result) l1 l2)))

(defun bbyac--difference (l1 l2)
  "Return a list of L1 with the elements also in L2 removed."
  (cl-delete-if (lambda (e) (member e l2)) l1))

(defun bbyac--regexp-quote (char)
  "Regexp-quote CHAR.

If bbyac--the-bit contains upcase chars, then
`case-fold-search will be turned on, in that case, we want the
upcase char to match exactly, but the downcase char to match
either an upcase or a downcase.

For e.g., given HelloWorld and helloWorld, we want Hw to match
the former, but not the later.  If the w in Hw was to made to
match w only, then none will match, which sometimes is a
surprise."
  (if (and bbyac--contains-upcase
           (>= char ?a)
           (<= char ?z))
      (format "[%c%c]" char (upcase char))
    (regexp-quote (string char))))

(defun bbyac--symbol-bbyac-extracter ()
  "Extract a bit for symbol-completing.

Return a regexp rewritten from the bit.

In addition, extracters can also set the variables
`bbyac--the-bit', `bbyac--start' and `bbyac--end'."
  (if mark-active
      (setq bbyac--start (region-beginning)
            bbyac--end (region-end))
    (when (looking-back "\\w\\|_" 1)
      (setq bbyac--end (point)
            bbyac--start (or (save-excursion
                               (search-backward-regexp "\\_<" (line-beginning-position) t))
                             bbyac--end))))
  (if (or (eq bbyac--start nil)
          (eq bbyac--end nil))
      (error "No BIT is found, thus can not BANG")
    (setq bbyac--the-bit (buffer-substring-no-properties bbyac--start bbyac--end)
          bbyac--contains-upcase (bbyac--contains-upcase-p bbyac--the-bit))
    (unless (string= "" bbyac--the-bit)
      (let ((symbol-chars "\\(?:\\sw\\|\\s_\\)*?"))
        (concat
         "\\_<"
         symbol-chars
         (mapconcat
          #'bbyac--regexp-quote
          (string-to-list bbyac--the-bit)
          symbol-chars)
         symbol-chars
         "\\_>")))))

(defun bbyac--line-bbyac-extracter ()
  "Extract a bit for line-completing.

If the region is active, the bit is what is in the region.

If the region is not active, the bit should be found by checking
white spaces (see the code).  For e.g., if we are looking back
at (with the last * denoting where the point is):

    [{)}& aonehua naoehu[)+{ *

we will get the bit:

    \"naoehu[)+{ \"

See also `bbyac--symbol-bbyac-extracter'."
  (if mark-active
      (setq bbyac--start (region-beginning)
            bbyac--end (region-end))
    (save-excursion
      (let* ((back-limit (line-beginning-position))
             (cp (point)))
        (while (and (looking-back "\\s ") (< back-limit (point)))
          (backward-char))
        (while (and (not (looking-back "\\s ")) (< back-limit (point)))
          (backward-char))
        (setq bbyac--start (point)
              bbyac--end cp))))
  (if (or (eq bbyac--start nil)
          (eq bbyac--end nil))
      (error "No BIT is found, thus can not BANG")
    (setq bbyac--the-bit (buffer-substring-no-properties bbyac--start bbyac--end)
          bbyac--contains-upcase (bbyac--contains-upcase-p bbyac--the-bit))
    (when bbyac--the-bit
      (let ((re-prefix "")
            (re-postfix ""))
        (when (string-match-p "\\.\\*\\.\\*" bbyac--the-bit)
          (setq re-prefix (concat (replace-regexp-in-string "\\(.*?\\)\\.\\*\\.\\*.*" "\\1" bbyac--the-bit) ".*?")
                bbyac--the-bit (replace-regexp-in-string ".*?\\.\\*\\.\\*" "" bbyac--the-bit)))
        (when (string-match-p "\\.\\*\\.\\*" bbyac--the-bit)
          (setq re-postfix (concat ".*?" (replace-regexp-in-string ".*\\.\\*\\.\\*\\(.*\\)" "\\1" bbyac--the-bit))
                bbyac--the-bit (replace-regexp-in-string "\\(.*\\)\\.\\*\\.\\*.*" "\\1" bbyac--the-bit)))
        (concat re-prefix
                (let ((the-regexp
                       (mapconcat
                        #'bbyac--regexp-quote
                        (string-to-list bbyac--the-bit)
                        ".*?")))
                  ;; performance consideration: if syntax of bit's first char
                  ;; is word, then it must match word boundary
                  (when (and (string-match-p "^[[:alnum:]]" bbyac--the-bit)
                             (string= re-prefix ""))
                    (setq the-regexp (concat "\\b" the-regexp)))
                  ;; if bit's last char is word syntax, should extend the
                  ;; completion to word boundaries
                  (when (and (string-match-p "[[:alnum:]]$" bbyac--the-bit)
                             (string= re-postfix ""))
                    (setq the-regexp (concat the-regexp "\\w*?\\b")))
                  the-regexp)
                re-postfix)))))

(defmacro bbyac--make-matcher (matcher-name matcher-doc extract-match move-along)
  "Make a new matcher function named as MATCHER-NAME.

MATCHER-DOC is the doc for the new matcher function.

EXTRACT-MATCH and MOVE-ALONG specifies how to extract the bit and
how to move to the next position after a match is found.


The function MATCHER-NAME thus created will take 3 arguments, RE,
BUFFER, and TAG.  When called, it will search the BUFFER for RE,
return all the matching substrings in an order dependent on
TAG.  See `bbyac--matcher'.

EXTRACT-MATCH and MOVE-ALONG are 2 Lisp sexp, you can use the
variables mb and me in these sexps.  Result of EXTRACT-MATCH
should be a string; MOVE-ALONG is only used for its side-effects."
  (declare (doc-string 2))
  `(defun ,matcher-name (re buffer tag)
     ,matcher-doc
     (let ((strlist-before nil)
           (strlist-after nil)
           (strlist nil)
           (re-2 nil)
           (old-point (point)))
       (when (string-match "" re)
         (setq re-2 (replace-regexp-in-string ".*" "" re)
               re (replace-regexp-in-string ".*" "" re)))
       (with-current-buffer buffer
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward re nil t)
             (let ((mb (match-beginning 0))
                   (me (match-end 0)))
               (when (and re-2
                          (goto-char me)
                          (or (end-of-line) t)
                          (re-search-forward re-2 nil t))
                 (setq me (match-end 0)))
               (let ((substr ,extract-match))
                 (cond
                  ((and (eq tag 'current)
                        (< mb bbyac--start)
                        (> me bbyac--start)))
                  (t
                   (if (and (eq tag 'current)
                            (< (point) old-point))
                       ;; substr closer to the old-point is at the head of strlist-before, in good order
                       (setq strlist-before (cons substr strlist-before))
                     ;; substr further to the old-point is at the head of strlist-after, in bad order
                     (setq strlist-after (cons substr strlist-after))))))
               ,move-along))
           (setq strlist (bbyac--interleave (nreverse strlist-after) strlist-before))
           ;; This expansion is useless, it's the same as the bit
           ;; (in fact, extracted from the same place, and thus at the
           ;; car of strlist. We must remove it here, or else the
           ;; buried buffers will never get matched).
           (if (and (eq tag 'current)
                    (stringp (car strlist))
                    (string= (car strlist) bbyac--the-bit))
               (cdr strlist)
             strlist))))))

(bbyac--make-matcher
 bbyac--matcher
 "Search the buffer to collect a list of all strings matching `re'.

If TAG is 'current, the returned list is sorted (interleaved)
roughly according to their distance to where the point is. First
one entry coming *before* the point, then one entry coming
*after* point, until either list runs out.

Or else the returned list of strings is in the order they appear in the buffer."
 (buffer-substring-no-properties mb me)
 (goto-char (1+ mb)))

(bbyac--make-matcher
 bbyac--line-extracting-matcher
 "Search the buffer to collect a list of all lines matching `re'.

See `bbyac--matcher'."
 (buffer-substring-no-properties (if (>= mb (line-beginning-position))
                                     (line-beginning-position)
                                   (save-excursion
                                     (goto-char mb)
                                     (line-beginning-position)))
                                 (line-end-position))
 (end-of-line))

(bbyac--make-matcher
 bbyac--sexp-extracting-matcher
 "Search the buffer to collect a list of all balanced expressions matching `re'.

See `bbyac--matcher'."
 (let ((e (save-excursion
            (goto-char mb)
            (condition-case nil (forward-sexp)
              (error nil))
            (point))))
   (buffer-substring-no-properties mb e))
 (let ((e (save-excursion
            (goto-char mb)
            (condition-case nil (forward-sexp)
              (error nil))
            (point))))
   (goto-char (if (= e mb)
                  (1+ mb)
                e))))

(bbyac--make-matcher
 bbyac--paragraph-extracting-matcher
 "Search the buffer to collect a list of all paragraphs matching `re'.

See `bbyac--matcher'."
 (buffer-substring-no-properties
  (save-excursion
    (start-of-paragraph-text)
    (point))
  (save-excursion
    (end-of-paragraph-text)
    (point)))
 (end-of-paragraph-text))

(defun bbyac--buffer-filter ()
  "Return an alist of buffers to be matched againt the bit for completions.

The entries in the returned alist is of the form (BUFFER . TAG)
where TAG is one of 'current, 'visible and 'buried.

With a 'current tag, the BUFFER is the `current-buffer' where
completing is taking place.

With a 'visible tag, the BUFFER is currently displayed in a
window.

With a 'buried tag, the BUFFER is currently invisible.

The buffers should be in the order of the tags listed above:
first 1 'current buffer, then 0 or more 'visible buffers, then
all 'buried buffers."
  (let* ((current-buffer (current-buffer))
         (visible-buffers (delete current-buffer
                                  (delete-dups
                                   (mapcar (lambda (w)
                                             (window-buffer w))
                                           (window-list)))))
         (buried-buffers (bbyac--difference (buffer-list) (cons current-buffer visible-buffers))))
    (cl-delete-if
     (lambda (buf-tag) (eq (with-current-buffer (car buf-tag) major-mode) 'image-mode))
     (nconc
      (list (cons current-buffer 'current))
      (mapcar
       (lambda (b)
         (cons b 'visible))
       visible-buffers)
      (mapcar
       (lambda (b)
         (cons b 'buried))
       buried-buffers)))))

(defun bbyac--get-matches (re matcher buffer-filter)
  "Find all matching strings.

RE is the regexp for the matching.

MATCHER specifies how to match.  See `bbyac--matcher'.

BUFFER-FILTER specifies which buffers should be searched.  See
`bbyac--buffer-filter'.

Return the list of strings thus matched."

  (bbyac--clean-up
   (let ((matched-buried nil)
         (matched-any nil)
         result)
     (cl-mapcan (lambda (tagged-buffer)
                  (let ((buffer (car tagged-buffer))
                        (tag (cdr tagged-buffer))
                        strlist)
                    (if (and
                         matched-buried
                         (not current-prefix-arg))
                        nil
                      (prog1
                          (when (or
                                 (not matched-any) ; found nothing yet
                                 (not (eq tag 'buried)) ; this buffer is important
                                 current-prefix-arg) ; user force to do all buffers with C-u
                            (setq strlist (funcall matcher re buffer tag)))
                        (when strlist
                          (setq matched-any t))
                        (when (and strlist (eq tag 'buried))
                          (setq matched-buried t))))))
                (funcall buffer-filter)))))

(defun bbyac--string-multiline-p (str)
  "Return t if STR is too long or span multilines."
  (or (> (length str) bbyac-max-chars)
      (string-match-p "\n" str)))

(defun bbyac--delete-region (start end)
  "Delete the region marked by START and END.

Works even in Term mode."
  (if (eq major-mode 'term-mode)
      (while (< start end)
        (setq start (1+ start))
        (term-send-backspace))
    (delete-region start end)))

(defun bbyac--insert (str)
  "Insert the STR.

Works even in Term mode."
  (if (eq major-mode 'term-mode)
      (term-send-raw-string str)
    (insert str)))

(defun bbyac--general-expand (extracter &optional matcher buffer-filter match-rewriter)
  "General function to expand a bit using the functional arguments.

EXTRACTER will extract a regexp from the current BIT of text.

MATCHER will match the regexp returned by EXTRACTER to match some
completions. The default matcher is `bbyac--matcher'.

BUFFER-FILTER should return some tagged buffers in which MATCHER
works.

MATCH-REWRITER, if present, will rewrite the final matched
completion before inserting it.

See `bbyac--symbol-bbyac-extracter' for EXTRACTER.  See
`bbyac--matcher' for MATCHER.  See `bbyac--buffer-filter' for
BUFFER-FILTER.

If wanted, user can extend bbyac with their own
EXTRACTER, MATCHER and BUFFER-FILTER."
  (bbyac--clear-vars)
  (setq matcher (or matcher #'bbyac--matcher)
        buffer-filter (or buffer-filter #'bbyac--buffer-filter))
  (let* ((the-regexp (funcall extracter))
         (case-fold-search (not bbyac--contains-upcase))
         matches
         match)
    (when (and the-regexp
               (setq matches (bbyac--get-matches the-regexp matcher buffer-filter)))
      (if (or (minibufferp)
              (cl-notany #'bbyac--string-multiline-p matches)
              match-rewriter)
          (progn
            (setq match (if (not (minibufferp))
                            (cond
                             ((not (cdr matches))
                              (car matches))
                             ((fboundp 'ivy-read)
                              (ivy-read "Select which match do you want: " matches))
                             ((fboundp 'helm-comp-read)
                              (helm-comp-read "Select which match do you want: " matches))
                             (t
                              (bbyac--display-matches matches)))
                          (bbyac--display-matches matches)))
            (when (and bbyac--start bbyac--end)
              (bbyac--delete-region bbyac--start bbyac--end))
            (when match-rewriter
              (setq match (funcall match-rewriter match)))
            (bbyac--insert match))
        (when (and bbyac--start bbyac--end)
          (delete-region bbyac--start bbyac--end))
        (deactivate-mark)
        (let ((kill-ring matches)
              (kill-ring-yank-pointer matches))
          (browse-kill-ring))))))

(defun bbyac-expand-symbols ()
  "Find and expand the bit into a symbol.

The bit itself is constructed by symbol constituting
characters before the point."
  (interactive)
  (bbyac--general-expand #'bbyac--symbol-bbyac-extracter))

(defun bbyac--major-mode-match-rewriter (matched-str)
  "Rewrite the MATCHED-STR from the major-mode's bbyac file."
  (replace-regexp-in-string ".*?{\\(.*\\)}.*" "\\1" matched-str))

(defun bbyac-expand-symbols-by-major-mode ()
  "Find and expand the bit into a symbol (method) for current major mode.

For e.g., when editing a .js file, a BIT like _subs_ is inputed,
and this command is called, than it should be able to complete
with _substring_ for the String objects."
  (interactive)
  (bbyac--general-expand #'bbyac--symbol-bbyac-extracter
                         #'bbyac--line-extracting-matcher
                         #'bbyac--buffer-filter-by-major-mode
                         #'bbyac--major-mode-match-rewriter))

(defun bbyac--buffer-filter-by-major-mode ()
  "Return the (buffer . tag) for current major mode completion.

See `bbyac--buffer-filter' for more info."
  (let* ((mode-name (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
         (mode-buffer (gethash major-mode bbyac--major-mode-buffer-hash))
         (bbyac-major-mode-match-file (expand-file-name (concat mode-name ".bbyac") bbyac--source-dir))
         (bbyac-major-mode-match-file-user (expand-file-name (concat mode-name ".bbyac") bbyac-user-config-dir)))
    (unless (buffer-live-p mode-buffer)
      (when (file-exists-p bbyac-major-mode-match-file-user)
        (setq bbyac-major-mode-match-file bbyac-major-mode-match-file-user))
      (when (file-exists-p bbyac-major-mode-match-file)
        (puthash major-mode (find-file-noselect bbyac-major-mode-match-file) bbyac--major-mode-buffer-hash)
        (setq mode-buffer (gethash major-mode bbyac--major-mode-buffer-hash))))
    (when (buffer-live-p mode-buffer)
      (list (cons mode-buffer 'visible)))))

(defun bbyac-expand-substring ()
  "Expand the bit into a partial line match.

This means expand to string from the beginning to the end of the
matched region."
  (interactive)
  (bbyac--general-expand #'bbyac--line-bbyac-extracter))

(defun bbyac-expand-lines ()
  "Expand the bit into a full line match.

This means expand to string from the beginning to the end of the
line where the match occured."
  (interactive)
  (bbyac--general-expand #'bbyac--line-bbyac-extracter
                         #'bbyac--line-extracting-matcher))

(defmacro bbyac--max-minibuffer-lines ()
  "Compute the max number of lines the minibuffer can display."
  `(if (floatp max-mini-window-height)
       (truncate (* (frame-height) max-mini-window-height))
     max-mini-window-height))

(defun bbyac--highlight-match-line (matches line max-line-num)
  "Highlight the light matching line.

This function is copy and modified from
`ecomplete-highlight-match-line'.  MATCHES and LINE are used by
that function.  MAX-LINE-NUM is the max number of lines."
  (let* ((max-lines (bbyac--max-minibuffer-lines))
         (max-lines-1 (- max-lines 2))
         (max-lines-2 (1- max-lines-1))
         (res
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
    (add-face-text-property 0 (length res) '(:height .8) t res)
    res))

(defun bbyac--display-matches (strlist)
  "Display STRLIST for the user to choose from its elements.

This func is copied and modified from `ecomplete-display-matches'."
  (let* ((matches (concat
                   (mapconcat 'identity (delete-dups strlist) "\n")
                   "\n"))
         (line 0)
         (max-line-num (when matches (- (length (split-string matches "\n")) 2)))
         (message-log-max nil)
         command highlight)
    (if (not matches)
        (progn
          (message "No bit matches")
          nil)
      (if (= max-line-num 0)
          (nth line (split-string matches "\n"))
        (setq highlight (bbyac--highlight-match-line matches line max-line-num))
        (while (not (memq (setq command (read-event highlight)) '(?  return ?\C-m)))
          (cond
           ((or (eq command ?\M-n)
                (eq command ?\C-n)
                (eq command ?n))
            (setq line (% (1+ line) (1+ max-line-num))))
           ((or (eq command ?\M-p)
                (eq command ?\C-p)
                (eq command ?p))
            (setq line (% (+ max-line-num line) (1+ max-line-num)))))
          (setq highlight (bbyac--highlight-match-line matches line max-line-num)))
        (when (or (eq command 'return)
                  (eq command ?\C-m))
          (nth line (split-string matches "\n")))))))

(defun bbyac--clean-up (list)
  "Clean up the LIST."
  (delete-dups
   (delete ""
           (if (and (boundp 'bbyac--the-bit)
                    (stringp bbyac--the-bit))
               (delete bbyac--the-bit list)
             list))))

(defvar bbyac-mode-map (make-sparse-keymap)
  "Bbyac mode map.")
(define-key bbyac-mode-map (kbd "M-g <return>") 'bbyac-expand-symbols)
(define-key bbyac-mode-map (kbd "M-g .") 'bbyac-expand-symbols-by-major-mode)
(define-key bbyac-mode-map (kbd "M-g <RET>") 'bbyac-expand-symbols)
(define-key bbyac-mode-map (kbd "M-s <return>") 'bbyac-expand-substring)
(define-key bbyac-mode-map (kbd "M-s <RET>") 'bbyac-expand-substring)
(define-key bbyac-mode-map (kbd "M-s l") 'bbyac-expand-lines)

;;;###autoload
(define-minor-mode bbyac-mode
  "Toggle the `bbyac-mode' minor mode."
  :lighter " BBC"
  :keymap bbyac-mode-map
  :group 'bbyac)

;;;###autoload
(define-globalized-minor-mode bbyac-global-mode
  bbyac-mode
  turn-on-bbyac-mode)

(defun turn-on-bbyac-mode ()
  "Turn on `bbyac-mode'."
  (interactive)
  (bbyac-mode 1))

(provide 'bbyac)

;;; bbyac.el ends here
