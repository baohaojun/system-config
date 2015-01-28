;;; bbyac.el --- Type a little Bit, and Bang! You Are Completed.

;; Copyright (C) 2015 Bao Haojun

;; Author: Bao Haojun <baohaojun@gmail.com>
;; Maintainer: Bao Haojun <baohaojun@gmail.com>
;; Created: 28th January 2015
;; Package-Requires: ((browse-kill-ring "1.3"))
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

;; For more information see the readme at:
;; https://github.com/baohaojun/bbyac

;;; Code:

(require 'ecomplete)
(require 'cl)
(require 'thingatpt)
(require 'browse-kill-ring)

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

(defvar bbyac--start
  nil
  "The start of the bit that is to be replaced with the
expansion.

This var should be set when you are extracting the bit, and
it is used when the expansion of the bit is to be inserted.")

(defvar bbyac--end
  nil
  "The end of the bit that is to be replaced with the
expansion.

See `bbyac--start'.")

(defvar bbyac--the-bit
  nil
  "The bit that is extracted with a bit extracter.")

(defvar bbyac--contains-upcase
  nil
  "Whether the bit contains upcase char.")

(defun bbyac--contains-upcase-p (str)
  (let ((case-fold-search nil))
    (string-match-p "[[:upper:]]" str)))

(defun bbyac--clear-vars ()
  (setq bbyac--start nil
        bbyac--end nil
        bbyac--the-bit nil
        bbyac--contains-upcase nil))

(defun bbyac--interleave (l1 l2)
  (let (result)
    (while (and l1 l2)
      (setq result (cons (car l1) result) ; consing like this makes strlist in bad order
            result (cons (car l2) result)
            l1 (cdr l1)
            l2 (cdr l2)))
    (append (nreverse result) l1 l2)))

(defun bbyac--difference (l1 l2)
  (delete-if (lambda (e) (member e l2)) l1))

(defun bbyac--regexp-quote (char)
  "Regexp-quote char smartly.
If bbyac--the-bit contains upcase chars, then
case-fold-search will be turned on, in that case, we want the
upcase char to match exactly, but the downcase char to match
either an upcase or a downcase.

For e.g., given HelloWorld and helloWorld, we want Hw to match
the former, but not the later. If the w in Hw was to made to
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
       "\\_>"))))

(defun bbyac--line-bbyac-extracter ()
  "Extract a bit for line-completing.

If the region is active, the bit is what is in the region.

If the region is not active, the bit should be found by
checking white spaces (see the code). For e.g., if we are looking
back at (with the last * denoting where the point is):

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
  (setq bbyac--the-bit (buffer-substring-no-properties bbyac--start bbyac--end)
        bbyac--contains-upcase (bbyac--contains-upcase-p bbyac--the-bit))
  (when bbyac--the-bit
    (let ((the-regexp
           (mapconcat
            #'bbyac--regexp-quote
            (string-to-list bbyac--the-bit)
            ".*?")))
      ;; performance consideration: if syntax of bit's first char
      ;; is word, then it must match word boundary
      (when (string-match-p "^\\w" bbyac--the-bit)
        (setq the-regexp (concat "\\b" the-regexp)))
      ;; if bit's last char is word syntax, should extend the
      ;; completion to word boundaries
      (when (string-match-p "\\w$" bbyac--the-bit)
        (setq the-regexp (concat the-regexp "\\w*?\\b")))
      ;; extend the regexp rewrite: use Ctrl-e to mean $ and Ctrl-a to mean ^
      (setq the-regexp (replace-regexp-in-string "$" "$" the-regexp)
            the-regexp (replace-regexp-in-string "^" "^" the-regexp))
      the-regexp)))

(defmacro bbyac--make-matcher (matcher-name doc extract-match move-along)
  "Make a new matcher named as MATCHER-NAME with EXTRACT-MATCH and MOVE-ALONG.

The function MATCHER-NAME thus created will take 3 arguments, RE,
BUFFER, and TAG. When called, it will search the BUFFER for RE,
return all the matching substrings in an order dependent on
TAG. See `bbyac--matcher'.

EXTRACT-MATCH and MOVE-ALONG are 2 lisp sexp, you can use the
variables mb and me in these sexps. Result of EXTRACT-MATCH
should be a string; MOVE-ALONG is only used for its side-effects."
  (declare (doc-string 2))
  `(defun ,matcher-name (re buffer tag)
     (let ((strlist-before nil)
           (strlist-after nil)
           (strlist nil)
           (old-point (point)))
       (with-current-buffer buffer
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward re nil t)
             (let ((mb (match-beginning 0))
                   (me (match-end 0)))
               (let ((substr ,extract-match))
                 (cond
                  ((and (eq tag 'current)
                       (< mb bbyac--start)
                       (> me bbyac--start)))
                  (t
                   (if (and (< (point) old-point) (eq tag 'current))
                       ;; substr closer to the old-point is at the head of strlist-before, in good order
                       (setq strlist-before (cons substr strlist-before))
                     ;; substr further to the old-point is at the head of strlist-after, in bad order
                     (setq strlist-after (cons substr strlist-after))))))
               ,move-along))
           (setq strlist (bbyac--interleave strlist-before (nreverse strlist-after)))
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
 (buffer-substring-no-properties (line-beginning-position) (line-end-position))
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
    (delete-if
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
  "Given a regexp RE, run MATCHER over all the buffers returned
  BUFFER-FILTER.

Return the list of strings thus matched.

See `bbyac--matcher' and `bbyac--buffer-filter'."
  (bbyac--clean-up
   (let ((matched-buried nil)
         (matched-any nil)
         result)
     (mapcan (lambda (tagged-buffer)
               (let ((buffer (car tagged-buffer))
                     (tag (cdr tagged-buffer))
                     strlist)
                 (if matched-buried
                     nil
                   (prog1
                       (unless (and matched-any (eq tag 'buried))
                         (setq strlist (funcall matcher re buffer tag)))
                     (when strlist
                       (setq matched-any t))
                     (when (and strlist (eq tag 'buried))
                       (setq matched-buried t))))))
             (funcall buffer-filter)))))

(defun bbyac--string-multiline-p (str)
  "Return t if STR is too long or span multilines"
  (or (> (length str) bbyac-max-chars)
      (string-match-p "\n" str)))

(defun bbyac--general-expand (extracter &optional matcher buffer-filter)
  "General function to expand a bit using the functional arguments.

See `bbyac--symbol-bbyac-extracter' for EXTRACTER. See
`bbyac--matcher' for MATCHER. See `bbyac--buffer-filter' for BUFFER-FILTER.

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
           (cl-notany #'bbyac--string-multiline-p matches))
          (progn
            (setq match (bbyac--display-matches matches))
            (when (and bbyac--start bbyac--end)
              (delete-region bbyac--start bbyac--end))
            (insert match))
        (when (and bbyac--start bbyac--end)
          (delete-region bbyac--start bbyac--end))
        (let ((kill-ring matches)
              (kill-ring-yank-pointer matches))
          (browse-kill-ring))))))

(defun bbyac-expand-symbols ()
  "Find and expand the bit into a symbol.

The bit itself is constructed by symbol constituting
characters before the point."
  (interactive)
  (bbyac--general-expand #'bbyac--symbol-bbyac-extracter))

(defun bbyac-expand-partial-lines ()
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

(defun bbyac-expand-sexp ()
  "Expand the bit into an S-expression.

This means expand to string from the beginning to the end of the
S-expression enclosing the matched region."
  (interactive)
  (bbyac--general-expand #'bbyac--line-bbyac-extracter
                            #'bbyac--sexp-extracting-matcher))

(defun bbyac-expand-paragraph ()
  "Expand the bit into a whole paragraph."
    (interactive)
    (bbyac--general-expand #'bbyac--line-bbyac-extracter
                              #'bbyac--paragraph-extracting-matcher))

(defmacro bbyac--max-minibuffer-lines ()
  `(if (floatp max-mini-window-height)
       (truncate (* (frame-height) max-mini-window-height))
     max-mini-window-height))

(defun bbyac--highlight-match-line (matches line max-line-num)
  "This function is copy and modified from ecomplete-highlight-match-line"
  (let* ((max-lines (bbyac--max-minibuffer-lines))
         (max-lines-1 (- max-lines 2))
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

(defun bbyac--display-matches (strlist)
  "Display a list of matches, allow the user to choose from them.

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
  (delete-dups
   (delete ""
           (if (and (boundp 'bbyac--the-bit)
                    (stringp bbyac--the-bit))
               (delete bbyac--the-bit list)
             list))))

(defvar bbyac-mode-map (make-sparse-keymap)
  "Bbyac mode map.")
(define-key bbyac-mode-map (kbd "M-g <return>") 'bbyac-expand-symbols)
(define-key bbyac-mode-map (kbd "M-g <RET>") 'bbyac-expand-symbols)
(define-key bbyac-mode-map (kbd "M-s <return>") 'bbyac-expand-partial-lines)
(define-key bbyac-mode-map (kbd "M-s <RET>") 'bbyac-expand-partial-lines)
(define-key bbyac-mode-map (kbd "M-g x") 'bbyac-expand-partial-lines)
(define-key bbyac-mode-map (kbd "M-s l") 'bbyac-expand-lines)
(define-key bbyac-mode-map (kbd "M-s s") 'bbyac-expand-sexp)
(define-key bbyac-mode-map (kbd "M-s p") 'bbyac-expand-paragraph)

(define-minor-mode bbyac-mode
  "Toggle the `bbyac-mode' minor mode."
  :lighter " BBC"
  :keymap bbyac-mode-map
  :group 'bbyac)

(define-globalized-minor-mode bbyac-global-mode
  bbyac-mode
  turn-on-bbyac-mode)

(defun turn-on-bbyac-mode ()
  "Turn on `bbyac-mode'."
  (interactive)
  (bbyac-mode 1))

(provide 'bbyac)

;;; bbyac.el ends here
