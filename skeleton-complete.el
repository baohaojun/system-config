;;; skeleton-complete.el --- Dynamically expand expressions by provided skeleton (flex matching)

;; Copyright (C) 2013 Bao Haojun

;; Author: Bao Haojun <baohaojun@gmail.com>
;; Maintainer: Bao Haojun <baohaojun@gmail.com>
;; Created: 15th April 2013
;; Package-Requires: ((browse-kill-ring "1.3"))
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
(require 'cl)
(require 'thingatpt)
(require 'browse-kill-ring)

(defgroup skeleton-complete nil
  "Dynamically expand expressions by provided skeleton (flex matching)."
  :group 'abbrev)

(defcustom skeleton--max-chars 80
  "Maximum chars in any single match to use ecomplete.

If exceeded, will use other schemes (such as browse-kill-ring) to
select from the matches."
  :type 'integer
  :group 'skeleton-complete)

(defvar skeleton--start
  nil
  "The start of the skeleton that is to be replaced with the
expansion.

This var should be set when you are extracting the skeleton, and
it is used when the expansion of the skeleton is to be inserted.")

(defvar skeleton--end
  nil
  "The end of the skeleton that is to be replaced with the
expansion.

See `skeleton--start'.")

(defvar skeleton--the-skeleton
  nil
  "The skeleton that is extracted with a skeleton extracter.")

(defvar skeleton--contains-upcase
  nil
  "Whether the skeleton contains upcase char.")

(defun skeleton--contains-upcase-p (str)
  (let ((case-fold-search nil))
    (string-match-p "[[:upper:]]" str)))

(defun skeleton--clear-vars ()
  (setq skeleton--start nil
        skeleton--end nil
        skeleton--the-skeleton nil
        skeleton--contains-upcase nil))

(defun skeleton--interleave (l1 l2)
  (let (result)
    (while (and l1 l2)
      (setq result (cons (car l1) result) ; consing like this makes strlist in bad order
            result (cons (car l2) result)
            l1 (cdr l1)
            l2 (cdr l2)))
    (append (nreverse result) l1 l2)))

(defun skeleton--difference (l1 l2)
  (delete-if (lambda (e) (member e l2)) l1))

(defun skeleton--regexp-quote (char)
  "Regexp-quote char smartly.
If skeleton--the-skeleton contains upcase chars, then
case-fold-search will be turned on, in that case, we want the
upcase char to match exactly, but the downcase char to match
either an upcase or a downcase.

For e.g., given HelloWorld and helloWorld, we want Hw to match
the former, but not the later. If the w in Hw was to made to
match w only, then none will match, which sometimes is a
surprise."
  (if (and skeleton--contains-upcase
           (>= char ?a)
           (<= char ?z))
      (format "[%c%c]" char (upcase char))
    (regexp-quote (string char))))

(defun skeleton--symbol-skeleton-extracter ()
  "Extract a skeleton for symbol-completing.

Return a regexp rewritten from the skeleton.

In addition, extracters can also set the variables
`skeleton--the-skeleton', `skeleton--start' and `skeleton--end'."
  (if mark-active
      (setq skeleton--start (region-beginning)
            skeleton--end (region-end))
    (when (looking-back "\\w\\|_" 1)
      (setq skeleton--end (point)
            skeleton--start (or (save-excursion
                                  (search-backward-regexp "\\_<" (line-beginning-position) t))
                                skeleton--end))))
  (setq skeleton--the-skeleton (buffer-substring-no-properties skeleton--start skeleton--end)
        skeleton--contains-upcase (skeleton--contains-upcase-p skeleton--the-skeleton))
  (unless (string= "" skeleton--the-skeleton)
    (let ((symbol-chars "\\(?:\\sw\\|\\s_\\)*?"))
      (concat
       "\\_<"
       symbol-chars
       (mapconcat
        #'skeleton--regexp-quote
        (string-to-list skeleton--the-skeleton)
        symbol-chars)
       symbol-chars
       "\\_>"))))

(defun skeleton--line-skeleton-extracter ()
  "Extract a skeleton for line-completing.

If the region is active, the skeleton is what is in the region.

If the region is not active, the skeleton should be found by
checking white spaces (see the code). For e.g., if we are looking
back at (with the last * denoting where the point is):

    [{)}& aonehua naoehu[)+{ *

we will get the skeleton:

    \"naoehu[)+{ \"

See also `skeleton--symbol-skeleton-extracter'."
  (if mark-active
      (setq skeleton--start (region-beginning)
            skeleton--end (region-end))
      (save-excursion
        (let* ((back-limit (line-beginning-position))
               (cp (point)))
          (while (and (looking-back "\\s ") (< back-limit (point)))
            (backward-char))
          (while (and (not (looking-back "\\s ")) (< back-limit (point)))
            (backward-char))
          (setq skeleton--start (point)
                skeleton--end cp))))
  (setq skeleton--the-skeleton (buffer-substring-no-properties skeleton--start skeleton--end)
        skeleton--contains-upcase (skeleton--contains-upcase-p skeleton--the-skeleton))
  (when skeleton--the-skeleton
    (let ((the-regexp
           (mapconcat
            #'skeleton--regexp-quote
            (string-to-list skeleton--the-skeleton)
            ".*?")))
      ;; performance consideration: if syntax of skeleton's first char
      ;; is word, then it must match word boundary
      (when (string-match-p "^\\w" skeleton--the-skeleton)
        (setq the-regexp (concat "\\b" the-regexp)))
      ;; if skeleton's last char is word syntax, should extend the
      ;; completion to word boundaries
      (when (string-match-p "\\w$" skeleton--the-skeleton)
        (setq the-regexp (concat the-regexp "\\w*?\\b")))
      ;; extend the regexp rewrite: use Ctrl-e to mean $ and Ctrl-a to mean ^
      (setq the-regexp (replace-regexp-in-string "$" "$" the-regexp)
            the-regexp (replace-regexp-in-string "^" "^" the-regexp))
      the-regexp)))

(defmacro skeleton--make-matcher (matcher-name doc extract-match move-along)
  "Make a new matcher named as MATCHER-NAME with EXTRACT-MATCH and MOVE-ALONG.

The function MATCHER-NAME thus created will take 3 arguments, RE,
BUFFER, and TAG. When called, it will search the BUFFER for RE,
return all the matching substrings in an order dependent on
TAG. See `skeleton--matcher'.

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
                       (< mb skeleton--start)
                       (> me skeleton--start)))
                  (t
                   (if (and (< (point) old-point) (eq tag 'current))
                       ;; substr closer to the old-point is at the head of strlist-before, in good order
                       (setq strlist-before (cons substr strlist-before))
                     ;; substr further to the old-point is at the head of strlist-after, in bad order
                     (setq strlist-after (cons substr strlist-after))))))
               ,move-along))
           (setq strlist (skeleton--interleave strlist-before (nreverse strlist-after)))
           ;; This expansion is useless, it's the same as the skeleton
           ;; (in fact, extracted from the same place, and thus at the
           ;; car of strlist. We must remove it here, or else the
           ;; buried buffers will never get matched).
           (if (and (eq tag 'current)
                    (stringp (car strlist))
                    (string= (car strlist) skeleton--the-skeleton))
               (cdr strlist)
             strlist))))))

(skeleton--make-matcher
 skeleton--matcher
 "Search the buffer to collect a list of all strings matching `re'.

If TAG is 'current, the returned list is sorted (interleaved)
roughly according to their distance to where the point is. First
one entry coming *before* the point, then one entry coming
*after* point, until either list runs out.

Or else the returned list of strings is in the order they appear in the buffer."
 (buffer-substring-no-properties mb me)
 (goto-char (1+ mb)))

(skeleton--make-matcher
 skeleton--line-extracting-matcher
 "Search the buffer to collect a list of all lines matching `re'.

See `skeleton--matcher'."
 (buffer-substring-no-properties (line-beginning-position) (line-end-position))
 (end-of-line))

(skeleton--make-matcher
 skeleton--sexp-extracting-matcher
 "Search the buffer to collect a list of all balanced expressions matching `re'.

See `skeleton--matcher'."
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

(skeleton--make-matcher
 skeleton--paragraph-extracting-matcher
 "Search the buffer to collect a list of all paragraphs matching `re'.

See `skeleton--matcher'."
 (buffer-substring-no-properties
  (save-excursion
    (start-of-paragraph-text)
    (point))
  (save-excursion
    (end-of-paragraph-text)
    (point)))
 (end-of-paragraph-text))

(defun skeleton--buffer-filter ()
  "Return an alist of buffers to be matched againt the skeleton for completions.

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
        (buried-buffers (skeleton--difference (buffer-list) (cons current-buffer visible-buffers))))
    (nconc
     (list (cons current-buffer 'current))
     (mapcar
      (lambda (b)
        (cons b 'visible))
      visible-buffers)
     (mapcar
      (lambda (b)
        (cons b 'buried))
      buried-buffers))))

(defun skeleton--get-matches (re matcher buffer-filter)
  "Given a regexp RE, run MATCHER over all the buffers returned
  BUFFER-FILTER.

Return the list of strings thus matched.

See `skeleton--matcher' and `skeleton--buffer-filter'."
  (skeleton--clean-up
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

(defun skeleton--string-multiline-p (str)
  "Return t if STR is too long or span multilines"
  (or (> (length str) skeleton--max-chars)
      (string-match-p "\n" str)))

(defun skeleton--general-expand (extracter &optional matcher buffer-filter)
  "General function to expand a skeleton using the functional arguments.

See `skeleton--symbol-skeleton-extracter' for EXTRACTER. See
`skeleton--matcher' for MATCHER. See `skeleton--buffer-filter' for BUFFER-FILTER.

If wanted, user can extend skeleton-complete with their own
EXTRACTER, MATCHER and BUFFER-FILTER."
  (skeleton--clear-vars)
  (setq matcher (or matcher #'skeleton--matcher)
        buffer-filter (or buffer-filter #'skeleton--buffer-filter))
  (let* ((the-regexp (funcall extracter))
         (case-fold-search (not skeleton--contains-upcase))
         matches
         match)
    (when (and the-regexp
               (setq matches (skeleton--get-matches the-regexp matcher buffer-filter)))
      (if (or (minibufferp)
           (cl-notany #'skeleton--string-multiline-p matches))
          (progn
            (setq match (skeleton--display-matches matches))
            (when (and skeleton--start skeleton--end)
              (delete-region skeleton--start skeleton--end))
            (insert match))
        (when (and skeleton--start skeleton--end)
          (delete-region skeleton--start skeleton--end))
        (let ((kill-ring matches)
              (kill-ring-yank-pointer matches))
          (browse-kill-ring))))))

(defun skeleton-expand-symbols ()
  "Find and expand the skeleton into a symbol.

The skeleton itself is constructed by symbol constituting
characters before the point."
  (interactive)
  (skeleton--general-expand #'skeleton--symbol-skeleton-extracter))

(defun skeleton-expand-partial-lines ()
  "Expand the skeleton into a partial line match.

This means expand to string from the beginning to the end of the
matched region."
  (interactive)
  (skeleton--general-expand #'skeleton--line-skeleton-extracter))

(defun skeleton-expand-lines ()
  "Expand the skeleton into a full line match.

This means expand to string from the beginning to the end of the
line where the match occured."
  (interactive)
  (skeleton--general-expand #'skeleton--line-skeleton-extracter
                            #'skeleton--line-extracting-matcher))

(defun skeleton-expand-sexp ()
  "Expand the skeleton into an S-expression.

This means expand to string from the beginning to the end of the
S-expression enclosing the matched region."
  (interactive)
  (skeleton--general-expand #'skeleton--line-skeleton-extracter
                            #'skeleton--sexp-extracting-matcher))

(defun skeleton-expand-paragraph ()
  "Expand the skeleton into a whole paragraph."
    (interactive)
    (skeleton--general-expand #'skeleton--line-skeleton-extracter
                              #'skeleton--paragraph-extracting-matcher))

(defmacro skeleton--max-minibuffer-lines ()
  `(if (floatp max-mini-window-height)
       (truncate (* (frame-height) max-mini-window-height))
     max-mini-window-height))

(defun skeleton--highlight-match-line (matches line max-line-num)
  "This function is copy and modified from ecomplete-highlight-match-line"
  (let* ((max-lines (skeleton--max-minibuffer-lines))
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

(defun skeleton--display-matches (strlist)
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
          (message "No skeleton matches")
          nil)
      (if (= max-line-num 0)
          (nth line (split-string matches "\n"))
        (setq highlight (skeleton--highlight-match-line matches line max-line-num))
        (while (not (memq (setq command (read-event highlight)) '(? return)))
          (cond
           ((or (eq command ?\M-n)
                (eq command ?\C-n)
                (eq command ?n))
            (setq line (% (1+ line) (1+ max-line-num))))
           ((or (eq command ?\M-p)
                (eq command ?\C-p)
                (eq command ?p))
            (setq line (% (+ max-line-num line) (1+ max-line-num)))))
          (setq highlight (skeleton--highlight-match-line matches line max-line-num)))
        (when (eq command 'return)
          (nth line (split-string matches "\n")))))))

(defun skeleton--clean-up (list)
  (delete-dups
   (delete ""
           (if (and (boundp 'skeleton--the-skeleton)
                    (stringp skeleton--the-skeleton))
               (delete skeleton--the-skeleton list)
             list))))

(defvar skeleton-complete-mode-map (make-sparse-keymap)
  "Skeleton-complete mode map.")
(define-key skeleton-complete-mode-map (kbd "M-g <return>") 'skeleton-expand-symbols)
(define-key skeleton-complete-mode-map (kbd "M-s <return>") 'skeleton-expand-partial-lines)
(define-key skeleton-complete-mode-map (kbd "M-g x") 'skeleton-expand-partial-lines)
(define-key skeleton-complete-mode-map (kbd "M-s l") 'skeleton-expand-lines)
(define-key skeleton-complete-mode-map (kbd "M-s s") 'skeleton-expand-sexp)
(define-key skeleton-complete-mode-map (kbd "M-s p") 'skeleton-expand-paragraph)

(define-minor-mode skeleton-complete-mode
  "Toggle the `skeleton-complete-mode' minor mode."
  :lighter " SkelC"
  :keymap skeleton-complete-mode-map
  :group 'skeleton-complete)

(define-globalized-minor-mode skeleton-complete-global-mode
  skeleton-complete-mode
  turn-on-skeleton-complete-mode)

(defun turn-on-skeleton-complete-mode ()
  "Turn on `skeleton-complete-mode'."
  (interactive)
  (skeleton-complete-mode 1))

(provide 'skeleton-complete)

;;; skeleton-complete.el ends here
