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

(defun skeleton--contains-upcase-p (str)
  (let ((case-fold-search nil))
    (string-match-p "[[:upper:]]" the-skeleton)))

(defun skeleton--expand-symbols ()
  "Find and expand the skeleton into a symbol.

The skeleton itself is constructed by symbol constituting
characters before the point."
  (interactive)
  (when (looking-back "\\w\\|_" 1)
    (let* ((skeleton-end (point))
           (skeleton-start (or (save-excursion
                        (search-backward-regexp "\\_<" (line-beginning-position) t)) skeleton-end))
           (the-skeleton (buffer-substring-no-properties skeleton-start skeleton-end))
           (the-regexp (mapconcat (lambda (x) (regexp-quote (string x))) (string-to-list the-skeleton) ".*?"))
           (case-fold-search (not (skeleton--contains-upcase-p the-skeleton)))
           (match (skeleton--display-matches (skeleton--get-symbol-matches the-regexp))))
      (when match
        (delete-region skeleton-start skeleton-end)
        (insert match)))))

(defun skeleton--expand-partial-lines ()
  "Find and expand the skeleton into an arbitrary string.

If the region is active, the skeleton is what is in the region.

If the region is not active, the skeleton should be found by
checking white spaces (see the code). For e.g., if we are looking
back at (with the last * denoting where the point is):

    [{)}& aonehua naoehu[)+{ *

we will get the skeleton:

    \"naoehu[)+{ \""
  (interactive)
  (let (the-skeleton the-regexp skeleton-start skeleton-end)
    (if mark-active
        (setq skeleton-start (region-beginning)
              skeleton-end (region-end))
      (save-excursion
        (let* ((back-limit (line-beginning-position))
               (cp (point)))
          (while (and (looking-back "\\s ") (< back-limit (point)))
            (backward-char))
          (while (and (not (looking-back "\\s ")) (< back-limit (point)))
            (backward-char))
          (setq skeleton-start (point)
                skeleton-end cp))))
    (setq the-skeleton (buffer-substring-no-properties skeleton-start skeleton-end))
    (setq the-regexp (mapconcat (lambda (x) (regexp-quote (string x))) (string-to-list the-skeleton) ".*?"))

    ;; performance consideration
    (when (string-match-p "^\\w" the-regexp)
      (setq the-regexp (concat "\\b" the-regexp)))

    (let ((case-fold-search (not (skeleton--contains-upcase-p the-skeleton)))
          (match (skeleton--display-matches (skeleton--get-line-matches the-regexp))))

      (when match
        (delete-region skeleton-start skeleton-end)
        (insert match)))))

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
                (eq command ?\C-n))
            (setq line (% (1+ line) (1+ max-line-num))))
           ((or (eq command ?\M-p)
                (eq command ?\C-p))
            (setq line (% (+ max-line-num line) (1+ max-line-num)))))
          (setq highlight (skeleton--highlight-match-line matches line max-line-num)))
        (when (eq command 'return)
          (nth line (split-string matches "\n")))))))

(defun skeleton--clean-up (list)
  (delete-dups
   (delete ""
           (if (and (boundp 'the-skeleton)
                    (stringp the-skeleton))
               (delete the-skeleton list)
             list))))

(defun skeleton--get-line-matches (re &optional no-recur)
  (nreverse
   (skeleton--clean-up ; to be paranoid
    (skeleton--get-matches-internal re #'skeleton--line-matching-loop-func no-recur))))

(defun skeleton--line-matching-loop-func (re)
  "Search the buffer to collect all strings that matches `re'.

It is an internal function, and will operate on the
strlist-before and strlist-after variables in the env."
  (while (re-search-forward re nil t)
    (let ((mb (match-beginning 0))
          (me (match-end 0)))
      (goto-char mb)

      ;; mb can not be in the middle of a word, if so, it is
      ;; considered a bad match.
      (unless (or (and (looking-at "\\w")
                       (looking-back "\\w")
                       ;; However, 你好*ma should provide a match as "ma", not "你好ma"
                       (not (looking-at "\\b")))
                  (and (boundp 'skeleton-start) ; the found string should not overlap with our skeleton
                       (boundp 'skeleton-end)
                       (= me skeleton-end)))
        ;; me should also not be in the middle of a word, if so, we
        ;; should find the end of the word.
        (save-excursion
          (goto-char me)
          (when (and (looking-at "\\w")
                     (looking-back "\\w"))
            (re-search-forward "\\b" nil t)
            (setq me (point))))
        (let ((substr (buffer-substring-no-properties mb me)))
          (if (< (point) old-point)
              ;; substr closer to the old-point is at the head of strlist-before, in good order
              (setq strlist-before (cons substr strlist-before))
            ;; substr further to the old-point is at the head of strlist-after, bad
            (setq strlist-after (cons substr strlist-after)))))
      (goto-char (1+ mb)))))

(defun skeleton--get-matches-internal (re loop-func &optional no-recur)
  "Get all the possible completions for the RE, using LOOP-FUNC
to collect matching completions from 1 buffer.

The list of matching completions are desirable to be
sorted (interleaved) according to their distance to the
point. First one entry coming *before* the point, then one entry
coming *after* point, until either list runs out.

The LOOP-FUNC is expected to update the strlist-before and
strlist-after, and when it finishes, strlist-before should be in
desirable order, but strlist-after should be in reversed order
according to the above standard.

But for ease of coding, this func returns a list of strings which
are in the reversed order, so one must remember to call nreverse
with the result before presenting it to the user."

  (let ((strlist-before nil)
        (strlist-after nil)
        (strlist nil)
        (old-point (point)))
    (save-excursion
      (goto-char (point-min))
      (funcall loop-func re)
      (setq strlist-before (skeleton--clean-up strlist-before) ; strlist-before is good
            strlist-after (skeleton--clean-up (nreverse strlist-after))) ; strlist-after is made also good with this nreverse
      (while (and strlist-before strlist-after)
        (setq strlist (cons (car strlist-before) strlist) ; consing like this makes strlist in bad order
              strlist (cons (car strlist-after) strlist)
              strlist-before (cdr strlist-before)
              strlist-after (cdr strlist-after)))

      ;; need 2 nreverse to make the 2 good strlist-* aligned with the
      ;; bad strlist; strlist must be the last of `append'
      (setq strlist (append (nreverse strlist-after) (nreverse strlist-before) strlist))

      (if no-recur
          strlist

        (or

         ;;get matches from other visible buffers
         (let* ((buf-old (current-buffer)))
           (save-excursion
             (mapcar (lambda (buf)
                       (with-current-buffer buf
                         ;; recursive call, and keep strlist still in
                         ;; the bad order by making it the last of `append'
                         (setq strlist (append (skeleton--get-matches-internal re loop-func t) strlist))))
                     (delete buf-old
                             (mapcar (lambda (w)
                                       (window-buffer w))
                                     (window-list))))
             strlist))

         ;;get matches from all buffers if nothing found so far
         (let* ((buf-old (current-buffer)))
           (save-excursion
             (mapcar (lambda (buf)
                       (with-current-buffer buf ;;next let's recursive call
                         (unless strlist ; exit as soon as something is found
                           (setq strlist (append (skeleton--get-matches-internal re loop-func t) strlist)))))
                     (delete buf-old
                             (buffer-list)))
             strlist)))))))

(defun skeleton--symbol-matching-loop-func (re)
  "Search the buffer for all symbols matching `re'.

It is an internal function, and will operate on the
strlist-before and strlist-after variables in the env."
  (while (not (eobp))
    (if (setq endpt (re-search-forward "\\(\\_<.*?\\_>\\)" nil t))
        (let ((substr (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
          (when (string-match re substr)
            (if (< (point) old-point)
                (setq strlist-before (cons substr strlist-before))
              (setq strlist-after (cons substr strlist-after)))))
      (goto-char (point-max)))))

(defun skeleton--get-symbol-matches (re &optional no-recur)
  (nreverse
   (skeleton--clean-up ; to be paranoid
    (skeleton--get-matches-internal re #'skeleton--symbol-matching-loop-func no-recur))))

(defgroup skeleton-complete nil
  "Dynamically expand expressions by provided skeleton (flex matching)."
  :group 'abbrev)

(defvar skeleton-complete-mode-map (make-sparse-keymap)
  "skeleton-complete mode map.")
(define-key skeleton-complete-mode-map (kbd "M-g <return>") 'skeleton--expand-symbols)
(define-key skeleton-complete-mode-map (kbd "M-s <return>") 'skeleton--expand-partial-lines)
(define-key skeleton-complete-mode-map (kbd "M-g x") 'skeleton--expand-partial-lines)

(define-minor-mode skeleton-complete-mode
  "Toggle the `skeleton-complete-mode' minor mode."
  :lighter " SkelC"
  :keymap skeleton-complete-mode-map
  :group 'skeleton-complete)

(define-globalized-minor-mode skeleton-complete-global-mode
  skeleton-complete-mode
  turn-on-skeleton-complete-mode)

(defun turn-on-skeleton-complete-mode ()
  "Turn on `skeleton-complete-mode'"
  (interactive)
  (skeleton-complete-mode 1))

(provide 'skeleton-complete)

;;; skeleton-complete.el ends here
