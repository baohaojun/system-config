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

(defun skeleton--expand-symbols ()
  "Expand the skeleton before point into a symbol. The skeleton
itself is constructed by symbol constituting characters."
  (interactive)
  (when (looking-back "\\w\\|_" 1)
    (let* ((end (point))
           (start (save-excursion
                    (search-backward-regexp "\\_<")))
           (the-skeleton (when start (buffer-substring-no-properties start end)))
           (the-regexp (mapconcat (lambda (x) (regexp-quote (string x))) (string-to-list the-skeleton) ".*?"))
           (match (when (and the-skeleton
                             (not (zerop (length the-skeleton))))
                    (let ((case-fold-search
                           (let ((case-fold-search nil))
                             (not (string-match-p "[A-Z]" the-skeleton)))))
                      (skeleton--expand-bounded-matching the-regexp)))))
      (when match
        (delete-region start end)
        (insert match)))))

(defun skeleton--expand-partial-lines ()
  "Simplify writing the regexp. If we are looking back at, for
e.g., \"sthe='\", the regexp for completion should be
\"s.*?t.*?h.*?e.*?=.*?'\". That is, fill in a \".*?\" pattern
between each 2 characters.

If the region is active, the original pattern is built from what
is in the region.

If the region is not active, the original pattern should be found
by checking white spaces (see the code). For e.g., if we are
looking back at (with the last * denoting where the point is):

    [{)}& aonehua naoehu[)+{ *

we will get the skeleton:

    \"naoehu[)+{ \""
  (interactive)
  (let (the-skeleton the-regexp search-start search-end)
    (if mark-active
        (setq search-start (region-beginning)
              search-end (region-end))
      (save-excursion
        (let* ((back-limit (line-beginning-position))
               (cp (point)))
          (while (and (looking-back "\\s ") (< back-limit (point)))
            (backward-char))
          (while (and (not (looking-back "\\s ")) (< back-limit (point)))
            (backward-char))
          (setq search-start (point)
                search-end cp))))
    (setq the-skeleton (buffer-substring-no-properties search-start search-end))
    (setq the-regexp (mapconcat (lambda (x) (regexp-quote (string x))) (string-to-list the-skeleton) ".*?"))
    (let ((case-fold-search
           (let ((case-fold-search nil))
             (not (string-match-p "[A-Z]" the-skeleton)))))
      (skeleton--expand-with-regexp the-regexp search-start search-end))))

(defun skeleton--expand-with-regexp (regexp &optional rb re)
  "Display the possible abbrevs for the regexp."
  (interactive)
  (let ((match (unless (string= "" regexp)
                 (skeleton--expand-matching regexp))))
    (when match
      (if (region-active-p)
          (delete-region (region-beginning) (region-end))
        (when (and rb re) (delete-region rb re)))
      (insert match))))

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

(defun skeleton--expand-bounded-matching (re)
  (skeleton--display-matches (skeleton--delete-the-skeleton (nreverse (skeleton--get-bounded-matches re)))))

(defun skeleton--expand-matching (regexp)
  (skeleton--display-matches (delete "" (nreverse (skeleton--get-matches regexp)))))

(defun skeleton--display-matches (strlist)
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

(defun skeleton--delete-the-skeleton (list)
  (if (and (boundp 'the-skeleton)
           (stringp the-skeleton))
      (delete the-skeleton list)
    list))

(defun skeleton--get-matches (re &optional no-recur)
  (skeleton--delete-the-skeleton (skeleton--get-matches-internal re no-recur)))

(defun skeleton--get-matches-internal (re &optional no-recur)
  "Display the possible completions for the regexp."
  (let ((strlist-before nil)
        (strlist-after nil)
        (strlist nil)
        (current-pos (point)))
    (save-excursion
      (goto-char (point-min))
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
                      (setq strlist (append (skeleton--get-matches re t) strlist))))
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
                          (setq strlist (append (skeleton--get-matches re t) strlist)))))
                    (delete buf-old
                            (buffer-list)))
            strlist))
        ))))

(defun skeleton--get-bounded-matches (re &optional no-recur)
  "Get all the words that contains every character of the
SKELETON from the current buffer The words are ordered such that
words closer to the (point) appear first"
  (let ((strlist-before nil)
        (strlist-after nil)
        (strlist nil)
        (current-pos (point)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (setq endpt (re-search-forward "\\(\\_<.*?\\_>\\)" nil t))
            (let ((substr (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
              (when (string-match re substr)
                (if (< (point) current-pos)
                    (setq strlist-before (cons substr strlist-before))
                  (setq strlist-after (cons substr strlist-after)))))
          (goto-char (point-max)))))
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
                      (setq strlist (append (skeleton--get-bounded-matches re t) strlist))))
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
                          (setq strlist (delete-dups (append (skeleton--get-bounded-matches skeleton t) strlist))))))
                    (delete buf-old (buffer-list)))
            strlist))))))

(defgroup skeleton-complete nil
  "Dynamically expand expressions by provided skeleton (flex matching)."
  :group 'abbrev)

(defvar skeleton-complete-mode-map (make-sparse-keymap)
  "skeleton-complete mode map.")
(define-key skeleton-complete-mode-map (kbd "M-g <return>") 'skeleton--expand-symbols)
(define-key skeleton-complete-mode-map (kbd "M-s <return>")'skeleton--expand-partial-lines)
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
