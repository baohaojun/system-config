;;; list-unicode-display.el --- Search for and list unicode characters by name

;; Copyright (C) 2015  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience
;; Package-Version: 20181121.2316
;; Package-X-Original-Version: 0
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a packaged version of code by @jpkotta, taken from a
;; comment on http://tromey.com/blog/?p=831.

;;; Code:

(defgroup list-unicode-display nil
  "Explore unicode characters."
  :group 'i18n)

(define-derived-mode list-unicode-display-mode help-mode "Unicode Characters"
  "Major mode to display a list of unicode characters.")

(defun list-unicode-display-describe ()
  "Apply `describe-char' to the character in a row of a `list-unicode-display-mode' buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (search-forward "\t" (line-end-position))
    (describe-char (point))))

(defun list-unicode-display-copy ()
  "Copy the character in a row of a `list-unicode-display-mode' buffer to the kill ring."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (search-forward "\t" (line-end-position))
    (kill-ring-save (point) (1+ (point)))
    (message "Saved `%s' to the kill-ring."
             (buffer-substring-no-properties (point) (1+ (point))))))

(define-key list-unicode-display-mode-map (kbd "RET") #'list-unicode-display-describe)
(define-key list-unicode-display-mode-map (kbd "w") #'list-unicode-display-copy)
(define-key list-unicode-display-mode-map (kbd "g") #'list-unicode-display)

;;;###autoload
(defun list-unicode-display (&optional regexp)
  "Display a list of unicode characters with names matching REGEXP.
If no regexp is supplied, all characters are shown.  This takes
some time."
  (interactive "sRegexp (default \".*\"): ")
  (let* ((regexp (or regexp ".*"))
         (case-fold-search t)
         (cmp (lambda (x y) (< (cdr x) (cdr y))))
         (pred (lambda (name) (string-match-p regexp name)))
         ;; alist like ("name" . code-point)
         (char-alist ()))

    (let ((names (ucs-names)))
      (if (hash-table-p names)
          ;; ucs-names returns a hash table in emacs 26+
          (maphash (lambda (name char)
                     (when (funcall pred name)
                       (push (cons name char) char-alist)))
                   names)
        (mapc (lambda (pair)
                (when (funcall pred (car pair))
                  (push pair char-alist)))
              names)))

    (setq char-alist (sort char-alist cmp))

    (with-current-buffer (get-buffer-create "*Unicode Characters*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (c char-alist)
          (insert (format "0x%06X\t" (cdr c)))
          (insert (char-to-string (cdr c)))
          (insert (format "\t%s\n" (car c))))
        (list-unicode-display-mode)
        (goto-char (point-min))
        (switch-to-buffer (current-buffer))))))

(provide 'list-unicode-display)
;;; list-unicode-display.el ends here
