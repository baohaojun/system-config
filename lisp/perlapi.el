;;; perlapi.el --- Show help of perlapi

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: 0.01
;; Keywords: tools
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Search document in perlapi

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'perlapi)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'woman)
(require 'pde-vars)

(defgroup perlapi nil
  "Search document for perlapi"
  :group 'tools
  :group 'pde)

(defcustom perlapi-file
  (let ((file (shell-command-to-string (format "%s -l perlapi" pde-perldoc-program))))
    (expand-file-name (substring file 0 -1)))
  "*Pod file of perlapi"
  :type 'file
  :group 'perlapi)

(defcustom perlapi-src-directory nil
  "*Directory where the Perl source files can be found."
  :type 'directory
  :group 'perlapi)

(defvar perlapi-obarray
  (let ((db (make-vector 293 0))
        (coding-system-for-read 'raw-text)
        beg name)
    (with-temp-buffer
      (insert-file-contents perlapi-file)
      (goto-char (point-min))
      (while (re-search-forward "^=item " nil t)
        (setq name (buffer-substring (match-end 0) (line-end-position))
              beg (1- (match-beginning 0)))
        (re-search-forward "^=for hackers")
        (forward-line 2)
        (set (intern name db)
             (cons beg (point)))))
    db)
  "Database for perlapi")

(defun perlapi (symbol)
  "Show documents for SYMBOL in perlapi.
If `perlapi-src-directory' sets, push button to find definition
of the symbol."
  (interactive
   (list (intern (completing-read "Describe: " perlapi-obarray nil t)
                 perlapi-obarray)))
  (let ((bufname (format "*Woman Perlapi %S*" symbol))
        (name (symbol-name symbol))
        bound)
    (if (buffer-live-p (get-buffer bufname))
        (display-buffer bufname)
      (with-current-buffer (get-buffer-create bufname)
        (setq bound (symbol-value symbol))
        (insert-file-contents perlapi-file nil
                              (car bound) (cdr bound))
        (goto-char (point-min))
        (insert (format "=head1 %s\n\n=over 8\n\n" name))
        (if (re-search-forward "=for hackers" nil t)
            (replace-match "=back\n\n=head2 for hackers\n"))
        (goto-char (point-max))
        (insert "\n=cut\n")
        (call-process-region (point-min) (point-max)
                             "pod2man" t t nil "-n" name)
        (woman-process-buffer)
        (goto-char (point-max))
        (if (re-search-backward "Found in file " nil t)
            (let ((inhibit-read-only t))
              (make-text-button (match-end 0) (line-end-position)
                                'id name
                                'action 'perlapi-find-symbol)))
        (display-buffer (current-buffer))))))

(defun perlapi-find-symbol (but)
  (when perlapi-src-directory
    (let ((label (button-label but))
          (id (button-get but 'id))
          file)
      (setq file (concat (file-name-as-directory perlapi-src-directory) label))
      (with-selected-window (display-buffer (find-file-noselect file))
        (goto-char (point-min))
        (re-search-forward (concat "^=for apidoc .*" (regexp-quote id) "\\>"))))))

(provide 'perlapi)
;;; perlapi.el ends here
