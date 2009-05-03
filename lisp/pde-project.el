;;; pde-project.el --- Project management for Perl

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 24 Dec 2007
;; Version: 0.01
;; Keywords: tools

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

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'pde-project)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'pde-vars)

(defgroup pde-project nil
  "Pde project"
  :group 'pde)

(defcustom pde-project-mark-files '("Makefile.PL" "Build.PL")
  "*The files tell the current directory should be project root."
  :type '(repeat string)
  :group 'pde-project)

(defcustom pde-file-list-regexp "^[^._#]"
  "Filenames matching this regexp will not be read when `pde-project-find-file'."
  :type 'regexp
  :group 'pde-project)

(defcustom pde-file-list-ignore-regexps nil
  "A list regexp to ignore when read."
  :type '(repeat regexp)
  :group 'pde-project)

(defcustom pde-file-list-filter-function nil
  "Predicate function to filter file to be read when `pde-project-find-file'.
Call with one argument, the path relative to pde-project-root,
return non-nil if ignore the file."
  :type 'function
  :group 'pde-project)

(defcustom pde-file-list-limit 200
  "Maximum number of files for read from project directory recursively."
  :type 'integer
  :group 'pde-project)

(defcustom pde-file-list-ignore-file ".pdeignore"
  "File name in this file will be ignore.
This file will find in the directory detect by `pde-detect-project-root'"
  :type 'string
  :group 'pde-project)

(defcustom pde-file-list-use-manifest nil
  "If you want read project files from manifest, set to \"MANIFEST\"."
  :type 'string
  :group 'pde-project)

(defvar pde-project-root nil)
(defvar pde-file-list-cache nil
  "")

(defun pde-detect-project-root ()
  (let ((dir (expand-file-name default-directory))
        found)
    (catch 'marked
      (while
          (progn
            ;; find Makefile.PL or Build.PL, set project root to current
            (mapc (lambda (f)
                    (when (file-exists-p (concat (file-name-as-directory dir) f))
                      (setq found dir)
                      (throw 'marked nil)))
                  pde-project-mark-files)
            ;; if dir is root, last and set project root to nil
            (cond ((string= dir (directory-file-name dir))
                   nil)
                  ;; if dir is in @INC, last and set project root to current
                  ((member dir pde-perl-inc)
                   (setq found dir)
                   nil)
                  ;; otherwise goes up
                  (t (setq dir (file-name-directory (directory-file-name dir))))))))
    (or found
        (file-name-as-directory default-directory))))

(defun pde-set-project-root ()
  (unless pde-project-root
    (set (make-local-variable 'pde-project-root)
         (pde-detect-project-root))))

(defun pde-file-package ()
  "Get the package name of current buffer."
  (let ((root (or pde-project-root (pde-detect-project-root)))
        package)
    (when (and buffer-file-name
               (string-match "\\.\\(pm\\|pod\\)$" buffer-file-name))
      (setq package (file-relative-name buffer-file-name root))
      (if (string-match (concat "^" (regexp-opt '("blib" "lib")) "/")
                        package)
          (setq package (substring package (match-end 0))))
      (replace-regexp-in-string
       "/" "::"
       (replace-regexp-in-string "\\.\\(pm\\|pod\\)" "" package)))))

(defun pde-directory-all-files (dir &optional full match filter limit)
  "Recursive read file name in DIR.
Return a cons cell which car indicate whether all files read
and cdr part is the real file list.

Like `directory-files', if FULL is non-nil, return absolute file
names, if match is non-nil, mention only file names match the
regexp MATCH. If FILTER is non-nil and is a function with one
argument, the file name relative to DIR, ignore the file when
FILTER function return non-nil. If LIMIT is non-nil,
when the files execeed the number will stop. The function is
search in wide-first manner."
  (let ((default-directory (file-name-as-directory dir)))
    (setq limit (or limit most-positive-fixnum))
    (let ((queue (list ""))
          (i 0)
          (finished t)
          list)
      (while (and queue (or (< i limit) (setq finished nil)))
        (setq dir (pop queue))
        (dolist (file (directory-files dir nil match))
          (unless (or (string= file ".") (string= file ".."))
            (setq file (concat dir file))
            (when (or (null filter) (not (funcall filter file)))
              (setq i (1+ i))
              (when (file-directory-p file)
                (setq file (file-name-as-directory file))
                (push file queue))
              (push file list)))))
      (cons finished
            (nreverse (if full (mapcar 'expand-file-name list) list))))))

(defun pde-project-files (dir)
  (let (file-list)
    (if (and pde-file-list-use-manifest
             (file-exists-p (expand-file-name pde-file-list-use-manifest dir)))
        (progn
          (dolist (file (pde-read-lines (expand-file-name pde-file-list-use-manifest dir)))
            (unless (string-match "This list of files$" file)
              (push file file-list)))
          (cons t file-list))
      (let ((pde-file-list-ignore-regexps pde-file-list-ignore-regexps)
            (filter pde-file-list-filter-function))
        (when (file-exists-p (expand-file-name pde-file-list-ignore-file dir))
          (setq pde-file-list-ignore-regexps
                (append pde-file-list-ignore-regexps
                        (pde-read-lines (expand-file-name pde-file-list-ignore-file dir)))))
        (when pde-file-list-ignore-regexps
          (setq filter
                (lambda (file)
                  (or (if pde-file-list-filter-function
                          (funcall pde-file-list-filter-function file))
                      (catch 'found
                        (dolist (re pde-file-list-ignore-regexps)
                          (if (string-match re file)
                              (throw 'found t))))))))
        (pde-directory-all-files dir nil
                                 pde-file-list-regexp filter
                                 pde-file-list-limit)))))

(defun pde-read-lines (file &optional comment-char)
  (or comment-char (setq comment-char "#"))
  (let ((re (regexp-quote comment-char))
        lines line)
    (with-temp-buffer
      (insert-file-contents file)
      (delete-trailing-whitespace)
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at re)
            (forward-line 1)
          (setq line (buffer-substring (point) (progn (forward-line 1) (1- (point)))))
          (if (> (length line) 0)
              (push line lines)))))
    lines))

(defun pde-project-uniquify-files (file-list)
  (let (file-alist)
    (mapc (lambda (file)
            (let ((base (if (string= (substring file -1) "/")
                            (concat (file-name-nondirectory (substring file 0 -1)) "/")
                          (file-name-nondirectory file)))
                  pair)
              (when (setq pair (assoc base file-alist))
                (setcar pair (pde-project-uniquify-file (car pair) (cdr pair)))
                (setq base (pde-project-uniquify-file base file)))
              (push (cons base file) file-alist)))
          file-list)
    file-alist))

(defun pde-project-uniquify-file (file full)
  (concat file ": "
          (car (last (split-string full "/")
                     (if (string= (substring full -1) "/")
                         3 2)))))

;;;###autoload 
(defun pde-project-find-file (&optional rebuild)
  "Find file in the project.
This command is will read all file in current project recursively.
With prefix argument, to rebuild the cache."
  (interactive "P")
  (let* ((dir (pde-detect-project-root))
         (pair (assoc dir pde-file-list-cache))
         (file-list (cdr pair)))
    (when (or rebuild (null file-list))
      (setq file-list (pde-project-files dir))
      (setcdr file-list (pde-project-uniquify-files (cdr file-list)))
      (if pair
          (setcdr pair file-list)
        (push (cons dir file-list) pde-file-list-cache)))
    (setq file-list (cdr file-list))
    (let ((file (funcall pde-completing-read-function
                         "Find file: " file-list nil t)))
      (setq file (expand-file-name (cdr (assoc file file-list)) dir))
      (if (file-directory-p file)
          (let ((default-directory file))
            (ido-find-file))
        (find-file file)))))

(provide 'pde-project)
;;; pde-project.el ends here
