;;; pde-util.el --- Utils for misc commands

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 26 Dec 2007
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
;;   (require 'pde-util)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'pde-vars)
(require 'apropos)

;; fix for don't install perl module
(defcustom pde-utils-module-location
  (when (file-exists-p (expand-file-name "../lib/" pde-load-path))
    (expand-file-name "../lib/" pde-load-path))
  "*Location for PDE perl module if not install them to @INC."
  :type 'directory
  :group 'pde)

(defcustom pde-module-regexp
  "\\([a-zA-Z]\\([a-zA-Z0-9]+\\)?::\\)*[a-zA-Z]\\([a-zA-Z0-9]+\\)?"
  "Regexp to match pde module"
  :type 'regexp
  :group 'pde)

(defun pde-module-bounds ()
  (let ((thing (thing-at-point-looking-at pde-module-regexp)))
    (if thing
        (cons (match-beginning 0) (match-end 0)))))
(put 'perl-module 'bounds-of-thing-at-point 'pde-module-bounds)

;;;###autoload 
(defun pde-list-module-shadows ()
  "Display a list of modules that shadow other modules."
  (interactive)
  (let* ((buf (get-buffer-create "*Module Shadows*"))
         (args (if pde-utils-module-location
                   (list (concat "-I" pde-utils-module-location))))
         proc)
    (with-current-buffer buf
      (erase-buffer)
      (outline-mode)
      (setq proc
            (apply 'start-process "list-shadow" buf pde-perl-program
                   "-MEmacs::PDE::Util" "-e" "list_shadows" args))
      (set-process-sentinel proc
                            (lambda (proc event)
                              (if (y-or-n-p "Module shadows generated, See now? ")
                                  (switch-to-buffer (process-buffer proc)))))
      (message "Wait for a while..."))))

(defun pde-hyperlink-modules ()
  (goto-char (point-min))
  (while (not (eobp))
    (make-text-button (point)
                      (progn (forward-line 1)
                             (1- (point)))
                      'action (lambda (but)
                                (perldoc (intern (button-label but) perldoc-obarray) t))))
  (goto-char (point-min))
  (setq buffer-read-only t)
  (view-mode t)
  (message "Push button to show the pod."))

;;;###autoload 
(defun pde-list-core-modules ()
  "Display a list of core modules."
  (interactive)
  (let (( inhibit-read-only t)
        (bufname "*Perl Core Modules*"))
    (if (get-buffer bufname)
        (switch-to-buffer bufname)
      (switch-to-buffer (get-buffer-create bufname))
      (apply 'call-process pde-perl-program nil t nil
             "-MEmacs::PDE::Util" "-e" "list_core_modules"
             (if pde-utils-module-location
                 (list (concat "-I" pde-utils-module-location))))
      (pde-hyperlink-modules))))

;;;###autoload 
(defun pde-apropos-module (re)
  "Search modules by name."
  (interactive (list (apropos-read-pattern "module")))
  (apropos-parse-pattern re)
  (let ((inhibit-read-only t)
        (bufname "*Apropos Perl Modules*")
        modules)
    (mapatoms
     (lambda (sym)
       (and (eq (perldoc-symbol-type sym) 'module)
            (string-match apropos-regexp (symbol-name sym))
            (push (symbol-name sym) modules)))
     perldoc-obarray)
    (setq modules (sort modules 'string<))
    (switch-to-buffer (get-buffer-create bufname))
    (erase-buffer)
    (insert (mapconcat 'identity modules "\n") "\n")
    (pde-hyperlink-modules)))

;;;###autoload 
(defun pde-search-cpan (mod)
  "Search anything in CPAN."
  (interactive
   (list (let ((def (thing-at-point 'perl-module)))
           (completing-read
            (if def
                (format "Search CPAN(default %s):" def)
              "Search CPAN")
            perldoc-obarray nil nil nil nil def))))
  (when (> (length mod) 0)
    (browse-url (format "http://search.cpan.org/search?query=%s&mode=all"
                        (url-hexify-string mod)))))

(defun pde-call-process-region (beg end &optional replace src)
  "Eval perl source and show the output.
If SRC is given, eval the string instead of the text in the region.
If REPLACE is non-nil, replace the region with the output.
For example call the command on region:
   print \"Hello World!\"
will echo \"Hello World!\"
"
  (interactive "r\nP")
  (let (str proc)
    (or src (setq src (buffer-substring beg end)))
    (with-temp-buffer
      (setq proc (start-process "perl" (current-buffer) "perl"))
      (process-send-string proc (concat src "\n"))
      (process-send-eof proc)
      (while (accept-process-output proc nil 100))
      (setq str (buffer-string)))
    (if (and (eq (process-status proc) 'exit)
             replace)
        (progn (delete-region beg end)
               (insert str))
      (message str))))

;;;###autoload
(defun pde-yaml-dump (beg end replace)
  "Read Perl data from region and dump as YAML.
For example call the command on region:
    {
      'session' => {
        'dbic_class' => 'AddressDB::Session',
        'flash_to_stash' => '1'
      }
    }
will turn out to be:
   ---
   session:
     dbic_class: AddressDB::Session
     flash_to_stash: 1
"
  (interactive "r\nP")
  (let ((str (buffer-substring beg end))
        (src "use YAML; my $var = %s; print Dump($var)\n"))
    (pde-call-process-region beg end replace (format src str))))

;;;###autoload
(defun pde-yaml-load (beg end replace)
  "Read YAML data and dump as Perl data.
For example call the command on region:
   ---
   session:
     dbic_class: AddressDB::Session
     flash_to_stash: 1
will turn out to be:
    $VAR1 = {
      'session' => {
        'dbic_class' => 'AddressDB::Session',
        'flash_to_stash' => '1'
      }
    }
"
  (interactive "r\nP")
  (let ((str (buffer-substring beg end))
        (src "use YAML; use Data::Dumper; $Data::Dumper::Indent=1;\
my @vars = Load(<<__YAML__);
%s
__YAML__
print Dumper(@vars)\n"))
    (pde-call-process-region beg end replace (format src str))))

(autoload 'generate-file-autoloads "autoload")
;;;###autoload 
(defun pde-generate-loaddefs (lisp-dir)
  "Create pde-loaddefs.el"
  (interactive "Dlisp directory: ")
  (with-temp-buffer
    (dolist (file (directory-files lisp-dir t "\\.el$"))
      (generate-file-autoloads file))
    (write-region (point-min) (point-max)
                  (concat lisp-dir "/" "pde-loaddefs.el"))))

(defvar pde-tip-index 0
  "Indicate which tip should display")

(defun pde-tip (&optional arg)
  (interactive "P")
  (let ((files (directory-files (concat pde-load-path "doc/tips/") t "^[^.]")))
    (save-excursion
      (with-help-window (help-buffer)
        (with-temp-buffer
          (if arg
              (while files
                (insert-file-contents (car files))
                (setq files (cdr files))
                (goto-char (point-max))
                (insert "\n")
                (when files
                  (insert (make-string 70 ?=))
                  (insert "\n\n")))
            (setq pde-tip-index (1+ pde-tip-index))
            (if (>= pde-tip-index (length files))
                (setq pde-tip-index 0))
            (insert-file-contents (nth pde-tip-index files)))
          (princ (buffer-string)))
        (with-current-buffer standard-output
          (buffer-string))))))

(defsubst pde-filename-to-module (file)
  (replace-regexp-in-string "/" "::" file))

(defsubst pde-module-to-filename (module)
  (replace-regexp-in-string "::" "/" module))

(defun pde-locate-module-completion (string ignore action)
  "Do completion for file names passed to `locate-file'.
PATH-AND-SUFFIXES is a pair of lists, (DIRECTORIES . SUFFIXES)."
  (let* ((prefix (pde-module-to-filename string))
         (result (locate-file-completion prefix
                                         (cons pde-perl-inc '(".pm"))
                                         action)))
    (cond ((eq action t)
           (mapcar 'pde-filename-to-module result))
          ((null action)
           (and result (pde-filename-to-module result)))
          (t result))))

(defun pde-read-module (prompt &optional default require-match
                               initial history)
  (completing-read
   (concat prompt (if default (format "(default %s) : " default) ": "))
   'pde-locate-module-completion
   nil require-match initial history))

(provide 'pde-util)
;;; pde-util.el ends here
