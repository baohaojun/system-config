;;; w3mhack.el --- a hack to setup the environment for building w3m

;; Copyright (C) 2001-2010, 2012, 2013, 2015, 2017, 2019
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; This file provides the stuffs to setup the environment for building
;; emacs-w3m and the installer for non-UNIX systems.


;;; How to install:

;; ( These document is a simple summary of README.  For more detail,
;;   see the original. )

;; In order to install emacs-w3m to non-UNIX systems which lack an
;; ability to execute `configure' script or have no `make' utility,
;; execute this command.
;;
;;     emacs -batch -q -no-site-file -l w3mhack.el NONE -f w3mhack-nonunix-install
;;
;; When required packages, such as APEL, are installed into unusual
;; places, the installer may miss them.  In this case, it is necessary
;; to tell their places to the installer, as follows:
;;
;;     emacs -batch -q -no-site-file -l w3mhack.el //c/share/apel://c/share/flim -f w3mhack-nonunix-install
;;
;; If you want to install programs and icons to unusual directories,
;; edit this file and set your request to `w3mhack-nonunix-lispdir'
;; and `w3mhack-nonunix-icondir'.


;;; Code:

(defvar w3mhack-nonunix-lispdir nil
  "*Directory to where emacs-w3m lisp programs are installed.
nil means that all programs are installed to the default directory.")

(defvar w3mhack-nonunix-icondir nil
  "*Directory to where emacs-w3m icons are installed.
nil means that all icons are installed to the default directory.")

(defvar w3mhack-nonunix-dryrun nil
  "*Non-nil means that print the commands to install programs and datas,
but do not execute them.")

;; Check whether the shell command can be used.
(let ((test (lambda (shell)
	      (let ((buffer (generate-new-buffer " *temp*"))
		    (msg "Hello World"))
		(with-current-buffer buffer
		  (condition-case nil
		      (call-process shell nil t nil "-c"
				    (concat "MESSAGE=\"" msg "\"&&"
					    "echo \"${MESSAGE}\""))
		    (error))
		  (prog2
		      (goto-char (point-min))
		      (search-forward msg nil t)
		    (kill-buffer buffer)))))))
  (or (funcall test shell-file-name)
      (progn
	(require 'executable)
	(let ((executable-binary-suffixes
	       (if (memq system-type '(OS/2 emx))
		   '(".exe" ".com" ".bat" ".cmd" ".btm" "")
		 executable-binary-suffixes))
	      shell)
	  (or (and (setq shell (executable-find "cmdproxy"))
		   (funcall test shell)
		   (setq shell-file-name shell))
	      (and (setq shell (executable-find "sh"))
		   (funcall test shell)
		   (setq shell-file-name shell))
	      (and (setq shell (executable-find "bash"))
		   (funcall test shell)
		   (setq shell-file-name shell))
	      (not (member (nth 1 (or (member "-f" command-line-args)
				      (member "-funcall" command-line-args)
				      (member "--funcall" command-line-args)
				      (member "-e" command-line-args)))
			   '("batch-byte-compile" "w3mhack-compile"
			     "w3mhack-makeinfo")))
	      (error "%s" "\n\
There seems to be no shell command which is equivalent to /bin/sh.
 Try ``make SHELL=foo [option...]'', where `foo' is the absolute
 path name for the proper shell command in your system.\n"))))))

(unless (dolist (var nil t))
  ;; Override the macro `dolist' which may have been defined in egg.el.
  (load "cl-macs" nil t))
(require 'bytecomp)

;; Add supplementary directories to `load-path'.
(let ((addpath (or (pop command-line-args-left) "NONE"))
      (index 0)
      path paths)
  (while (string-match "\\([^\0-\37:]+\\)[\0-\37:]*" addpath index)
    (setq path (file-name-as-directory
		(expand-file-name (match-string 1 addpath)))
	  index (match-end 0))
    (when (file-directory-p path)
      (push path paths)))
  (unless (null paths)
    (setq load-path (nconc (nreverse paths) load-path))))

(defconst shimbun-module-directory "shimbun")

(defvar w3mhack-load-file "w3m-load.el")

;; Needed for interdependencies among w3m and shimbun modules.
(push default-directory load-path)
(push (expand-file-name shimbun-module-directory default-directory) load-path)

(defun w3mhack-module-list ()
  "Returna a list of w3m modules should be byte-compile'd."
  (let* ((modules (directory-files default-directory nil "\\`[^#]+\\.el\\'"))
	 ;; Modules not to be byte-compiled.
	 (ignores '(".dir-locals.el" "w3mhack.el"))
	 (shimbun-dir (file-name-as-directory shimbun-module-directory))
	 print-level print-length)
    (unless (locate-library "mew")
      (push "mew-w3m.el" ignores))
    (if (locate-library "mime-def")
	(progn
	  ;; Add shimbun modules.
	  (dolist (file (directory-files (expand-file-name shimbun-dir)
					 nil "\\`[^#]+\\.el\\'"))
	    (setq modules (nconc modules (list (concat shimbun-dir file)))))
	  ;; mew-shimbun check
	  (when (or (member "mew-w3m.el" ignores)
		    (not (locate-library "mew-nntp")))
	    (push (concat shimbun-dir "mew-shimbun.el") ignores))
	  ;; nnshimbun check
	  (unless (let ((gnus (locate-library "gnus")))
		    (when gnus
		      (let ((load-path (list (file-name-directory gnus))))
			(locate-library "nnimap"))))
	    (push (concat shimbun-dir "nnshimbun.el") ignores)))
      (push "octet.el" ignores))
    (if (not (locate-library "mime-view"))
	(push "mime-w3m.el" ignores))
    ;; List shimbun modules which cannot be byte-compiled with this system.
    (let (list)
      ;; Byte-compile w3m-util.el first.
      (push "w3m-util.el" list)
      (setq modules (delete "w3m-util.el" modules))
      (push "w3m-proc.el" list)
      (setq modules (delete "w3m-proc.el" modules))
      (dolist (module modules)
	(unless (member module ignores)
	  (push module list)))
      (nreverse list))))

(defun w3mhack-examine-modules ()
  "Examine w3m modules should be byte-compile'd."
  (dolist (module (w3mhack-module-list))
    (princ (format "%sc " module))))

(defun w3mhack-compile-file (file)
  "Byte-compile FILE after reporting that it's being compiled."
  ;; The byte compiler in Emacs >=25 doesn't say much.
  (message "Compiling %s..." (file-name-nondirectory file))
  (byte-compile-file file))

(defun w3mhack-compile ()
  "Byte-compile the w3m modules."
  (w3mhack-generate-load-file)
  (let (modules)
    (dolist (el (w3mhack-module-list))
      (let ((elc (concat el "c")))
	(if (file-exists-p elc)
	    (if (file-newer-than-file-p elc el)
		(message " `%s' is up to date" elc)
	      (delete-file elc)
	      (push el modules))
	  (push el modules))))
    (setq modules (nreverse modules))
    (while modules
      (condition-case nil
	  (w3mhack-compile-file (car modules))
	(error))
      (setq modules (cdr modules)))))

(defun w3mhack-nonunix-install ()
  "Byte-compile the w3m modules and install them."
  (w3mhack-compile)
  (unless w3mhack-nonunix-lispdir
    (setq w3mhack-nonunix-lispdir
	  (expand-file-name "../../site-lisp/w3m" data-directory)))
  (and (not w3mhack-nonunix-icondir)
       (setq w3mhack-nonunix-icondir
	     (expand-file-name "images/w3m" data-directory)))
  (let ((mkdir (lambda (dir)
		 (unless (file-directory-p dir)
		   (message "mkdir %s" dir)
		   (unless w3mhack-nonunix-dryrun
		     (make-directory dir 'parents)))))
	(install (lambda (srcdir dstdir pattern)
		   (dolist (src (directory-files srcdir t pattern))
		     (let ((dst (expand-file-name
				 (file-name-nondirectory src) dstdir)))
		       (message "cp %s %s"
				(file-relative-name src default-directory) dst)
		       (unless w3mhack-nonunix-dryrun
			 (copy-file src dst t t)))))))
    (funcall mkdir w3mhack-nonunix-lispdir)
    (funcall install default-directory w3mhack-nonunix-lispdir "\\.elc?\\'")
    (let ((shimbun-directory
	   (expand-file-name shimbun-module-directory default-directory)))
      (when (file-exists-p (expand-file-name "shimbun.elc" shimbun-directory))
	(funcall install shimbun-directory w3mhack-nonunix-lispdir
		 "\\.elc?\\'")))
    (when w3mhack-nonunix-icondir
      (funcall mkdir w3mhack-nonunix-icondir)
      (funcall install (expand-file-name "icons")
	       w3mhack-nonunix-icondir "\\.\\(?:png\\|xpm\\)\\'"))))

;; Byte optimizers and version specific functions.

(defun w3mhack-load-path ()
  "Print default value of additional load paths for w3m.el."
  (let (paths x)
    (and (locate-library "mime-def")
	 (setq x (locate-library "poe"))
	 (progn
	   (setq x (file-name-directory x))
	   (if (string-match "/emu/\\'" x)
	       (push (substring x 0 (1+ (match-beginning 0))) paths))
	   (push x paths)))
    (if (setq x (locate-library "mime-def"))
	(push (file-name-directory x) paths))
    (if (setq x (locate-library "mew"))
	(push (file-name-directory x) paths))
    (if (setq x (locate-library "gnus"))
	(push (file-name-directory x) paths))
    (let (print-level print-length)
      (princ (mapconcat
	      (function directory-file-name)
	      (nreverse paths) ":")))))

;; FIXME: How to do it for Windows?
(defun w3mhack-expand-file-name (name destdir)
  "Convert filename NAME to the one relative to DESTDIR."
  (if (and name destdir
	   (eq (aref name 0) ?/)) ;; Not "NONE"?
      (expand-file-name (substring name 1) destdir)
    name))

(defun w3mhack-what-where ()
  "Show what files should be installed and where should they go."
  (let* ((destdir (getenv "DESTDIR"))
	 (lisp-dir (w3mhack-expand-file-name (pop command-line-args-left)
					     destdir))
	 (icon-dir (w3mhack-expand-file-name (pop command-line-args-left)
					     destdir))
	 (info-dir (w3mhack-expand-file-name (pop command-line-args-left)
					     destdir)))
    (message "
lispdir=%s
ICONDIR=%s
infodir=%s"
	     lisp-dir icon-dir info-dir)
    (setq info-dir (file-name-as-directory info-dir))
    (message "
install:
  *.el, *.elc, ChangeLog* -> %s
  *.info, *.info-*        -> %s"
	     (file-name-as-directory lisp-dir) info-dir)
    (setq icon-dir (file-name-as-directory icon-dir))
    (unless (string-equal "NONE/" icon-dir)
      (message "
install-icons:
  *.gif, *.png, *.xpm     -> %s"
	       icon-dir))
    (message "
install-info:
  *.info, *.info-*        -> %s"
	     info-dir))
  (message ""))

(defun w3mhack-makeinfo ()
  "Emacs makeinfo in batch mode.
NOTE: This function must be called from the top directory."
  (let ((file (pop command-line-args-left))
	auto-save-default
	find-file-run-dired
	coding-system-for-write
	output-coding-system
	(error 0)
	(texinfmt (locate-library "texinfmt")))
    ;; Normally texinfmt.elc loads texinfo.elc that exists in the same
    ;; directory where texinfmt.elc is.  However, another version of
    ;; texinfo.elc (e.g. auctex provides such one) might be loaded
    ;; depending on `load-path'.  So, force it load the correct one.
    (let ((load-path (if texinfmt
			 (cons (file-name-directory texinfmt) load-path)
		       load-path)))
      (require 'texinfmt))

    ;; `texinfo-format-uref' will not work if @uref{} contains "@@"
    ;;  in its operand in order to print a single "@" (bug#36186).
    (unless (condition-case nil
		(with-temp-buffer
		  (insert "@setfilename test.info\n"
			  "@uref{mailto:foo@@noncommand.example.com}\n")
		  (let ((omsg (symbol-function 'message)))
		    (fset #'message #'ignore)
		    (unwind-protect
			(texinfo-format-buffer)
		      (fset #'message omsg)
		      (setq buffer-file-name nil)
		      (kill-buffer (current-buffer))))
		  t)
	      (error nil))
      (fset 'texinfo-format-uref
	    (lambda nil "\
Format URL and optional URL-TITLE.
Insert \\=` ... \\=' around URL if no URL-TITLE argument;
otherwise, insert URL-TITLE followed by URL in parentheses."
	      (let ((args (texinfo-format-parse-args)))
		(texinfo-discard-command)
		;; if url-title
		(if (nth 1 args)
		    (insert  (nth 1 args) " (" (nth 0 args) ")")
		  (insert "`" (nth 0 args) "'"))))))

    (cd "doc")
    (condition-case err
	(progn
	  (find-file file)
	  (setq buffer-read-only nil)
	  (buffer-disable-undo (current-buffer))
	  (setq coding-system-for-write buffer-file-coding-system)
	  ;; Remove unsupported commands.
	  (goto-char (point-min))
	  (while (re-search-forward "@\\(?:end \\)?ifnottex" nil t)
	    (replace-match ""))
	  (goto-char (point-min))
	  (while (search-forward "\n@iflatex\n" nil t)
	    (delete-region (1+ (match-beginning 0))
			   (search-forward "\n@end iflatex\n")))
	  ;; Insert @include files.
	  (goto-char (point-min))
	  (set-syntax-table texinfo-format-syntax-table)
	  (let (start texinfo-command-end filename)
	    (while (re-search-forward "^@include" nil t)
	      (setq start (match-beginning 0)
		    texinfo-command-end (point)
		    filename (texinfo-parse-line-arg))
	      (delete-region start (point-at-bol 2))
	      (message "Reading included file: %s" filename)
	      (save-excursion
		(save-restriction
		  (narrow-to-region
		   (point) (+ (point)
			      (car (cdr (insert-file-contents filename)))))
		  (goto-char (point-min))
		  ;; Remove `@setfilename' line from included file, if any,
		  ;; so @setfilename command not duplicated.
		  (if (re-search-forward "^@setfilename"
					 (point-at-eol 100) t)
		      (delete-region (point-at-bol 1) (point-at-bol 2)))))))
	  ;; Remove ignored areas.
	  (goto-char (point-min))
	  (while (re-search-forward "^@ignore[\t\r ]*$" nil t)
	    (delete-region (match-beginning 0)
			   (if (re-search-forward
				"^@end[\t ]+ignore[\t\r ]*$" nil t)
			       (1+ (match-end 0))
			     (point-max))))
	  ;; Format @key{...}.
	  (goto-char (point-min))
	  (while (re-search-forward "@key{\\([^}]+\\)}" nil t)
	    (replace-match "<\\1>"))
	  ;;
	  (texinfo-every-node-update)
	  (set-buffer-modified-p nil)
	  (message "texinfo formatting %s..." file)
	  (let ((si:message (symbol-function 'message))
		(si:push-mark (symbol-function 'push-mark)))
	    (fset
	     'message
	     ;; Encode messages to terminal.
	     (let ((coding
		    (or (and (string-equal "Japanese"
					   current-language-environment)
			     locale-coding-system)
			'iso-2022-7bit)))
	       (byte-compile
		`(lambda (fmt &rest args)
		   (let ((coding-system-for-write ',coding))
		     (apply ,si:message fmt args))))))
	    ;; Silence it when formatting @multitable section.
	    (fset 'push-mark
		  (byte-compile
		   `(lambda (&rest args)
		      (apply ,si:push-mark (car args) t (cddr args)))))
	    (unwind-protect
		(texinfo-format-buffer t)
	      (fset 'message si:message)
	      (fset 'push-mark si:push-mark)))
	  (if (buffer-modified-p)
	      (progn (message "Saving modified %s" (buffer-file-name))
		     (save-buffer))))
      (error
       (message ">> Error: %s" (prin1-to-string err))
       (message ">>  point at")
       (let ((s (buffer-substring (point) (min (+ (point) 100) (point-max))))
	     (tem 0))
	 (while (setq tem (string-match "\n+" s tem))
	   (setq s (concat (substring s 0 (match-beginning 0))
			   "\n>>  "
			   (substring s (match-end 0)))
		 tem (1+ tem)))
	 (message ">>  %s" s))
       (setq error 1)))
    (kill-emacs error)))

(defun w3mhack-insert-git-revision ()
  (let ((revision
	 (with-temp-buffer
	   (when (and (file-directory-p ".git")
		      (executable-find "git")
		      (zerop (call-process "git"
					   nil
					   t
					   nil
					   "log" "--oneline" "-n" "1" ".")))
	     (goto-char (point-min))
	     (skip-chars-forward "^ ")
	     (concat "\"" (buffer-substring (point-min) (point)) "\"")))))
    (goto-char (point-max))
    (insert (format "\C-l
(defconst emacs-w3m-git-revision %s
  \"Git revision string of this package.\")
" revision))))

(defun w3mhack-generate-load-file ()
  "Generate a file including all autoload stubs."
  (require 'autoload)
  (let ((files (w3mhack-module-list))
	(generated-autoload-file (expand-file-name w3mhack-load-file))
	autoload-modified-buffers generated-autoload-load-name)
    (if (and (file-exists-p w3mhack-load-file)
	     (not (catch 'modified
		    (dolist (file (cons "w3mhack.el" files))
		      (when (file-newer-than-file-p file w3mhack-load-file)
			(throw 'modified t))))))
	(message " `%s' is up to date" w3mhack-load-file)
      (with-temp-buffer
	(dolist (file files)
	  (setq generated-autoload-load-name
		(file-name-sans-extension (file-name-nondirectory file)))
	  (autoload-generate-file-autoloads file (current-buffer)))
	(goto-char (point-min))
	(insert ";;; " w3mhack-load-file
		" --- automatically extracted autoload\n;;
;; This file should have been generated by make in emacs-w3m source directory.
;;\n;;; Code:\n\n")
	(w3mhack-insert-git-revision)
	(insert "\n(provide '"
		(file-name-sans-extension w3mhack-load-file) ")\n\n\
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:\n\n;;; " w3mhack-load-file " ends here\n")
	(write-region (point-min) (point-max)
		      w3mhack-load-file nil 'silent)))))

(defun w3mhack-locate-library ()
  "Print the precise file name of Emacs library remaining on the commane line."
  (princ (or (locate-library (pop command-line-args-left)) "")))

(defun w3mhack-print-status ()
  (princ "OK"))

(defun w3mhack-version ()
  "Print the value of `emacs-w3m-version'."
  (require 'w3m)
  (let (print-level print-length)
    (princ emacs-w3m-version)))

;;; w3mhack.el ends here
