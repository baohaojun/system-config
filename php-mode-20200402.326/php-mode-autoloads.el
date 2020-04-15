;;; php-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "php" "php.el" (0 0 0 0))
;;; Generated autoloads from php.el

(let ((loads (get 'php 'custom-loads))) (if (member '"php" loads) nil (put 'php 'custom-loads (cons '"php" loads))))

(autoload 'php-mode-maybe "php" "\
Select PHP mode or other major mode." t nil)

(autoload 'php-current-class "php" "\
Insert current class name if cursor in class context." t nil)

(autoload 'php-current-namespace "php" "\
Insert current namespace if cursor in namespace context." t nil)

(autoload 'php-copyit-fqsen "php" "\
Copy/kill class/method FQSEN." t nil)

(autoload 'php-run-builtin-web-server "php" "\
Run PHP Built-in web server.

`ROUTER-OR-DIR': Path to router PHP script or Document root.
`HOSTNAME': Hostname or IP address of Built-in web server.
`PORT': Port number of Built-in web server.
`DOCUMENT-ROOT': Path to Document root.

When `DOCUMENT-ROOT' is NIL, the document root is obtained from `ROUTER-OR-DIR'.

\(fn ROUTER-OR-DIR HOSTNAME PORT &optional DOCUMENT-ROOT)" t nil)

(autoload 'php-find-system-php-ini-file "php" "\
Find php.ini FILE by `php --ini'.

\(fn &optional FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php" '("php-")))

;;;***

;;;### (autoloads nil "php-align" "php-align.el" (0 0 0 0))
;;; Generated autoloads from php-align.el

(autoload 'php-align-setup "php-align" "\
Setup alignment configuration for PHP code." nil nil)

(autoload 'php-align-mode "php-align" "\
Alignment lines for PHP script.

If called interactively, enable Php-Align mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-align" '("php-align-")))

;;;***

;;;### (autoloads nil "php-face" "php-face.el" (0 0 0 0))
;;; Generated autoloads from php-face.el

(let ((loads (get 'php-faces 'custom-loads))) (if (member '"php-face" loads) nil (put 'php-faces 'custom-loads (cons '"php-face" loads))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-face" '("php-annotations-annotation-face")))

;;;***

;;;### (autoloads nil "php-mode" "php-mode.el" (0 0 0 0))
;;; Generated autoloads from php-mode.el

(let ((loads (get 'php-mode 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php-mode 'custom-loads (cons '"php-mode" loads))))

(if (version< emacs-version "24.4") (dolist (i '("php" "php5" "php7")) (add-to-list 'interpreter-mode-alist (cons i 'php-mode))) (add-to-list 'interpreter-mode-alist (cons "php\\(?:-?[3457]\\(?:\\.[0-9]+\\)*\\)?" 'php-mode)))

(define-obsolete-variable-alias 'php-available-project-root-files 'php-project-available-root-files "1.19.0")

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.\\(?:php\\.inc\\|stub\\)\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-mode-maybe))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-mode" '("php-")))

;;;***

;;;### (autoloads nil "php-mode-debug" "php-mode-debug.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from php-mode-debug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-mode-debug" '("php-mode-debug")))

;;;***

;;;### (autoloads nil "php-project" "php-project.el" (0 0 0 0))
;;; Generated autoloads from php-project.el

(defvar-local php-project-root 'auto "\
Method of searching for the top level directory.

`auto' (default)
      Try to search file in order of `php-project-available-root-files'.

SYMBOL
      Key of `php-project-available-root-files'.

STRING
      A file/directory name of top level marker.
      If the string is an actual directory path, it is set as the absolute path
      of the root directory, not the marker.")

(put 'php-project-root 'safe-local-variable #'(lambda (v) (or (stringp v) (assq v php-project-available-root-files))))

(defvar-local php-project-etags-file nil)

(put 'php-project-etags-file 'safe-local-variable #'(lambda (v) (or (functionp v) (eq v t) (php-project--eval-bootstrap-scripts v))))

(defvar-local php-project-bootstrap-scripts nil "\
List of path to bootstrap php script file.

The ideal bootstrap file is silent, it only includes dependent files,
defines constants, and sets the class loaders.")

(put 'php-project-bootstrap-scripts 'safe-local-variable #'php-project--eval-bootstrap-scripts)

(defvar-local php-project-php-executable nil "\
Path to php executable file.")

(put 'php-project-php-executable 'safe-local-variable #'(lambda (v) (and (stringp v) (file-executable-p v))))

(defvar-local php-project-phan-executable nil "\
Path to phan executable file.")

(put 'php-project-phan-executable 'safe-local-variable #'php-project--eval-bootstrap-scripts)

(defvar-local php-project-coding-style nil "\
Symbol value of the coding style of the project that PHP major mode refers to.

Typically it is `pear', `drupal', `wordpress', `symfony2' and `psr2'.")

(put 'php-project-coding-style 'safe-local-variable #'symbolp)

(defvar-local php-project-align-lines t "\
If T, automatically turn on `php-align-mode' by `php-align-setup'.")

(put 'php-project-align-lines 'safe-local-variable #'booleanp)

(defvar-local php-project-php-file-as-template 'auto "\

`auto' (default)
      Automatically switch to mode for template when HTML tag detected in file.

`t'
      Switch all PHP files in that directory to mode for HTML template.

`nil'
      Any .php  in that directory is just a PHP script.

\((PATTERN . SYMBOL))
      Alist of file name pattern regular expressions and the above symbol pairs.
      PATTERN is regexp pattern.
")

(put 'php-project-php-file-as-template 'safe-local-variable #'php-project--validate-php-file-as-template)

(defvar-local php-project-repl nil "\
Function name or path to REPL (interactive shell) script.")

(put 'php-project-repl 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v))))

(defvar-local php-project-unit-test nil "\
Function name or path to unit test script.")

(put 'php-project-unit-test 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v))))

(defvar-local php-project-deploy nil "\
Function name or path to deploy script.")

(put 'php-project-deploy 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v))))

(defvar-local php-project-build nil "\
Function name or path to build script.")

(put 'php-project-build 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v))))

(defvar-local php-project-server-start nil "\
Function name or path to server-start script.")

(put 'php-project-server-start 'safe-local-variable #'(lambda (v) (or (functionp v) (php-project--eval-bootstrap-scripts v))))

(autoload 'php-project-get-bootstrap-scripts "php-project" "\
Return list of bootstrap script." nil nil)

(autoload 'php-project-get-root-dir "php-project" "\
Return path to current PHP project." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-project" '("php-project-")))

;;;***

;;;### (autoloads nil "php-util-buffer" "php-util-buffer.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from php-util-buffer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-util-buffer" '("php-util-buffer-")))

;;;***

;;;### (autoloads nil nil ("php-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; php-mode-autoloads.el ends here
