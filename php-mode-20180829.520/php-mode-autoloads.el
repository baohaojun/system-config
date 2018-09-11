;;; php-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "php-array" "php-array.el" (0 0 0 0))
;;; Generated autoloads from php-array.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-array" '("php-")))

;;;***

;;;### (autoloads nil "php-classobj" "php-classobj.el" (0 0 0 0))
;;; Generated autoloads from php-classobj.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-classobj" '("php")))

;;;***

;;;### (autoloads nil "php-control-structures" "php-control-structures.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from php-control-structures.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-control-structures" '("php-")))

;;;***

;;;### (autoloads nil "php-crack" "php-crack.el" (0 0 0 0))
;;; Generated autoloads from php-crack.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-crack" '("php-crack-")))

;;;***

;;;### (autoloads nil "php-dio" "php-dio.el" (0 0 0 0))
;;; Generated autoloads from php-dio.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-dio" '("php-dio_")))

;;;***

;;;### (autoloads nil "php-dom" "php-dom.el" (0 0 0 0))
;;; Generated autoloads from php-dom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-dom" '("php-dom")))

;;;***

;;;### (autoloads nil "php-exceptions" "php-exceptions.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from php-exceptions.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-exceptions" '("php-try-catch")))

;;;***

;;;### (autoloads nil "php-exif" "php-exif.el" (0 0 0 0))
;;; Generated autoloads from php-exif.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-exif" '("php-exif_")))

;;;***

;;;### (autoloads nil "php-ext" "php-ext.el" (0 0 0 0))
;;; Generated autoloads from php-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-ext" '("php-")))

;;;***

;;;### (autoloads nil "php-filesystem" "php-filesystem.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from php-filesystem.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-filesystem" '("php-")))

;;;***

;;;### (autoloads nil "php-gd" "php-gd.el" (0 0 0 0))
;;; Generated autoloads from php-gd.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-gd" '("php-")))

;;;***

;;;### (autoloads nil "php-math" "php-math.el" (0 0 0 0))
;;; Generated autoloads from php-math.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-math" '("php-")))

;;;***

;;;### (autoloads nil "php-mode" "php-mode.el" (0 0 0 0))
;;; Generated autoloads from php-mode.el

(let ((loads (get 'php 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php 'custom-loads (cons '"php-mode" loads))))

(defvar php-extra-constants 'nil "\
A list of additional strings to treat as PHP constants.")

(custom-autoload 'php-extra-constants "php-mode" nil)

(if (version< emacs-version "24.4") (dolist (i '("php" "php5" "php7")) (add-to-list 'interpreter-mode-alist (cons i 'php-mode))) (add-to-list 'interpreter-mode-alist (cons "php\\(?:-?[3457]\\(?:\\.[0-9]+\\)*\\)?" 'php-mode)))

(define-obsolete-variable-alias 'php-available-project-root-files 'php-project-available-root-files "1.19.0")

(let ((loads (get 'php-faces 'custom-loads))) (if (member '"php-mode" loads) nil (put 'php-faces 'custom-loads (cons '"php-mode" loads))))

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

(autoload 'php-current-class "php-mode" "\
Insert current class name if cursor in class context.

\(fn)" t nil)

(autoload 'php-current-namespace "php-mode" "\
Insert current namespace if cursor in namespace context.

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons (eval-when-compile (rx (or (: "." (or (: "php" (32 (in "s345t"))) "amk" "phtml")) (: "/" (or "Amkfile" ".php_cs" ".php_cs.dist"))) string-end)) 'php-mode) t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-mode" '("php-")))

;;;***

;;;### (autoloads nil "php-pcre" "php-pcre.el" (0 0 0 0))
;;; Generated autoloads from php-pcre.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-pcre" '("php-preg_")))

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

(autoload 'php-project-get-bootstrap-scripts "php-project" "\
Return list of bootstrap script.

\(fn)" nil nil)

(autoload 'php-project-get-root-dir "php-project" "\
Return path to current PHP project.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-project" '("php-project-")))

;;;***

;;;### (autoloads nil "php-regex" "php-regex.el" (0 0 0 0))
;;; Generated autoloads from php-regex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-regex" '("php-")))

;;;***

;;;### (autoloads nil "php-simplexml" "php-simplexml.el" (0 0 0 0))
;;; Generated autoloads from php-simplexml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-simplexml" '("php-simplexml_")))

;;;***

;;;### (autoloads nil "php-strings" "php-strings.el" (0 0 0 0))
;;; Generated autoloads from php-strings.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-strings" '("php-")))

;;;***

;;;### (autoloads nil "php-var" "php-var.el" (0 0 0 0))
;;; Generated autoloads from php-var.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-var" '("php-")))

;;;***

;;;### (autoloads nil "php-xmlparser" "php-xmlparser.el" (0 0 0 0))
;;; Generated autoloads from php-xmlparser.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-xmlparser" '("php-")))

;;;***

;;;### (autoloads nil "php-xmlreader" "php-xmlreader.el" (0 0 0 0))
;;; Generated autoloads from php-xmlreader.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "php-xmlreader" '("php-xmlreader")))

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
