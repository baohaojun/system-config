;;; ac-php-core.el --- The core library of the ac-php.

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>
;; Copyright (C) 2014-2019 jim <xcwenn@qq.com>
;; Copyright (C) 2011-2016 Jan Erik Hanssen and Anders Bakken
;; Copyright (C) 2011 Joseph <jixiuf@gmail.com>
;; Copyright (C) 2010 Brian Jiang

;; Author: jim <xcwenn@qq.com>
;;      Serghei Iakovlev <sadhooklay@gmail.com>
;; Maintainer: jim
;; URL: https://github.com/xcwen/ac-php
;; Version: 2.0.8
;; Keywords: completion, convenience, intellisense
;; Package-Requires: ((dash "1") (php-mode "1") (s "1") (f "0.17.0") (popup "0.5.0") (xcscope "1.0"))
;; Compatibility: GNU Emacs: 24.4, 25.x, 26.x, 27.x

;; This file is NOT part of GNU Emacs.

;;; License

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

;; The core library of the `ac-php' package.  Acts like a backend for the
;; following components:
;;
;; - `ac-php'
;; - `company-php'
;; - `helm-ac-php-apropros'
;;
;; Can be used as an API to build your own components.  This engine currently
;; provides:
;;
;; - Support of PHP code completion
;; - Support of jumping to definition/declaration/inclusion-file
;;
;; When creating this package, the ideas of the following packages were used:
;;
;; - auto-java-complete
;;
;;   - `ac-php-remove-unnecessary-items-4-complete-method'
;;   - `ac-php-split-string-with-separator'
;;
;; - auto-complete-clang
;;
;; - rtags
;;
;;   - `ac-php-location-stack-index'
;;
;; Many options available under Help:Customize
;; Options specific to ac-php-core are in
;;   Convenience/Completion/Auto Complete
;;
;; Known to work with Linux and macOS.  Windows support is in beta stage.
;; For more info and examples see URL `https://github.com/xcwen/ac-php' .
;;
;; Bugs: Bug tracking is currently handled using the GitHub issue tracker
;; (see URL `https://github.com/xcwen/ac-php/issues')

;;; Code:

(require 'json)    ; `json-encode', `json-read-file'
(require 's)       ; `s-equals', `s-upcase', `s-matches-p', `s-replace', ...
(require 'f)       ; `f-write-text', `f-full', `f-join', `f-exists?', ...

(require 'xcscope) ; `cscope-find-egrep-pattern', `cscope-prompt-for-symbol'
(require 'popup)   ; `popup-tip'
(require 'dash)
(require 'eldoc)

(require 'cl-lib) ; `cl-reduce', `cl-decf'

;;; Customization

;;;###autoload
(defgroup ac-php nil
  "Auto Completion source for PHP."
  :prefix "ac-php-"
  :group 'auto-complete
  :group 'completion
  :group 'convenience
  :link '(url-link :tag "Bug Tracker" "https://github.com/xcwen/ac-php/issues")
  :link '(url-link :tag "GitHub Page" "https://github.com/xcwen/ac-php")
  :link '(emacs-commentary-link :tag "Commentary" "ac-php"))

(defcustom ac-php-php-executable (executable-find "php")
  "Set PHP command line interpreter executable path.
For more see URL `http://php.net/manual/en/features.commandline.php'."
  :group 'ac-php
  :type 'string)

(defcustom ac-php-cscope (executable-find "cscope")
  "Set the Csope executable path.
For more see URL `http://cscope.sourceforge.net/'."
  :group 'ac-php
  :type 'string)

(defcustom ac-php-use-cscope-flag nil
  "Non-nil means use Cscope if it is possible.
To use this feature you'll need to set cscope executable path in
`ac-php-cscope'.  For more see URL `http://cscope.sourceforge.net'."
  :group 'ac-php
  :type 'boolean)

(defcustom ac-php-auto-update-intval 3600
  "The interval between automatic re-indexing project's files (in seconds)."
  :group 'ac-php
  :type 'integer)

(defcustom ac-php-project-root-dir-use-truename t
  "Non-nil means always expand filenames using function `file-truename'."
  :group 'ac-php
  :type 'boolean)

(defcustom ac-php-mode-line
  '(:eval (format "AP%s" (ac-php-mode-line-project-status)))
  "Mode line lighter for ac-php.
Set this variable to nil to disable the lighter."
  :group 'ac-php
  :type 'sexp
  :risky t)

(defcustom ac-php-tags-path (concat (getenv "HOME") "/.ac-php")
  "Use this directory as a base path for the per-projects tags directories..

The idea is to have a common local directory for the all projects.  This path
get extended with the directory tree of the project that you are indexing the
tags for."
  :group 'ac-php
  :type 'string)

;;; Internal configuration

(defconst ac-php-config-file ".ac-php-conf.json"
  "Per-project configuration file.")

(defvar ac-php-root-directory (file-name-directory (or load-file-name buffer-file-name))
  "The ac-php package location.")

(defvar ac-php-ctags-executable (concat ac-php-root-directory "phpctags")
  "Set the Phpctags executable path.  Don't change the value of this variable.")

(defvar ac-php-common-json-file (concat ac-php-root-directory "ac-php-comm-tags-data.el")
  "Default tags file.
Will be used as a fallback when unable to obtain the project related tags.")

(defvar ac-php-debug-flag nil
  "Non-nil means enable verbose mode when processing autocomplete.
Please notice, enabling this option entails detailed output of debugging
information to the ‘*Messages*’ buffer.  This feature is designed for
ac-php developer only.")

(defvar ac-php-gen-tags-flag nil
  "Non-nil means that remaking tags currently is under process.")

(defvar ac-php-phptags-index-progress 0
  "The re-index progress indicator.
Meant for `ac-php-mode-line-project-status'")

;; The database of the all tags.
;;
;; A schematic designation of every element of this database:
;;
;;   (tags-file-name tags-file-mtime
;;    (class-list
;;     functions-array
;;     inherit-list
;;     project-files-array
;;     project-root))
;;
;; Below is a small example to understand the structure
;; of this database:
;;
;;   (("/path/to/the/tags.el" 1553935654
;;     (("class1" "class2" "...")
;;      ["fn1" "fn2" "fn3" "fn4" "fn5" "..."]
;;      ("inherit1" "inherit2" "...")
;;      ["/file3.php" "/file4.php" "..."]
;;      "/path/to/the/project-1/root"))
;;    ("/path/to/the/tags.el" 1553935654
;;     (("class1" "class2")
;;      ["fn1" "fn2" "fn3" "fn4" "fn5" "..."]
;;      ("inherit1" "inherit2" "...")
;;      ["/file1.php" "/file2.php" "..."]
;;     "/path/to/the/project-2/root"))
;;    (...)
;;    (...))
;;
;; The number '1553935654' in this example means the modification
;; time of the '/path/to/the/tags.el' file.
(defvar ac-php-tag-last-data-list nil
  "Holds in-memory database for per-project tags.")

(defconst ac-php-re-classlike-pattern
  (concat
   ;; Class declaration may begin at the 1st line.
   ;; The file may start with <?php, <? or <%.
   ;; Example:
   ;;   <?php class Foo {}
   "^\\(?:<\\(?:\\?\\(?:php\\)?\\|%\\)\\)?"
   ;; Then see if 'abstract' or 'final' appear
   "\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?"
   ;; The classlike type
   "\\(?:class\\|trait\\)"
   ;; Its name, which is the first captured group in the regexp.
   ;; See URL `https://www.php.net/manual/en/language.oop5.basic.php'
   "\\s-+\\([a-zA-Z_-ÿ][a-zA-Z0-9_-ÿ]*\\)")
  "The regular expression for classlike.")

(defconst ac-php-re-namespace-unit-pattern
  (concat
   ;; First see if '\' appear, although really it is not valid for all use cases
   "\\(?:\\\\\\)?"
   ;; We allow backslashes in the name to handle namespaces, parts of namespaces
   ;; and fully qualified class names, but again this is not necessarily correct
   ;; for all use cases.
   ;; See URL `https://www.php.net/manual/en/language.oop5.basic.php'
   "\\(?:[a-zA-Z_-ÿ][a-zA-Z0-9_-ÿ\\]*\\)")
  "The regular expression for a part of a namespace.")

(defconst ac-php-re-namespace-pattern
  (concat
   ;; Namespace declaration may begin at the 1st line.
   ;; The file may start with <?php, <? or <%.
   ;; Example:
   ;;   <?php namespace Acme;
   "^\\(?:<\\(?:\\?\\(?:php\\)?\\|%\\)\\)?"
   ;; Namespace keyword
   "\\s-*namespace"
   ;; Namespace value, which is the first captured group in the regexp
   "\\s-+\\(" ac-php-re-namespace-unit-pattern "\\)\\s-*;")
  "The regular expression for a namespace.")

(defconst ac-php-re-beginning-of-defun-pattern
  (concat
   "^\\s-*"
   "\\(?:\\(?:abstract\\|final\\|private\\|protected"
   "\\|public\\|static\\)\\s-+\\)"
   "*function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(")
  "Regular expression for a PHP function.")

(defconst ac-php-re-annotated-var-pattern
  (concat
   "@var"
   "\\s-+\\(" ac-php-re-namespace-unit-pattern  "\\)\\>\\s-+")
  "The regular expression for a class inside an annotated variable.")

(defvar ac-php-prefix-str "")

(defvar ac-php-location-stack-index 0)

(defvar ac-php-location-stack nil)

(defvar ac-php--php-key-list '("public"
 "class" "namespace" "protected"
 "private" "function" "while"
 "extends" "return" "static" "global" "continue" "abstract" "finally" "instanceof"))

(defvar ac-php-rebuild-tmp-error-msg nil)

(defvar ac-php-max-bookmark-count 500)

;;; Utils

(defmacro ac-php--debug (format-string &rest args)
  "Display a debug message at the bottom of the screen.
The message also goes into the ‘*Messages*’ buffer, if ‘message-log-max’
is non-nil.  Return the debug message.  For FORMAT-STRING and ARGS explanation
refer to `message' function."
  `(when ac-php-debug-flag
       (message (concat "[DEBUG]: " ,format-string) ,@args)))

(defun ac-php--get-timestamp (time-spec)
  "Get UNIX timestamp from the TIME-SPEC."
  (+ (* (nth 0 time-spec) 65536)
     (nth 1 time-spec)))

(defun ac-php--reduce-path (path max-len)
  "Return a modified version of PATH no longer than MAX-LEN.

This function replaces some components with single characters starting from the
left to try and get the path down to MAX-LEN"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(defun ac-php-g--project-root-dir (tags-data)
  "Return a project path using the TAGS-DATA list."
  (nth 4 tags-data))

(defsubst ac-php--in-comment-p (&optional pos)
  "Determine whether POS is inside a comment."
  (let ((state (save-excursion (syntax-ppss pos))))
    (nth 4 state)))

(defsubst ac-php--in-string-or-comment-p (&optional pos)
  "Determine whether POS is inside a string or comment."
  (let ((state (save-excursion (syntax-ppss pos))))
    (nth 8 state)))

;; See: https://github.com/emacs-php/php-mode/issues/503
(defun ac-php--beginning-of-defun (&optional arg)
  "Move to the beginning of the ARGth PHP function from point.
A replacemant for PHP's version `php-beginning-of-defun'."
  (let (found-p (arg (or arg 1)))
    (while (> arg 0)
      (setq found-p (re-search-backward
                     ac-php-re-beginning-of-defun-pattern
                     nil 'noerror))
      (setq arg (1- arg)))
    (while (< arg 0)
      (end-of-line 1)
      (let ((opoint (point)))
        (ac-php--beginning-of-defun 1)
        (forward-list 2)
        (forward-line 1)
        (if (eq opoint (point))
            (setq found-p (re-search-forward
                           ac-php-re-beginning-of-defun-pattern
                           nil 'noerror)))
        (setq arg (1+ arg))))
    (not (null found-p))))

;; See: https://github.com/emacs-php/php-mode/issues/503
(defun ac-php--end-of-defun (&optional arg)
  "Move the end of the ARGth PHP function from point.
A replacemant for PHP's version `php-en-of-defun'.

See `ac-php--beginning-of-defun'."
  (ac-php--beginning-of-defun (- (or arg 1))))

(defsubst ac-php--in-function-p (&optional pos)
  "Determine whether POS is inside a function."
  (let (bof (pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      (when (ac-php--beginning-of-defun)
        (setq bof (point))
        (ac-php--end-of-defun)
        (and (> pos bof)
             (< pos (point)))))))

(defun ac-php-toggle-debug ()
  "Toggle debug mode.
Please notice, enabling debug mode entails detailed output of debugging
information to the ‘*Messages*’ buffer.  This feature is designed for
ac-php developer only."

  (interactive)
  (let ((debug-p (not ac-php-debug-flag)))
    (progn
      (setq ac-php-debug-flag debug-p
            debug-on-error debug-p)
      (message "Debug mode was %s in ac-php"
               (if debug-p "enabled" "disabled")))))

(defun ac-php-mode-line-project-status ()
  "Report status of current project index."
  (format ":%02d%%%%" ac-php-phptags-index-progress))

(defun ac-php-location-stack-push ()
  (let ((bm (ac-php-current-location)))
    (if  (functionp 'xref-push-marker-stack)
        (xref-push-marker-stack)
      (ring-insert (with-no-warnings find-tag-marker-ring) (point-marker)))
    (while (> ac-php-location-stack-index 0)
      (cl-decf ac-php-location-stack-index)
      (pop ac-php-location-stack))
    (unless (string= bm (nth 0 ac-php-location-stack))
      (push bm ac-php-location-stack)
      (when (> (length ac-php-location-stack) ac-php-max-bookmark-count)
        (nbutlast ac-php-location-stack
                  (- (length ac-php-location-stack)
                     ac-php-max-bookmark-count))))))

;;function
(defun ac-php-goto-line-col (line column)
  (goto-char (point-min))
  (forward-line (1- line))
  (beginning-of-line)
  (forward-char (1- column)))

(defun ac-php-current-location (&optional offset)
  (format "%s:%d:%d" (or (buffer-file-name) (buffer-name))
          (line-number-at-pos offset) (1+ (- (or offset (point)) (point-at-bol)))))
(defun ac-php--string=-ignore-care( str1 str2  )
  (s-equals?(s-upcase str1 ) (s-upcase str2 ))
  ;;(not (integer-or-marker-p ( compare-strings  str1  0 nil str2  0 nil t ))  )
  )

(defun ac-php-find-file-or-buffer (file-or-buffer &optional other-window)
  (if (file-exists-p file-or-buffer)
      (if other-window
          (find-file-other-window file-or-buffer)
        (find-file file-or-buffer))
    (let ((buf (get-buffer file-or-buffer)))
      (cond ((not buf) (message "No buffer named %s ;you can M-x: ac-php-remake-tags-all  fix it " file-or-buffer))
            (other-window (switch-to-buffer-other-window file-or-buffer))
            (t (switch-to-buffer file-or-buffer))))))


(defun ac-php-goto-location (location &optional other-window )
  "Go to a location passed in. It can be either: file,12 or file:13:14 or plain file"
  ;; (message (format "ac-php-goto-location \"%s\"" location))
  (when (> (length location) 0)
    (cond ((string-match "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)" location)
           (let ((line (string-to-number (match-string-no-properties 2 location)))
                 (column (string-to-number (match-string-no-properties 3 location))))
             (ac-php-find-file-or-buffer (match-string-no-properties 1 location) other-window)
             ;;(run-hooks ac-php-after-find-file-hook)
             (ac-php-goto-line-col line column)
             t))
          ((string-match "\\(.*\\):\\([0-9]+\\)" location)
           (let ((line (string-to-number (match-string-no-properties 2 location))))
             (ac-php-find-file-or-buffer (match-string-no-properties 1 location) other-window)
             ;;(run-hooks ac-php-after-find-file-hook)
             (goto-char (point-min))
             (forward-line (1- line))
             t))
          ((string-match "\\(.*\\),\\([0-9]+\\)" location)
           (let ((offset (string-to-number (match-string-no-properties 2 location))))
             (ac-php-find-file-or-buffer (match-string-no-properties 1 location) other-window)
             ;;(run-hooks ac-php-after-find-file-hook)
             (goto-char (1+ offset))
             t))
          (t
           (if (string-match "^ +\\(.*\\)$" location)
               (setq location (match-string-no-properties 1 location)))
           (ac-php-find-file-or-buffer location other-window)))
    ;;(ac-php-location-stack-push)
    ))

(defsubst ac-php-clean-document (s)
  (when s
    (setq s (replace-regexp-in-string "<#\\|#>\\|\\[#" "" s))
    (setq s (replace-regexp-in-string "#\\]" " " s)))
  s)
(defun  ac-php--tag-name-is-function ( tag-name )
  (s-matches-p "(" tag-name )
    )

(defun ac-php-split-string-with-separator (str regexp &optional replacement omit-nulls)
  "Split STR into substrings bounded by  REGEXP.

This function is a tool like `split-string', but it treat separator as an
element of returned list for example:

  \(ac-php-split-string-with-separator 'abc.def.g' '\\.' '.')

will return:

  '('abc' '.' 'def' '.' 'g')

The REPLACEMENT may used to return instead of REGEXP.  For OMIT-NULLS
refer to original `split-string' function.

Note: To conveniently describe in the documentation, double quotes (\") have
been replaced by '."
  (when str
    (let (split-list substr match-end)
      (if (string-match regexp str)
          (progn
            (while (string-match regexp str)
              (setq match-end (match-end 0))
              (setq substr (substring-no-properties str 0 (- match-end 1)))
              (when (or (not omit-nulls) (> (length substr) 0))
                (setq split-list (append split-list (list substr))))
              (setq split-list (append split-list (list (or replacement regexp))))
              (setq str (substring-no-properties str match-end)))
            (when (or (not omit-nulls) (> (length str) 0))
              (setq split-list (append split-list (list str)))))
        (setq split-list (list str)))
      split-list)))

(defun ac-php--get-clean-node (parser-data &optional check-len)
  "Clean PARSER-DATA from unnecessary elements.

This function is used to drop all elements before ';'.  For example:

  \(ac-php--get-clean-node '('A' ';' 'B'))

will return:

  \('B')

The CHECK-LEN may be passed to indicate the limit to analyze items:

  \(ac-php--get-clean-node '('A' 'B' 'C' 'D') 2)

will return:

  \('A' 'B')

Note: To conveniently describe in the documentation, double quotes (\") have
been replaced by '."
  (ac-php--debug "Going to clean parser data: %S" parser-data)
  (let ((i 0) ret-data item)
    (unless check-len
      (setq check-len (length parser-data)))
    (while (< i check-len)
      (setq item (nth i parser-data))
      (if (and (stringp item)
               (string= item ";"))
          (setq ret-data nil)
        (push item ret-data))
      (setq i (1+ i)))

    (setq ret-data (reverse ret-data))
    (ac-php--debug "Parser data after cleaning up is: %S" ret-data)
    ret-data))

(defun ac-php--get-node-parser-data (parser-data)
  "Get keywords node from a PARSER-DATA."
  (let* ((check-len (1- (length parser-data)))
         (last-item (nth check-len parser-data))
         ret-data)
    (if (and (stringp last-item)
             (string= last-item "__POINT__"))
        (setq ret-data (ac-php--get-clean-node parser-data check-len))
      ;; TODO: Until version 2.0.7 the code below worked incorrectly.
      ;; Previous implementation just did the following test:
      ;;
      ;; (when last-item
      ;;   (setq ret-data (ac-php--get-node-parser-data last-item)))
      ;;
      ;; So I fixed this.  However I'll need to verify that
      ;; all still works as expected.  Consider this as an experimental branch.
      (when (and last-item (listp last-item))
        (progn
          (setq ret-data (ac-php--get-node-parser-data last-item))
          (ac-php--debug "The node after deep scan is: %S" ret-data))))
    ret-data))

(defun ac-php--get-key-list-from-parser-data (parser-data)
  "Get keywords list from the PARSER-DATA list."
  (ac-php--debug "Building a key list from the parser data: %S" parser-data)
  (let ((first-key (nth 0 parser-data))
        item
        (i 1)
        ret
        (parser-data-len (length parser-data)))
    (if (and (listp first-key) first-key)
        (setq ret (ac-php--get-clean-node
                   (ac-php--get-key-list-from-parser-data first-key)))
      (if (and (> parser-data-len 1) (listp (nth 1 parser-data)))
          (setq ret (list (concat first-key "(")))
        (setq ret (list first-key))))
    (while (< i parser-data-len)
      (setq item (nth i parser-data))
      (cond
       ((and (stringp item)
             (and (< (1+ i) parser-data-len)
                  (listp (nth (1+ i) parser-data))))
        ;; function
        (setq ret (append ret (list (concat item "("))))
        (setq i (+ i 2)))
       ((stringp item)
        ;; variable
        (setq ret (append ret (list item)))
        (setq i (1+ i)))
       (t (setq i (1+ i)))))
    ret))

(defun ac-php-remove-unnecessary-items-4-complete-method (splited-line-items)
  "Remove unnecessary items in the SPLITED-LINE-ITEMS.

Used to sanitize auto completion data.  Below are some examples for possible
return values:

  :-------------------------------:------------------------:
  | SPLITED-LINE-ITEMS            | Will return            |
  :-------------------------------------------:------------:
  | ('foo' '.' 'bar' '(' ')' '.') | ('foo' '.' 'bar(' '.') |
  | ('foo' '.' 'bar' '(' 'a')     | ('a')                  |
  | ('foo' '.' 'bar')             | ('foo' '.' 'bar')      |
  | ('foo' '.')                   | ('foo' '.')            |
  | ('foo')                       | ('foo')                |
  :-------------------------------:------------------------:

Meant for `ac-php-get-class-at-point' .

Note: To conveniently describe in the documentation, double quotes (\") have
been replaced by '."
  (ac-php--debug "Start removing unnecessary items for complete method...")
  (ac-php--debug "Intial items are: %S" splited-line-items)
  (let ((need-add-right-count 1)
        (item-count (length splited-line-items))
        (i 0)
        item
        (elisp-str "(")
        parser-data
        ret)
    (while (< i item-count)
      (setq item (nth i splited-line-items))
      (cond
       ((string= "(" item)
        (setq elisp-str (concat elisp-str "("))
        (setq need-add-right-count (1+ need-add-right-count)))
       ((string= ")" item)
        (setq elisp-str (concat elisp-str ")"))
        (setq need-add-right-count (1- need-add-right-count)))
       (t
        (setq elisp-str (concat
                         elisp-str "\""
                         (s-replace "\\" "\\\\" item) "\" "))))
      (setq i (1+ i)))

    (if (> need-add-right-count 0)
        (progn
          (setq elisp-str (concat elisp-str "\"__POINT__\""))
          (setq i 0)
          (while (< i need-add-right-count)
            (setq elisp-str (concat elisp-str ")"))
            (setq i (1+ i))))
      (setq elisp-str "()"))

    (ac-php--debug "Prepared Elisp string to read: %s" elisp-str)
    (setq parser-data (read elisp-str))
    (setq parser-data (ac-php--get-node-parser-data parser-data))
    (setq ret (ac-php--get-key-list-from-parser-data parser-data))

    (ac-php--debug "The list after removing unnecessary items is: %S" ret)
    ret))

(defun ac-php--get-class-full-name-in-cur-buffer ( first-key function-map get-return-type-flag)
    "DOCSTRING"
  (let (cur-namespace tmp-name ret-name tmp-ret)
    (let (  split-arr   cur-class-name )
      (ac-php--debug " ac-php--get-class-full-name-in-cur-buffer  first-key:%s" first-key )


      (if ( ac-php--check-global-name  first-key )
          (setq tmp-name first-key)
        (progn
          (setq split-arr (s-split-up-to "\\\\"   first-key  1 ))
          (ac-php--debug "  split-arr 22 len:%d " (length split-arr)  )

          ;;check for use
          (cond
           ((= 2 (length split-arr))

            (setq cur-namespace (nth 0 split-arr) )
            (setq cur-class-name (nth 1 split-arr) )
            (setq tmp-name (ac-php-get-use-as-name  cur-namespace ) )
            (ac-php--debug "tmp-name 22 %s" tmp-name)
            (if tmp-name
                (setq tmp-name (concat tmp-name "\\" cur-class-name ) )
              (setq tmp-name  first-key  )
              )
            )

           ((= 1 (length split-arr))
            ;;check use as
            (setq cur-class-name (nth 0 split-arr) )
            (setq  tmp-name (ac-php-get-use-as-name  cur-class-name ) )
            (unless tmp-name (setq tmp-name  first-key  ))
            (ac-php--debug "XXXX %s " tmp-name)

            ))
          (unless ( ac-php--check-global-name  tmp-name )
            (let ( (tmp-name-as-global  (concat "\\" tmp-name ))
                   (cur-namepace-tmp-name  (concat  (ac-php-get-cur-namespace-name)   tmp-name   )
                   ))
              (ac-php--debug " check as cur namespace %s " tmp-name)
              (if (ac-php--get-item-from-funtion-map  cur-namepace-tmp-name  function-map )
                  (setq tmp-name cur-namepace-tmp-name    )
                (setq tmp-name tmp-name-as-global ))
                ))

          (ac-php--debug " 22222 %s " tmp-name)

          )
        ))


    (when tmp-name
      (setq tmp-name  (ac-php--as-global-name tmp-name) )
      (setq tmp-ret  (ac-php--get-item-from-funtion-map    tmp-name  function-map ))
      (ac-php--debug "11 tmp-ret %S" tmp-ret)
      (if tmp-ret
          (if get-return-type-flag
              (setq ret-name  (aref tmp-ret  4) )
            (setq ret-name  (aref  tmp-ret 1) )
            )
        ))

    (unless ret-name
      (setq tmp-name  (if ( ac-php--check-global-name  first-key  )  first-key   (concat "\\"  first-key   )  ))
      (setq tmp-ret  (ac-php--get-item-from-funtion-map   tmp-name  function-map ))

      (ac-php--debug "22 tmp-ret %S" tmp-ret)
      (if tmp-ret
          (if get-return-type-flag
              (setq ret-name  (aref  tmp-ret 4) )
            (setq ret-name  (aref tmp-ret 1 ) )
            )
        ))
    (ac-php--debug " ac-php--get-class-full-name-in-cur-buffer ret-name %s" ret-name)
    ret-name
    ))

(defun ac-php-split-line-4-complete-method (line-string)
  "This function is used to tokinize PHP string.

First this function will split LINE-STRING to small items.

For example, suppose LINE-STRING is:

  '$class->method($parameter)'

then this function split it to:

  'class' '.' 'method' '(' 'parameter' ')'

Note: To conveniently describe in the documentation, double quotes (\") have
been replaced by '."
  (ac-php--debug "Start splitting the string to items")
  (save-excursion
    (let ((stack-list nil)
          (old-string line-string))

      ;; "a sequence of characters" => string
      (setq line-string (replace-regexp-in-string
                         "\".*?\"" "string"
                         line-string))

      ;; dot => ;
      (setq line-string (replace-regexp-in-string
                         "[.]"  ";"
                         line-string))

      ;; foo:bar => foo;bar
      (setq line-string (replace-regexp-in-string
                         "\\([^:]\\):\\([^:]\\)" "\\1;\\2"
                         line-string))

      ;; class->method => class.method
      (setq line-string (replace-regexp-in-string
                         "[ \t]*->[ \t]*" "."
                         line-string))

      ;; :: => ::.
      (setq line-string (replace-regexp-in-string
                         "[ \t]*::[ \t]*" "::."
                         line-string))

      ;; new | return | echo => ;
      (setq line-string (replace-regexp-in-string
                         "\\bnew\\b\\|\\breturn\\b\\|\\becho\\b" ";"
                         line-string))

      ;; case | yield => ;
      (setq line-string (replace-regexp-in-string
                         "\\bcase\\b\\|\\byield\\b" ";"
                         line-string))

      ;; $ => (empty string)
      (setq line-string (replace-regexp-in-string
                         "\\$" ""
                         line-string))

      ;; @ | equal operators => ;
      (setq line-string (replace-regexp-in-string
                         "@\\|!?=>?\\|<=?\\|>=?\\|=" ";"
                         line-string))

      ;; Some operators => ;
      (setq line-string (replace-regexp-in-string
                         "[&|!,?^+/*\-]" ";"
                         line-string))

      (unless (string= old-string line-string)
        (ac-php--debug "Input string was changed during to splitting: \"%s\""
                       line-string))

      ;; Split ‘line-string’ with ".", but add "." as an element at
      ;; its position in list
      (setq stack-list (ac-php-split-string-with-separator
                        line-string "[ \t]*\\.[ \t]*"  "." t))

      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list
                (append tmp-list
                        (ac-php-split-string-with-separator ele "[{}]" ";" t))))
        (setq stack-list tmp-list))

      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list
                (append tmp-list
                        (ac-php-split-string-with-separator ele "[>)]\\|]" ")" t))))
        (setq stack-list tmp-list))

      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list
                (append tmp-list
                        (ac-php-split-string-with-separator ele "[<([]" "(" t))))
        (setq stack-list tmp-list))

      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list
                (append tmp-list
                        (ac-php-split-string-with-separator ele ";" ";" t))))
        (setq stack-list tmp-list))

      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list
                (append tmp-list (split-string ele "[ \t]+" t))))
        (setq stack-list tmp-list))

      stack-list)))

(defun ac-php-get-syntax-backward (regexp &rest args)
  "Search backward from current point for regular expression REGEXP.

Possible additional ARGS:

    :sexp       Specifies which parenthesized expression in the REGEXP
                should be returned.

    :comment    Indicates should we search inside a comment or not.

    :defun      Indicates should we search inside a defun or not.

    :bound      A buffer position that bounds the search.  The match found must
                not end after that position.  A value of nil means search to the
                end of the accessible portion of the buffer.

Return a propertized string in a format:

  #(\"some string\" 0 11 (pos POINT))

where POINT is a point position that bounds the search.  Return nil in case of
unsuccessful search."
  (let ((old-cfs case-fold-search)
        (found-p nil)
        line-txt
        ret-str
        search-pos
        in-comment-ctx
        in-defun-ctx
        (sexp (plist-get args :sexp))
        (in-comment-p (plist-get args :comment))
        (in-defun-p (plist-get args :defun))
        (bound (plist-get args :bound)))
    (save-excursion
      (ac-php--debug "Search backward from current point up to %s"
                     (if bound (format "point: %d" bound)
                       "accessible portion of the buffer"))
      (ac-php--debug "Used regular expression: \"%s\"" regexp)
      (while (not found-p)
        (setq search-pos (re-search-backward regexp bound t 1))
        (if search-pos
            (progn
              (setq
               ;; Determine actual comment context
               in-comment-ctx (if in-comment-p
                                  (ac-php--in-comment-p (point))
                                (not (ac-php--in-string-or-comment-p (point))))
               ;; Determine actual defun context
               in-defun-ctx (if in-defun-p
                               (ac-php--in-function-p (point))
                             (not (ac-php--in-function-p (point)))))

              (when (and in-comment-ctx in-defun-ctx)
                (setq line-txt (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
                (when (string-match regexp line-txt)
                  (setq ret-str (match-string sexp line-txt))
                  (setq ret-str (propertize ret-str 'pos search-pos))
                  (setq found-p t))))
          (setq found-p t))))
    (setq case-fold-search old-cfs)
    (ac-php--debug "Search result: %s" ret-str)
    ret-str))

(defun ac-php-get-cur-class-name ()
  "Get current class name.

Tries to retrieve current class name if it is possible.
Returns the name of the current class as a string or nil if the search failed."
  (ac-php-get-syntax-backward
   ac-php-re-classlike-pattern
   :sexp 1))

(defun ac-php-get-cur-namespace-name (&optional trim-trailing-backslash-p)
  "Get fully qualified namespace.

Tries to retrieve current fully qualified namespace if it is possible.
TRIM-TRAILING-BACKSLASH-P is used to indicate whether we should trim trailng
backslash or not.  Always returns a string, even if the namespace was not found."
  (let (namespace (not-found ""))
    (setq namespace (ac-php-get-syntax-backward
                     ac-php-re-namespace-pattern
                     :sexp 1))
    (if namespace
        (progn
          ;; Concatenate leading backslash
          (unless (string= (substring namespace 0 1) "\\")
            (setq namespace (concat "\\" namespace)))
          ;; Trim trailng backslash
          (setq namespace (replace-regexp-in-string "\\\\$" "" namespace))
          ;; Add trailing backslash only if needed
          (if (not trim-trailing-backslash-p)
              (setq namespace (concat namespace "\\"))
            namespace))
      not-found)))

(defun ac-php-clean-namespace-name (namespace-name)
  (if (and (stringp namespace-name)
           (> (length namespace-name)   1)
           ( string=  (substring-no-properties  namespace-name 0 1  ) "\\" ) )
        ( substring-no-properties namespace-name 1 )
      namespace-name))

(defun ac-php-get-cur-full-class-name ()
  "Get current class name in a fully qualified form.
Returns nil if could not find class name in current buffer."
  (let (class-name namespace)
    (setq class-name (ac-php-get-cur-class-name)
          namespace (ac-php-get-cur-namespace-name))
    (if class-name
        (progn
          (when (string= "" namespace)
            (setq namespace "\\"))
          (concat namespace class-name))
      nil)))

(defun ac-php-get-use-as-name (item-name)
  "DOCSTRING"
  (let ((item-name (nth 0 (s-split "(" item-name))))
    (or
     (ac-php-get-syntax-backward
      (concat
       "^[ \t]*use[ \t]+\\("
       ac-php-re-namespace-unit-pattern
       "\\\\"
       item-name
       "\\)[ \t]*;")
      :sexp 1)
     (ac-php-get-syntax-backward
      (concat
       "^[ \t]*use[ \t]+\\("
       ac-php-re-namespace-unit-pattern
       "\\)[ \t]+as[ \t]+"
       item-name
       "[ \t]*;")
      :sexp 1))))

(defun ac-php--get-all-use-as-name-in-cur-buffer ()
  "Make a regex to match use statements."
  (let ( ret-list (search-re (concat "use[ \t]+" ac-php-re-namespace-unit-pattern ".*;")  ) line-txt match-ret )
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward search-re  nil t )
          (setq line-txt (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position )))
          (ac-php--debug "line-text:%s" line-txt)

           (setq match-ret (s-match   (concat "use[ \t]+\\(" ac-php-re-namespace-unit-pattern "\\)[ \t]+as[ \t]+\\(" ac-php-re-namespace-unit-pattern "\\)[ \t]*;") line-txt ))
          (if match-ret
              (add-to-list 'ret-list (list    (ac-php--as-global-name (nth 1 match-ret)) (nth 2 match-ret)   ))
            (progn
              (setq match-ret (s-match   (concat "use[ \t]+\\(" ac-php-re-namespace-unit-pattern "\\)[ \t]*;") line-txt ))
              (when match-ret
                (let ((key-arr (s-split "\\\\" (nth 1 match-ret) ) ))
                  (ac-php--debug "key-arr %S " key-arr)

                  (add-to-list 'ret-list (list  (ac-php--as-global-name  (nth 1 match-ret))  (nth (1- (length key-arr)) key-arr )   ))))))

          (end-of-line))))
    ret-list ))

(defun ac-php-get-annotated-var-class (variable &optional pos)
  "Get a class name for an annotated VARIABLE.

The optional second argument POS specifies current point.  Returns a class
name as a string or nil if the search failed.  At this time doesn't aimed to
work for multi class hint:

/** @var Foo|Bar $baz */"
  (ac-php--debug "Scan for annotated variable")
  ;; TODO: Doesn't aimed to work for multi class hint:
  ;;  /** @var Foo|Bar $baz */
  (let ((in-defun-p (ac-php--in-function-p pos)))
    (ac-php-get-syntax-backward
     (concat ac-php-re-annotated-var-pattern "$" variable)
     :sexp 1
     :comment t
     :defun in-defun-p
     :bound (when in-defun-p
              (save-excursion (beginning-of-defun) (beginning-of-line) (point))))))

(defun ac-php-get-class-at-point (tags-data &optional pos)
  "Docstring."
  (let (line-txt
        old-line-txt
        key-line-txt
        key-list
        tmp-key-list
        first-class-name
        first-key
        ret-str
        first-key-str)
    (unless pos
      (setq pos (point)))

    (setq line-txt (s-trim
                    (buffer-substring-no-properties
                     (line-beginning-position) pos)))

    ;; Get out early from function
    (catch 'empty-code
      (when (= (length line-txt) 0)
        (throw 'empty-code "Got empty string")))

    ;; Looking for method chaining like this:
    ;;
    ;;   $class->method1()
    ;;         ->method2()
    ;;         ->method3();
    ;;
    ;; and setting the ‘line-txt’ variable to:
    ;;
    ;;   $class->method1()->method2()->method3();
    ;;
    (save-excursion
      (ac-php--debug "Looking for method chaining...")
      (while (and (> (length line-txt) 0) (= (aref line-txt 0) ?-))
        (forward-line -1)
        (let ((no-comment-code "")
              (line-start-pos (line-beginning-position))
              (line-end-pos (line-end-position)))
          (while (< line-start-pos line-end-pos)
            (unless (ac-php--in-comment-p line-start-pos)
              (setq no-comment-code
                    (concat no-comment-code
                            (buffer-substring-no-properties
                             line-start-pos (1+ line-start-pos)))))
            (setq line-start-pos (1+ line-start-pos)))
          ;;  fix comment  : xx #  => xx , xx/* => xx , xx// =>  xx
          (setq no-comment-code (s-replace-all
                                 '(("#" . "")
                                   ("/*" . "")
                                   ("//" . "")
                                   )
                                 no-comment-code ))

          (setq line-txt (concat (s-trim no-comment-code) line-txt)))))

    (ac-php--debug "Current working string: \"%s\"" line-txt)

    (setq old-line-txt line-txt)

    ;; Looking for callable form like this:
    ;;
    ;;   array ($foo, "bar")
    ;;
    ;; and setting the ‘line-txt’ variable to:
    ;;
    ;;   $foo->bar
    ;;
    (ac-php--debug "Looking for callable form #1...")
    (setq line-txt
          (replace-regexp-in-string
           (concat ".*array[ \t\n]*"
                   "([ \t\n]*\\(\\$[a-z0-9A-Z_> \t-]+\\)[ \t\n]*,"
                   "[ \t\n]*['\"]\\([a-z0-9A-Z_]*\\).*")
           "\\1->\\2"
           line-txt))

    ;; Looking for callable form like this:
    ;;
    ;;   [$foo, "bar"]
    ;;
    ;; and setting the ‘line-txt’ variable to:
    ;;
    ;;   $foo->bar
    ;;
    (ac-php--debug "Looking for callable form #2...")
    (setq line-txt
          (replace-regexp-in-string
           (concat ".*\\[[ \t\n]*"
                   "\\(\\$[a-z0-9A-Z_> \t-]+\\)[ \t\n]*,"
                   "[ \t\n]*['\"]\\([a-z0-9A-Z_]*\\).*")
           "\\1->\\2"
           line-txt))

    (if (or (not (ac-php--in-string-or-comment-p pos))
            (not (string= line-txt old-line-txt)))
        (progn
          (if (not (string= line-txt old-line-txt))
              (ac-php--debug "Updated working string: \"%s\"" line-txt))

          (setq key-list (ac-php-remove-unnecessary-items-4-complete-method
                          (ac-php-split-line-4-complete-method line-txt)))

          (ac-php--debug "Keyword list is: %S" key-list)
          (if (not (and (stringp (nth 1 key-list))
                        (string= "." (nth 1 key-list))))
              (setq key-list nil)))
      (setq key-list nil))

    (when key-list
      (setq first-key-str (nth 0 (ac-php--get-item-info (nth 0 key-list))))
      (when (and (string-match "::" first-key-str)
               (not (string-match "\\/\\*" line-txt))
               (not (string-match "\$[a-zA-Z0-9_]*[\t ]*::" old-line-txt) )
               )
          (progn
            (ac-php--debug "Detected a static method call")
            (setq first-key (substring-no-properties first-key-str 0 -2)
                  first-class-name first-key)
            (cond
             ((string= first-key "parent")
              (setq first-class-name (concat (ac-php-get-cur-full-class-name)
                                             ".__parent__")))
             ((or (string= first-key "self")
                  (string= first-key "static"))
              (setq first-class-name (concat (ac-php-get-cur-full-class-name))))
             )))
      (when (not first-class-name )
        (progn

          (if(string-match ".*::" first-key-str)
              (setq first-key (substring-no-properties first-key-str 0 -2))
            (setq first-key first-key-str))

          (when (and (not first-class-name) (string= first-key "this"))
            (ac-php--debug "Detected call on $this")
            (setq first-class-name (ac-php-get-cur-full-class-name)))

          (ac-php--debug "Class name is: %s" first-class-name)

          ;; Scan for annotated variable like:
          ;;
          ;;   /** @var Extension $extension */
          ;;
          ;; TODO: Doesn't aimed to work for multi class hint:
          ;;
          ;;  /** @var Foo|Bar $baz */
          ;;
          (unless first-class-name
            (setq first-class-name (ac-php-get-annotated-var-class first-key pos)))

          ;; Scan for function like calls or catch statements like:
          ;;
          ;;   - function hello (Request $request)
          ;;   - function () use (Filter $filter)
          ;;   - catch (\Exception $e)
          ;;
          ;; TODO: Doesn't aimed to work for multi catch exception handling:
          ;;
          ;;   - catch (MyException | MyOtherException $e)
          ;;
          (unless first-class-name
            (ac-php--debug "Scan for funcation like call or a catch statement")
            (setq first-class-name
                  (ac-php-get-syntax-backward
                   (concat "\\(" ac-php-re-namespace-unit-pattern "\\)"
                           "\\s-+\\(&\\)?$" first-key "\\s-*[),]")
                   :sexp 1
                   :defun  (ac-php--in-function-p pos)
                   :bound (save-excursion
                            (beginning-of-defun)
                            (beginning-of-line)
                            (point)))))

          ;; Scan for  instanceof :
          ;;
          ;;   -  ( $e instanceof \Exception )
          ;;
          (unless first-class-name
            (ac-php--debug "Scan for funcation like call or a catch statement")
            (setq first-class-name
                  (ac-php-get-syntax-backward
                   (concat "$\\(" first-key   "\\)"
                           "\\s-+instanceof\\s-+\\(" ac-php-re-namespace-unit-pattern "\\)\\s-*[),]")
                   :sexp 2
                   :defun  (ac-php--in-function-p pos)
                   :bound (save-excursion
                            (beginning-of-defun)
                            (beginning-of-line)
                            (point)))))


          ;; Scan for @param annotation like this:
          ;;
          ;;   @param \Phalcon\Http\Request $request
          ;;
          ;; TODO: @property, @property-read, @property-write
          ;;
          (unless first-class-name
            (ac-php--debug "Scan for method annotations")
            (setq first-class-name
                  (ac-php-get-syntax-backward
                   (concat "@param\\s-+"  "\\("
                           ac-php-re-namespace-unit-pattern "\\)\\s-+$" first-key)
                   :sexp 1
                   :comment t
                   :bound (save-excursion (beginning-of-defun) (beginning-of-line)))))

          ;; check $v = new .... or $v = $this->sadfa() ;
          (unless first-class-name
            (let (define-str symbol-ret symbol-type)
              (setq define-str
                    (ac-php-get-syntax-backward
                     (concat "$" first-key "\\s-*=\\([^=]*\\)[;]*")
                     :sexp 1
                     :defun  (ac-php--in-function-p pos)
                     :bound (save-excursion (beginning-of-defun) (beginning-of-line))))
              (when define-str
                (save-excursion
                  (goto-char (get-text-property 0 'pos define-str))
                  (end-of-line)

                  (setq line-txt (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position)))

                  (if (string-match "(" line-txt)
                      (let (beginning-of-line-pos)
                        (ac-php--debug "XXXXXX: %s" line-txt)
                        (beginning-of-line)
                        (setq beginning-of-line-pos (point))
                        ;; Function
                        (re-search-forward ".[ \t]*(" )
                        (re-search-backward "[a-zA-Z_0-9][ \t]*(" nil t)
                        (ac-php--debug "XXXXXX: pos22=[%s]"
                                       (buffer-substring-no-properties
                                        beginning-of-line-pos (point))))
                    ;; Property
                    (re-search-backward ".[ \t]*;" nil t))

                  ;; TODO: (backward-char 1)
                  (ac-php--debug " ===== define-str :%s pos=%d check_pos=%d"
                                 define-str
                                 (get-text-property 0 'pos define-str)
                                 (point))
                  (setq symbol-ret (ac-php-find-symbol-at-point-pri tags-data))
                  (unless symbol-ret
                    (setq symbol-ret (ac-php-find-symbol-at-point-pri
                                      tags-data nil t)))

                  (when symbol-ret
                    (setq symbol-type  (car symbol-ret) )
                    (ac-php--debug "XXLLL %s" symbol-type )
                    (when (or (string= symbol-type "class_member")
                              (string= symbol-type "user_function"))
                      (setq first-class-name  (nth 2 symbol-ret))))))))

          (unless first-class-name (setq first-class-name first-key)))))

    ;; fix use-as-name, same namespace
    (when ( and first-class-name
                (= 1 (length  (s-split "\\." first-class-name))))
      (setq first-class-name
            (ac-php--get-class-full-name-in-cur-buffer
             first-class-name
             (ac-php-g--function-map tags-data) t)))

    (ac-php--debug "22===first-class-name :%s" first-class-name)

    (if first-class-name
        (progn
          (setq ret-str  (concat first-class-name ))
          (dolist (field-value (cdr key-list))
            (setq ret-str  (concat  ret-str field-value )))
          (setq ret-str (ac-php--as-global-name ret-str)))
      (if (>(length key-list) 1) "null" nil))))


(defun ac-php-candidate-class ( tags-data key-str-list  )
  ;;得到变量
  (let ( ret-list key-word output-list  class-name
                  (class-map (ac-php-g--class-map tags-data ))
                  (inherit-map (ac-php-g--inherit-map tags-data ))
                  item-list check-item  arr-len )
    (setq key-str-list (replace-regexp-in-string "\\.[^.]*$" "" key-str-list ))
    (setq class-name (ac-php-get-class-name-by-key-list  tags-data key-str-list ))

    (progn

      (setq  output-list (ac-php-get-class-member-list  class-map inherit-map  class-name ) )
      (ac-php--debug " 22 class-name:%s output-list= %S" class-name output-list )
      (mapc (lambda (x)
                (setq key-word   (aref  x  1)   )
                (setq check-item  (concat  (aref  x 0 ) "_" key-word     ))
                (if (assoc-string  check-item item-list t )
                    (progn
                      )
                  (progn
                    (setq  item-list (append  (list key-word nil) item-list))

                    (setq arr-len  (length x ) )
                    (ac-php--debug "ITEM:%S" x )
                    (ac-php--debug "2:%s" (ac-php--get-array-string x arr-len  2 ) )
                    (setq key-word (propertize key-word 'ac-php-help   (ac-php--get-array-string x arr-len  2 ) ))

                    (setq key-word (propertize key-word 'ac-php-return-type ( ac-php--get-array-string x arr-len   4 ) ))
                    (setq key-word (propertize key-word 'ac-php-tag-type ( ac-php--get-array-string x arr-len   0 ) ))
                    (setq key-word (propertize key-word 'ac-php-access (  ac-php--get-array-string x arr-len   6 ) ))
                    (setq key-word (propertize key-word 'ac-php-from ( ac-php--get-array-string x arr-len   5 ) ))
                    (setq key-word (propertize key-word 'summary  ( ac-php--get-array-string x arr-len   4 )  ))
                    (push key-word ret-list  )))


                nil
                ) output-list )
      )

    (ac-php--debug " ret-list  = %S" ret-list)
    ret-list))
(defun ac-php--get-item-from-funtion-map (  key-word function-map )
  "DOCSTRING"
   (gethash key-word function-map )
    )

(defun ac-php-candidate-other ( tags-data)
  (let (ret-list
        (cur-word (ac-php-get-cur-word-without-clean))
        cur-word-len
        cmp-value
        start-word-pos
        (function-map (ac-php-g--function-map tags-data))
        key-word func-name
        function-item-len)

    (setq cur-word-len (length cur-word ))
    (setq start-word-pos (- cur-word-len (length ac-php-prefix-str) ) )
    (when (>=  cur-word-len 1 )
      ;;user func + class
      (if ( string= (substring-no-properties cur-word 0 1 ) "\\")
          (progn
            (maphash
             (lambda (_k  function-item )
               (setq function-item-len  (length function-item ) )
               (when (s-prefix-p  cur-word (aref function-item 1 )  t )
                 (setq key-word (substring-no-properties (aref  function-item  1)  ))
                 (setq key-word (propertize key-word 'ac-php-help
                                            (ac-php--get-array-string  function-item function-item-len  2 )
                                            ))
                 (setq key-word (propertize key-word 'ac-php-return-type
                                            (ac-php--get-array-string  function-item function-item-len  4 )
                                            ))
                 (setq key-word (propertize key-word 'summary
                                            (ac-php--get-array-string  function-item function-item-len  4 )
                                            ))
                 (push key-word ret-list  )
                 )
               ) function-map)
            )
        (let ( start-word  (word-arr (s-split "\\\\" cur-word  ) ) )

          (setq start-word (nth 0 word-arr ))
          ;;use as
          (dolist ( use-item (ac-php--get-all-use-as-name-in-cur-buffer  ) )
            (ac-php--debug "XXX use-item  %s cur-word=%s" use-item cur-word)
            (if ( string= start-word  cur-word )
                (when (s-prefix-p  cur-word (nth 1 use-item ) t )
                  (setq key-word  (substring-no-properties (nth  1  use-item ) start-word-pos  ))
                  (setq key-word (propertize key-word 'ac-php-tag-type (nth 0  use-item ) ))
                  (setq key-word (propertize key-word 'ac-php-help  (nth 1  use-item ) ))
                  (setq key-word (propertize key-word 'ac-php-return-type   (nth 0  use-item ) ))
                  (setq key-word (propertize key-word 'summary   (nth 0  use-item ) ))
                  (push key-word ret-list  )
                  )
              (let (find-now-word find-now-word-len)

                (when (string= start-word (nth 1 use-item )  )

                  (setq find-now-word (concat (nth 0 use-item)
                                              ( substring cur-word (length  start-word  )  ) )  )
                  (setq find-now-word-len (length  find-now-word) )

                  (ac-php--debug"  XXX use namespace ... %s %d "  find-now-word  find-now-word-len)

                  ;;XXXXXXX

                  (maphash
                   (lambda (_k  function-item )
                     (setq function-item-len (length function-item) )
                     (when( s-prefix-p  find-now-word (aref function-item 1 )  t  )
                       (setq key-word
                             (concat
                              cur-word
                              (substring-no-properties (aref  function-item  1) find-now-word-len )))

                       (setq key-word (propertize key-word 'ac-php-help
                                                  (ac-php--get-array-string  function-item  function-item-len  2) ))
                       (setq key-word (propertize key-word 'ac-php-return-type
                                                  (ac-php--get-array-string  function-item  function-item-len  4) ))
                       (setq key-word (propertize key-word 'ac-php-tag-type (aref  function-item 0 ) ))
                       (setq key-word (propertize key-word 'summary
                                                  (ac-php--get-array-string  function-item function-item-len   4) ))
                       (push key-word ret-list  )

                       )
                     ) function-map)

                  )
                )))

          ;;; key word
          (dolist  (k ac-php--php-key-list)
             (when( and ( s-prefix-p  cur-word  k ) (not  (string=   k cur-word   )) )
               (setq key-word   k   )
               (setq key-word (propertize key-word 'ac-php-help  "" ))
               (setq key-word (propertize key-word 'ac-php-return-type   "" ))
               (setq key-word (propertize key-word 'ac-php-tag-type "" ))
               (setq key-word (propertize key-word 'summary   "" ))
               (push key-word ret-list  )
               ))

          ;;cur namespace
          (let ((cur-namespace (ac-php-get-cur-namespace-name)) cur-full-fix   start-word-pos-with-namespace   )
            (ac-php--debug "XX check cur-namespace === %s" cur-namespace  )
            (setq cur-full-fix (concat cur-namespace  cur-word  ) )
            (setq start-word-pos-with-namespace (+  start-word-pos (length cur-namespace  )  ) )
            (ac-php--debug "check cur-namespace === %s" cur-namespace  )


            (maphash
             (lambda (_k  function-item )
               (when( s-prefix-p   cur-full-fix (aref function-item 1 ))
                 (setq key-word  (substring-no-properties (aref  function-item  1 ) start-word-pos-with-namespace  ))
                 (setq key-word (propertize key-word 'ac-php-help  (aref  function-item 2) ))
                 (setq key-word (propertize key-word 'ac-php-return-type   (aref  function-item 4 ) ))
                 (setq key-word (propertize key-word 'ac-php-tag-type (aref  function-item 0 ) ))
                 (setq key-word (propertize key-word 'summary   (aref  function-item 4 ) ))
                 (push key-word ret-list  )

                 )
               )  function-map)
            )

          ;;system : trim
          (let ((cur-namespace "\\") cur-full-fix   start-word-pos-with-namespace   )
            (ac-php--debug "XX check cur-namespace === %s" cur-namespace  )
            (setq cur-full-fix (concat cur-namespace  cur-word  ) )
            (setq start-word-pos-with-namespace (+  start-word-pos (length cur-namespace  )  ) )
            (ac-php--debug "check cur-namespace === %s" cur-namespace  )


            (maphash
             (lambda (_k  function-item )
               (when( s-prefix-p   cur-full-fix (aref function-item 1 ))
                 (setq key-word  (substring-no-properties (aref  function-item  1 ) start-word-pos-with-namespace  ))
                 (setq key-word (propertize key-word 'ac-php-help  (aref  function-item 2) ))
                 (setq key-word (propertize key-word 'ac-php-return-type   (aref  function-item 4 ) ))
                 (setq key-word (propertize key-word 'ac-php-tag-type (aref  function-item 0 ) ))
                 (setq key-word (propertize key-word 'summary   (aref  function-item 4 ) ))
                 (push key-word ret-list  )

                 )
               )  function-map)
            )
          ;;; cur function vars
          (maphash
           (lambda (k _v )
             (ac-php--debug " check %s %s  " cur-word  k )
             (when( and ( s-prefix-p  cur-word  k ) (not  (string=   k cur-word   )) )
               (setq key-word   k   )
               (setq key-word (propertize key-word 'ac-php-help  "" ))
               (setq key-word (propertize key-word 'ac-php-return-type   "" ))
               (setq key-word (propertize key-word 'ac-php-tag-type "" ))
               (setq key-word (propertize key-word 'summary   "" ))
               (push key-word ret-list  )
               ))
           (ac-php--get-cur-function-vars)
           )

          )))
    (ac-php--debug "ret-list:%S" ret-list )
    ret-list))
(defun  ac-php--get-cur-function-vars( )
  (let ( txt start-pos end-pos  var-list  ret-map  var-name first-char )
    (save-excursion
      (setq  end-pos (- (point) 1)   )
      (beginning-of-defun )
      (setq  start-pos (point)   )
      (setq txt (buffer-substring-no-properties start-pos end-pos ))
      (setq var-list (s-match-strings-all "[$\"'][0-9_a-z]*" txt) )
      (setq ret-map (make-hash-table :test  'case-fold ))
      (dolist  (item  var-list )
        (setq var-name   (nth 0 item )  )
        (setq first-char  (aref var-name 0) )
        (when (or  (= first-char ?\" ) (= first-char ?' ))
          (setq var-name   (substring  var-name 1 )  )
          )

        (puthash var-name  nil   ret-map  )
        )
      ret-map
      )))
;;; ==============BEGIN
(defun ac-php-find-php-files ( project-root-dir regex also-find-subdir )
  "get all php file list"
  (let (results sub-results files file-name file-dir-flag file-change-time file-change-unixtime )
    (setq files (directory-files-and-attributes project-root-dir t))
    (dolist  (file-item  files )
      (setq file-name  (nth 0 file-item ) )
      (setq file-dir-flag  (nth 1 file-item ) )
      (setq file-change-time (nth 6 file-item ) )

      (if (stringp  file-dir-flag  );;link
          (setq  file-dir-flag (file-directory-p file-dir-flag )))


      (when (and (not file-dir-flag) ;;file
                 (string-match  regex file-name )
                 )

        (setq file-change-unixtime (+ (* (nth 0 file-change-time )  65536  ) (nth 1 file-change-time )   ) )
        (if results
            (nconc results (list (list file-name  file-change-unixtime)) )
          (setq results  (list (list file-name  file-change-unixtime) ))))

      (when ( and   file-dir-flag
                    ;;(not (string= "."   (file-name-base file-name)  ))
                    ;;(not (string= ".."   (file-name-base file-name)  ))
                    (not (string= "."  (substring (file-name-base file-name)  0 1 ))) ;; not start with "."
                    )
        (when (and also-find-subdir
                   ;;no find in vendor tests
                   (not (s-matches-p "/vendor/.*/tests/"  file-name ) ))
          (setq sub-results  (ac-php-find-php-files file-name regex also-find-subdir ) )

          (if results
              (nconc results sub-results)
            (setq results sub-results)))
        ))
    results
    ))
(defun ac-php--clean-return-type (return-type)
  (when return-type
   (s-trim (replace-regexp-in-string "|.*" "" return-type ) ) )
  )

(defun ac-php--json-save-data (conf-file data-list)
  "Creating configuration by populating CONF-FILE using DATA-LIST."
  (ac-php--debug "Populate configuration file")

  (let ((old-pp-value json-encoding-pretty-print) json-data)
    (setq json-encoding-pretty-print t
          json-data (json-encode data-list))

    (f-write-text json-data 'utf-8 conf-file)
    (setq json-encoding-pretty-print old-pp-value)))

(defun ac-php--cache-files-save  (file-path cache1-files )
  (ac-php--json-save-data file-path (list :cache1-files   cache1-files)  )
  )

(defun ac-php--ctags-opts (project-root-dir rebuild)
  "Create phpctags command options.

This function uses PROJECT-ROOT-DIR as a base path for the project files.  In
addition this function takes into account REBUILD flag which means that the
files should be processed even though they were recently processed (so-called
force rebuild)."
  `(,(concat "--config-file=" (f-join project-root-dir "./" ac-php-config-file))
    ,(concat "--tags_dir=" ac-php-tags-path)
    ,(concat "--rebuild=" (if rebuild "yes" "no"))
    ,(concat "--realpath_flag="
            (if ac-php-project-root-dir-use-truename "yes" "no"))))

(defun ac-php--rebuild-file-list (project-root-dir save-tags-dir rebuild)
    "Indexing project files.

This function use PROJECT-ROOT-DIR as a base path for the project files and
SAVE-TAGS-DIR as a destination path for the index cache.  In addition this
function takes into account REBUILD flag which means that the files should be
processed even though they were recently processed (so-called force rebuild)."
    (message "ac-php: Rebuild file list...")
    (let* ((arguments (ac-php--ctags-opts project-root-dir rebuild))
           (process (apply 'start-process
                           "ac-phptags"
                           "*AC-PHPTAGS*"
                           ac-php-php-executable
                           ac-php-ctags-executable
                           arguments)))

      (ac-php--debug
       "%s %s %s %s %s %s"
       ac-php-php-executable
       ac-php-ctags-executable
       (nth 0 arguments)
       (nth 1 arguments)
       (nth 2 arguments)
       (nth 3 arguments))

      (ac-php-mode t)

      (setq ac-php-rebuild-tmp-error-msg nil
            ac-php-phptags-index-progress 0)

      (force-mode-line-update)

      (set-process-sentinel
       process
       '(lambda (process event)
          (ac-php-mode 0)
          (cond
           ((string-match "finished" event)
            (if ac-php-rebuild-tmp-error-msg
                (message "ac-php: An error occurred during to re-index: %s"
                         ac-php-rebuild-tmp-error-msg)
              (message "ac-php: The project has been successfully re-indexed")))
           ((string-match "exited abnormally" event)
            (progn
              (message (concat "ac-php: Something went wrong\n"
                               "ac-php: The re-indexing process exited abnormally\n"
                               "ac-php: Please re-check for incorrect syntax and "
                               "possible PHP errors and try again later"))
              (ac-php--debug event))))))

      (set-process-filter process 'ac-php-phptags-index-process-filter)))

(defun ac-php-phptags-index-process-filter (process strings)
  "Process status update for the indexing process.

This callback function accepts two arguments:

  - PROCESS, the process that created the output.
  - STRINGS, the message containing the currently produced output."
  (dolist (string (split-string strings "\n"))
    (ac-php--debug "%s" string)
    (cond
     ((string-match "PHPParser:" string)
      (setq ac-php-rebuild-tmp-error-msg
            (concat ac-php-rebuild-tmp-error-msg "\n" string)))
     ((string-match "\\([0-9]+\\)%" string)
      (let ((progress (string-to-number (match-string 1 string))))
        (unless (= ac-php-phptags-index-progress progress)
          (setq ac-php-phptags-index-progress progress)
          (force-mode-line-update)))))))

(defun ac-php--remake-tags (project-root-dir force)
  "Re-index project located at PROJECT-ROOT-DIR taking into account FORCE flag.
This function attempts to re-index project files only if currently no other
process is doing the same."
  (ac-php--debug "Attempting to determine whether need to re-index project...")
  (if (or ac-php-debug-flag ; Force re-index on debug mode
          (not ac-php-gen-tags-flag))
      (progn
        (setq ac-php-gen-tags-flag t)
        (ac-php--remake-tags-ex project-root-dir force))
    (progn
      (ac-php--debug (concat "ac-php: Skip re-indexing project; "
                       "there is already a process that doing the same"))
      nil)))

(defun ac-php--remake-tags-ex (project-root-dir force)
  "Re-index project located at PROJECT-ROOT-DIR taking into account FORCE flag.
This function is used internally by the function `ac-php--remake-tags'."
  (let (save-tags-dir
        all-file-list
        last-phpctags-errmsg
        update-tag-file-list
        (file-name (buffer-file-name)))

    ;; Always rebuild tags if currently opened file is from vendor directory
    (when (and file-name (s-match "/vendor/" file-name))
      (setq force t))

    (message "ac-php: Starting to re-index the project located at %s%s"
             (ac-php--reduce-path project-root-dir 60)
             (if force "with a forced rebuilding of all tags" ""))

    (unless (f-exists? ac-php-ctags-executable)
      (message (concat "ac-php: Unable to locate phpctags executable at %s\n"
                       "ac-php: Restarting GNU Emacs might help")
               ac-php-ctags-executable))

    (unless (and (not (s-blank? ac-php-php-executable))
                 (f-exists? ac-php-php-executable))
      (message (concat "ac-php: Unable to locate PHP executable at %s\n"
                       "ac-php: You need to install PHP CLI and restart GNU Emacs")
               ac-php-php-executable))

    (unless project-root-dir
      (message "ac-php: The per-project configuration file '%s' doesn't exist at %s"
              ac-php-config-file
              (file-name-directory (buffer-file-name))))

    (if (and (f-exists? ac-php-ctags-executable) ac-php-php-executable project-root-dir)
        (progn
          (setq save-tags-dir (ac-php--get-tags-save-dir project-root-dir))
          (ac-php--rebuild-file-list project-root-dir save-tags-dir force))
      (setq ac-php-gen-tags-flag nil))))

(defun  ac-php-gen-el-func (  doc)
  " example doc 'xxx($x1,$x2)' => $x1 , $x2  "
  (let ( func-str )
    (if (string-match "[^(]*(\\(.*\\))[^)]*" doc)
        (progn
          (setq func-str (s-trim (match-string 1 doc) ) )
          (setq func-str (replace-regexp-in-string "[\t ]*,[\t ]*" "," func-str  ) )
          (setq func-str (replace-regexp-in-string "[\t ]+" " " func-str  ) )
          )
      ""
      )))

(defun ac-php--get-tags-save-dir (project-root-dir)
  "Get an absolute path to directory where tags should be saved.

This function uses PROJECT-ROOT-DIR as an of path to the directory,
where tags should be saved.  If the directory does not exist, it will
be created."
  (ac-php--debug "Lookup for tags directory...")
  (let (ret tag-dir conf-list old-default-directory)
    (setq conf-list (ac-php--get-config project-root-dir)
          tag-dir (cdr (assoc-string "tag-dir" conf-list)))

    (if tag-dir
        (progn
         (ac-php--debug "Found tags directory")
         (setq old-default-directory default-directory
               default-directory project-root-dir
               ret (file-truename tag-dir)
               default-directory old-default-directory))

      (when (memq system-type '(windows-nt ms-dos))
        ;; Sanitize path: C:\my-project => /C/my-project
        (setq project-root-dir
              (concat "/"
                      (replace-regexp-in-string
                       (regexp-quote ":")
                       ""
                       project-root-dir))))

      (setq ret (concat
                 ac-php-tags-path
                 "/tags"
                 (replace-regexp-in-string
                  (regexp-quote "/") "-"
                  (replace-regexp-in-string "[/\\]*$" "" project-root-dir)))))

    (unless (f-exists? ret)
      (mkdir ret t))
    (f-full ret)))

(defun ac-php-get-tags-file ()
  "Get the actual project's tag file.

Returns a list where the 1st element will be a real path to the project,
and the 2nd element will be the a real path to the tags file.  Will return
nil when unable to read tags file.

This function checks for modification time of the procject's tags file.
If it is outdated, a re-index process will be performed."
  (ac-php--debug "Retrieving tags file...")
  (let ((project-root-dir (ac-php--get-project-root-dir))
        tags-file
        file-attr
        file-last-time
        now)
    (if project-root-dir
        (progn
          (setq tags-file (concat (ac-php--get-tags-save-dir project-root-dir) "tags.el")
                file-attr (file-attributes tags-file))

          (when file-attr
            (progn
              (ac-php--debug "Found tags file")
              (setq file-last-time (ac-php--get-timestamp (nth 5 file-attr))
                    now (ac-php--get-timestamp (current-time))))

            (when (and (> (- now file-last-time) ac-php-auto-update-intval))
              (progn
                (ac-php--debug "The tags file is out of date")
                (ac-php--remake-tags project-root-dir nil))))

          (list project-root-dir tags-file))
      nil)))

(defun ac-php--get-config-path-noti-str ( project-root-dir path-str)
  (if  (s-ends-with? "*.php" path-str )
      (format "php-path-list-without-subdir->%s" (f-relative (f-parent path-str) project-root-dir) )
    (format "php-path-list->%s" (f-relative path-str project-root-dir ))))


(defun ac-php--get-config (project-root-dir)
  "Get configuration related to a project.

Reads the configuration located at PROJECT-ROOT-DIR and returns it.
This function tries to recreate and / or populate configuration
file in case of its absence, or if it is empty."
  (ac-php--debug "Lookup for per-project configuration file...")
  (let (config-file-name)
    (setq config-file-name (f-join project-root-dir ac-php-config-file))

    ;; Lookup for `ac-php-config-file'
    (when (and
           (not (s-starts-with-p "/ssh:" config-file-name))
           (not (s-starts-with-p "/server:" config-file-name))
           (or (not (f-exists? config-file-name))
               (= (f-size config-file-name) 0)
               ;; Use case for "echo '' > `config-file-name'"
               (= (f-size config-file-name) 1)))
      (ac-php--debug "Configuration file either empty or absent. Creating...")
      (ac-php--json-save-data
       config-file-name
       '(:use-cscope nil
         :tag-dir nil
         :filter (:php-file-ext-list ("php")
                  :php-path-list (".")
                  :php-path-list-without-subdir []))))

    (json-read-file config-file-name)))

(defun  ac-php--get-use-cscope-from-config-file (project-root-dir)
  (let ( conf-list  )
    (setq conf-list  (ac-php--get-config project-root-dir) )
    (cdr (assoc-string "use-cscope" conf-list ))
    )
)


(defun ac-php-remake-tags ( )
  " reset tags , if  php source  is changed  "
  (interactive)
  ( ac-php--remake-tags  (ac-php--get-project-root-dir) nil )
)



(defun ac-php-remake-tags-all (  )
  "  remake tags without check modify time "
  (interactive)
  ( ac-php--remake-tags  (ac-php--get-project-root-dir) t)
)

(defun ac-php--remake-cscope (  project-root-dir all-file-list )
  "DOCSTRING"
  (let ( tags-dir-len save-dir)
    (when (and ac-php-cscope
               (or (ac-php--get-use-cscope-from-config-file  project-root-dir)
               ac-php-use-cscope-flag )
               )
      (ac-php--debug "ac-php--remake-cscope  %d"  (length  all-file-list) )
      (message "rebuild cscope  data file " )
      (setq tags-dir-len (length project-root-dir) )
      ;;write cscope.files
      (setq save-dir (ac-php--get-tags-save-dir  project-root-dir) )
      (let ((file-name-list ) cscope-file-name )
        (dolist (file-item all-file-list )
          (setq cscope-file-name (concat project-root-dir  (substring (nth  0 file-item ) tags-dir-len)  ))
          (push  cscope-file-name   file-name-list ))
        (f-write
         (s-join  "\n" file-name-list )
         'utf-8
         (concat  save-dir  "cscope.files" ) ))
      (shell-command-to-string
       (concat " cd " save-dir "  &&  cscope -bkq -i cscope.files  ") ) )
    ))



(defun  ac-php--get-obj-tags-dir( save-tags-dir )
    (concat  save-tags-dir "/tags_dir_" (getenv "USER") "/"))

(defun  ac-php--get-obj-tags-file-list( save-tags-dir )
  "DOCSTRING"
  (let ( (obj-tags-dir ( ac-php--get-obj-tags-dir save-tags-dir ) ))
    (if (not (file-directory-p obj-tags-dir ))
        (mkdir obj-tags-dir t))
    (ac-php-find-php-files obj-tags-dir  "\\.el$" t )
    ))

(defun ac-php-save-data (file data)
  (message "save to  %s ..." file)
  ;;(f-write  (format "%S" data ) 'utf-8  file)
  (with-temp-file file
    (let ((standard-output (current-buffer))
          (print-circle t) ; Allow circular data
          )
      (prin1 data)))
  )
(defun case-fold-string= (a b)
  (eq t (compare-strings a nil nil b nil nil t)))
(defun case-fold-string-hash (a)
  (sxhash (upcase a)))

(define-hash-table-test 'case-fold
  'case-fold-string= 'case-fold-string-hash)

(defun ac-php-load-data (tags-file project-root-dir)
  "Return the autocompleted data for the project.

This function tries to use the `ac-php-tag-last-data-list' variable to query the
 necessary data.  The `ac-php-tag-last-data-list' is used as a temporary
in-memory storage of all the tags.

The TAGS-FILE argument is used as an assoc key to search the data for the
project located at the PROJECT-ROOT-DIR.

If no data is found for the autocomplete, or the data is outdated, the tags file
will be loaded and the in-memory storage will be updated."
  (let ((file-attr (file-attributes tags-file))
        file-data
        tags-old-mtime
        tags-new-mtime
        class-map
        function-map
        inherit-map
        g-ac-php-tmp-tags)
    (when file-attr
      (setq tags-new-mtime (ac-php--get-timestamp (nth 5 file-attr))
            tags-old-mtime (nth 1 (assoc-string
                                   tags-file
                                   ac-php-tag-last-data-list)))

      (when (or (null tags-old-mtime) (> tags-new-mtime tags-old-mtime))
        (message (concat "ac-php: Reloading the autocompletion "
                         "data from the tags file..."))
        (load tags-file nil t)

        ;; `g-ac-php-tmp-tags' will be populated from tags.el file
        (setq file-data g-ac-php-tmp-tags)

        (assq-delete-all tags-file ac-php-tag-last-data-list)

        ;; The `file-data' here is an array of:
        ;;
        ;; - class map (list):     (aref file-data 0)
        ;; - function map (array): (aref file-data 1)
        ;; - inherit map (list):   (aref file-data 2)
        ;; - file list (array):    (aref file-data 3)
        ;;
        (setq class-map (make-hash-table :test 'case-fold)
              function-map (make-hash-table :test 'case-fold)
              inherit-map (make-hash-table :test 'case-fold))

        (mapc
         (lambda (class-item)
           (puthash (format "%s" (car class-item)) (cdr class-item) class-map)
           )

         (aref file-data 0))

        (mapc
         (lambda (function-item)
           (puthash (aref function-item 1) function-item function-map))
         (aref file-data 1))

        (mapc
         (lambda (inherit-item)
           (puthash  (format "%s" (car inherit-item ))
                     (cdr inherit-item) inherit-map))
         (aref file-data 2))

        (push (list tags-file
                    tags-new-mtime
                    (list class-map
                          function-map
                          inherit-map
                          (aref file-data 3)
                          project-root-dir))

              ac-php-tag-last-data-list)

        (message "ac-php: Reloading has been successfully finished")))

    (nth 2 (assoc-string tags-file ac-php-tag-last-data-list))))

(defun ac-php-g--class-map (tags-data ) (nth 0  tags-data ) )
(defun ac-php-g--function-map (tags-data ) (nth 1  tags-data ) )
(defun ac-php-g--inherit-map (tags-data ) (nth 2  tags-data ) )
(defun ac-php-g--file-list (tags-data ) (nth 3  tags-data ) )

(defun ac-php-get-tags-data ()
  "Load a tags data for the particular project."
  (let (tags-file project-root-dir (tags-definition (ac-php-get-tags-file)))
    (if tags-definition
        (progn
          (setq tags-file (nth 1 tags-definition)
                project-root-dir (nth 0 tags-definition)))
      (setq tags-file ac-php-common-json-file))
    (ac-php--debug "Loading tags file: %s" (ac-php--reduce-path tags-file 60))
    (if (file-exists-p tags-file)
        (ac-php-load-data tags-file project-root-dir)
      (progn
        (ac-php--debug (concat "The per-project tags file doesn't exist. "
                               "Starting create a new one..."))
        (ac-php-remake-tags)))))

(defun ac-php--get-project-root-dir ()
  "Get the project root directory of the curent opened buffer."
  (ac-php--debug "Lookup for the project root...")
  (let (project-root-dir tags-file (file-name buffer-file-name))

    ;; 1. Get working directory using `buffer-file-name' or `default-directory'
    (if file-name
        (setq project-root-dir (file-name-directory file-name))
      (setq project-root-dir (expand-file-name default-directory)))

    ;; 2. Expand real path of the obtained working directory (if enabled)
    (when ac-php-project-root-dir-use-truename
      (setq project-root-dir (file-truename project-root-dir)))

    ;; 3. Scan for the real project root of the opend file
    ;; We're looking either for the `ac-php-config-file' file
    ;; or the '.projectile' file, or the 'vendor/autoload.php' file
    (let (last-dir)
      (while
          (not (or
                (file-exists-p (concat project-root-dir ac-php-config-file))
                (file-exists-p (concat project-root-dir ".projectile"))
                (file-exists-p (concat project-root-dir "vendor/autoload.php"))
                (string= project-root-dir "/")))
        (setq last-dir project-root-dir
              project-root-dir (file-name-directory
                                (directory-file-name project-root-dir)))
          (when (string= last-dir project-root-dir)
            (setq project-root-dir "/"))))

    (when (string= project-root-dir "/")
      (progn
        (message "ac-php: Unable to resolve project root")
        (setq project-root-dir nil)))

    project-root-dir))

(defun ac-php--get-check-class-list ( class-name inherit-map  class-map )
  (let ( ret  )
    (setq ret  (nreverse ( ac-php--get-check-class-list-ex class-name  (ac-php-get-cur-namespace-name t)  inherit-map class-map nil )) )
    (ac-php--debug "XXXX check-class list:%S"  ret)
    ret
    ))

(defun  ac-php--check-global-name( name)
  (s-prefix-p  "\\"  name )
  )


(defun  ac-php--as-global-name( name)
  (if (ac-php--check-global-name name)
      name
    (concat   "\\" name )
  ))

(defun ac-php--get-check-class-list-ex ( class-name parent-namespace inherit-map class-map cur-list  )
  "DOCSTRING"

  (let ((check-class-list nil ) inherit-item  check-class-name )

    (ac-php--debug "  00 class-name=%s"  class-name )
    (unless (ac-php--check-global-name class-name )
      (setq check-class-name  (concat  parent-namespace "\\" class-name ))
      (ac-php--debug "  111 check-class-name=%s"  check-class-name )
      (unless (gethash check-class-name  class-map )
        (setq check-class-name  (concat   "\\" class-name ))
        (ac-php--debug " 222 check-class-name=%s"  check-class-name )
        (unless (gethash check-class-name  class-map )
          (ac-php--debug " 222 00 check-class-name= nil"  )
          (setq check-class-name nil)
          )
        )
      (setq class-name  check-class-name)
      )

    (when  class-name
      (setq inherit-item (gethash class-name inherit-map    ))

      (push class-name  check-class-list )
      (unless ( assoc-string class-name cur-list t )
        (push class-name cur-list )
        (let ( (i 0 ) (list-length (length inherit-item  )  ) item)
          (ac-php--debug  "check- inherit-item %S" inherit-item   )
          (while (< i list-length )
            (setq  item (aref  inherit-item   i )  )
            (ac-php--debug  "check- item %S" item   )
            (setq check-class-list (append
                                    (ac-php--get-check-class-list-ex
                                     item
                                     (ac-php--get-namespace-from-classname class-name )
                                     inherit-map
                                     class-map
                                     cur-list
                                     )
                                    check-class-list )  )
            (setq i (1+ i) )
            ))
        )
      check-class-list
      )))

(defun ac-php--get-item-info (member)
  "Recognize current MEMBER type.

This function tries to determine whether passed MEMBER is a method or a
property.  Return a cons cell `(MEMBER . TYPE)' where TYPE will be either
\"m\" (method) or \"p\" (property).

Note that this function does not perform in-depth analysis and its main task is
to determine whether the current MEMBER is a 'method call'.  All other cases are
considered at this stage as a 'property usage', although in fact they may not be."
  (ac-php--debug "Recognize current member type")
  (let (type-str)
    (if (and (> (length member) 1) (string= "(" (substring member -1)))
        (progn
          (setq type-str "m"))
      (setq type-str "p"))
    (list member type-str)))

(defun ac-php-get-class-member-return-type (class-map inherit-map  class-name member )
  "get class member return type from super classes "
  (let ((check-class-list ) (ret ) find-flag  type-str tmp-ret tag-type )
    (setq check-class-list  (ac-php--get-check-class-list class-name inherit-map  class-map ) )

    (setq tmp-ret (ac-php--get-item-info member ) )
    (setq member (nth 0 tmp-ret))
    (setq type-str (nth 1 tmp-ret))

    (let (  class-member-list )
      (cl-loop for opt-class in check-class-list do
        (setq  class-member-list  (nth 1 (assoc-string opt-class class-map  t )))
        ;;(ac-php--debug "member %s class=%s, %S" member opt-class  class-member-list )
        (cl-loop for member-info in class-member-list do
          (when (and  (ac-php--string=-ignore-care (nth 1 member-info ) member    )
                      (string= (nth 0 member-info)  "m")
                      (nth 4 member-info)
                      )
            (setq ret (nth 4 member-info) )

            (setq find-flag t)
            (cl-return)))
        (if find-flag (cl-return) )
        ))
    (ac-php--debug "return-type ac-php-get-class-member-info  ret=%S" ret)
    ret))


(defun ac-php-get-class-member-info (class-map inherit-map  class-name member )
  "DOCSTRING"
  (let ((check-class-list ) (ret ) find-flag  type-str tmp-ret tag-type )
    (setq check-class-list  (ac-php--get-check-class-list class-name inherit-map class-map) )

    (setq tmp-ret (ac-php--get-item-info member ) )
    (setq member (nth 0 tmp-ret))
    (setq type-str (nth 1 tmp-ret))
    (ac-php--debug " LLLLLLLLLLLLLLL:%S " tmp-ret )

    (let (  class-member-list )
      (cl-loop for opt-class in check-class-list do
               (ac-php--debug " LL:%s" opt-class  )
               (setq  class-member-list  (gethash opt-class class-map   ))
               (ac-php--debug "member %s class=%s, %S" member opt-class  class-member-list )
               (let ( (i 0 ) (list-length (length class-member-list )  ) member-info member-name )
                 (ac-php--debug " 55" )
                 (while (and (< i list-length ) (not ret))
                   (setq member-info (aref class-member-list i )  )
                   (when(ac-php--string=-ignore-care (aref member-info 1 ) member    )
                     (setq  ret member-info )
                     )
                   (setq i (1+ i) )
                   ))
               (if ret (cl-return) )
               ))

    (ac-php--debug "ac-php-get-class-member-info  ret=%S" ret)
    ret))


(defun ac-php-get-class-member-list (class-map inherit-map  class-name  )
  "DOCSTRING"
  (let ( (check-class-list ) (ret ) find-flag   )
    (setq check-class-list  (ac-php--get-check-class-list class-name inherit-map class-map) )
    (ac-php--debug "KKKK check-class-list %s = %S" class-name check-class-list)

    (let (  class-member-list unique-list member-name  )
      (ac-php--debug " 11 :%S"  check-class-list )
      (dolist (opt-class check-class-list)
        (ac-php--debug " 22" )
        (setq  class-member-list  (gethash  opt-class class-map  ) )

        (let ( (i 0 ) (list-length (length class-member-list )  ) member-info)
          (ac-php--debug " 55" )
          (while (< i list-length )
            (setq member-info (aref class-member-list i )  )
            (setq member-name  (aref member-info 1) )
            (unless (assoc-string  member-name   unique-list  t)
              (push member-info ret  )
              (push member-name unique-list )
              )
            (setq i (1+ i) )
            ))

        ))
    ret
    ))

(defun  ac-php--get-class-name-from-parent-define(  parent-list-str )
    " '\\Class1,interface1' => Class1  "
     (s-trim (aref (s-split ","  parent-list-str ) 1 ) )
)

(defun ac-php-get-class-name-by-key-list( tags-data key-list-str )
  (let (temp-class (cur-class "" )
                   (class-map (ac-php-g--class-map tags-data ) )
                   (inherit-map (ac-php-g--inherit-map tags-data ))
                   (key-list (split-string key-list-str "\\." ) ) )
    (ac-php--debug "====XXKK:%S " key-list )
    (cl-loop for item in key-list do
      (if (string= cur-class "" )
          (if (or (gethash  item inherit-map   ) (gethash  item class-map  )  )
              (setq cur-class item)
            (cl-return))
        (progn
          (setq temp-class cur-class)

          (if (string= item "__parent__" )
              (let (parent-list)
                (setq parent-list (gethash cur-class inherit-map  )  )

                (ac-php--debug "XXKK:%S " parent-list )

                (if parent-list
                    (setq cur-class (aref parent-list 0   ))
                  (setq cur-class "")
                ))

            (let ( member-info)
              (setq member-info (ac-php-get-class-member-info class-map inherit-map cur-class  item ))
              (setq cur-class (if  member-info
                                  (let (tmp-class cur-namespace relative-classname member-local-class-name )
                                    (setq tmp-class (aref member-info 4 ) )
                                    (ac-php--debug "tmp-class %s member-info:%S" tmp-class member-info )
                                    (when (stringp tmp-class )
                                      (if   (ac-php--check-global-name tmp-class )
                                          ;;  global name, like  \test\ss
                                          tmp-class
                                        (progn;; tmp-class like   test\ss
                                              ;; relative name, MUST be resolved relatively as  \cur-namespace\test\ss
                                              (setq member-local-class-name (aref member-info 5) )
                                              (setq cur-namespace (ac-php--get-namespace-from-classname member-local-class-name ))
                                              (setq relative-classname (concat cur-namespace "\\" tmp-class  ) )
                                              (ac-php--debug " 2 relative-classname %s " relative-classname )
                                              relative-classname
                                              ))
                                      ))
                                ""))

              ))

          (when (string= cur-class "")
              (message (concat " class[" temp-class "]'s member[" item "] not define type "))
            (cl-return))

          ))
      )
    cur-class
    ))
(defun ac-php--get-namespace-from-classname (classname)
  (nth 1 (s-match  "\\(.*\\)\\\\[a-zA-Z0-9_]+$" classname ) ) )

(defun ac-php-find-symbol-at-point-pri (tags-data &optional as-fn-p as-id-p)
  "Docstring."
  (let (key-str-list
        cur-word
        val-name
        class-name
        output-vec
        jump-pos
        cmd
        complete-cmd
        find-flag ret
        (project-root-dir (ac-php-g--project-root-dir tags-data)))

    ;; TODO: How about new line
    (if as-id-p
        (setq cur-word (ac-php--get-cur-word))
      (if as-fn-p
          (setq cur-word (concat (ac-php--get-cur-word) "("))
        (setq cur-word (ac-php--get-cur-word-with-function-flag))))

    (when cur-word
      (ac-php--debug "Current working string: \"%s\"" cur-word))

    (setq key-str-list (ac-php-get-class-at-point tags-data))

    (ac-php--debug "key-str-list==end:%s" key-str-list)

    (if  key-str-list
        (progn
          (let (class-name member-info  )
            ;;(setq key-str-list (replace-regexp-in-string "\\.[^.]*$" (concat "." cur-word ) key-str-list ))
            (when (string= cur-word "")
              (let ((key-arr (s-split "\\." key-str-list  ) ) )
                (ac-php--debug "key-arr %S " key-arr)
                (setq cur-word (nth (1- (length key-arr)) key-arr ))))

            (setq key-str-list (replace-regexp-in-string "\\.[^.]*$" "" key-str-list ))
            (ac-php--debug "class. key-str-list = %s "  key-str-list )
            (setq class-name (ac-php-get-class-name-by-key-list  tags-data key-str-list ))

            (ac-php--debug "class.member= %s.%s " class-name  cur-word )
            (if (not (string= class-name "" ) )
                (progn
                  (setq member-info (ac-php-get-class-member-info (ac-php-g--class-map tags-data )  (ac-php-g--inherit-map tags-data )  class-name cur-word ) )
                  (if member-info
                      (setq ret (list "class_member"  (aref member-info 3)  (aref member-info 4) member-info )  )
                    (progn
                      (message "no find %s.%s " class-name cur-word  )
                      )))
              ;;(message "no find class  from key-list %s " key-str-list  )
              )
            )
          )
      (progn ;;function
        (let ((function-map (ac-php-g--function-map tags-data  ))
               full-name tmp-ret file-pos  )

          (when (string= "" cur-word) ;;new
            (setq tmp-ret (ac-php-get-syntax-backward
                           (concat "new[ \t]+\\(" ac-php-re-namespace-unit-pattern "\\)")
                           :sexp 1))
            (when tmp-ret (setq cur-word   tmp-ret ))
            )
          ;;check "namespace" "use as"
          (setq full-name (ac-php--get-class-full-name-in-cur-buffer
                           cur-word
                           function-map  nil ) )

          (when full-name  (setq  cur-word  full-name) )

          ;;TODO FIX namespace function like Test\ff()
          (ac-php--debug "check user function===%s" cur-word )
          (when (string=  cur-word "self"  )
            (setq cur-word (concat (ac-php-get-cur-class-name)  ) )
            )

          (let  ( function-item )
            (setq  function-item (ac-php--get-item-from-funtion-map  cur-word function-map ))
            (when  function-item
              (setq ret (list "user_function" (aref  function-item 3)  (aref  function-item 4)   function-item  ) )
              )
            )
          )
        ))

    (ac-php--debug  "ac-php-find-symbol-at-point-pri :%S "  ret )
    ret
    ))

(defun ac-php--goto-local-var-def ( local-var )
  "goto local-var like vim - gd"
  (let ( )
    (ac-php--debug " local-var %s " local-var )
    (ac-php-location-stack-push)
    (beginning-of-defun)

    (re-search-forward (concat "\\" local-var "\\b"  ) ) ; => \\$var\\b
    (while (ac-php--in-string-or-comment-p (point))
      (re-search-forward (concat "\\" local-var "\\b"  ) ) ; => \\$var\\b
      )
    ;;(ac-php-location-stack-push)
    ))

(defun ac-php-find-symbol-at-point (&optional prefix)
  (interactive "P")
  ;;检查是类还是 符号
  (let ( (tags-data  (ac-php-get-tags-data ) )
         symbol-ret  type jump-pos  local-var  local-var-flag   )
    (setq local-var (ac-php-get-cur-word-with-dollar ) )
    (setq local-var-flag  (s-matches-p "^\\$"  local-var)  )


    (setq symbol-ret  (ac-php-find-symbol-at-point-pri tags-data) )

    (ac-php--debug "11goto  %s"  symbol-ret )
    (unless symbol-ret
      (setq symbol-ret (ac-php-find-symbol-at-point-pri tags-data t))
      )
    (ac-php--debug "22goto  %s"  symbol-ret )
    (unless symbol-ret
      (setq symbol-ret (ac-php-find-symbol-at-point-pri tags-data nil t))
      )
    (ac-php--debug "33goto  %s %s"  symbol-ret local-var-flag )


    (if symbol-ret
      (progn
        (ac-php--debug "goto  %s"  symbol-ret )
        (setq type (car symbol-ret ))
        (if   (and (not (string= type "class_member") ) local-var-flag  )
            (let ((item-info (nth 3 symbol-ret)) )
              (if  (string=  (nth 0  item-info ) "v")
                  (progn
                    (setq jump-pos  (nth 1  symbol-ret ) )
                    (ac-php-location-stack-push)
                    (ac-php-goto-location jump-pos )
                    ;;(ac-php-location-stack-push)
                    )
                ( ac-php--goto-local-var-def local-var  )
                )
              )
          (cond
           ((or (string= type "class_member") (string= type "user_function") )
            (let ((file-pos (nth 1 symbol-ret)) tmp-arr)
              (setq tmp-arr  (s-split ":" file-pos))
              (ac-php--debug " tmp-arr %S"  tmp-arr)
              (cond
               ((s-matches-p "sys" (nth 0 tmp-arr))
                (let( (sys-item-name (aref (nth 3 symbol-ret) 1))) ;;system function
                  ;; \trim( => trim
                  (if (string= type "user_function")
                      (setq sys-item-name
                            (substring-no-properties
                             sys-item-name 1
                             (if (string= "(" (substring  sys-item-name -1))
                                 -1 nil)))
                    ;; class name
                    (setq sys-item-name (nth 2 symbol-ret)))
                  (if (fboundp 'php-search-documentation)
                      (php-search-documentation sys-item-name)
                    (message "Unable to find php-search-documentation function"))))
               (t
                (let ((file-list (ac-php-g--file-list tags-data)))
                  ;; from  get index
                  (setq jump-pos
                        (concat
                         (aref file-list (string-to-number (nth 0 tmp-arr)))
                         ":" (nth 1 tmp-arr)))
                  (ac-php-location-stack-push)
                  (ac-php-goto-location jump-pos)))))))))
      (when local-var-flag (ac-php--goto-local-var-def local-var)))))

(defun ac-php-gen-def ()
  "DOCSTRING"
  (interactive)
  (let ((tags-data (ac-php-get-tags-data))
        line-txt
        (cur-word (ac-php--get-cur-word)))
    (setq line-txt (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position )))
    (if  (string-match ( concat  "$" cur-word ) line-txt)
        (let ((class-name "<...>" ) )
          (when (string-match (concat  cur-word"[\t ]*=[^(]*[(;]" ) line-txt)
            ;;call function
            (let (key-str-list  pos)
              (save-excursion
                (re-search-forward "[;]")
                (re-search-backward "[^ \t]" nil t)
                (setq pos (point) )
                )

              (when pos (setq key-str-list (ac-php-get-class-at-point pos ) ))

              (if  key-str-list ;;class-name
                  (setq class-name (ac-php-get-class-name-by-key-list tags-data  key-str-list ))
                (progn ;;function TODO

                  ))))

          (kill-new (concat "\n\t/**  @var  " class-name "  $" cur-word "  */\n") ))
      (kill-new (concat "\n * @property " cur-word "  $" cur-word "\n") ))))

(defun ac-php-location-stack-forward ()
  (interactive)
  (ac-php-location-stack-jump -1))

(defun ac-php-location-stack-back ()
  (interactive)
  (ac-php-location-stack-jump 1))

(defun ac-php-location-stack-jump (by)
  (let ((instack (nth ac-php-location-stack-index ac-php-location-stack))
        (cur (ac-php-current-location)))
    (if (not (string= instack cur))
        (ac-php-goto-location instack )
      (let ((target (+ ac-php-location-stack-index by)))
        (when (and (>= target 0) (< target (length ac-php-location-stack)))
          (setq ac-php-location-stack-index target)
          (ac-php-goto-location (nth ac-php-location-stack-index ac-php-location-stack) ))))))



(defun ac-php--get-array-string( arr arr-len index )
  (let ( v )
    (if ( < index arr-len    )
        (progn
          (setq v  (aref arr index ) )
          (if  v  v "" )
          )
      "")
 ))

(defun ac-php-candidate ()
  (let ( key-str-list  tags-data)
    (ac-php--debug "=== 1ac-php-candidate" )
    (setq  tags-data  (ac-php-get-tags-data )  )
    (setq key-str-list (ac-php-get-class-at-point tags-data ))
    (ac-php--debug "GET key-str-list  :%s" key-str-list)
    (if key-str-list
        (ac-php-candidate-class tags-data key-str-list  )
      (ac-php-candidate-other tags-data))
    ))

(defun ac-php--get-cur-word ()
  "Return a 'word' before current point.

The word 'word' means a combination of characters that forms a valid identifier
in PHP except the dollar sign.  Meant for `ac-php-find-symbol-at-point-pri'.

Examples:

  :-------------------------:--------------------:
  | If the point at the end | Will return        |
  :-------------------------:--------------------:
  | $someVariable           | someWariable       |
  | Acme\\Service\\Foo      | Acme\\Service\\Foo |
  | foo()->bar              | bar                |
  | foo()                   |                    |
  | 'some string'           |                    |
  :-------------------------:--------------------:

Return empty string if there is no valid sequence of characters.

Note: To conveniently describe in the documentation, double quotes (\") have
been replaced by '."
  (let (start-pos cur-word)
  (save-excursion
    (skip-chars-backward "a-z0-9A-Z_\\\\")
    (setq start-pos (point))
    (skip-chars-forward "a-z0-9A-Z_\\\\")
      (buffer-substring-no-properties start-pos (point)))))

(defun ac-php--get-cur-word-with-function-flag ()
  "Return a 'function' name before current point.

The word 'function' means a combination of characters that forms a valid
function name.  Meant for `ac-php-find-symbol-at-point-pri'.

Examples:

  :-------------------------:--------------------:
  | If the point at #       | Will return        |
  :-------------------------:--------------------:
  | function foo()#         |                    |
  | function foo(#          |                    |
  | function foo#()         | foo(               |
  | foo()->bar# ();         | bar(               |
  | fo#o()->bar ();         | foo(               |
  :-------------------------:--------------------:

Return empty string if there is no valid sequence of characters."
  (let (start-pos cur-word)
    (save-excursion
      (skip-chars-backward "a-z0-9A-Z_\\\\")
      (setq start-pos (point))
      (skip-chars-forward "a-z0-9A-Z_\\\\")
      (skip-chars-forward " \t")
      (skip-chars-forward "(")
      (s-replace-all '((" " . "")
                       ("\t" . ""))
                     (buffer-substring-no-properties start-pos (point))))))

(defun ac-php-get-cur-word-with-dollar ( )
  (let (start-pos cur-word)
  (save-excursion
    (skip-chars-backward "\\$a-z0-9A-Z_")
    (setq start-pos (point))
    (skip-chars-forward "\\$a-z0-9A-Z_")
      (buffer-substring-no-properties start-pos (point))
    )
    ))

(defun ac-php-get-cur-word-without-clean ( )
  (let (start-pos cur-word)
  (save-excursion
    (skip-chars-backward "\\$a-z0-9A-Z_\\\\")
    (setq start-pos (point))
    (skip-chars-forward "\\$a-z0-9A-Z_\\\\")
    )
      (buffer-substring-no-properties start-pos (point))
    ))

(defun ac-php-show-tip(&optional prefix)
  (interactive "P")
  ;;检查是类还是 符号
  (let (
        (tags-data  (ac-php-get-tags-data ) )
        symbol-ret
        type  doc class-name access return-type member-info tag-name function-item file-pos )
    (setq symbol-ret (ac-php-find-symbol-at-point-pri tags-data))
    (when symbol-ret
      (setq type (car symbol-ret ))
      (setq member-info (nth 3 symbol-ret))
      (cond
       ((string= type "class_member")

        (setq tag-name  (aref member-info  1))
        (if ( string= (aref member-info 0 )  "m" )
            (setq  doc   (concat  tag-name   (aref member-info 2) ")" )   )
          (setq  doc    tag-name ))

        (setq  class-name    (aref member-info 5) )
        (setq  return-type   (aref member-info 4) )
        (setq  access   (aref member-info 6) )
        (popup-tip (concat  (ac-php-clean-document doc)  "\n\t[  type]:"  return-type  "\n\t[access]:" access  "\n\t[  from]:"   class-name   ))

        )
       ((string= type "user_function")
        (setq function-item (nth 3 symbol-ret))
        (setq tag-name  (aref function-item 1 ))
        (if ( ac-php--tag-name-is-function   tag-name )
            (setq  doc   (concat  tag-name  (aref function-item 2) ")" )   )
          (setq  doc   (aref function-item 2) ))

        (setq file-pos (aref function-item 3) )

        (setq  return-type (aref function-item 4) )
        (popup-tip (concat "[" (if (string= "S" file-pos ) "system" "  user" )  "]:"  (ac-php-clean-document doc) "\n[  type]:"  return-type   ))

        )) )))

(defun ac-php-cscope-find-egrep-pattern (symbol)
  "Set `cscope-initial-directory' and run egrep over the cscope database."
  (interactive
   (list
    (let (cscope-no-mouse-prompts)
      (cscope-prompt-for-symbol "Find this egrep pattern " nil t t))))
  (let ((project-root-dir (ac-php--get-project-root-dir)))

    (if (or ac-php-use-cscope-flag
            (ac-php--get-use-cscope-from-config-file project-root-dir))
        (progn
          (setq cscope-initial-directory
                (ac-php--get-tags-save-dir project-root-dir))
          (cscope-find-egrep-pattern symbol))
      (message "need config: %s -> use-cscope:true" ac-php-config-file))))

(defun ac-php-eldoc-documentation-function ()
  "A function to provide ElDoc support.

Returns a doc string appropriate for the current context, or nil.
See `eldoc-documentation-function' for what this function is
supposed to do."

  (interactive "P")

  (let ((tags-data (ac-php-get-tags-data))
        symbol-ret
        type
        doc
        class-name
        access
        return-type
        member-info
        tag-name
        function-item
        file-pos
        member-info-len)
    (when tags-data
      (setq symbol-ret (ac-php-find-symbol-at-point-pri tags-data))
      (when symbol-ret
        (setq type (car symbol-ret ))
        (setq member-info (nth 3 symbol-ret))
        (cond
         ((string= type "class_member")

          (setq member-info-len (length member-info ) )
          (setq tag-name  (aref  member-info 1))
          (if ( string= (aref member-info 0 )  "m" )
              (setq  doc   (concat
                            (propertize  tag-name  'face 'font-lock-function-name-face)
                            (aref  member-info 2) ")" )   )
            (setq  doc
                   (propertize  tag-name  'face 'font-lock-variable-name-face)
                   ))

          (setq  class-name    (ac-php--get-array-string member-info  member-info-len 5) )
          (setq  return-type   (aref member-info 4) )
          (setq  access   (ac-php--get-array-string member-info  member-info-len 6)  )
          (concat
           (propertize  access 'face 'font-lock-keyword-face ) "  " class-name "::" doc   ":" return-type   )

          )
         ((string= type "user_function")
          (setq function-item (nth 3 symbol-ret))
          (setq tag-name  (aref  function-item 1 ))
          (if ( ac-php--tag-name-is-function   tag-name )
              (setq  doc   (concat
                            (propertize (substring  tag-name 0 -1 ) 'face 'font-lock-function-name-face)
                            "(" (aref function-item 2) ")" )   )
            (setq  doc
                   (propertize (aref function-item 2) 'face 'font-lock-variable-name-face)))

          (setq file-pos (aref function-item 3) )

          (setq  return-type (aref function-item 4) )

          (concat  doc ":"  return-type   )

          )) )

      )
    ))

(defun ac-php-show-cur-project-info ()
  "show current project ac-php info "
  (interactive)
  (let ( (tags-arr (ac-php-get-tags-file )) tags-file  project-root-dir  file-attr  file-last-time   )
    (if tags-arr
        (progn
          (setq tags-file   (nth 1 tags-arr)   )
          (setq project-root-dir (nth 0 tags-arr) )
          )
      (setq tags-file   ac-php-common-json-file   )
      )
    (when  tags-file
      (setq file-attr   (file-attributes   tags-file ) )
      (setq file-last-time (format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 file-attr)  ) )
      )
    (message (concat "root dir          : %s\n"
                     "config file       : %s%s\n"
                     "tags file         : %s\n"
                     "tags last gen time: %s")
             project-root-dir
             project-root-dir
             ac-php-config-file
             tags-file
             file-last-time)))

;;; Initialization

(define-minor-mode ac-php-mode
  "Minor mode to enable autocompletion for the PHP language.

When called interactively, toggle `ac-php-mode'.  With prefix
ARG, enable `ac-php-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `ac-php-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `ac-php-mode'.
Otherwise behave as if called interactively.

Usually you shouldn't call this function manually.  It will be
called from Lisp when necessary."

  ;; The indicator for the mode line.
  :lighter ac-php-mode-line
  ;; The minor mode should be buffer-local
  :global nil
  ;; Custom group name to use in all generated ‘defcustom’ forms
  :group 'ac-php
  ;; The initial value
  (cond
   (ac-php-mode
    ;; Enable `ac-php-mode'
    (setq ac-php-gen-tags-flag t))
   (t
    ;; Disable `ac-php-mode'
    (setq ac-php-gen-tags-flag nil))))

;;;###autoload
(defun ac-php-core-eldoc-setup ()
  "Enable the ElDoc support for the PHP language.
Configure the variable `eldoc-documentation-function' and
call the command `eldoc-mode'."
  (interactive)

  (setq-local eldoc-documentation-function
              #'ac-php-eldoc-documentation-function)
  (eldoc-mode +1))

(provide 'ac-php-core)
;;; ac-php-core.el ends here
