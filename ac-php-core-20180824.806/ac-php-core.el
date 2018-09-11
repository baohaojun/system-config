;;; ac-php-core.el ---  gen tags for php

;; Copyright (C) 2014 - 2016 jim

;; Author: xcwenn@qq.com [https://github.com/xcwen]
;; URL: https://github.com/xcwen/ac-php
;; Keywords: completion, convenience, intellisense

;; Package-Requires: ((emacs "24") (dash "1") (php-mode "1")   (xcscope "1") (s "1") (f "0.17.0") (popup "0.5.0") )

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

;; thanks auto-complete-clang , rtags ( ac-php-location-stack-index ) , auto-java-complete  ( ac-php-remove-unnecessary-items-4-complete-method   )

;;; Commentary:
;; Auto Completion source for php.
;; Only support  Linux and OSX , not support Windows
;; More info and **example** at : https://github.com/xcwen/ac-php
;;

;;; Code:

(require 'json)
(require 's) ;;https://github.com/magnars/s.el
(require 'f) ;;https://github.com/rejeep/f.el


;;(require 'php-mode)
(require 'popup)
(require 'dash)
(require 'eldoc)

(defvar ac-php-php-executable (executable-find "php") )

(defvar ac-php-root-directory (file-name-directory (or load-file-name buffer-file-name)))
(defvar ac-php-ctags-executable (concat   ac-php-root-directory "phpctags"))
(defvar ac-php-common-json-file (concat   ac-php-root-directory "ac-php-comm-tags-data.el"))


(defvar ac-php-debug-flag nil)


(defmacro ac-php--debug (  fmt-str &rest args )
  `(if ac-php-debug-flag
       (message (concat "[AC-PHP-DEBUG]:" ,fmt-str ) ,@args )
       ))

(defvar ac-php-use-cscope-flag nil)

(defvar ac-php-cscope (executable-find "cscope"))

(defvar ac-php-prefix-str "")

(defvar ac-php-auto-update-intval 3600
  "auto remake tag intval ( seconds) " )

(defvar ac-php-phptags-index-progress  0 )

;;data
(defvar ac-php-tag-last-data-list nil) ;;(("/home/xxx/projecj/.tags".(  1213141011  data )   ))

(defvar ac-php-word-re-str "[0-9a-zA-Z_\\]+" )


(defvar ac-php-location-stack-index 0)
(defvar ac-php-location-stack nil)
(defvar ac-php-gen-tags-flag  nil )

(defvar ac-php-tags-path (concat (getenv "HOME") "/.ac-php")
  "PATH for tags to be saved, default value is \"~/.ac-php\" as base for
directories.

This path get extended with the directory tree of the project that you are
indexing the tags for.")

(defvar ac-php-project-root-dir-use-truename t
  "  project-root-dir use truename  no link  ")

(defvar ac-php-max-bookmark-count 500 )
(defun ac-php-location-stack-push ()
  (let ((bm (ac-php-current-location)))
    (if  ( functionp  'xref-push-marker-stack)
        (xref-push-marker-stack)
      (ring-insert find-tag-marker-ring (point-marker))
      )
    (while (> ac-php-location-stack-index 0)
      (decf ac-php-location-stack-index)
      (pop ac-php-location-stack))
    (unless (string= bm (nth 0 ac-php-location-stack))
      (push bm ac-php-location-stack)
      (if (> (length ac-php-location-stack) ac-php-max-bookmark-count)
          (nbutlast ac-php-location-stack (- (length ac-php-location-stack) ac-php-max-bookmark-count)))))
  )

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


(defun ac-php-check-not-in-string-or-comment (pos)
  "ac-php-check-not-in-string-or-comment"
  (save-excursion (if  (nth 8 (syntax-ppss pos))  nil  t ))
  )
(defun ac-php-check-not-in-comment (pos)
  "ac-php-check-in-string-or-comment"
  (save-excursion (if  (nth 4 (syntax-ppss pos))  nil  t ))
  )


(defun ac-php-split-string-with-separator(str regexp &optional replacement omit-nulls)
  "this function is a tool like split-string,
  but it treat separator as an element of returned list
  for example (ac-php-split-string-with-separator abc.def.g \"\\.\" \".\")
  will return '(\"abc\" \".\" \"def\" \".\" \"g\" )"
  (when str
    (let (split-list  substr match-end)
      (if  (string-match regexp str)
          (progn
            (while (string-match regexp  str)
              (setq match-end (match-end 0))
              (setq  substr (substring-no-properties str 0 (- match-end 1)))
              (when (or (not omit-nulls) (> (length substr ) 0))
                (setq split-list (append split-list (list  substr))) )
              (setq split-list (append split-list (list (or replacement regexp))))
              (setq str (substring-no-properties str  match-end)))
            (when (or (not omit-nulls) (> (length str ) 0))
              (setq split-list (append split-list (list str)))))
        (setq split-list (list str)))
      split-list)))
(defun ac-php--get-clean-node( paser-data &optional check-len )
    "clean  before ';'  "
    (let ((i 0 )  ret-data  item )
      (unless check-len
        (setq check-len (length paser-data) ) )
    (while (< i check-len )
      (setq item (nth i paser-data ) )
      (if (and (stringp item   )
               (string= item ";" )
               )
          (setq ret-data nil)
        (push item ret-data)
        )
      (setq i (1+ i))
      )


    (ac-php--debug "clean-node:%S" ret-data)
    (reverse ret-data)
    ))
(defun ac-php--get-node-paser-data ( paser-data)
  (let ((last-item (nth (1- (length paser-data)) paser-data )) ret-data )

    (if ( and ( stringp last-item )
              (string=  last-item  "__POINT__" ))
        (let ((i 0 )
              (check-len (1- (length paser-data)))
              item )
          (setq ret-data (ac-php--get-clean-node  paser-data check-len ) )
            )
      (when last-item
        (setq ret-data (ac-php--get-node-paser-data last-item )) ))
    ret-data
    ))
(defun ac-php--get-key-list-from-paser-data( paser-data)
  (let (
        (frist-key (nth 0  paser-data ))
        item
        (i 1)
        (ret)
        (len-paser-data (length paser-data)))
    ;;处理list


    (if (and (listp  frist-key)  frist-key   )
        (progn
          (setq ret  (ac-php--get-clean-node (ac-php--get-key-list-from-paser-data frist-key )) )
        )
      (if  (and  (> len-paser-data 1)  (not (nth 1 paser-data  )) )
          (setq ret  (list (concat frist-key "(" )) )
        (setq ret  (list frist-key) )
        )
      )


    (setq i 1)
    (while (< i len-paser-data)
      (setq item (nth i paser-data) )
      (cond
       ((and (stringp item) ( and (<(1+ i) len-paser-data ) (listp (nth (1+ i) paser-data))  ) )
        ;;function
        (setq ret (append ret  (list (concat item "("))))
        (setq i (+ i 2) ))

       ((stringp item)
        ;;var
        (setq ret (append ret  (list item)))
        (setq i (1+ i) ))
       (t (setq i (1+ i) ))
      ))
    ret
    ))

(defun ac-php-remove-unnecessary-items-4-complete-method (splited-line-items)
  (let ((need-add-right-count 1  )
        ( item-count (length splited-line-items ))
        (i 0)
        item
        (elisp-str "(")
        paser-data
        ret
        )

    (while (< i item-count )
      (setq item (nth i splited-line-items) )
      (cond
       ((string= "(" item )
        (setq elisp-str (concat elisp-str "(" )   )
        (setq need-add-right-count (1+ need-add-right-count ) )
        )
       ((string= ")" item )
        (setq elisp-str (concat elisp-str ")" )   )
        (setq need-add-right-count (1- need-add-right-count ) )
        )
       (t
        (setq elisp-str (concat elisp-str "\"" (s-replace "\\"  "\\\\" item )  "\" " )  )))
      (setq i (1+ i))
      )

    (if (> need-add-right-count  0)
        (progn
          (setq  elisp-str (concat  elisp-str "\"__POINT__\"") )
          (setq i 0)
          (while (< i need-add-right-count )
            (setq  elisp-str (concat  elisp-str ")" ))

            (setq i (1+ i))
            )
          )
      (setq  elisp-str "()"  ))
     (setq paser-data (read  elisp-str) )
     (setq paser-data (ac-php--get-node-paser-data  paser-data  ))
     (setq ret (ac-php--get-key-list-from-paser-data  paser-data) )
     ret
    ))

(defun ac-php--get-class-full-name-in-cur-buffer ( first-key function-map get-return-type-flag)
    "DOCSTRING"
  (let (cur-namespace tmp-name ret-name tmp-ret)
    (let (  split-arr   cur-class-name )
      (ac-php--debug " ac-php--get-class-full-name-in-cur-buffer  frist-key:%s" first-key )


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


(defun ac-php-split-line-4-complete-method(line-string  )
  "this function is used to complete method ,first this function will split line-string to small items
for example : suppose line-string is
System.getProperty(str.substring(3)).to
then this function split it to
'System' '.' 'getProperty' '(' 'str' '.' 'substring' '(' '3' ')' ')' '.' 'to' "
  (save-excursion
    (let (  (stack-list nil))

      (setq line-string  (replace-regexp-in-string   "\".*?\"" "String" line-string))
      (setq line-string  (replace-regexp-in-string   "[.]"   ";"       line-string))
      ;;  do : without ::

      (setq line-string  (replace-regexp-in-string   "\\([^:]\\):\\([^:]\\)"   ";\\1"  line-string))
      (setq line-string  (replace-regexp-in-string   "[ \t]*->[ \t]*" "."       line-string))
      (setq line-string  (replace-regexp-in-string   "[ \t]*::[ \t]*" "::."       line-string))

      (setq line-string  (replace-regexp-in-string   "\\bnew\\b\\|\\breturn\\b\\|\\becho\\b\\|\\bcase\\b\\|\\byield\\b"    ";"  line-string))

      (setq line-string  (replace-regexp-in-string   "\\$" ""  line-string))
      (setq line-string  (replace-regexp-in-string   "@\\|!?=>?\\|<=?\\|>=?\\|=" ";"  line-string))
      (setq line-string  (replace-regexp-in-string    "[&|!,?^+/*\-]"  ";"  line-string))


      ;;split line-string with "." ,but add "." as an element at its position in list
      (setq stack-list (ac-php-split-string-with-separator  line-string "[ \t]*\\.[ \t]*"  "." t))
      ;;split each element  with "(" ,but add "(" as an element at its position in list
      ;;and merge all the list in a list
      (let((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ac-php-split-string-with-separator ele "[{}]"  ";"  t))))
        (setq stack-list tmp-list))


      (let((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ac-php-split-string-with-separator ele "[>)]\\|]"  ")"  t))))
        (setq stack-list tmp-list))

      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ac-php-split-string-with-separator ele "[<([]"  "("  t))))
        (setq stack-list tmp-list))


      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (ac-php-split-string-with-separator ele ";"  ";"  t))))
        (setq stack-list tmp-list))



      (let ((ele)(tmp-list))
        (dolist (ele stack-list)
          (setq tmp-list (append tmp-list (split-string ele "[ \t]+"  t))))
        (setq stack-list tmp-list))

      stack-list

      ))
  )


(defun ac-php-get-syntax-backward ( re-str  pos  &optional  in-comment-flag  min-pos )
  "DOCSTRING"
  (let (line-txt ret-str find-pos need-find-flag  old-case-fold-search  search-pos)
    (setq  old-case-fold-search case-fold-search )
    (setq need-find-flag t )

    (save-excursion
      (while  need-find-flag
        (if (setq search-pos (re-search-backward  re-str  min-pos t 1) )
            (progn
              (when (if in-comment-flag
                        (not (ac-php-check-not-in-comment (point) ) )
                      (ac-php-check-not-in-string-or-comment (point)))
                (setq line-txt (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                (when (string-match   re-str    line-txt)
                  (setq  ret-str  (match-string pos line-txt))
                  (setq  ret-str (propertize ret-str 'pos  search-pos))

                  (setq need-find-flag nil))))
          (setq need-find-flag nil)
          )
        )
      )
    (setq   case-fold-search old-case-fold-search )
    (ac-php--debug "regstr=:%s; min-pos=%S ret-str:%s" re-str min-pos ret-str )
    ret-str
    ))




(defun ac-php-get-cur-class-name ()
  "DOCSTRING"
  ( ac-php-get-syntax-backward "^[ \t]*\\(abstract[ \t]+\\|final[ \t]+\\)*\\(class\\|trait\\)[ \t]+\\([a-zA-Z0-9_]+\\)" 3 ))
(defun ac-php-clean-namespace-name (namespace-name)
  (if (and (stringp namespace-name)
           (> (length namespace-name)   1)
           ( string=  (substring-no-properties  namespace-name 0 1  ) "\\" ) )
        ( substring-no-properties namespace-name 1 )
      namespace-name))

(defun ac-php-get-cur-full-class-name ()
  "DOCSTRING"
  (let (class-name namespace-name )
    (setq class-name (ac-php-get-cur-class-name) )
    (setq namespace-name   (ac-php-get-cur-namespace-name) )

    (if class-name
        (concat namespace-name   class-name )
      nil
        )))


(defun ac-php-get-cur-namespace-name ( &optional not-need-end-flag )
  (interactive)
  "DOCSTRING"
  (let (namespace  )
    (setq namespace
          (or
           ( ac-php-get-syntax-backward  (concat "^[ \t]*namespace[ \t]+\\(" ac-php-word-re-str "\\)")  1  )
           ( ac-php-get-syntax-backward  (concat "<\\?php[ \t]+namespace[ \t]+\\(" ac-php-word-re-str "\\)")  1  )
           ))
    (if not-need-end-flag
        (if namespace  (concat  "\\"  namespace  )  "" )
      (if namespace  (concat  "\\"  namespace "\\" )  "\\" )
      )
    ))


(defun ac-php-get-use-as-name (item-name   )
  "DOCSTRING"
  (let ( use-name )
    (setq item-name (nth 0 (s-split "(" item-name  )) )
    (setq use-name (or
                    ( ac-php-get-syntax-backward (concat "^[ \t]*use[ \t]+\\(" ac-php-word-re-str "\\\\" item-name "\\)[ \t]*;") 1  nil  )
                    ( ac-php-get-syntax-backward (concat "^[ \t]*use[ \t]+\\(" ac-php-word-re-str "\\)[ \t]+as[ \t]+" item-name "[ \t]*;" ) 1 nil ) )
          )
    ))

(defun ac-php--get-all-use-as-name-in-cur-buffer () "make a regex to match   use statements "
  (let ( ret-list (search-re (concat "use[ \t]+" ac-php-word-re-str ".*;")  ) line-txt match-ret )
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward search-re  nil t )
          (setq line-txt (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position )))
          (ac-php--debug "line-text:%s" line-txt)

           (setq match-ret (s-match   (concat "use[ \t]+\\(" ac-php-word-re-str "\\)[ \t]+as[ \t]+\\("ac-php-word-re-str "\\)[ \t]*;") line-txt ))
          (if match-ret
              (add-to-list 'ret-list (list    (ac-php--as-global-name (nth 1 match-ret)) (nth 2 match-ret)   ))
            (progn
              (setq match-ret (s-match   (concat "use[ \t]+\\(" ac-php-word-re-str "\\)[ \t]*;") line-txt ))
              (when match-ret
                (let ((key-arr (s-split "\\\\" (nth 1 match-ret) ) ))
                  (ac-php--debug "key-arr %S " key-arr)

                  (add-to-list 'ret-list (list  (ac-php--as-global-name  (nth 1 match-ret))  (nth (1- (length key-arr)) key-arr )   ))))))

          (end-of-line))))
    ret-list ))
(defun ac-php-toggle-debug ( )
    "DOCSTRING"
    (interactive)
  (let ()
    (setq ac-php-debug-flag  (not ac-php-debug-flag) )
    (setq debug-on-error ac-php-debug-flag  )
    (message "set:  ac-php-debug-flag: %s " ac-php-debug-flag )
    ))

(defun ac-php-test ()
    "DOCSTRING"
  (interactive)
  (let (v)
    (message "==%s"
             (ac-php--check-is-comment  (point) )
             ) ;ss
    ))





(defun ac-php--check-is-comment (pos )
  (let ( ( face (get-text-property pos 'face) ) )
    (and   face
         (or
          (equal face font-lock-comment-delimiter-face)
          (equal face font-lock-comment-face)
          ))
    )
  )

(defun ac-php-get-class-at-point( tags-data  &optional pos  )

  (let (line-txt
        old-line-txt  key-line-txt  key-list   tmp-key-list first-class-name  frist-key  ret-str frist-key-str
        reset-array-to-class-function-flag )

    ;; default use cur point
    (unless  pos (setq pos (point) ))

    (setq line-txt (s-trim (buffer-substring-no-properties
                    (line-beginning-position)
                     pos  )))
    ;;; join   $this->xxx()
    ;;;             ->yyy();
    (save-excursion
      (while  (and (> (length line-txt   ) 0 ) (= (aref line-txt 0 ) ?- ) )
        (previous-line)
        (let ( (no-comment-code "") (text-check-pos (line-beginning-position)) (line-end-pos (line-end-position )) )
          (while (< text-check-pos line-end-pos )
            (unless (ac-php--check-is-comment text-check-pos  )
              (setq  no-comment-code (concat  no-comment-code (buffer-substring-no-properties text-check-pos  (1+ text-check-pos)  ) ))
              )
            (setq text-check-pos  (1+ text-check-pos) )
            )
          (setq line-txt (concat
                          (s-trim  no-comment-code  )
                          line-txt ) )
          )
        )
      )

    (ac-php--debug "11 line-txt:%s " line-txt )

    (setq old-line-txt line-txt)
    ;;  array($xxx, "abc" ) => $xxx->abc
    ;;(setq line-string  (replace-regexp-in-string   ".*array[ \t]*([ \t]*\\(\\$[a-z0-9A-Z_]+\\)[ \t]*,[ \t]*['\"]\\([a-z0-9A-Z_]*\\).*"   "\\1->\\2"  "$server->on('sdfa',array($this, \"sss" ))
    (setq line-txt (replace-regexp-in-string   ".*array[ \t]*([ \t]*\\(\\$[a-z0-9A-Z_> \t-]+\\)[ \t]*,[ \t]*['\"]\\([a-z0-9A-Z_]*\\).*"   "\\1->\\2"  line-txt ))
    (setq line-txt (replace-regexp-in-string   ".*\\[[ \t]*\\(\\$[a-z0-9A-Z_> \t-]+\\)[ \t]*,[ \t]*['\"]\\([a-z0-9A-Z_]*\\).*"   "\\1->\\2"  line-txt ))
    (ac-php--debug "line-txt:%s" line-txt)

    (setq reset-array-to-class-function-flag (not (string= line-txt old-line-txt )))

    (if (or (ac-php-check-not-in-string-or-comment pos)
            reset-array-to-class-function-flag
            )
        (progn
          (setq key-list (ac-php-remove-unnecessary-items-4-complete-method
                          (ac-php-split-line-4-complete-method line-txt )))

          (ac-php--debug "ac-php-remove-unnecessary-items-4-complete-method:%S" key-list  )
          ;; out of function like : class Testb  extends test\Testa[point]
          (if (not (and (stringp (nth 1 key-list ) )
                        (string= "."  (nth 1 key-list )  )
                        ))
              (setq  key-list nil )))
      (setq  key-list nil ))



    (when  key-list
      (setq frist-key-str (nth 0 (ac-php--get-item-info (nth 0  key-list)   )))
      ;;检查 ::
      (if (and (string-match  "::"  frist-key-str  ) (not (string-match  "\\/\\*"  line-txt ) ))
          (progn
            (setq frist-key (substring-no-properties  frist-key-str  0 -2  ) )
            (setq first-class-name  frist-key  )
            (cond
             ((string= frist-key "parent" )
              (setq first-class-name (concat (ac-php-get-cur-full-class-name) ".__parent__" ) ))
             ((or (string= frist-key "self" ) (string= frist-key "static" )   )
              (setq first-class-name (concat (ac-php-get-cur-full-class-name) ) ))
             ((string-match  "\$[a-zA-Z0-9_]*[\t ]*::" old-line-txt  )  (setq first-class-name nil))
             ))
        (progn
          (setq frist-key  frist-key-str )

          (when (and(not first-class-name) (or (string= frist-key "this")  ) )
            (setq first-class-name (ac-php-get-cur-full-class-name)  ))


          (ac-php--debug " 00 first-class-name  %s" first-class-name)
          ;;check for new define  /* @var $v  class_type  */
          (unless first-class-name
            (setq first-class-name
                                     (ac-php-get-syntax-backward
                                      (concat "@var[\t ]+\\("
                                              ac-php-word-re-str "\\)[\t ]+$" frist-key )
                                      1 t
                                      (save-excursion  (beginning-of-defun)  (beginning-of-line) ))))


          ;;check  function xxx (classtype $val)
          ;;check   catch ( classtype $val)
          (unless first-class-name
            (setq first-class-name
                                     (ac-php-get-syntax-backward
                                      (concat "\\(" ac-php-word-re-str "\\)" "[\t ]+\\(&\\)?$" frist-key  "[ \t]*[),]" )
                                      1 nil
                                      (save-excursion  (beginning-of-defun) (beginning-of-line)  ))))


          ;;check for @param  \Illuminate\Http\Request  $request
          (unless first-class-name
            (setq first-class-name
                  (ac-php-get-syntax-backward
                   (concat "@param[\t ]+"  "\\("
                           ac-php-word-re-str "\\)[\t ]+$" frist-key  )
                   1 t
                   (save-excursion  (beginning-of-defun)  (beginning-of-line) ))))




          (ac-php--debug " 11 first-class-name  %s" first-class-name)

          ;; check $v = new .... or $v = $this->sadfa() ;
          (unless first-class-name
            (let (define-str symbol-ret symbol-type )
              (setq define-str (ac-php-get-syntax-backward
                                (concat   "$" frist-key "[\t ]*=\\([^=]*\\)[;]*" )
                                1 nil
                                (save-excursion  (beginning-of-defun)  (beginning-of-line)  )) )
              (when define-str
                (save-excursion
                  (goto-char (get-text-property 0 'pos  define-str))
                  (end-of-line)

                  (setq line-txt (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position )))


                  (if  (string-match "(" line-txt)
                      (let ( beginning-of-line-pos )
                        (ac-php--debug "XXXXXX:%s" line-txt)
                        (beginning-of-line )
                        (setq beginning-of-line-pos (point))
                        (re-search-forward ".[ \t]*(" ) ;; func
                        ( re-search-backward "[a-zA-Z_0-9][ \t]*(" )
                        (ac-php--debug "XXXXXX:pos22=[%s]" (buffer-substring-no-properties beginning-of-line-pos (point)  )  )
                      )
                    (re-search-backward ".[ \t]*;" ) ;;  p
                      )
                  ;;(backward-char 1)
                  (ac-php--debug " ===== define-str :%s pos=%d check_pos=%d"  define-str (get-text-property 0 'pos  define-str) (point) )
                  (setq symbol-ret (ac-php-find-symbol-at-point-pri tags-data))
                  (unless symbol-ret
                    (setq symbol-ret (ac-php-find-symbol-at-point-pri tags-data nil t))
                    )

                  (when symbol-ret
                    (setq symbol-type  (car symbol-ret) )
                    (ac-php--debug "XXLLL %s" symbol-type )
                    (when (or (string= symbol-type "class_member" )
                            (string= symbol-type "user_function" ) )

                      (setq first-class-name  (nth 2 symbol-ret)  )

                      )
                    )
                ))
            ))


          (unless first-class-name (setq first-class-name frist-key)))))


    ;;fix use-as-name ,same namespace
    (when ( and first-class-name
                (= 1 (length  (s-split "\\."  first-class-name ) ) )
                )
      (setq first-class-name (ac-php--get-class-full-name-in-cur-buffer
                              first-class-name
                              (ac-php-g--function-map tags-data  ) t ) ))




    (ac-php--debug "22===first-class-name :%s" first-class-name)

    (if first-class-name
        (progn
          (setq ret-str  (concat first-class-name ))

          (dolist (field-value (cdr key-list) )
            ;;(when  (not (string= "." field-value))
            (setq ret-str  (concat  ret-str  field-value )))
          ;;)
          (setq ret-str ( ac-php--as-global-name  ret-str ))
          )
      (if (>(length   key-list ) 1) "null" nil) )))



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

  (let (ret-list ( cur-word  (ac-php-get-cur-word-without-clean )) cur-word-len  cmp-value  start-word-pos (function-map (ac-php-g--function-map tags-data  )  ) key-word func-name  )

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
      (beginning-of-defun )
      (setq  start-pos (point)   )
      (end-of-defun )
      (setq  end-pos (point)   )
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

(defun ac-php--json-save-data(file-path data-list )
  (let ((old-config-value json-encoding-pretty-print) json-data )
    (setq  json-encoding-pretty-print  t)
    (setq  json-data
           (json-encode data-list ))

    (f-write-text json-data 'utf-8 file-path)
    (setq  json-encoding-pretty-print  old-config-value)
    ))


(defun ac-php--cache-files-save  (file-path cache1-files )
  (ac-php--json-save-data file-path (list :cache1-files   cache1-files)  )
  )


(defvar ac-php-rebuild-tmp-error-msg nil )
(defun ac-php--rebuild-file-list ( project-root-dir   save-tags-dir  do-all-flag )
    "DOCSTRING"
    (let ( tags-dir-len file-list  obj-tags-list update-tag-file-list all-file-list last-phpctags-errmsg
                        )

    (setq tags-dir-len (length project-root-dir))


    (message " REBUILD:  rebuild file start")

    (let  ( file-name src-time  obj-file-name   obj-item all-opt-list tmp-array)


      (let* (process)

        (ac-php--debug "EXEC  %s %s %s %s %s"
        ac-php-ctags-executable
        (concat "--config-file="  (f-join project-root-dir "./.ac-php-conf.json" )  )
        (concat "--tags_dir=" ac-php-tags-path    )
        (concat "--rebuild="  (if do-all-flag "yes" "no" )    )
        (concat "--realpath_flag="  (if  ac-php-project-root-dir-use-truename "yes" "no" )    )
        )


        (setq process  (start-process
                        "ac-phptags"
                        "*AC-PHPTAGS*"
                        ac-php-php-executable
                        ac-php-ctags-executable
                        (concat "--config-file="  (f-join project-root-dir "./.ac-php-conf.json" )  )
                        (concat "--tags_dir=" ac-php-tags-path    )
                        (concat "--rebuild="  (if do-all-flag "yes" "no" )    )
                        (concat "--realpath_flag="  (if  ac-php-project-root-dir-use-truename "yes" "no" )    )
                        ))
         (ac-php--debug
         "%s %s %s %s %s %s "
         ac-php-php-executable
         ac-php-ctags-executable
         (concat "--config-file="  (f-join project-root-dir "./.ac-php-conf.json" )  )
         (concat "--tags_dir=" ac-php-tags-path    )
         (concat "--rebuild="  (if do-all-flag "yes" "no" )    )
         (concat "--realpath_flag="  (if  ac-php-project-root-dir-use-truename "yes" "no" )    )
         )

        (ac-php-mode t)

        (setq ac-php-rebuild-tmp-error-msg nil )
        (setq ac-php-phptags-index-progress 0)
        (force-mode-line-update)


        (set-process-sentinel
         process
         '(lambda ( process event)

            (ac-php-mode 0)
            (cond
             ((string-match "finished" event)
              (if ac-php-rebuild-tmp-error-msg
                  (message "REBUILD END :ERROR %s" ac-php-rebuild-tmp-error-msg )
                (message "REBUILD SUCCESS ")
                )
              )
             ((string-match "exited abnormally" event)
              (message "ERROR ----- ")
              ))))

        (set-process-filter process 'ac-php-phptags-index-process-filter)
        )


      ;; (let ( cmd cmd-output   )

      ;;   (ac-php--json-save-data  opt-file-name all-opt-list )
      ;;   (message "parser php files count=[%d] ,wait ... " (length all-opt-list ))
      ;;   (setq cmd (concat ac-php-ctags-executable  " --files="   ) )
      ;;   (ac-php--debug "exec cmd:%s" cmd)
      ;;   (setq cmd-output (shell-command-to-string  cmd) )

      ;;   (when (> (length cmd-output) 3)
      ;;     (if ( f-exists? ac-php-ctags-executable    )
      ;;         (setq last-phpctags-errmsg (format "phpctags ERROR:%s "  cmd-output  ))
      ;;       (setq last-phpctags-errmsg (format "%s no find ,you need restart emacs" ac-php-ctags-executable ))
      ;;       )
      ;;     ))

      )

    ;;(list last-phpctags-errmsg all-file-list  update-tag-file-list)
    ))


(defun ac-php-phptags-index-process-filter (process strings &optional silent)
  "Process status updates for the indexing process.

Non-nil SILENT will supress extra status info in the minibuffer."


  (dolist  (string (split-string strings "\n" ) )

    (ac-php--debug " AC-PHP: %s" string )
    (cond
     ( (string-match "PHPParser:" string)


       (setq ac-php-rebuild-tmp-error-msg  (concat ac-php-rebuild-tmp-error-msg  "\n" string ) )

       )

     ((string-match "\\([0-9]+\\)%" string)
      (let ((progress (string-to-number (match-string 1 string))))


        (unless(= ac-php-phptags-index-progress progress )
          (setq ac-php-phptags-index-progress progress)
          (force-mode-line-update))
        )
      )
     )))


(defun ac-php--remake-tags (project-root-dir do-all-flag )
  (let ()
    (if (not ac-php-gen-tags-flag  )
        (progn
          (setq ac-php-gen-tags-flag  t )
          ( ac-php--remake-tags-ex  project-root-dir  do-all-flag )
          )
      (progn
        (message "remake: doing ...  [maybe you need restart emacs for remake tags]" )
        nil
        )
      )))

;;for auto check file
(defun ac-php--remake-tags-ex (project-root-dir do-all-flag )
  "DOCSTRING cache1-files: last edit files:  cache2-files: others"
  (let (  save-tags-dir all-file-list  last-phpctags-errmsg update-tag-file-list
                        (file-name (buffer-file-name) ) )

    ;; if location at  vendor dir
    (when  (and  file-name (s-match "/vendor/" file-name ))
      (setq do-all-flag t))

    (message "do remake %s do-all-flag :%s "  project-root-dir do-all-flag  )

    (unless ( f-exists? ac-php-ctags-executable    )
      (message "%s no find ,you need restart emacs" ac-php-ctags-executable ))

    (if (not ac-php-php-executable ) (message "no find cmd:  php  ,you need  install php-cli and restart emacs " ))
    (if (not project-root-dir) (message "no find file '.ac-php-conf.json'   in path list :%s " (file-name-directory (buffer-file-name)  )   ) )
    (if ( and (f-exists? ac-php-ctags-executable) ac-php-php-executable  project-root-dir)
        (progn
          ;;get last-save-info
          (setq save-tags-dir (ac-php--get-tags-save-dir project-root-dir) )
          (ac-php--rebuild-file-list  project-root-dir   save-tags-dir  do-all-flag)
          )
      ( setq ac-php-gen-tags-flag nil )
      )
    ))



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

(defun ac-php--get-tags-save-dir(project-root-dir  )
  (let (  ret tag-dir  old-default-directory )
    (setq conf-list  (ac-php--get-config project-root-dir) )
    (setq tag-dir (cdr (assoc-string "tag-dir" conf-list )) )
    ;;(message "project-root-dir:%s,  tag-dir:%s" project-root-dir tag-dir )
    (if tag-dir
        (progn
          (setq old-default-directory  default-directory )
          (setq default-directory project-root-dir)
          (setq  ret  (file-truename tag-dir ) )
          (setq  default-directory old-default-directory )

          )
   (when (memq system-type '(windows-nt ms-dos))
      (setq project-root-dir (concat "/"
        (replace-regexp-in-string  (regexp-quote ":") ""  project-root-dir ))))
      (setq ret (concat ac-php-tags-path "/tags"
                        (replace-regexp-in-string (regexp-quote "/") "-"
                                                  (replace-regexp-in-string  "/$" ""  project-root-dir )
                                                  )  ))
      )


    (unless (f-exists?  ret  )
      (mkdir ret t))
    (f-full ret )
    ))

(defun ac-php--get-timestamp (  time-arr )
  (+  (*(nth 0  time-arr )  65536)  (nth 1  time-arr) )
  )

(defun ac-php-get-tags-file ()
  (let ((project-root-dir (ac-php--get-project-root-dir)) tags-file file-attr  file-last-time now  )
    (if project-root-dir
        (progn
          (setq tags-file (concat  (ac-php--get-tags-save-dir project-root-dir)  "tags.el"  ) )
          (setq file-attr   (file-attributes  tags-file ) )

          (when file-attr
            (setq file-last-time  (ac-php--get-timestamp  (nth 5 file-attr) ))
            (setq now  (ac-php--get-timestamp (current-time)  ))
            ;;; check time , and delete tags file if  time out
            (when  (and  (> (- now  file-last-time )  ac-php-auto-update-intval  )
                       )
              ( ac-php--remake-tags  project-root-dir  nil )
              )
            )

          (list  project-root-dir tags-file )
          )
      nil)))

(defun ac-php--get-config-path-noti-str ( project-root-dir path-str)
  (if  (s-ends-with? "*.php" path-str )
      (format "php-path-list-without-subdir->%s" (f-relative (f-parent path-str) project-root-dir) )
    (format "php-path-list->%s" (f-relative path-str project-root-dir ))))


(defun ac-php--get-config ( project-root-dir )
  (let ( config-file-name )

    (setq config-file-name (f-join project-root-dir ".ac-php-conf.json"  ) )
    (when (or (not (f-exists?  config-file-name ) )
             ( =  (f-size  config-file-name ) 0 ))
      (ac-php--json-save-data config-file-name
                              '(
                                :use-cscope  nil
                                :tag-dir nil
                                :filter
                                (
                                 :php-file-ext-list
                                 ("php")
                                 :php-path-list (".")
                                 :php-path-list-without-subdir []
                                 )
                                )
                              ))

    (json-read-file  config-file-name  )

    ))

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


(defun ac-php-load-data (file project-root-dir )
  (let  ((file-attr   (file-attributes  file ) ) file-data  conf-last-time  file-last-time
         class-map  function-map inherit-map  )

    ;;  check time  and reload
    (when file-attr
      (setq file-last-time (+  (*(nth 0 (nth 5 file-attr) )  65536)  (nth 1 (nth 5 file-attr)) ))
      (setq  conf-last-time (nth  1 (assoc-string file  ac-php-tag-last-data-list   ) ) )


      (when (or (null conf-last-time) (> file-last-time conf-last-time ))
        (message "ac-php reload from json-data start")

        ;;(setq  file-data (json-read-file  file ) )

        (load  file )
        (setq file-data g-ac-php-tmp-tags )
        (message "ac-php reload from json-data  deal data  ")
        (assq-delete-all  file   ac-php-tag-last-data-list )
        ;;; file-data  => class-map, function-map, inherit-list-map, file-list,
        ;;; to ->  hash_table
        ;;;  class-map
        (setq class-map (make-hash-table :test  'case-fold ))
        (setq function-map (make-hash-table :test  'case-fold ))
        (setq inherit-map (make-hash-table :test  'case-fold ))
        (mapc
         (lambda (class-item)
           (puthash   (format "%s"  (car class-item )) (cdr class-item)  class-map )
           ) (aref file-data 0 ) )


        (mapc
         (lambda (function-item)
           (puthash (aref  function-item 1 ) function-item  function-map )
           ) (aref file-data 1 ) )

        (mapc
         (lambda (inherit-item)
           (puthash  (format "%s" (car inherit-item )) (cdr inherit-item ) inherit-map )
           ) (aref file-data 2 ) )


        (push (list file file-last-time
                    (list
                     class-map
                     function-map
                     inherit-map
                     (aref file-data 3 ) ;; file-list
                     project-root-dir
                     ) ) ac-php-tag-last-data-list  )
        (message "ac-php reload from json-data end")
        ))

    (nth  2 (assoc-string file  ac-php-tag-last-data-list   ))))

(defun ac-php-g--class-map (tags-data ) (nth 0  tags-data ) )
(defun ac-php-g--function-map (tags-data ) (nth 1  tags-data ) )
(defun ac-php-g--inherit-map (tags-data ) (nth 2  tags-data ) )
(defun ac-php-g--file-list (tags-data ) (nth 3  tags-data ) )
(defun ac-php-g--project-root-dir  (tags-data ) (nth 4  tags-data ) )

(defun ac-php-get-tags-data ()
  (let ( tags-file  project-root-dir (tags-arr   (ac-php-get-tags-file )))
    (if tags-arr
        (progn
          (setq tags-file   (nth 1 tags-arr)   )
          (setq project-root-dir (nth 0 tags-arr) )
          )
      (setq tags-file   ac-php-common-json-file   )
      )
    (ac-php--debug  "LOAD TAGS:%s"  tags-file )
    (if  (file-exists-p tags-file )
      (ac-php-load-data  tags-file  project-root-dir  )
    (ac-php-remake-tags )
      )
    ))


;;; ==============END

(defun ac-php--get-project-root-dir  ()
  "DOCSTRING"
  (let (project-root-dir tags-file  (file-name buffer-file-name)  )
    (when file-name
      (setq project-root-dir (file-name-directory  file-name  ))

    )

    (unless project-root-dir
      (setq project-root-dir ( expand-file-name  default-directory) ))

    (when ac-php-project-root-dir-use-truename
      (setq project-root-dir (file-truename project-root-dir  ) )
      )

    (let (last-dir)
        (while (not (or
                     (file-exists-p  (concat project-root-dir  ".ac-php-conf.json" ))
                     (file-exists-p  (concat project-root-dir  "vendor/autoload.php" )) ;; is a composer dir
                     (string= project-root-dir "/")
                     ))
          (setq  last-dir project-root-dir  )
          (setq project-root-dir  (file-name-directory (directory-file-name  project-root-dir ) ))
          (when (string= last-dir project-root-dir  )
            (setq project-root-dir "/" )
            )))

    (if (string= project-root-dir "/") (setq project-root-dir nil )   )
    project-root-dir
    ))

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


(defun ac-php--get-item-info (member )
    "DOCSTRING"
  (let (type-str )
    (if (and (> (length member ) 1)  (string=  "(" ( substring  member  -1 )) )
        (progn
          (setq type-str "m"))
      (setq type-str "p"))
    (list member type-str )
    ))

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

(defun ac-php-find-symbol-at-point-pri ( tags-data &optional  as-function-flag as-name-flag )
  (let ( key-str-list
         cur-word val-name class-name output-vec
         jump-pos  cmd complete-cmd  find-flag ret
         (project-root-dir ( ac-php-g--project-root-dir tags-data ))
         )


    (if as-name-flag
        (setq cur-word  (ac-php--get-cur-word ))
      (if as-function-flag
          (setq cur-word  (concat (ac-php--get-cur-word ) "(" ))
        (setq cur-word  (ac-php--get-cur-word-with-function-flag ))
        )
      )

    (ac-php--debug "key-str-list==begin:cur-word:%s" cur-word )
    (setq key-str-list (ac-php-get-class-at-point  tags-data ))

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
              (class-map ( ac-php-g--function-map tags-data  )) full-name tmp-ret file-pos  )

          (when (string= "" cur-word) ;;new
            (setq tmp-ret  ( ac-php-get-syntax-backward (concat "new[ \t]+\\(" ac-php-word-re-str "\\)") 1 ))
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
    (while  (not (ac-php-check-not-in-string-or-comment (point )) )
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
           ((or (string= type "class_member")  (string= type "user_function") )
            (let ((file-pos (nth 1 symbol-ret) ) tmp-arr  )
              (setq tmp-arr  (s-split ":" file-pos ) )
              (ac-php--debug " tmp-arr %S"  tmp-arr )
              (cond
               ((s-matches-p "sys" (nth 0 tmp-arr) )
                (let( (sys-item-name (aref  (nth 3 symbol-ret ) 1 )) ) ;;system function
                  ;; \trim( => trim
                  (if (string= type "user_function")
                      (setq sys-item-name (substring-no-properties
                                           sys-item-name 1
                                           (if (string=  "(" ( substring  sys-item-name -1 )) -1 nil  )
                                           )         )
                    (setq sys-item-name  (nth 2 symbol-ret ) ) ;; class name
                    )

                  (php-search-documentation sys-item-name )
                  ))
               (t
                (let ( (file-list (ac-php-g--file-list tags-data  ) )  )
                  ;; from  get index
                  (setq jump-pos
                        (concat
                         (aref file-list (string-to-number (nth 0 tmp-arr)  )  )
                         ":" (nth 1 tmp-arr)
                         ))
                  (ac-php-location-stack-push)
                  (ac-php-goto-location jump-pos )
                  )
                ))
              )
            )
           )))
      (when local-var-flag ( ac-php--goto-local-var-def local-var  ) )
      )
    ))


(defun ac-php-gen-def ()
  "DOCSTRING"
  (interactive)
  (let ( (tags-data (ac-php-get-tags-data ) )  line-txt (cur-word  (ac-php--get-cur-word ) ) )
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
                (re-search-backward "[^ \t]")
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
  (interactive)
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
(defun ac-php--get-cur-word ( )
  (let (start-pos cur-word)
  (save-excursion
    (skip-chars-backward "a-z0-9A-Z_\\\\")
    (setq start-pos (point))
    (skip-chars-forward "a-z0-9A-Z_\\\\")
      (buffer-substring-no-properties start-pos (point))
    )
    ))
(defun ac-php-get-cur-word-with-dollar ( )
  (let (start-pos cur-word)
  (save-excursion
    (skip-chars-backward "\\$a-z0-9A-Z_")
    (setq start-pos (point))
    (skip-chars-forward "\\$a-z0-9A-Z_")
      (buffer-substring-no-properties start-pos (point))
    )
    ))

(defun ac-php--get-cur-word-with-function-flag ( )
  (let (start-pos cur-word)
  (save-excursion
      (ac-php--debug "ac-php--get-cur-word-with-function-flag:%S" (point)  )
    (skip-chars-backward "a-z0-9A-Z_\\\\")
    (setq start-pos (point))
    (skip-chars-forward "a-z0-9A-Z_\\\\")
    (skip-chars-forward " \t")
    (skip-chars-forward "(")
      (s-replace-all '((" "."" )  ("\t"."" ))    (buffer-substring-no-properties start-pos (point)))
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
  "auto set  cscope-initial-directory and  Run egrep over the cscope database."
  (interactive (list
                (let (cscope-no-mouse-prompts)
                  (cscope-prompt-for-symbol "Find this egrep pattern " nil t t))
                ))
  (let ((project-root-dir ( ac-php--get-project-root-dir )))

    (if (or ac-php-use-cscope-flag
            (ac-php--get-use-cscope-from-config-file project-root-dir   ))
        (progn
          (setq cscope-initial-directory  (ac-php--get-tags-save-dir  project-root-dir  )  )
          (cscope-find-egrep-pattern symbol)
          )
      (message "need  config:  .ac-php-conf.json -> use-cscope:true  ")
    )))


;; mode --- info
(defcustom ac-php-mode-line
  '(:eval (format "AP%s"
                  ( ac-php-mode-line-project-status)))
  "Mode line lighter for AC-PHP.

Set this variable to nil to disable the lighter."
  :group 'ac-php
  :type 'sexp
  :risky t)

(defun ac-php-mode-line-project-status ()
  "Report status of current project index."
  (format ":%02d%%%%" ac-php-phptags-index-progress   )
    )


(define-minor-mode ac-php-mode
  "AC-PHP "
  :lighter ac-php-mode-line
  :global nil
  :group 'ac-php
  (cond (ac-php-mode
         ;; Enable ac-php-mode
         (setq ac-php-gen-tags-flag t )
         )
        (t
         (setq ac-php-gen-tags-flag nil )
         ;; Disable ac-php-mode
         ;; Disable semantic
         )))



(defun  ac-php-core-eldoc--documentation-function(&optional prefix)
  (interactive "P")
  ;;检查是类还是 符号
  (let ( (tags-data  (ac-php-get-tags-data )  )
         symbol-ret   type  doc class-name access return-type member-info tag-name function-item file-pos member-info-len )

    (setq symbol-ret (ac-php-find-symbol-at-point-pri tags-data ))
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

        )) )))

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
                     "config file       : %s.ac-php-conf.json \n"
                     "tags file         : %s \n"
                     "tags last gen time: %s  ")
             project-root-dir
             project-root-dir
             tags-file  file-last-time )

    )
  )


;;;###autoload
(defun ac-php-core-eldoc-setup ()
  "Set up eldoc function and enable eldoc-mode."
  (interactive)
  (setq-local eldoc-documentation-function #'ac-php-core-eldoc--documentation-function)
  (eldoc-mode +1))


(provide 'ac-php-core)

;;; ac-php-core.el ends here
