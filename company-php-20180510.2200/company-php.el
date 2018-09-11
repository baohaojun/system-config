;;; company-php.el --- company completion source for php
;; Copyright (C) 2014 - 2016 jim
;; Author: xcwenn@qq.com [https://github.com/xcwen]
;; URL: https://github.com/xcwen/ac-php
;; Package-Version: 20180510.2200
;; Package-X-Original-Version: 20171209.2243
;; Keywords: completion, convenience, intellisense
;; Package-Requires: ( (cl-lib "0.5") (ac-php-core "1") (company "0.9")  )

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
;;  company source for php.
;; support  Linux and OSX,  but windows need more test
;; More info and **example** at : https://github.com/xcwen/ac-php

;;(add-hook 'php-mode-hook
;;          '(lambda ()
;;             (require 'company-php)
;;             (company-mode t)
;;             (add-to-list 'company-backends 'company-ac-php-backend )))


;;; Code:

(require 'ac-php-core)
(require 'cl-lib)
(require 'company)
(require 'company-template)

(defgroup company-php nil
  "Completion backend for PHP."
  :group 'company)

(defcustom company-php-begin-after-member-access  t
  "When non-nil, automatic completion will start whenever the current
symbol is preceded by \"->\" or \"::\", ignoring
`company-minimum-prefix-length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and
\":\".")

(defun company-ac-php-annotation (item)
  (let ((doc (ac-php-clean-document (get-text-property 0 'ac-php-help item))))
	(if (ac-php--tag-name-is-function item)
		(concat doc ")")
	  "")))

(defun company-ac-php-fuzzy-match (prefix candidate)
  (cl-subsetp (string-to-list prefix) (string-to-list candidate)))

(defun company-ac-php--prefix-symbol ()
  (buffer-substring
   (point)
   (save-excursion
	 (skip-chars-backward "\\$a-z0-9A-Z_\\\\") (point))))

(defun company-ac-php-company-grab-symbol-cons (idle-begin-after-re &optional max-len)
  "Return a string SYMBOL or a cons (SYMBOL . t).
SYMBOL is as returned by `company-grab-symbol'.  If the text before point
matches IDLE-BEGIN-AFTER-RE, return it wrapped in a cons."
  (let ((symbol  (company-ac-php--prefix-symbol)))
    (when symbol
      (save-excursion
        (forward-char (- (length symbol)))
        (if (looking-back idle-begin-after-re (if max-len
                                                  (- (point) max-len)
                                                (line-beginning-position)))
            (cons symbol t)
          symbol)))))

;; TODO it bad for namespace like \App\add\ss
(defun company-ac-php--prefix ()
  (if company-php-begin-after-member-access
	  (company-ac-php-company-grab-symbol-cons "->\\|::" 2)
	(company-ac-php--prefix-symbol)))


(defun company-ac-php-candidate (arg)
  (let* ((ac-php-prefix-str (company-ac-php--prefix-symbol))
		 (ac-php-prefix-str-len (length ac-php-prefix-str))
		 (find-count 0)
		 raw-help
		 candidate-list
		 ac-php-company-list)

	(ac-php--debug "company-ac-php-candidate :%s " ac-php-prefix-str)
	(setq candidate-list (ac-php-candidate))

	(dolist (candidate-item candidate-list)
	  (setq raw-help (or (get-text-property 0 'ac-php-help candidate-item) ""))

	  (when (ac-php--string=-ignore-care ac-php-prefix-str (s-left ac-php-prefix-str-len candidate-item))
		(setq find-count (1+ find-count))

		(if (ac-php--tag-name-is-function candidate-item)
			(dolist (item (split-string raw-help "\n"))
			  (let ((option-start-index 1000000)
					(i 0)
					(item-pre-str "")
					(args-list (s-split "," item))
					find-flag)

				(dolist (arg args-list)
				  (when (and (not find-flag) (s-matches-p "=" arg))
					(setq find-flag t)
					(setq option-start-index i))
				  (setf (nth i args-list) (replace-regexp-in-string "=.*" "" arg))
				  (setq i (1+ i)))

				(setq i 0)

				(dolist (arg args-list)
				  (when (>= i option-start-index)
					(push (propertize candidate-item 'ac-php-help (concat item-pre-str)) ac-php-company-list))
				  (setq item-pre-str (concat item-pre-str (if (= i 0) "" ",") arg))
				  (setq i (1+ i)))

				(push (propertize candidate-item 'ac-php-help (concat item-pre-str)) ac-php-company-list)))

		  (push candidate-item ac-php-company-list))))

	;;fix one function bug
	(when (and (= find-count 1) (> (length ac-php-company-list) 1))
	  (push (propertize ac-php-prefix-str 'ac-php-help "") ac-php-company-list))

	(nreverse ac-php-company-list)))



(defun company-ac-php-document (item)
  (if (stringp item)
      (let (doc  tag-type return-type access from-class)
        (setq doc (ac-php-clean-document (get-text-property 0 'ac-php-help item)))
        (setq tag-type (get-text-property 0 'ac-php-tag-type item))
        (setq return-type (get-text-property 0 'ac-php-return-type item))
        (setq access (get-text-property 0 'ac-php-access item))
        (setq from-class (get-text-property 0 'ac-php-from item))
        (if ( ac-php--tag-name-is-function item)
            (setq doc (concat item  doc ")" ) )
          (setq doc item )
          )



        (cond
         ( (or (string= tag-type "p") ( string= tag-type "m") ( string= tag-type "d")  )
           (format "%s\n\t[  type]:%s\n\t[access]:%s\n\t[  from]:%s" doc  return-type access  from-class  ) )
         (return-type
          (format "%s  %s " return-type doc   ) )
         (t
          doc))
        ))
  )


(defun company-ac-php--doc-buffer (candidate)
  (let ((doc (company-ac-php-document candidate)))
    (message "llllllllll" )
    (when (s-present? doc)
      (company-doc-buffer doc))))

;;;###autoload
(defun company-ac-php-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
	(interactive (company-begin-backend 'company-ac-php-backend))
	(prefix (company-ac-php--prefix))
	(candidates (company-ac-php-candidate arg))
	(annotation (company-ac-php-annotation arg))
	(duplicates t)
  (doc-buffer (company-ac-php--doc-buffer arg))
	(post-completion (company-ac-php-backend-post-completion arg))
	))

(defun company-ac-php-backend-post-completion (arg)
  (let ((doc))
	(when (ac-php--tag-name-is-function arg)
	  (setq doc (s-replace "&" "" (ac-php-clean-document (get-text-property 0 'ac-php-help arg))))
	  (insert (concat  doc ")"))
	  (when (bound-and-true-p smartparens-mode)
		(delete-char 1))
	  (company-template-c-like-templatify (concat arg doc ")")))))

(provide 'company-php)

;;; company-php.el ends here
