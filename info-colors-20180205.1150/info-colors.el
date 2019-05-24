;;; info-colors.el --- Extra colors for Info-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Tuấn-Anh Nguyễn

;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; Author: Drew Adams
;; Keywords: faces
;; URL: https://github.com/ubolonton/info-colors
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
;; Package-Version: 20180205.1150
;; Package-X-Original-Version: 0.2
;; Package-X-Original-Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a modern adaption of the extra coloring provided by Drew
;; Adams' `info+' package.

;; To enable this:

;;     (add-hook 'Info-selection-hook 'info-colors-fontify-node)

;;; Code:

(require 'info)
(require 'cl-lib)


;;;###autoload
(defgroup info-colors nil
  "Extra font lock rules for Info-mode."
  :group 'info)

(defface info-colors-ref-item-type
  '((t (:inherit font-lock-type-face)))
  "Face for type of info's reference items."
  :group 'info-colors)

(defface info-colors-ref-item-syntax-class
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for names of `Syntax class' reference items in `info' nodes."
  :group 'info-colors)

(defface info-colors-ref-item-constant
  '((t (:inherit font-lock-constant-face)))
  "Face for names of `Constant' reference items in `info' nodes."
  :group 'info-colors)

(defface info-colors-ref-item-variable
  '((t (:inherit font-lock-variable-name-face)))
  "Face for names of `Variable' reference items in `info' nodes."
  :group 'info-colors)

(defface info-colors-ref-item-user-option
  '((t (:inherit font-lock-variable-name-face)))
  "Face for names of `User Option' reference items in `info' nodes."
  :group 'info-colors)

(defface info-colors-ref-item-special-form
  '((t (:inherit font-lock-keyword-face)))
  "Face for names of `Special Form' reference items in `info' nodes."
  :group 'info-colors)

(defface info-colors-ref-item-macro
  '((t (:inherit font-lock-keyword-face)))
  "Face for names of `Macro' reference items in `info' nodes."
  :group 'info-colors)

(defface info-colors-ref-item-function
  '((t (:inherit font-lock-function-name-face)))
  "Face for names of `Function' reference items in `info' nodes."
  :group 'info-colors)

(defface info-colors-ref-item-command
  '((t (:inherit font-lock-function-name-face)))
  "Face for names of `Command' reference items in `info' nodes."
  :group 'info-colors)

(defface info-colors-ref-item-other
  '((t (:inherit font-lock-variable-name-face)))
  "Face for the remaining parts of reference items in `info' nodes."
  :group 'info-colors)

(defface info-colors-lisp-code-block
  '((t (:inherit fixed-pitch)))
  "Face for Lisp code blocks in `info' nodes."
  :group 'info-colors)

;;; TODO: Don't fontify indents.
;;; TODO: Fontify as Lisp code.
;;;###autoload
(defun info-colors-fontify-lisp-code-blocks ()
  "Fontify Lisp code blocks in an `info' node."
  (goto-char (point-min))
  (while (re-search-forward
          "^ \\{5,\\}(.*\
\\(\n \\{5\\}.*\\)*\
\\()\\|\n\\)$"
          nil t)
    (put-text-property (match-beginning 0) (match-end 0)
                       'font-lock-face 'info-colors-lisp-code-block)))

;;; TODO: Use syntax table or something?
;;;###autoload
(defun info-colors-fontify-reference-items ()
  "Fontify reference items in an `info' node."
  (goto-char (point-min))
  (while (re-search-forward
          "^ --? \
\\(Command\\|Constant\\|Function\\|Macro\\|Special Form\\|Syntax class\\|User Option\\|Variable\\):\
 *\\(\\S-+\\)\
\\(\\( .*\\)?\\([\n] \\{8\\}.*\\)*\\)"
          nil t)
    (let ((sym (intern (match-string 1))))
      (put-text-property (match-beginning 1) (match-end 1)
                         'font-lock-face 'info-colors-ref-item-type)
      (put-text-property
       (match-beginning 2) (match-end 2)
       'font-lock-face (cl-case sym
                         ('Constant      'info-colors-ref-item-constant)
                         ('Variable      'info-colors-ref-item-variable)
                         ('User\ Option  'info-colors-ref-item-user-option)
                         ('Special\ Form 'info-colors-ref-item-special-form)
                         ('Macro         'info-colors-ref-item-macro)
                         ('Function      'info-colors-ref-item-function)
                         ('Command       'info-colors-ref-item-command)
                         ('Syntax\ class 'info-colors-ref-item-syntax-class)))
      (when (match-beginning 3)
        (put-text-property (match-beginning 3) (match-end 3)
                           'font-lock-face 'info-colors-ref-item-other)))))

;;;###autoload
(defun info-colors-fontify-node ()
  "Fontify an `info' node."
  (save-excursion
    (let* ((inhibit-read-only t)
           (case-fold-search t))
      (info-colors-fontify-lisp-code-blocks)
      (info-colors-fontify-reference-items)
      (set-buffer-modified-p nil))))

(provide 'info-colors)
;;; info-colors.el ends here
