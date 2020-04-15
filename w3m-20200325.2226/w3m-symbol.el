;;; w3m-symbol.el --- Stuffs to replace symbols for emacs-w3m

;; Copyright (C) 2002-2007, 2009, 2017, 2019
;; ARISAWA Akihiro <ari@mbf.sphere.ne.jp>

;; Author: ARISAWA Akihiro <ari@mbf.sphere.ne.jp>
;; Keywords: w3m, WWW, hypermedia, i18n

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;

;;; Code:

(defvar w3m-output-coding-system)
(defvar w3m-language)
(defvar w3m-use-symbol)
(declare-function w3m-mule-unicode-p "w3m")

(defgroup w3m-symbol nil
  "Symbols for w3m"
  :group 'w3m)

(defvar w3m-symbol-custom-type
  '(list
    :convert-widget w3m-widget-type-convert-widget
    (let* ((w `(sexp :match (lambda (_widget value) (stringp value))
		     :size 4))
	   (a `(,@w :format "%v "))
	   (b `(,@w :format "%v\n"))
	   (c (list a a a a a a a b))
	   (d (list a a a a a b)))
      `(:indent 4 :tag "Customized"
		:value ,(if (equal w3m-language "Japanese")
			    w3m-Japanese-symbol
			  w3m-default-symbol)
		,@c ,@c ,@c ,@c ,@d ,@d ,b ,b))))

(defcustom w3m-default-symbol
  '("-+" " |" "--" " +" "-|" " |" "-+" ""
    "--" " +" "--" ""   "-+" ""   ""   ""
    "-+" " |" "--" " +" "-|" " |" "-+" ""
    "--" " +" "--" ""   "-+" ""   ""   ""
    " *" " +" " o" " #" " @" " -"
    " =" " x" " %" " *" " o" " #"
    " #"
    "<=UpDn ")
  "List of symbol string used by default."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-Chinese-BIG5-symbol
  '("┼" "├" "┬" "┌" "┤" "│" "┐" ""
    "┴" "└" "─" ""   "┘" ""   ""   ""
    "┼" "├" "┬" "┌" "┤" "│" "┐" ""
    "┴" "└" "─" ""   "┘" ""   ""   ""
    "‧" "□" "☆" "○" "■" "★"
    "◎" "●" "△" "●" "○" "□"
    "●"
    "《↑↓")
  "List of symbol string, used in Chienese-BIG5 environment."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-Chinese-CNS-symbol
  '("┼" "├" "┬" "┌" "┤" "│" "┐" ""
    "┴" "└" "─" ""   "┘" ""   ""   ""
    "┼" "├" "┬" "┌" "┤" "│" "┐" ""
    "┴" "└" "─" ""   "┘" ""   ""   ""
    "・" "□" "☆" "○" "■" "★"
    "◎" "●" "△" "●" "○" "□"
    "●"
    "《↑↓")
  "List of symbol string, used in Chienese-CNS environment."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-Chinese-GB-symbol
  '("┼" "├" "┬" "┌" "┤" "│" "┐" ""
    "┴" "└" "─" ""   "┘" ""   ""   ""
    "┼" "┠" "┯" "┏" "┨" "┃" "┓" ""
    "┷" "┗" "━" ""   "┛" ""   ""   ""
    "・" "□" "☆" "○" "■" "★"
    "◎" "●" "△" "●" "○" "□"
    "●"
    "《↑↓")
  "List of symbol string, used in Chienese-GB environment."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-Japanese-symbol
  '("┼" "├" "┬" "┌" "┤" "│" "┐" ""
    "┴" "└" "─" ""   "┘" ""   ""   ""
    "┼" "┠" "┯" "┏" "┨" "┃" "┓" ""
    "┷" "┗" "━" ""   "┛" ""   ""   ""
    "・" "□" "☆" "○" "■" "★"
    "◎" "●" "△" "●" "○" "□"
    "●"
    "≪↑↓")
  "List of symbol string, used in Japanese environment."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-Korean-symbol
  '("┼" "├" "┬" "┌" "┤" "│" "┐" ""
    "┴" "└" "─" ""   "┘" ""   ""   ""
    "┼" "┠" "┯" "┏" "┨" "┃" "┓" ""
    "┷" "┗" "━" ""   "┛" ""   ""   ""
    "·" "□" "☆" "○" "■" "★"
    "◎" "●" "△" "●" "○" "□"
    "●"
    "≪↑↓")
  "List of symbol string, used in Korean environment."
  :group 'w3m-symbol
  :type w3m-symbol-custom-type)

(defcustom w3m-mule-unicode-symbol
  (when (w3m-mule-unicode-p)
    (append
     (mapcar (lambda (p)
	       (if p
		   (char-to-string
		    (make-char (or (nth 2 p) 'mule-unicode-2500-33ff)
			       (car p) (cadr p)))
		 ""))
	     '((32 92) (32 60) (32 76) (32 44) (32 68) (32 34) (32 48) nil
	       (32 84) (32 52) (32 32) nil     (32 56) nil     nil     nil
	       (32 92) (32 64) (32 79) (32 47) (32 72) (32 35) (32 51) nil
	       (32 87) (32 55) (32 33) nil     (32 59) nil     nil     nil
	       (115 34 mule-unicode-0100-24ff) (33 97) (34 102) (34 43) (33 96) (34 101)
	       (34 46) (34 47) (33 115) (34 47) (34 43) (33 97)
	       (34 47)))
     (list (format "%c %c %c "
		   (make-char 'mule-unicode-0100-24ff 121 42)
		   (make-char 'mule-unicode-0100-24ff 118 113)
		   (make-char 'mule-unicode-0100-24ff 118 115)))))
  "List of symbol string, using mule-unicode characters."
  :group 'w3m-symbol
  :type (if (w3m-mule-unicode-p)
	    w3m-symbol-custom-type
	  '(const :format "%{%t%}: %v")))

(defcustom w3m-symbol nil
  "List of symbol string."
  :group 'w3m-symbol
  :type `(radio (const :format "Auto detect  " nil)
		(const :tag "Default" w3m-default-symbol)
		(const :format "Chinese BIG5 " w3m-Chinese-BIG5-symbol)
		(const :format "Chinese CNS " w3m-Chinese-CNS-symbol)
		(const :tag "Chinese GB" w3m-Chinese-GB-symbol)
		(const :format "Japanese     " w3m-Japanese-symbol)
		(const :format "Korean      " w3m-Korean-symbol)
		,(when w3m-mule-unicode-symbol
		   '(const :tag "Mule-Unicode" w3m-mule-unicode-symbol))
		(variable :format "%t symbol: %v"
			  :value w3m-default-symbol)
		,w3m-symbol-custom-type))

(defun w3m-use-symbol ()
  (cond ((functionp w3m-use-symbol)
	 (funcall w3m-use-symbol))
	(t w3m-use-symbol)))

(defvar current-language-environment)

(defun w3m-symbol ()
  (cond (w3m-symbol
	 (if (symbolp w3m-symbol)
	     (symbol-value w3m-symbol)
	   w3m-symbol))
	((and (eq w3m-output-coding-system 'utf-8)
	      w3m-mule-unicode-symbol))
	((let ((lang (or w3m-language current-language-environment)))
	   (when (boundp (intern (format "w3m-%s-symbol" lang)))
	     (symbol-value (intern (format "w3m-%s-symbol" lang))))))
	(t w3m-default-symbol)))

;;;###autoload
(defun w3m-replace-symbol ()
  (when (w3m-use-symbol)
    (let ((symbol-list (w3m-symbol)))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "<_SYMBOL TYPE=\\([0-9]+\\)>" nil t)
	  (let ((symbol (nth (string-to-number (match-string 1)) symbol-list))
		(start (point))
		end symbol-cnt)
	    (search-forward "</_SYMBOL>" nil t)
	    (setq end (match-beginning 0)
		  symbol-cnt (/ (string-width (buffer-substring start end))
				(string-width symbol)))
	    (goto-char start)
	    (delete-region start end)
	    (insert (apply 'concat (make-list symbol-cnt symbol)))))))))

(provide 'w3m-symbol)

;;; w3m-symbol.el ends here
