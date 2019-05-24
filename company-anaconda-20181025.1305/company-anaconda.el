;;; company-anaconda.el --- Anaconda backend for company-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2018 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/anaconda-mode
;; Package-Version: 20181025.1305
;; Version: 0.2.0
;; Package-Requires: ((company "0.8.0") (anaconda-mode "0.1.1") (cl-lib "0.5.0") (dash "2.6.0") (s "1.9"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'anaconda-mode)
(require 'company)
(require 'python)
(require 'cl-lib)
(require 'dash)
(require 's)

(defgroup company-anaconda nil
  "Company back-end for Python code completion."
  :group 'programming)

(defcustom company-anaconda-annotation-function
  'company-anaconda-annotation
  "Function that returns candidate annotations."
  :group 'company-anaconda
  :type 'function)

(defcustom company-anaconda-case-insensitive t
  "Use case insensitive candidates match."
  :group 'company-anaconda
  :type 'boolean)

(defun company-anaconda-at-the-end-of-identifier ()
  "Check if the cursor at the end of completable identifier."
  (or
   ;; At the end of the symbol, but not the end of int number
   (and (looking-at "\\_>")
        (not (looking-back "\\_<\\(0[bo]\\)?[[:digit:]]+" (line-beginning-position)))
        (not (looking-back "\\_<0x[[:xdigit:]]+" (line-beginning-position))))
   ;; After the dot, but not when it's a dot after int number
   ;; Although identifiers like "foo1.", "foo111.", or "foo1baz2." are ok
   (and (looking-back "\\." (- (point) 1))
        (not (looking-back "\\_<[[:digit:]]+\\." (line-beginning-position))))
   ;; After dot in float constant like "1.1." or ".1."
   (or (looking-back "\\_<[[:digit:]]+\\.[[:digit:]]+\\." (line-beginning-position))
       (looking-back "\\.[[:digit:]]+\\." (line-beginning-position)))))

(defun company-anaconda-prefix ()
  "Grab prefix at point."
  (and anaconda-mode
       (not (company-in-string-or-comment))
       (company-anaconda-at-the-end-of-identifier)
       (let* ((line-start (line-beginning-position))
              (start
               (save-excursion
                 (if (not (re-search-backward
                           (python-rx
                            (or whitespace open-paren close-paren string-delimiter))
                           line-start
                           t 1))
                     line-start
                   (forward-char (length (match-string-no-properties 0)))
                   (point))))
              (symbol (buffer-substring-no-properties start (point))))
         (if (s-blank-p symbol)
             (if (string-match-p
                  (rx (* space) word-start (or "from" "import") word-end space)
                  (buffer-substring-no-properties line-start (point)))
                 (buffer-substring-no-properties line-start (point))
               'stop)
           (if (s-ends-with-p "." symbol)
               (cons symbol t)
             symbol)))))

(defun company-anaconda-candidates (callback given-prefix)
  "Pass candidates list for GIVEN-PREFIX to the CALLBACK asynchronously."
  (anaconda-mode-call
   "company_complete"
   (lambda (result)
     (funcall callback
	      (--map
               (let ((candidate (s-concat given-prefix (aref it 0))))
                 (put-text-property 0 1 'struct it candidate)
                 candidate)
               result)))))

(defun company-anaconda-annotation (candidate)
  "Return the description property of CANDIDATE inside chevrons."
  (--when-let (aref (get-text-property 0 'struct candidate) 1)
    (concat "<" it ">")))

(defun company-anaconda-doc-buffer (candidate)
  "Return documentation buffer for chosen CANDIDATE."
  (let ((docstring (aref (get-text-property 0 'struct candidate) 2)))
    (unless (s-blank? docstring)
      (anaconda-mode-documentation-view (vector (vector "" docstring))))))

(defun company-anaconda-meta (candidate)
  "Return short documentation string for chosen CANDIDATE."
  (let ((docstring (aref (get-text-property 0 'struct candidate) 2)))
    (unless (s-blank? docstring)
      (car (s-split-up-to "\n" docstring 1)))))

(defun company-anaconda-location (candidate)
  "Return location (path . line) for chosen CANDIDATE."
  (-when-let* ((struct (get-text-property 0 'struct candidate))
               (module-path (pythonic-emacs-readable-file-name (aref struct 3)))
               (line (aref struct 4)))
    (cons module-path line)))

;;;###autoload
(defun company-anaconda (command &optional arg &rest _args)
  "Anaconda backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-anaconda))
    (prefix (company-anaconda-prefix))
    (candidates (cons :async
                      (let ((given-prefix (s-chop-suffix (company-grab-symbol) arg)))
			(lambda (callback)
			  (company-anaconda-candidates callback given-prefix)))))
    (doc-buffer (company-anaconda-doc-buffer arg))
    (meta (company-anaconda-meta arg))
    (annotation (funcall company-anaconda-annotation-function arg))
    (location (company-anaconda-location arg))
    (ignore-case company-anaconda-case-insensitive)
    (sorted t)))

(provide 'company-anaconda)

;;; company-anaconda.el ends here
