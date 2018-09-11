;;; ivy-xcdoc.el --- Search Xcode documents with ivy interface.  -*- lexical-binding: t; -*-

;; Copyright (c) 2016 C.T.Chen

;; Author: C.T.Chen <chenct@7adybird.com>
;; URL: https://github.com/hex2010/emacs-ivy-xcdoc
;; Package-Version: 20160917.1055
;; Version: 0.0.1
;; Package-Requires: ((ivy "0.8.0") (emacs "24.4"))
;; Keywords: ivy, xcode, xcdoc

;; This file is NOT part of GNU Emacs.

;; MIT License

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Commentary:
;;
;; This package provides functions to search Xcode documents within Emacs,
;; Currently support API search only, result topics will show in web browser.
;; Other features may be added later.
;;
;; Example usage:
;;
;;   (require 'ivy-xcdoc)
;;
;; Then:
;;
;;   M-x `ivy-xcdoc-search-api' RET
;;
;; or
;;
;;   M-x `ivy-xcdoc-search-api-at-point' RET
;;
;; Tips:
;;
;; You may want to customization `ivy-xcdoc-docsets', default settings is
;; known to work with Xcode 8.0,  for other verison  the docset paths
;; maybe different, and the default settings contains OSX and iOS documents
;; only, whatchOS and tvOS is not include by default.
;;
;; You can also set the `ivy-xcdoc-url-browser-function' variable to change
;; the default display style, for example use an external browser.
;;

;;; Code:

(require 'ivy)
(require 'browse-url)

(defgroup ivy-xcdoc nil
  "Search Xcode documents with ivy interface."
  :group 'ivy)

(defcustom ivy-xcdoc-command "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil"
  "Path of `docsletutil' utils, which can be found in Xcode.app. try \"find /Applications/Xcode.app -name docsetutil\" in your terminal to find it out."
  :type 'string
  :group 'ivy-xcdoc)

(defcustom ivy-xcdoc-docsets
  '(("OSX" .
     "~/Library/Developer/Shared/Documentation/DocSets/com.apple.adc.documentation.OSX.docset")
    ("iOS" .
     "~/Library/Developer/Shared/Documentation/DocSets/com.apple.adc.documentation.iOS.docset"))
  "Association list of docsets to use, docsets come with Xcode, try \"find ~ -name '*.docset'\" in your terminal to find them out."
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Path"))
  :group 'ivy-xcdoc)

(defcustom ivy-xcdoc-url-browser-function nil
  "Function use to open document topics, Topics is html files on local file system. Set to nil will use `eww-browse-url'."
  :type 'function
  :group 'ivy-xcdoc)

(defun ivy-xcdoc--show-topic (url)
  "Use specified function to open topic url."
  (if ivy-xcdoc-url-browser-function (funcall ivy-xcdoc-url-browser-function url)
    (eww-browse-url url nil)))


(defun ivy-xcdoc--make-safe-query (query)
  "Make query keywords safe for command line."
  (replace-regexp-in-string "\"" "\\\"" query))


(defun ivy-xcdoc--parse-result (docset output)
  "Parse the result, construct url for each topic."
  (let ((name (car docset))
        (path (concat "file://" (expand-file-name (cdr docset))))
        (lines (split-string output "\n" t))
        (ret nil))
    (dolist (line lines ret)
      (let ((sl (split-string line " " t)))
        (setq ret (cons (cons (concat name "  " (nth 0 sl))
                              (concat path "/Contents/Resources/Documents/" (nth 1 sl))) ret))))))

(defun ivy-xcdoc--docsetutil (safe-query docset)
  "Invoke the docsetutil on single docset."
  (let ((cmd (concat ivy-xcdoc-command " search -skip-text -query \"" safe-query "\"  " (cdr
                                                                                         docset))))
    (ivy-xcdoc--parse-result  docset (or (shell-command-to-string cmd)
                                         ""))))

(defun ivy-xcdoc--docsetutil-multi (query docsets)
  "Invoke the docsetutil on multi docsets."
  (setq query (ivy-xcdoc--make-safe-query query))
  (let (ret)
    (dolist (docset docsets ret)
      (setq ret (append ret (ivy-xcdoc--docsetutil query docset))))))


(defun ivy-xcdoc--search-and-select (query)
  "Search topics and select one with ivy-read."
  (let ((topics (ivy-xcdoc--docsetutil-multi query ivy-xcdoc-docsets)))
    (if topics (ivy-read "Select Topic: " topics
                         :action (lambda (x)
                                   (with-ivy-window (ivy-xcdoc--show-topic (cdr x))))
                         :require-match t
                         :caller 'ivy-xcdoc--search-and-select)
      (message "Topic not found."))))

(defvar ivy-xcdoc-query-history nil
  "Ivy xcdoc query history.")

(defun ivy-xcdoc--prompt-and-search
    (&optional
     at-point)
  "Prompt for query keywords and search them."
  (ivy-read (concat "Search Xcode API: ") ivy-xcdoc-query-history
            :initial-input (and at-point
                                (ivy-thing-at-point))
            :action (lambda (x)
                      (with-ivy-window (ivy-xcdoc--search-and-select x)))
            :require-match nil
            :caller 'ivy-xcdoc--prompt-and-search
            :history 'ivy-xcdoc-query-history))

;;;###autoload
(defun ivy-xcdoc-search-api ()
  "Search Xcode documents for API keywords."
  (interactive)
  (ivy-xcdoc--prompt-and-search nil))

;;;###autoload
(defun ivy-xcdoc-search-api-at-point ()
  "Search Xcode documents for API keywords at point."
  (interactive)
  (ivy-xcdoc--prompt-and-search t))


(provide 'ivy-xcdoc)

;;; ivy-xcdoc.el ends here
