;;; nim-capf.el --- Implementation of Completion At Point Function for nim-mode -*- lexical-binding: t -*-

;; Copyright (C) 2016  Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; Keywords: completion

;; This package was made from company-nim.el and original authors were
;; Simon Hafner.

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

;; This file provides Completion At Point Function future (capf in
;; short) and you can use it via C-M-i and M-TAB keys by Emacs'
;; default key bindings.
;;
;; Also capf allows you to work with company-mode without adding
;; backends.

;; For company-mode users:
;; You can configure minimum string length that company's auto-completion
;; starts by `company-minimum-prefix-length'.  Also you can change the idle
;; time by `company-idle-delay'.

;; TODO:
;;   - make sure with company-flx package (https://github.com/PythonNut/company-flx)
;;     (currently somehow I couldn't use it)
;;
;;; Code:

(require 'let-alist)
(require 'nim-syntax)
(require 'nim-suggest)
(require 'nim-helper)

(defcustom nim-capf--type-abbrevs
  ;; From ast.nim, some of them aren't used as completion maybe...
  '(("skUnknown"      . "U")

    ("skConditional"  . "")
    ("skDynLib"       . "D")

    ("skParam"        . "p")
    ("skGenericParam" . "P")                         ;
    ("skTemp"         . "t")
    ("skModule"       . ".")
    ("skType"         . "T")
    ("skVar"          . "V")
    ("skLet"          . "L")
    ("skConst"        . "C")
    ("skResult"       . "r")
    ("skProc"         . "f")
    ("skFunc"         . "F")
    ("skMethod"       . "m")
    ("skIterator"     . "I")
    ("skConverter"    . "c")
    ("skMacro"        . "M")
    ("skTemplate"     . "T")

    ("skField"        . "Fi")
    ("skEnumField"    . "en")
    ("skForVar"       . "fv")
    ("skLabel"        . "la")
    ("skStub"         . "st")

    ("skPackage"      . "P")
    ("skAlias"        . "A")
    )
  "Abbrevs for completion."
  :type 'assoc
  :group 'nim)

(defun nim-capf--format-candidate (cand)
  "Put text property to CAND."
  (let ((qpath (nim--epc-qpath cand)))
    (propertize
     (car (last qpath))
     :nim-line   (nim--epc-line     cand)
     :nim-column (nim--epc-column   cand)
     :nim-type   (nim--epc-forth    cand)
     :nim-doc    (nim--epc-doc      cand)
     :nim-qpath  qpath
     :nim-file   (nim--epc-file cand)
     :nim-sk     (nim--epc-symkind  cand)
     :nim-sig    (assoc-default
                  (nim--epc-symkind cand) nim-capf--type-abbrevs)
     :nim-prefix     (nim--epc-prefix   cand))))

(defun nim-capf--format-candidates (_arg candidates)
  "Put text attributes to CANDIDATES."
  (mapcar #'nim-capf--format-candidate candidates))

(defun nim-capf--nimsuggest-async (prefix callback)
  "Query to nimsuggest asynchronously.

The PREFIX is passed to the CALLBACK."
  ;; currently only support nim-mode (not nimscript-mode)
  (when (derived-mode-p 'nim-mode)
    (nimsuggest--call-epc
     'sug
     (lambda (x) (funcall callback (nim-capf--format-candidates prefix x))))))

(defun nim-capf--prefix-p (beg end &optional skip)
  "Return t if completion should be triggered for prefix between BEG and END.
If SKIP is non-nil, skip length check ."
  (and
   (if (or skip (eq this-command 'company-idle-begin)
           (eq ?. (char-before beg)))
       t
     (let ((diff (- end beg))
           (len (bound-and-true-p company-minimum-prefix-length)))
       (and len (<= len diff))))
   (<= beg end)
   (or (eolp)
       (let ((c-end (char-after end)))
         (and c-end (not (eq ?w (char-syntax c-end))))))))

(defun nim-capf--annotation (cand)
  "Get annotation info for CAND."
  (let ((ann (get-text-property 0 :nim-type cand))
        (symbol (get-text-property 0 :nim-sig cand)))
    (format " %s [%s]" (substring ann 0 (cl-search "{" ann)) symbol)))

(defun nim-capf--docsig (candidate)
  "Return meta/docsig information for company-mode of CANDIDATE."
  (apply 'nimsuggest--format
           (mapcar `(lambda (x) (get-text-property 0 x ,candidate))
                   '(:nim-type :nim-sk :nim-qpath :nim-doc))))

(defun nim-capf--location (cand)
  "Get location info for CAND."
  (let ((line (get-text-property 0 :nim-line cand))
        (path (get-text-property 0 :nim-file cand)))
    (cons path line)))

(defun nim-company--doc-buffer (candidate)
  "Get doc-buffer info for CANDIDATE."
  (when (fboundp 'company-doc-buffer)
    (let ((doc (get-text-property 0 :nim-doc candidate)))
      (unless (equal doc "")
        (funcall 'company-doc-buffer doc)))))

;;;###autoload
(defun nim-capf-nimsuggest-completion-at-point ()
  "Complete the symbol at point using nimsuggest."
  (when nimsuggest-mode
    (unless (nth 3 (syntax-ppss)) ;; not in string
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (beg (or (car bounds) (point)))
             (end (or (cdr bounds) (point)))
             (c-beg (char-after beg))
             ;; avoid length check if previous char is "."
             (skip-len-check (and (not (bobp)) (eq ?. (char-before (point))))))
        (list beg end
              (completion-table-with-cache 'nim-capf--nimsuggest-complete)
              ;; See `completion-extra-properties' for details
              :exit-function #'nim-capf--exit-function    ; replacement of company's :post-completion
              :annotation-function #'nim-capf--annotation ; show annotation right after completion
              ;; default property of ‘completion-at-point-functions’
              :exclusive 'no
              :predicate `(lambda (candidate)
                            (if ,(and c-beg (< 65 c-beg 90)) ; whether A-Z
                                (let ((thing (thing-at-point 'symbol)))
                                  (if thing
                                      ;; If user inputs capitalized string,
                                      ;; check only the first char.
                                      (eq ,c-beg (string-to-char candidate))
                                    ;; let default predicate function
                                    t))
                              t))
              ;; Company-mode integration
              ;; predicate by length
              :company-prefix-length (nim-capf--prefix-p beg end skip-len-check)
              ;; show something on minibuffer
              :company-docsig #'nim-capf--docsig
              ;; you can activate via F1 key, but currently no documentation available.
              :company-doc-buffer #'nim-company--doc-buffer
              ;; C-w key to open the source location
              :company-location #'nim-capf--location)))))

(defun nim-capf--nimsuggest-complete (prefix)
  ;; Note this function is not async function
  "Completion symbol of PREFIX at point using nimsuggest."
  (unless (or (nim-inside-pragma-p)
              (nim-syntax-comment-or-string-p))
    (cond
     ((or (string< "" prefix)
          (eq ?. (char-before (point))))
      (nimsuggest--call-sync
       'sug (lambda (args) (nim-capf--format-candidates prefix args)))))))

(defun nim-capf--post-completion (candidate)
  "Post complete function based on CANDIDATE."
  (when-let* ((type-sig (get-text-property 0 :nim-sig candidate)))
    (cl-case (intern type-sig)
      ((f F m I c M T)
       (insert "()")
       (backward-char 1)
       (run-hook-with-args 'nim-capf-after-exit-function-hook candidate)))))

(defun nim-capf--exit-function (str status)
  "Insert necessary things for STR, when completion is done.
You may see information about STATUS at `completion-extra-properties'.
But, for some reason, currently this future is only supporting
company-mode.  See also: https://github.com/company-mode/company-mode/issues/583"
  (unless (eq 'completion-at-point this-command)
    (cl-case status
      ;; finished -- completion was finished and there is no other completion
      ;; sole -- completion was finished and there is/are other completion(s)
      ((finished sole)
       (nim-capf--post-completion str))
      (t
       ;; let other completion backends
       (setq this-command 'self-insert-command)))))

;; completion at point
(defun nim-capf-builtin-completion ()
  "This might not be precise, but maybe enough to someone."
  (eval-when-compile
    (append nim-keywords
            nim-types
            nim-exceptions
            nim-variables
            nim-constants
            nim-nonoverloadable-builtins
            nim-builtin-functions)))

(defconst nim-capf-builtin-words
  (append (nim-capf-builtin-completion)
          nim-builtins-without-nimscript))

(defconst nim-capf-builtin-words-nimscript
  (append (nim-capf-builtin-completion)
          (append nimscript-builtins
                  nimscript-variables)))

(defvar nim-capf--pragma-words
  (eval-when-compile (cl-loop for (kwd . _) in nim-pragmas collect kwd))
  "List of pragmas for `complietion-at-point-functions'.")

(defun nim-capf--static-completion (words)
  "Return list of completion-at-point’s elements.
List of WORDS are used as completion candidates."
  (when (or this-command (thing-at-point 'symbol))
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (beg (or (car bounds) (point)))
           (end (or (cdr bounds) (point))))
      (list beg end words
            :company-prefix-length (nim-capf--prefix-p beg end)
            :exclusive 'no))))

;;;###autoload
(defun nim-builtin-completion-at-point ()
  "Complete the symbol at point for .nim files."
  (nim-capf--static-completion
   (if (nim-inside-pragma-p)
       nim-capf--pragma-words
     (append
      nim-capf-builtin-words
      ;; in string -- there are some env variable for unittest for nim
      ;; and I can't remember all the time.
      (when (nth 3 (syntax-ppss))
        nim-environment-variables)))))

;;;###autoload
(defun nimscript-builtin-completion-at-point ()
  "Complete the symbol at point for nimscript files."
  (nim-capf--static-completion nim-capf-builtin-words-nimscript))


;;; Company-mode integration
(with-eval-after-load "company"
  (defun company-nimsuggest (command &optional arg &rest _args)
     "A function used to be as company-backend for `nim-mode'."
     (interactive (list 'interactive))
     (cl-case command
       (interactive (company-begin-backend 'company-nimsuggest))
       (candidates (cons :async (lambda (cb) (nim-capf--nimsuggest-async arg cb))))
       (prefix (and
                (memq major-mode '(nim-mode nimscript-mode))
                (not (or (nim-inside-pragma-p)
                         (nim-syntax-comment-or-string-p)))
                (company-grab-symbol-cons "\\." 2)))
       (doc-buffer (nim-company--doc-buffer arg))
       (annotation (nim-capf--annotation arg)) ; displayed on popup
       (meta       (nim-capf--docsig     arg)) ; displayed on minibuffer
       (location   (nim-capf--location   arg))
       (post-completion (nim-capf--post-completion arg))
       (sorted t))))


;; Setup function

;;;###autoload
(defun nim-capf-setup ()
  "Setup."
  (let ((capf (cl-case major-mode
                (nim-mode       'nim-builtin-completion-at-point)
                (nimscript-mode 'nimscript-builtin-completion-at-point)
                (t (error "Unexpected major mode")))))

    ;; Add builtin capf function (pragma and some keywords)
    (unless (memq capf completion-at-point-functions)
      (add-hook 'completion-at-point-functions
                capf nil 'local))

    ;; if company-mode is disabled, just add nimsuggest's capf function.
    (unless (or (bound-and-true-p company-mode)
                (bound-and-true-p global-company-mode))
      (unless (memq 'nim-capf-nimsuggest-completion-at-point completion-at-point-functions)
        (add-hook 'completion-at-point-functions
                  'nim-capf-nimsuggest-completion-at-point nil 'local)))

    ;; Add an asynchronous backend for company-mode.
    ;; The big difference between `company-nimsuggest' and
    ;; `nim-capf-nimsuggest-completion-at-point' is that company
    ;; version works with :async keyword.
    (when (bound-and-true-p company-backends)
      (add-to-list 'company-backends 'company-nimsuggest))))

;;;###autoload
(add-hook 'nimsuggest-mode-hook 'nim-capf-setup)


;; Suggestion-box-el
;; https://github.com/yuutayamada/suggestion-box-el
(eval-after-load "suggestion-box"
  '(add-hook 'nim-capf-after-exit-function-hook 'suggestion-box-nim-by-type))

(provide 'nim-capf)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; nim-capf.el ends here
