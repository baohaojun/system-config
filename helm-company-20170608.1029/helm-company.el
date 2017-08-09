;;; helm-company.el --- Helm interface for company-mode

;; Copyright (C) 2013 Yasuyuki Oka <yasuyk@gmail.com>

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; Maintainer: Daniel Ralston <Sodel-the-Vociferous@users.noreply.github.com>
;; Version: 0.2.0
;; Package-Version: 20170608.1029
;; URL: https://github.com/Sodel-the-Vociferous/helm-company
;; Package-Requires: ((helm "1.5.9") (company "0.6.13"))

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

;; Add the following to your Emacs init file:
;;
;; (autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-mode-map (kbd "C-:") 'helm-company)
;;      (define-key company-active-map (kbd "C-:") 'helm-company)))

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-multi-match)
(require 'helm-files)
(require 'helm-elisp) ;; For with-helm-show-completion
(require 'company)
(require 'subr-x)

(defgroup helm-company nil
  "Helm interface for company-mode."
  :prefix "helm-company-"
  :group 'helm)

(defcustom helm-company-candidate-number-limit 300
  "Limit candidate number of `helm-company'.

Set it to nil if you don't want this limit."
  :group 'helm-company
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom helm-company-show-annotations t
  "Show annotations provided by company-backend when completing.

Annotations will be formatted in the `company-tooltip-annotation'
face."
  :group 'helm-company
  :type 'boolean )

(defcustom helm-company-initialize-pattern-with-prefix nil
  "Use the thing-at-point as the initial helm completion pattern.

The thing-at-point is whatever partial thing you've typed that
you're trying to complete."
  :group 'helm-company
  :type 'boolean)

(defvar helm-company-help-window nil)
(defvar helm-company-candidates nil)
(defvar helm-company-backend nil)
(defvar helm-company-display-candidates-hash nil
  "A hash table.

KEYS: Candidate display strings with no properties.

VALUES: (FORMATTED-DISPLAY . COMPANY-CANDIDATE) pairs.
COMPANY-CANDIDATE is the candidate string exactly as provided by
company-backend (properties and all); FORMATTED-DISPLAY is the
formatted display string (with font-lock properties) of
COMPANY-CANDIDATE, for Helm to display.

Some completion backends use string properties to store and
retrieve annotation data. Helm strips all properties off before
completion, which may break this feature. So, the original
strings provided by company-backend are stored here, so they can
be retrieved and passed to company-backend when asking for
annotations.")

(defun helm-company-call-backend (&rest args)
  "Bridge between helm-company and company"
  (let ((company-backend helm-company-backend))
    (apply 'company-call-backend args)))

(defun helm-company-init ()
  "Prepare helm for company."
  (helm-attrset 'company-candidates company-candidates)
  (helm-attrset 'company-common company-common)
  (helm-attrset 'company-prefix company-prefix)
  (helm-attrset 'company-backend company-backend)
  (setq helm-company-help-window nil)
  (if (<= (length company-candidates) 1)
      (helm-exit-minibuffer)
    (setq helm-company-backend                 company-backend
          helm-company-candidates              company-candidates
          helm-company-display-candidates-hash (helm-company--make-display-candidate-hash company-candidates))))

(defun helm-company-cleanup ()
  (setq helm-company-backend             nil
        helm-company-candidates          nil)
  (company-abort))

(defun helm-company-cleanup-post-action ()
  (helm-attrset 'company-candidates nil)
  (setq helm-company-backend             nil
        helm-company-candidates          nil
        helm-company-display-candidates-hash nil))

(defun helm-company-action-insert (candidate)
  "Insert CANDIDATE."
  (let* ((company-candidates (helm-attr 'company-candidates))
         (company-backend (helm-attr 'company-backend))
         (company-common (helm-attr 'company-common))
         (company-prefix (helm-attr 'company-prefix)))
    (company-finish candidate))
  ;; for GC
  (helm-company-cleanup-post-action))

(defun helm-company-action-show-document (candidate)
  "Show the documentation of the CANDIDATE."
  (interactive)
  (let* ((selection (cl-find-if (lambda (s) (string-match-p candidate s)) helm-company-candidates))
         (buffer (helm-company-call-backend 'doc-buffer selection)))
    (when buffer
      (display-buffer buffer))))

(defun helm-company-show-doc-buffer (candidate)
  "Temporarily show the documentation buffer for the CANDIDATE."
  (interactive)
  (let* ((selection (cl-find-if (lambda (s) (string-match-p candidate s)) helm-company-candidates))
         (buffer (helm-company-call-backend 'doc-buffer selection)))
    (when buffer
      (if (and helm-company-help-window
               (window-live-p helm-company-help-window))
          (with-selected-window helm-company-help-window
            (helm-company-display-document-buffer buffer))
        (setq helm-company-help-window
              (helm-company-display-document-buffer buffer))))))

(defun helm-company-find-location (candidate)
  "Find location of CANDIDATE."
  (interactive)
  (let* ((selection (cl-find-if (lambda (s) (string-match-p candidate s)) helm-company-candidates))
         (location (save-excursion (helm-company-call-backend 'location selection)))
         (pos (or (cdr location) (error "No location available")))
         (buffer (or (and (bufferp (car location)) (car location))
                     (find-file-noselect (car location) t))))
    (with-selected-window (display-buffer buffer t)
      (save-restriction
        (widen)
        (if (bufferp (car location))
            (goto-char pos)
          (goto-char (point-min))
          (forward-line (1- pos))))
      (set-window-start nil (point)))))

(defun helm-company-display-document-buffer (buffer)
  "Temporarily show the documentation BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min)))
  (display-buffer buffer
                  '((display-buffer-same-window . t)
                    (display-buffer-reuse-window . t))))

(defmacro helm-company-run-action (&rest body)
  `(with-helm-window
     (save-selected-window
       ;; `with-helm-display-same-window' has been removed from recent helm
       ;; versions.
       (if (fboundp 'with-helm-display-same-window)
           (with-helm-display-same-window
            ,@body)
         ,@body))))

(defun helm-company-run-show-doc-buffer ()
  "Run showing documentation action from `helm-company'."
  (interactive)
  (helm-company-run-action
   (helm-company-show-doc-buffer (helm-get-selection))))

(defun helm-company-run-show-location ()
  "Run showing location action from `helm-company'."
  (interactive)
  (helm-company-run-action
   (helm-company-find-location (helm-get-selection))))

(defun helm-company--propertize-annotation (str)
  (let ((str (concat str)))             ; Copy the string
    (put-text-property 0 (length str) 'font-lock-face 'company-tooltip-annotation
                       str)
    str))

(defun helm-company--make-display-string (candidate annotation)
  (let ((candidate (substring-no-properties candidate)))
    (if (null annotation)
        candidate
      (concat candidate " " (helm-company--propertize-annotation annotation)))))

(defun helm-company--get-annotations (candidate)
  "Return a list of the annotations (if any) supplied for a
candidate by company-backend.

When getting annotations from `company-backend', first it tries
with the `candidate' arg. If that doesn't work, it gets the
original candidate string(s) from
`helm-company-display-candidates-hash', and tries with those."
  (company-call-backend 'annotation candidate))

(defun helm-company--make-display-candidate-pairs (candidates)
  (cl-loop for cand in candidates
           append
           (cl-loop for annot in (helm-company--get-annotations cand)
                    collect (cons (helm-company--make-display-string cand annot)
                                  cand))))

(defun helm-company--make-display-candidate-hash (candidates)
  (let ((hash (make-hash-table :test 'equal :size 1000)))
    (cl-loop for candidate in candidates
             for annot = (helm-company--get-annotations candidate)
             for display-str = (helm-company--make-display-string candidate annot)
             for key = (substring-no-properties display-str)
             do (puthash key (cons display-str candidate) hash))
    hash))

(defun helm-company-add-annotations-transformer-1 (candidates &optional sort)
  (with-helm-current-buffer
    (let ((results (helm-company--make-display-candidate-pairs candidates)))
      (if sort
          (sort results #'helm-generic-sort-fn)
        results))))

(defun helm-company-add-annotations-transformer (candidates _source)
  "Transform a flat list of completion candidate strings
into (DISPLAY . REAL) pairs.

The display strings have the company-provided annotation
appended, and formatted in the `company-tooltip-annotation'
face."
  (if (or (not helm-company-show-annotations) (consp (car candidates)))
      candidates
    (helm-company-add-annotations-transformer-1 candidates (null helm--in-fuzzy))))

(defun helm-company-get-display-strings ()
  (let ((sort (null helm--in-fuzzy))
        (display-strs (hash-table-keys helm-company-display-candidates-hash)))
    (if sort
        (sort display-strs #'helm-generic-sort-fn)
      display-strs)))

(defun helm-company-get-real-candidate (display-str)
  (let ((display-str (substring-no-properties display-str)))
    (cdr (gethash display-str helm-company-display-candidates-hash))))

(defun helm-company-get-formatted-display-strings (display-strs &optional _)
  (let ((display-strs (mapcar 'substring-no-properties display-strs)))
    (mapcar (lambda (display-str) (car (gethash display-str helm-company-display-candidates-hash)))
            display-strs)))

(defvar helm-company-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap helm-map)
    (define-key keymap (kbd "M-s") 'helm-company-run-show-location)
    (define-key keymap (kbd "C-s") 'helm-company-run-show-doc-buffer)
    (delq nil keymap))
  "Keymap used in Company sources.")

(defvar helm-company-actions
  '(("Insert" . helm-company-action-insert)
    ("Show documentation (If available)" . helm-company-action-show-document)
    ("Find location (If available)" . helm-company-find-location))
  "Actions for `helm-company'.")

(defcustom helm-company-fuzzy-match t
  "Enable fuzzy matching for Helm Company."
  :type 'boolean)

(defvar helm-source-company
  (helm-build-in-buffer-source "Company"
    :data (lambda ()
            (helm-company-init)
            (helm-company-get-display-strings))
            ;(list "position -> int"))
    :filtered-candidate-transformer 'helm-company-get-formatted-display-strings
    :display-to-real 'helm-company-get-real-candidate
    :cleanup 'helm-company-cleanup
    :fuzzy-match helm-company-fuzzy-match
    :keymap helm-company-map
    :persistent-action 'helm-company-show-doc-buffer
    :persistent-help "Show documentation (If available)"
    :action helm-company-actions)
  ;; "Helm source definition for recent files in current project."
  )

;;;###autoload
(defun helm-company ()
  "Select `company-complete' candidates by `helm'.
It is useful to narrow candidates."
  (interactive)
  (unless company-candidates
    (company-complete)
    ;; (company-call-frontends 'hide) Work around a quirk with company.
    ;; `company-complete' inserts the common part of all candidates into the
    ;; buffer. But, it doesn't update `company-prefix' -- and `company-prefix'
    ;; is all `company-finish' replaces in the buffer. (issue #9)
    (when company-common
      (setq company-prefix company-common)))
  (let ((initial-pattern (and helm-company-initialize-pattern-with-prefix company-prefix)))
    (when company-point
      (helm :sources 'helm-source-company
            :buffer  "*helm company*"
            :input initial-pattern
            :candidate-number-limit helm-company-candidate-number-limit))))

(provide 'helm-company)

;; Local Variables:
;; coding: utf-8
;; eval: (setq byte-compile-not-obsolete-vars '(display-buffer-function))
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; helm-company.el ends here
