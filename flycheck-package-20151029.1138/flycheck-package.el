;;; flycheck-package.el --- A Flycheck checker for elisp package authors

;; Copyright (C) 2014-2015  Steve Purcell, Fanael Linithien

;; Author: Steve Purcell <steve@sanityinc.com>
;;         Fanael Linithien <fanael4@gmail.com>
;; Keywords: lisp
;; Package-Version: 20151029.1138
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (flycheck "0.22") (emacs "24"))

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

;; Provides feedback via flycheck about issues with the package metadata
;; of a file, e.g. the package dependencies it requires.

;; To enable, use something like this:

;;    (eval-after-load 'flycheck
;;      '(flycheck-package-setup))

;; Checks will currently be enabled only if a "Package-Requires:" or
;; "Package-Version:" header is present in the file.

;;; Code:

(eval-when-compile (require 'pcase))    ; `pcase-dolist' is not autoloaded
(eval-when-compile (require 'cl-lib))
(require 'flycheck)
(require 'package)
(require 'lisp-mnt)


;;; Compatibility

(defalias 'flycheck-package--package-desc-summary
  (if (fboundp 'package-desc-summary)
      'package-desc-summary
    'package-desc-doc))


;;; Machinery

(defun flycheck-package--start (checker callback)
  "Flycheck start function for CHECKER, invoking CALLBACK."
  (funcall callback
           'finished
           (mapcar (lambda (x)
                     (apply #'flycheck-error-new-at `(,@x :checker ,checker)))
                   (condition-case err
                       (flycheck-package--check-all)
                     (error
                      (funcall callback 'errored (error-message-string err))
                      (signal (car err) (cdr err)))))))

(defvar flycheck-package--errors nil
  "List of errors and warnings for the current buffer.
This is bound dynamically while the checks run.")

(defun flycheck-package--check-all ()
  "Return a list of errors/warnings for the current buffer."
  (let ((flycheck-package--errors '()))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (when (flycheck-package--looks-like-a-package)
            (flycheck-package--check-package-version-present)
            (flycheck-package--check-lexical-binding-is-on-first-line)
            (let ((desc (flycheck-package--check-package-el-can-parse)))
              (when desc
                (flycheck-package--check-package-summary desc)))
            (flycheck-package--check-dependency-list)))))
    flycheck-package--errors))

(defun flycheck-package--error (line col type message)
  "Construct a datum for error at LINE and COL with TYPE and MESSAGE."
  (push (list line col type message) flycheck-package--errors))


;;; Checks

(defun flycheck-package--check-dependency-list ()
  "Return position and contents of the \"Package-Requires\" header, if any."
  (when (flycheck-package--goto-header "Package-Requires")
    (let ((position (match-beginning 3))
          (line-no (line-number-at-pos))
          (deps (match-string 3)))
      (condition-case err
          (pcase-let ((`(,parsed-deps . ,parse-end-pos) (read-from-string deps)))
            (unless (= parse-end-pos (length deps))
              (flycheck-package--error
               line-no 1 'error
               "More than one expression provided."))
            (let ((deps (flycheck-package--check-well-formed-dependencies position line-no parsed-deps)))
              (when deps
                (flycheck-package--check-packages-installable deps)
                (flycheck-package--check-deps-use-non-snapshot-version deps)
                (flycheck-package--check-deps-do-not-use-zero-versions deps)
                (flycheck-package--check-lexical-binding-requires-emacs-24 deps)
                (flycheck-package--check-do-not-depend-on-cl-lib-1.0 deps))))
        (error
         (flycheck-package--error
          line-no 1 'error
          (format "Couldn't parse \"Package-Requires\" header: %s" (error-message-string err))))))))

(defun flycheck-package--check-well-formed-dependencies (position line-no parsed-deps)
  "Check that dependencies listed at POSITION on LINE-NO are well-formed.
These PARSED-DEPS must have the format (name \"version\").
Return a list of well-formed dependencies, where each element is of
the form (PACKAGE-NAME PACKAGE-VERSION LINE-NO LINE-BEGINNING-OFFSET)."
  (let (valid-deps)
    (dolist (entry parsed-deps)
      (pcase entry
        ((and `(,package-name ,package-version)
              (guard (symbolp package-name))
              (guard (stringp package-version)))
         ;; Find the column at which the dependency is declared so we can
         ;; properly report the position of errors.
         (let ((offset
                (save-excursion
                  (goto-char position)
                  (let ((line-start (line-beginning-position))
                        (pattern
                         (format "( *\\(%s\\)\\(?:)\\|[^[:alnum:]_\\-].*?)\\)"
                                 (regexp-quote (symbol-name package-name)))))
                    (if (re-search-forward pattern (line-end-position) t)
                        (- (1+ (match-beginning 1)) line-start)
                      1)))))
           (if (ignore-errors (version-to-list package-version))
               (push (list package-name
                           (version-to-list package-version)
                           line-no
                           offset)
                     valid-deps)
             (flycheck-package--error
              line-no offset 'error
              (format "%S is not a valid version string: see `version-to-list'."
                      package-version)))))
        (_
         (flycheck-package--error
          line-no 1 'error
          (format "Expected (package-name \"version-num\"), but found %S." entry)))))
    valid-deps))

(defun flycheck-package--check-packages-installable (valid-deps)
  "Check that all VALID-DEPS are available for installation."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (if (eq 'emacs package-name)
        (unless (version-list-<= (list 24) package-version)
          (flycheck-package--error
           line-no offset 'error
           "You can only depend on Emacs version 24 or greater."))
      ;; Not 'emacs
      (let ((archive-entry (assq package-name package-archive-contents)))
        (if archive-entry
            (let ((best-version (flycheck-package--lowest-installable-version-of package-name)))
              (when (version-list-< best-version package-version)
                (flycheck-package--error
                 line-no offset 'warning
                 (format "Version dependency for %s appears too high: try %s" package-name
                         (package-version-join best-version)))))
          (flycheck-package--error
           line-no offset 'error
           (format "Package %S is not installable." package-name)))))))

(defun flycheck-package--check-deps-use-non-snapshot-version (valid-deps)
  "Warn about any VALID-DEPS on snapshot versions of packages."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (unless (version-list-< package-version (list 19001201 1))
      (flycheck-package--error
       line-no offset 'warning
       (format "Use a non-snapshot version number for dependency on \"%S\" if possible."
               package-name)))))

(defun flycheck-package--check-deps-do-not-use-zero-versions (valid-deps)
  "Warn about VALID-DEPS on \"0\" versions of packages."
  (pcase-dolist (`(,package-name ,package-version ,line-no ,offset) valid-deps)
    (when (equal package-version '(0))
      (flycheck-package--error
       line-no offset 'warning
       (format "Use a properly versioned dependency on \"%S\" if possible."
               package-name)))))

(defun flycheck-package--check-lexical-binding-requires-emacs-24 (valid-deps)
  "Warn about use of `lexical-binding' when Emacs 24 is not among VALID-DEPS."
  (goto-char (point-min))
  (when (flycheck-package--lexical-binding-declared-in-header-line-p)
    (let* ((lexbind-line (line-number-at-pos))
           (lexbind-col (1+ (- (match-beginning 1) (line-beginning-position)))))
      (unless (assq 'emacs valid-deps)
        (flycheck-package--error
         lexbind-line lexbind-col 'warning
         "You should depend on (emacs \"24\") if you need lexical-binding.")))))

(defun flycheck-package--check-lexical-binding-is-on-first-line ()
  "Check that any `lexical-binding' declaration is on the first line of the file."
  (cl-block return
    (let ((original-buffer (current-buffer)))
      (with-temp-buffer
        (let ((lexical-binding-found-at-end nil))
          (insert-buffer-substring-no-properties original-buffer)
          (condition-case err
              (cl-letf (((symbol-function #'hack-local-variables-apply) #'ignore)
                        ((symbol-function #'hack-local-variables-filter)
                         (lambda (variables _dir-name)
                           (setq file-local-variables-alist variables)))
                        ;; Silence any messages Emacs may want to share with the user.
                        ;; There's no user.
                        ((symbol-function #'display-warning) #'ignore)
                        ((symbol-function #'message) #'ignore))
                ;; HACK: this is an internal variable!
                ;; Unfortunately, Emacsen that have this variable also have
                ;; `hack-local-variables' that doesn't store `lexical-binding'
                ;; in `file-local-variables-alist'.
                (defvar hack-local-variables--warned-lexical)
                (let ((hack-local-variables--warned-lexical nil)
                      (enable-dir-local-variables nil)
                      (enable-local-variables t)
                      (local-enable-local-variables t))
                  (hack-local-variables)
                  (setq lexical-binding-found-at-end
                        hack-local-variables--warned-lexical)))
            (error
             (flycheck-package--error 1 1 'error (error-message-string err))
             (cl-return-from return nil)))
          (when (or lexical-binding-found-at-end
                    ;; In case this is an Emacs from before `hack-local-variables'
                    ;; started to warn about `lexical-binding' on a line other
                    ;; than the first.
                    (and (cdr (assq 'lexical-binding file-local-variables-alist))
                         (not (flycheck-package--lexical-binding-declared-in-header-line-p))))
            (flycheck-package--error
             1 1 'error
             "`lexical-binding' must be set in the first line.")))))))

(defun flycheck-package--check-do-not-depend-on-cl-lib-1.0 (valid-deps)
  "Check that any dependency on \"cl-lib\" is on a remotely-installable version."
  (let ((cl-lib-dep (assq 'cl-lib valid-deps)))
    (when cl-lib-dep
      (let ((cl-lib-version (nth 1 cl-lib-dep)))
        (when (version-list-<= '(1) cl-lib-version)
          (flycheck-package--error
           (nth 2 cl-lib-dep) (nth 3 cl-lib-dep) 'error
           (format "Depend on the latest 0.x version of cl-lib rather than on version \"%S\".
Alternatively, depend on Emacs 24.3, which introduced cl-lib 1.0."
                   cl-lib-version)))))))

(defun flycheck-package--check-package-version-present ()
  "Check that a valid \"Version\" header is present."
  (let ((version (flycheck-package--goto-header (rx (? "Package-") "Version"))))
    (if version
        (unless (ignore-errors (version-to-list version))
          (flycheck-package--error
           (line-number-at-pos)
           (1+ (- (match-beginning 3) (line-beginning-position)))
           'warning
           (format "\"%s\" is not a valid version. MELPA will handle this, but other archives will not." version)))
      (flycheck-package--error
       1 1 'warning
       "\"Version:\" or \"Package-Version:\" header is missing. MELPA will handle this, but other archives will not."))))

(defun flycheck-package--check-package-el-can-parse ()
  "Check that `package-buffer-info' can read metadata from this file.
If it can, return the read metadata."
  (condition-case err
      (let ((orig-buffer (current-buffer)))
        ;; We've reported version header issues separately, so rule them out here
        (with-temp-buffer
          (insert-buffer-substring-no-properties orig-buffer)
          (flycheck-package--update-or-insert-version "0")
          (package-buffer-info)))
    (error
     (flycheck-package--error
      1 1
      'error
      (format "package.el cannot parse this buffer: %s" (error-message-string err)))
     nil)))

(defun flycheck-package--check-package-summary (desc)
  "Check the summary for package with descriptor DESC.
DESC is a struct as returned by `package-buffer-info'."
  (let ((summary (flycheck-package--package-desc-summary desc)))
    (cond
     ((string-empty-p summary)
      (flycheck-package--error
       1 1
       'warning
       "Package should have a non-empty summary."))
     ((> (length summary) 50)
      (flycheck-package--error
       1 1
       'warning
       "The package summary is too long. It should be at most 50 characters.")))
    (when (save-match-data
            (let ((case-fold-search t))
              (and (string-match "\\<emacs\\>" summary)
                   (not (string-match-p "[[:space:]]+lisp" summary (match-end 0))))))
      (flycheck-package--error
       1 1
       'warning
       "Including \"Emacs\" in the package description is usually redundant."))))


;;; Helpers and checker definition

(defun flycheck-package--looks-like-a-package ()
  "Return non-nil if this buffer appears to be intended as a package."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat lm-header-prefix (regexp-opt '("Package-Version" "Package-Requires")))
     nil t)))

(defun flycheck-package--lowest-installable-version-of (package)
  "Return the lowest version of PACKAGE available for installation."
  (let ((descriptors (cdr (assq package package-archive-contents))))
    (if (fboundp 'package-desc-version)
        (car (sort (mapcar 'package-desc-version descriptors)
                   #'version-list-<))
      (aref descriptors 0))))

(defun flycheck-package--goto-header (header-name)
  "Move to the first occurrence of HEADER-NAME in the file.
If the return value is non-nil, then point will be at the end of
the file, and the second and third match groups will contain the name and
value of the header with any leading or trailing whitespace removed."
  (let ((initial-point (point)))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat (lm-get-header-re header-name) "\\(.*?\\) *$") nil t)
          (substring-no-properties (match-string 3))
        (goto-char initial-point)
        nil))))

(defun flycheck-package--update-or-insert-version (version)
  "Ensure current buffer has a \"Version: VERSION\" header."
  (if (flycheck-package--goto-header "Version")
      (move-beginning-of-line nil)
    (forward-line))
  (insert (format ";; Version: %s" version))
  (newline))

(defun flycheck-package--get-header-line-file-local-variables ()
  "Return local variables specified in the -*- line.
Returns an alist of elements (VAR . VAL), where VAR is a variable
and VAL is the specified value.

For details, see `hack-local-variables-prop-line'."
  (cl-letf (((symbol-function #'message) #'ignore))
    (hack-local-variables-prop-line)))

(defun flycheck-package--lexical-binding-declared-in-header-line-p ()
  "Test if `lexical-binding' is declared in the -*- line."
  ;; Test the `cdr' to see if it's actually true, because
  ;; -*- lexical-binding: nil -*-
  ;; is legal, if silly.
  (cdr (assq 'lexical-binding (flycheck-package--get-header-line-file-local-variables))))

(flycheck-define-generic-checker 'emacs-lisp-package
  "A checker for \"Package-Requires\" headers."
  :start #'flycheck-package--start
  :modes '(emacs-lisp-mode))

;;;###autoload
(defun flycheck-package-setup ()
  "Setup flycheck-package.
Add `flycheck-emacs-lisp-package' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'emacs-lisp-package t)
  (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-package t)
  (flycheck-add-next-checker 'emacs-lisp-checkdoc 'emacs-lisp-package t))

(provide 'flycheck-package)
;;; flycheck-package.el ends here
