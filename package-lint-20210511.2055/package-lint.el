;;; package-lint.el --- A linting library for elisp package authors -*- lexical-binding: t -*-

;; Copyright (C) 2014-2020  Steve Purcell, Fanael Linithien

;; Author: Steve Purcell <steve@sanityinc.com>
;;         Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/purcell/package-lint
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (emacs "24.1") (let-alist "1.0.6"))

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

;; Provides a list of issues with the package metadata of a file,
;; e.g. the package dependencies it requires.

;; See function `package-lint-buffer'.

;; Checks will currently be enabled only if a "Package-Requires:" or
;; "Package-Version:" header is present in the file.

;;; Code:

(eval-when-compile (require 'pcase))    ; `pcase-dolist' is not autoloaded
(require 'cl-lib)
(require 'package)
(require 'lisp-mnt)
(require 'finder)
(require 'imenu)
(require 'let-alist)
(require 'rx)


(defvar package-lint-main-file nil
  "For multi-file packages, set this to the main .el or -pkg.el file.

When set, the package prefix and dependencies are obtained from
that file instead of the buffer being linted.  This allows
package-lint to operate on secondary files in a package.

The path can be absolute or relative to that of the linted file.")
(put 'package-lint-main-file 'safe-local-variable #'stringp)


;;; Compatibility

(defalias 'package-lint--package-desc-summary
  (if (fboundp 'package-desc-summary)
      'package-desc-summary
    'package-desc-doc))

(defalias 'package-lint--package-desc-name
  (if (fboundp 'package-desc-name)
      'package-desc-name
    (lambda (desc) (intern (elt desc 0)))))

(defalias 'package-lint--package-desc-from-define
  (if (fboundp 'package-desc-from-define)
      'package-desc-from-define
    (lambda (_name-string version-string &optional docstring requirements &rest _args)
      (vector
       (version-to-list version-string)
       (mapcar
        (lambda (x)
          (list (nth 0 x) (version-to-list (nth 1 x))))
        (nth 1 requirements))
       docstring))))


;;; Machinery

(defvar package-lint--errors nil
  "List of errors and warnings for the current buffer.
This is bound dynamically while the checks run.")

(defconst package-lint-backport-libraries
  (list (list 'cl-lib "\\`cl-")
        (list 'cl-generic "\\`cl-\\(?:def\\)?generic")
        (list 'cl-print "\\`cl-print")
        (list 'jsonrpc "\\`jsonrpc-")
        (list 'map "\\`map-")
        (list 'nadvice "\\`advice-")
        (list 'ol "\\`org-link-" 'org "9.3")
        (list 'seq "\\`seq-")
        (list 'let-alist "\\`let-alist"))
  "A sequence of (FEATURE SYMBOL-NAME-MATCH) for backport libraries.
These are libraries that are built into newer Emacsen and also
published in ELPA for use by older Emacsen.

Alternatively, items can take a form of (FEATURE
SYMBOL-NAME-MATCH PACKAGE VERSION?) where PACKAGE is the name of
a backport library shipping the feature and VERSION is an
optional minimum version containing the feature.")

(defconst package-lint-symbol-info
  (let* ((stdlib-changes (with-temp-buffer
                           (insert-file-contents
                            (expand-file-name "data/stdlib-changes"
                                              (if load-file-name
                                                  (file-name-directory load-file-name)
                                                default-directory)))
                           (read (current-buffer))))
         (info (make-hash-table)))
    (pcase-dolist (`(,version . ,data) stdlib-changes)
      (pcase-dolist (`(,syms . ,action)
                     (let-alist data
                       (list (cons .variables.added 'variable-added)
                             (cons .variables.removed 'variable-removed)
                             (cons .features.added 'library-added)
                             (cons .features.removed 'library-removed)
                             (cons .functions.added 'function-added)
                             (cons .functions.removed 'function-removed))))
        (dolist (sym syms)
          (puthash sym (cons (cons action version) (gethash sym info)) info))))
    info)
  "A hash table from SYMBOL to a list of events in its history.
Each event is of the form (ACTION . EMACS-VER), where ACTION is a
symbol such as 'variable-added.")

(defun package-lint-symbol-info (sym)
  "Retrieve information about SYM, as an alist of (ACTION . EMACS-VER)."
  (gethash (cl-etypecase sym
             (string (intern sym))
             (symbol sym))
           package-lint-symbol-info))

(defun package-lint--read-known-symbol (prompt)
  "Read a symbol `package-lint-symbol-info' with PROMPT."
  (let* ((at-point (symbol-at-point))
         (s (completing-read
             prompt
             (cl-loop for k being the hash-keys of package-lint-symbol-info collect k)
             nil
             t
             (when at-point (symbol-name at-point)))))
    (when s (intern s))))

;;;###autoload
(defun package-lint-describe-symbol-history (sym)
  "Show the version history of SYM, if any."
  (interactive (list (package-lint--read-known-symbol "Show history of symbol: ")))
  (message
   (concat (format "History of %s:\n" sym)
           (mapconcat (lambda (info)
                        (pcase-let ((`(,action . ,version) info))
                          (format "* %s in Emacs %s"
                                  action (mapconcat 'number-to-string version "."))))
                      (sort (package-lint-symbol-info sym)
                            (lambda (a b) (version-list-<= (cdr a) (cdr b))))
                      "\n"))))

(defun package-lint--added-or-removed-function-p (sym)
  "Predicate that returns t if SYM is a function added/removed in any known emacs version."
  (let-alist (package-lint-symbol-info sym)
    (or .function-added .function-removed)))

(defconst package-lint--sane-prefixes
  (rx
   string-start
   (or
    "org-dblock-write:"
    "org-babel-execute:"
    "org-babel-prep-session:"
    "org-babel-variable-assignments:"
    "org-babel-default-header-args:"
    "pcomplete/"))
  "A regexp matching whitelisted non-standard symbol prefixes.")

(defvar package-lint--allowed-prefix-mappings
  '(("ob-" . ("org-"))
    ("ol-" . ("org-"))
    ("ox-" . ("org-")))
  "Alist containing mappings of package prefixes to symbol prefixes.")

(defun package-lint--main-file-p ()
  "Return non-nil if the current buffer corresponds to the package's main file."
  (or (null package-lint-main-file)
      (null (buffer-file-name))
      (string= (file-truename (expand-file-name package-lint-main-file))
               (file-truename (buffer-file-name)))))

(defun package-lint--check-all ()
  "Return a list of errors/warnings for the current buffer."
  (let ((package-lint--errors '())
        (case-fold-search nil))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (let (prefix deps)
            (if (package-lint--main-file-p)
                (progn
                  ;; TODO: handle when the main file is a -pkg.el file
                  (setq prefix (package-lint--get-package-prefix))
                  (let ((desc (package-lint--check-package-el-can-parse)))
                    (when desc
                      (package-lint--check-package-summary desc)
                      (package-lint--check-provide-form desc)
                      (package-lint--check-no-emacs-in-package-name desc)))
                  (setq deps (package-lint--check-dependency-list))

                  (package-lint--check-url-header)
                  (package-lint--check-package-version-present)
                  (package-lint--check-commentary-existence))
              ;; Need to look at the main file to find prefix and dependencies
              (setq prefix (replace-regexp-in-string
                            "\\(-mode\\)?\\(-pkg\\)?\\'" ""
                            (file-name-sans-extension
                             (file-name-nondirectory package-lint-main-file))))
              (let ((main-file package-lint-main-file))
                (if (string-match-p "-pkg\\.el\\'" main-file)
                    (let ((expr (with-temp-buffer
                                  (insert-file-contents (expand-file-name main-file))
                                  (read (current-buffer)))))
                      (if (eq (car-safe expr) 'define-package)
                          (setq deps (package-desc-reqs (apply #'package-lint--package-desc-from-define (cdr expr))))
                        (package-lint--error-at-bob 'error (format "Malformed package descriptor file \"%s\"" main-file))))
                  (when (package-lint--goto-header "Package-Requires")
                    (package-lint--error-at-bol 'error "Package-Requires outside the main file have no effect."))
                  (package-lint--check-provide-form-secondary-file)
                  (condition-case err
                      (with-temp-buffer
                        (insert-file-contents (expand-file-name main-file))
                        (setq deps (package-desc-reqs (package-lint--liberal-package-buffer-info))))
                    (error
                     (package-lint--error-at-bob
                      'error
                      (format "Error parsing main package file \"%s\": %s" main-file err)))))))

            ;; Source code checks
            (package-lint--check-keywords-list)
            (package-lint--check-lexical-binding-is-on-first-line)
            (package-lint--check-reserved-keybindings)
            (when prefix
              (package-lint--check-objects-by-regexp
               (concat "(" (regexp-opt '("defalias" "defvaralias")) "\\s-")
               (apply-partially #'package-lint--check-defalias prefix)))
            (package-lint--check-objects-by-regexp
             "(define-minor-mode\\s-"
             #'package-lint--check-minor-mode)
            (package-lint--check-objects-by-regexp
             "(define-global\\(?:ized\\)?-minor-mode\\s-"
             #'package-lint--check-globalized-minor-mode)
            (package-lint--check-objects-by-regexp
             "(defgroup\\s-" #'package-lint--check-defgroup)

            (package-lint--check-no-use-of-cl)
            (package-lint--check-no-use-of-cl-lib-sublibraries)
            (package-lint--check-eval-after-load)
            (package-lint--check-lexical-binding-requires-emacs-24 deps)
            (package-lint--check-libraries-available-in-emacs deps)
            (package-lint--check-libraries-removed-from-emacs)
            (package-lint--check-macros-functions-available-in-emacs deps)
            (package-lint--check-macros-functions-removed-from-emacs deps)
            (package-lint--check-objects-by-regexp
             (concat "(" (regexp-opt '("format" "message" "error")) "\\s-")
             (apply-partially #'package-lint--check-format-string deps))
            (package-lint--check-for-literal-emacs-path)
            (let ((definitions (package-lint--get-defs)))
              (package-lint--check-autoloads-on-private-functions definitions)
              (when prefix
                (package-lint--check-defs-prefix prefix definitions))
              (package-lint--check-symbol-separators definitions)))
          (package-lint--check-lonely-parens))))
    (sort package-lint--errors
          (lambda (a b)
            (pcase-let ((`(,a-line ,a-column ,_ ,a-message) a)
                        (`(,b-line ,b-column ,_ ,b-message) b))
              (cond
               ((/= a-line b-line) (< a-line b-line))
               ((/= a-column b-column) (< a-column b-column))
               (t
                (string-lessp a-message b-message))))))))

(defun package-lint--error (line col type message)
  "Construct a datum for error at LINE and COL with TYPE and MESSAGE."
  (push (list line col type message) package-lint--errors))

(defun package-lint--error-at-point (type message &optional pos)
  "Construct a datum for error at POS with TYPE and MESSAGE.
POS defaults to `point'."
  (save-excursion
    (when pos
      (goto-char pos))
    (package-lint--error (line-number-at-pos) (- (point) (line-beginning-position)) type message)))

(defun package-lint--error-at-bol (type message)
  "Construct a datum for error at the beginning of the current line with TYPE and MESSAGE."
  (package-lint--error (line-number-at-pos) 0 type message))

(defun package-lint--error-at-bob (type message)
  "Construct a datum for error at the beginning of the buffer with TYPE and MESSAGE."
  (package-lint--error 1 0 type message))


;;; Checks

(defun package-lint--check-reserved-keybindings ()
  "Warn about reserved keybindings."
  (let ((re (rx "(" (*? space) (or "kbd" "global-set-key" "local-set-key" "define-key") symbol-end)))
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (unless (nth 8 (save-match-data (syntax-ppss)))
        ;; Read form and get key-sequence
        (goto-char (match-beginning 0))
        (let ((seq (package-lint--extract-key-sequence
                    (read (current-buffer)))))
          (when seq
            (let ((message (package-lint--test-keyseq seq)))
              (when message
                (package-lint--error-at-point 'error message)))))))))

(defun package-lint--check-commentary-existence ()
  "Warn about nonexistent or empty commentary section."
  (let ((start (lm-commentary-start)))
    (if (null start)
        (package-lint--error-at-bob
         'error
         "Package should have a ;;; Commentary section.")
      ;; Skip over the section header.
      (goto-char start)
      (forward-line)
      (when (package-lint--region-empty-p (point) (lm-commentary-end))
        (package-lint--error-at-point
         'error
         "Package should have a non-empty ;;; Commentary section."
         start)))))

(defun package-lint--check-autoloads-on-private-functions (definitions)
  "Verify that private functions DEFINITIONS don't have autoload cookies."
  (pcase-dolist (`(,symbol . ,position) definitions)
    (when (string-match-p (rx "--") symbol)
      (goto-char position)
      (forward-line -1)
      (when (looking-at-p (rx ";;;###autoload"))
        (package-lint--error-at-point
         'warning
         "Private functions generally should not be autoloaded.")))))

(defun package-lint--check-for-literal-emacs-path ()
  "Verify package does not refer to \"\.emacs\.d\" literally.
Instead it should use `user-emacs-directory' or `locate-user-emacs-file'."
  (goto-char (point-min))
  ;; \b won't find a word boundary between a symbol and the "." in
  ;; ".emacs.d". / is a valid symbol constituent in Emacs Lisp, so
  ;; must be explicitly blacklisted.
  (while (re-search-forward "\\(?:\\_<\\|/\\)\\.emacs\\.d\\b" nil t)
    (unless (nth 4 (syntax-ppss))
      ;; Not in a comment
      (package-lint--error-at-point
       'warning
       "Use variable `user-emacs-directory' or function `locate-user-emacs-file' instead of a literal path to the Emacs user directory or files."))))

(defun package-lint--check-keywords-list ()
  "Verify that package keywords are listed in `finder-known-keywords'."
  (when (package-lint--goto-header "Keywords")
    (let ((err-pos (match-beginning 2)))
      (let ((keywords (lm-keywords-list)))
        (unless (cl-some (lambda (keyword) (assoc (intern keyword) finder-known-keywords)) keywords)
          (package-lint--error-at-point
           'warning
           (format "You should include standard keywords: see the variable `finder-known-keywords'.")
           err-pos))))))

(defun package-lint--check-url-header ()
  "Verify that the package has an HTTPS or HTTP Homepage/URL header."
  (if (package-lint--goto-header "\\(?:URL\\|Homepage\\)")
      (let ((url (match-string 3))
            (url-start (match-beginning 3)))
        (unless (and (equal (thing-at-point 'url) url)
                     (string-match-p "^https?://" url))
          (package-lint--error-at-point
           'error
           "Package URLs should be a single HTTPS or HTTP URL."
           url-start)))
    (package-lint--error-at-bob
     'error
     "Package should have a Homepage or URL header.")))

(defun package-lint--check-dependency-list ()
  "Check the contents of the \"Package-Requires\" header.
Return a list of well-formed dependencies, same as
`package-lint--check-well-formed-dependencies'."
  (when (package-lint--goto-header "Package-Requires")
    (let ((position (match-beginning 3))
          (deps (match-string 3)))
      (condition-case err
          (pcase-let ((`(,parsed-deps . ,parse-end-pos) (read-from-string deps)))
            (unless (= parse-end-pos (length deps))
              (package-lint--error-at-bol
               'error
               "More than one expression provided."))
            (let ((deps (package-lint--check-well-formed-dependencies position parsed-deps)))
              (package-lint--check-emacs-version deps)
              (package-lint--check-packages-installable deps)
              (package-lint--check-deps-use-non-snapshot-version deps)
              (package-lint--check-deps-do-not-use-zero-versions deps)
              (package-lint--check-cl-lib-version deps)
              deps))
        (error
         (package-lint--error-at-bol
          'error
          (format "Couldn't parse \"Package-Requires\" header: %s" (error-message-string err)))
         nil)))))

(defun package-lint--check-well-formed-dependencies (position parsed-deps)
  "Check that dependencies listed at POSITION are well-formed.
These PARSED-DEPS must have the format (name \"version\").
Return a list of well-formed dependencies, where each element is of
the form (PACKAGE-NAME PACKAGE-VERSION DEP-POSITION)."
  (let (valid-deps)
    (dolist (entry parsed-deps)
      (pcase entry
        ((and `(,package-name ,package-version)
              (guard (symbolp package-name))
              (guard (stringp package-version)))
         ;; Find the column at which the dependency is declared so we can
         ;; properly report the position of errors.
         (let ((dep-pos
                (save-excursion
                  (goto-char position)
                  (let ((pattern
                         (format "( *\\(%s\\)\\(?:)\\|[^[:alnum:]_\\-].*?)\\)"
                                 (regexp-quote (symbol-name package-name)))))
                    (if (re-search-forward pattern (line-end-position) t)
                        (match-beginning 1)
                      position)))))
           (if (ignore-errors (version-to-list package-version))
               (push (list package-name
                           (version-to-list package-version)
                           dep-pos)
                     valid-deps)
             (package-lint--error-at-point
              'error
              (format "%S is not a valid version string: see `version-to-list'."
                      package-version)
              dep-pos))))
        (_
         (package-lint--error-at-bol
          'error
          (format "Expected (package-name \"version-num\"), but found %S." entry)))))
    valid-deps))

(defun package-lint--check-package-installable (archive-entry package-version dep-pos)
  "Check that ARCHIVE-ENTRY is installable from a configured package archive.

Check that package described by ARCHIVE-ENTRY can be installed at
required version PACKAGE-VERSION.  If not, raise an error for DEP-POS."
  (let* ((package-name (car archive-entry))
         (best-version (package-lint--highest-installable-version-of package-name)))
    (when (version-list-< best-version package-version)
      (package-lint--error-at-point
       'warning
       (format "Version dependency for %s appears too high: try %s" package-name
               (package-version-join best-version))
       dep-pos))))

(defun package-lint--check-emacs-version (valid-deps)
  "Check that all VALID-DEPS are available for installation."
  (pcase-dolist (`(,package-name ,package-version ,dep-pos) valid-deps)
    (when (eq 'emacs package-name)
      (cond
       ((version-list-< package-version '(24))
        (package-lint--error-at-point
         'error
         "You can only depend on Emacs version 24 or greater: package.el for Emacs 23 does not support the \"emacs\" pseudopackage."
         dep-pos))
       ((version-list-<= '(28) package-version)
        (package-lint--error-at-point
         'warning
         "This makes the package uninstallable in all released Emacs versions."
         dep-pos))))))

(defun package-lint--check-packages-installable (valid-deps)
  "Check that all VALID-DEPS are available for installation."
  (pcase-dolist (`(,package-name ,package-version ,dep-pos) valid-deps)
    (unless (eq 'emacs package-name)
      (let ((archive-entry (assq package-name package-archive-contents)))
        (if archive-entry
            (package-lint--check-package-installable archive-entry package-version dep-pos)
          (package-lint--error-at-point
           'error
           (format "Package %S is not installable." package-name)
           dep-pos))))))

(defun package-lint--check-deps-use-non-snapshot-version (valid-deps)
  "Warn about any VALID-DEPS on snapshot versions of packages."
  (pcase-dolist (`(,package-name ,package-version ,dep-pos) valid-deps)
    (unless (version-list-< package-version '(19001201 1))
      (package-lint--error-at-point
       'warning
       (format "Use a non-snapshot version number for dependency on \"%S\" if possible."
               package-name)
       dep-pos))))

(defun package-lint--check-deps-do-not-use-zero-versions (valid-deps)
  "Warn about VALID-DEPS on \"0\" versions of packages."
  (pcase-dolist (`(,package-name ,package-version ,dep-pos) valid-deps)
    (when (equal package-version '(0))
      (package-lint--error-at-point
       'warning
       (format "Use a properly versioned dependency on \"%S\" if possible."
               package-name)
       dep-pos))))

(defun package-lint--check-lexical-binding-requires-emacs-24 (valid-deps)
  "Warn about use of `lexical-binding' when Emacs 24.1 is not among VALID-DEPS."
  (goto-char (point-min))
  (when (package-lint--lexical-binding-declared-in-header-line-p)
    (unless (assq 'emacs valid-deps)
      (package-lint--error-at-point
       'warning
       "You should depend on (emacs \"24.1\") if you need lexical-binding."
       (match-beginning 1)))))

(defun package-lint--inside-comment-or-string-p ()
  "Return non-nil if point is inside a comment or string."
  (let ((ppss (save-match-data (syntax-ppss))))
    (or (nth 3 ppss) (nth 4 ppss))))

(defun package-lint--seen-fboundp-check-for (sym)
  "Return non-nil if a `fboundp' check for SYM is present before point."
  (save-excursion
    (save-match-data
      (and (re-search-backward
            (concat "(" (regexp-opt '("fboundp" "macrop")) "\\s-+#?'" (regexp-quote sym) "\\_>") (point-min) t)
           (not (package-lint--inside-comment-or-string-p))))))

(defun package-lint--map-regexp-match (regexp callback)
  "For every match of REGEXP, call CALLBACK with the first match group.
If callback returns non-nil, the return value - which must be a
list - will be applied to `package-lint--error-at-point'.  If
REGEXP doesn't produce a match group 1, then match group
0 (ie. the whole match string string) will be passed to
CALLBACK."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((sym (or (match-string-no-properties 1)
                     (match-string-no-properties 0))))
        (save-excursion
          (goto-char (or (match-beginning 1)
                         (match-beginning 0)))
          (let ((err (funcall callback sym)))
            (when err
              ;; Check this as late as possible, just before reporting,
              ;; because otherwise the checking process is extremely slow,
              ;; being bottlenecked by `syntax-ppss'.
              (unless (package-lint--inside-comment-or-string-p)
                (apply #'package-lint--error-at-point err)))))))))

(defun package-lint--check-version-regexp-list (valid-deps symbol-regexp type)
  "Warn if symbols matched by SYMBOL-REGEXP are unavailable in the target Emacs.
The target Emacs version is taken from VALID-DEPS, which are the
declared dependencies of this package.  TYPE is the
type of the symbol, either FUNCTION or FEATURE."
  (let ((emacs-version-dep (or (cadr (assq 'emacs valid-deps)) '(0))))
    (package-lint--map-regexp-match
     symbol-regexp
     (lambda (sym)
       (let-alist (package-lint-symbol-info sym)
         (let ((added-in-version (cl-ecase type
                                   ('function .function-added)
                                   ('feature .library-added))))
           (when (and added-in-version (version-list-< emacs-version-dep added-in-version))
             (unless (and (eq type 'function) (package-lint--seen-fboundp-check-for sym))
               (let* ((available-backport-with-ver
                       (cl-ecase type
                         ('feature
                          (cl-some (lambda (bp)
                                     (when (string= (car bp) sym)
                                       (or (cddr bp)
                                           (list (car bp)))))
                                   package-lint-backport-libraries))
                         ('function
                          (cl-some (lambda (bp)
                                     (when (string-match-p (nth 1 bp) sym)
                                       (or (cddr bp)
                                           (list (car bp)))))
                                   package-lint-backport-libraries))))
                      (available-backport (car available-backport-with-ver))
                      (required-backport-version (cadr available-backport-with-ver))
                      (matching-dep (when available-backport
                                      (assoc available-backport valid-deps))))
                 (unless (and matching-dep
                              (or (not required-backport-version)
                                  (version-list-<= (version-to-list required-backport-version)
                                                   (cadr matching-dep))))
                   (list
                    'error
                    (format "You should depend on (emacs \"%s\")%s if you need `%s'."
                            (mapconcat #'number-to-string added-in-version ".")
                            (if available-backport
                                (format " or the %s package"
                                        (if required-backport-version
                                            (format "(%s \"%s\")"
                                                    available-backport
                                                    required-backport-version)
                                          available-backport))
                              "")
                            sym))))))))))))

(defun package-lint--check-eval-after-load ()
  "Warn about use of `eval-after-load' and co."
  (package-lint--map-regexp-match
   "(\\s-*?\\(\\(?:with-\\)?eval-after-load\\)\\_>"
   (lambda (match)
     (list 'warning
           (format "`%s' is for use in configurations, and should rarely be used in packages." match)))))

(defun package-lint--check-no-use-of-cl ()
  "Warn about use of deprecated `cl' library."
  (package-lint--map-regexp-match
   "(\\s-*?require\\s-*?'\\(cl\\)\\_>"
   (lambda (_)
     (list
      'warning
      "Replace deprecated `cl' with `cl-lib'.  The `cl-libify' package can help with this."))))

(defun package-lint--check-no-use-of-cl-lib-sublibraries ()
  "Warn about use of `cl-macs', `cl-seq' etc."
  (package-lint--map-regexp-match
   "(\\s-*?require\\s-*?'\\(cl-macs\\|cl-seq\\)\\_>"
   (lambda (_)
     (list
      'warning
      "This file is not in the `cl-lib' ELPA compatibility package: require `cl-lib' instead."))))

(defconst package-lint--unconditional-require-regexp
  ;; Match the ending paren so we can be sure it's a single argument
  ;; `require'. If there are additional arguments, we don't want to warn,
  ;; because (require 'foo nil t) indicates an optional dependency and
  ;; (require 'foo "filename") is very uncommon.
  "(\\s-*?require\\s-*?'\\_<\\(.*?\\)\\_>\\s-*?)"
  "Regexp to match unconditional `require' forms.")

(defun package-lint--check-libraries-available-in-emacs (valid-deps)
  "Warn about use of libraries that are not available in the Emacs version in VALID-DEPS."
  (package-lint--check-version-regexp-list
   valid-deps
   package-lint--unconditional-require-regexp
   'feature))

(defun package-lint--check-libraries-removed-from-emacs ()
  "Warn about use of libraries that have been removed from Emacs."
  (package-lint--map-regexp-match
   package-lint--unconditional-require-regexp
   (lambda (sym)
     (let-alist (package-lint-symbol-info sym)
       (let ((removed-in-version .library-removed))
         (when removed-in-version
           (list
            'error
            (format "The `%s' library was removed in Emacs version %s."
                    sym (mapconcat #'number-to-string removed-in-version ".")))))))))

(defconst package-lint--function-name-regexp
  (rx
   (seq
    (or "#'"
        (seq "(" (* (syntax whitespace))
             (? (seq
                 (or "funcall" "apply" "advice-add")
                 (+
                  (syntax whitespace))
                 "'"))))
    (group
     (*\? nonl))
    symbol-end))
  "Regexp to match function names.")

(defun package-lint--check-macros-functions-available-in-emacs (valid-deps)
  "Warn about use of functions/macros that are not available in the Emacs version in VALID-DEPS."
  (package-lint--check-version-regexp-list
   valid-deps
   package-lint--function-name-regexp
   'function))

(defun package-lint--check-macros-functions-removed-from-emacs (valid-deps)
  "Warn about use of functions/macros that have been removed from Emacs.
If an Emacs version is specified in VALID-DEPS, don't warn about
functions/macros that were removed and later re-added, as long as
the Emacs dependency matches the re-addition."
  (package-lint--map-regexp-match
   package-lint--function-name-regexp
   (lambda (sym)
     (let-alist (package-lint-symbol-info sym)
       (let ((removed-in-version .function-removed))
         (when removed-in-version
           (let ((emacs-version-dep (or (cadr (assq 'emacs valid-deps)) '(0))))
             (unless (and .function-added (version-list-<= .function-added emacs-version-dep))
               (list
                'error
                (format "`%s' was removed in Emacs version %s."
                        sym (mapconcat #'number-to-string removed-in-version ".")))))))))))

(defun package-lint--check-lexical-binding-is-on-first-line ()
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
                (defvar enable-dir-local-variables)
                (defvar hack-local-variables--warned-lexical)
                (let ((hack-local-variables--warned-lexical nil)
                      (enable-dir-local-variables nil)
                      (enable-local-variables t)
                      (local-enable-local-variables t))
                  (hack-local-variables)
                  (setq lexical-binding-found-at-end
                        hack-local-variables--warned-lexical)))
            (error
             (package-lint--error-at-bob 'error (error-message-string err))
             (cl-return-from return nil)))
          (when (or lexical-binding-found-at-end
                    ;; In case this is an Emacs from before `hack-local-variables'
                    ;; started to warn about `lexical-binding' on a line other
                    ;; than the first.
                    (and (cdr (assq 'lexical-binding file-local-variables-alist))
                         (not (package-lint--lexical-binding-declared-in-header-line-p))))
            (package-lint--error-at-bob
             'error
             "`lexical-binding' must be set in the first line.")))))))

(defun package-lint--check-cl-lib-version (valid-deps)
  "Check that any dependency in VALID-DEPS on \"cl-lib\" is on a remotely-installable version."
  (let ((emacs-version-dep (or (nth 1 (assq 'emacs valid-deps)) '(0)))
        (cl-lib-dep (assq 'cl-lib valid-deps)))
    (when cl-lib-dep
      (let ((cl-lib-version (nth 1 cl-lib-dep)))
        (cond ((and emacs-version-dep
                    (version-list-<= '(24 3) emacs-version-dep)
                    (version-list-<= cl-lib-version '(1 0)))
               (package-lint--error-at-point
                'warning
                "An explicit dependency on cl-lib <= 1.0 is not needed on Emacs >= 24.3."))
              ((version-list-<= '(1) cl-lib-version)
               (package-lint--error-at-point
                'error
                (format "Depend on the latest 0.x version of cl-lib rather than on version \"%S\".
Alternatively, depend on (emacs \"24.3\") or greater, in which cl-lib is bundled."
                        cl-lib-version)
                (nth 2 cl-lib-dep))))))))

(defun package-lint--check-package-version-present ()
  "Check that a valid \"Version\" header is present."
  (let ((version (package-lint--goto-header (rx (? "Package-") "Version"))))
    (if version
        (unless (ignore-errors (version-to-list version))
          (package-lint--error-at-point
           'warning
           (format "\"%s\" is not a valid version. MELPA will handle this, but other archives will not." version)
           (match-beginning 3)))
      (package-lint--error-at-bob
       'warning
       "\"Version:\" or \"Package-Version:\" header is missing. MELPA will handle this, but other archives will not."))))

(defun package-lint--liberal-package-buffer-info ()
  "Like `package-buffer-info', but tolerate missing version header."
  (let ((orig-buffer (current-buffer)))
    ;; We've reported version header issues separately, so rule them out here
    (with-temp-buffer
      (insert-buffer-substring-no-properties orig-buffer)
      (goto-char (point-min))
      (package-lint--update-or-insert-version "0")
      (package-buffer-info))))

(defun package-lint--check-package-el-can-parse ()
  "Check that `package-buffer-info' can read metadata from this file.
If it can, return the read metadata."
  (condition-case err
      (package-lint--liberal-package-buffer-info)
    (error
     (package-lint--error-at-bob
      'error
      (format "package.el cannot parse this buffer: %s" (error-message-string err)))
     nil)))

(defun package-lint--check-package-summary (desc)
  "Check the summary for package with descriptor DESC.
DESC is a struct as returned by `package-buffer-info'."
  (let ((summary (package-lint--package-desc-summary desc)))
    (cond
     ((string= summary "")
      (package-lint--error-at-bob
       'error
       "Package should have a non-empty summary."))
     (t
      (unless (let ((case-fold-search nil))
                (string-match-p "^[A-Z0-9]" summary))
        (package-lint--error-at-bob
         'warning
         "The package summary should start with an uppercase letter or a digit."))
      (when (> (length summary) 60)
        (package-lint--error-at-bob
         'warning
         "The package summary is too long. It should be at most 60 characters."))
      (when (string-match "\\.\\'" summary)
        (package-lint--error-at-bob
         'warning
         "The package summary should not end with a period."))
      (when (save-match-data
              (let ((case-fold-search t))
                (and (string-match "[^.]\\<emacs\\>" summary)
                     (not (string-match-p "[[:space:]]+lisp"
                                          summary (match-end 0))))))
        (package-lint--error-at-bob
         'warning
         "Including \"Emacs\" in the package summary is usually redundant."))))))

(defun package-lint--check-provide-form (desc)
  "Check the provide form for package with descriptor DESC.
DESC is a struct as returned by `package-buffer-info'."
  (let ((name (symbol-name (package-lint--package-desc-name desc))))
    (if (string-match-p "-theme\\'" name)
        (let ((theme-name (replace-regexp-in-string "-theme\\'" "" name))
              (provided-theme (package-lint--provided-theme)))
          (unless (string-equal theme-name provided-theme)
            (package-lint--error-at-bob
             'error
             (format "There is no (provide-theme '%s) form." theme-name))))
      (let ((feature (package-lint--provided-feature)))
        (unless (string-equal name feature)
          (package-lint--error-at-bob
           'error
           (format "There is no (provide '%s) form." name)))))))

(defun package-lint--check-provide-form-secondary-file ()
  "Check there is a provide form."
  ;; We don't require that the provided feature have a name consistent
  ;; with the overall package prefix, but this check may later be added.
  (unless (package-lint--provided-feature)
    (package-lint--error-at-bob
     'error
     "There is no `provide' form.")))

(defun package-lint--check-no-emacs-in-package-name (desc)
  "Check that the package name doesn't contain \"emacs\".
DESC is a struct as returned by `package-buffer-info'."
  (let ((name (package-lint--package-desc-name desc)))
    (when (string-match-p "emacs" (symbol-name name))
      (package-lint--error-at-bob
       'warning
       "The word \"emacs\" is redundant in Emacs package names."))))

(defun package-lint--check-symbol-separators (definitions)
  "Check that symbol DEFINITIONS don't contain non-standard separators."
  (pcase-dolist (`(,name . ,position) definitions)
    (when (and (string-match "[:/]" name)
               (not (string-match-p package-lint--sane-prefixes name)))
      (let ((match-pos (match-beginning 0)))
        ;; As a special case, allow `/=' when at the end of a symbol.
        (when (or (not (string-match (rx "/=" string-end) name))
                  (/= match-pos (match-beginning 0)))
          (goto-char position)
          (package-lint--error
           (line-number-at-pos) 0 'error
           (format "`%s' contains a non-standard separator `%s', use hyphens instead (see Elisp Coding Conventions)."
                   name (substring-no-properties name match-pos (1+ match-pos)))))))))

(defun package-lint--valid-prefix-mapping-p (short-prefix prefix name)
  "Check if NAME corresponds to a valid mappings for PREFIX.
Check is done according to the definitions in
`package-lint--allowed-prefix-mappings' for SHORT-PREFIX."
  (let ((regexp (concat "^" (regexp-opt
                             (cdr (assoc short-prefix package-lint--allowed-prefix-mappings))))))
    (string-prefix-p prefix (replace-regexp-in-string regexp short-prefix name))))

(defun package-lint--valid-definition-name-p (name prefix &optional position)
  "Return non-nil if NAME denotes a valid definition name.

Valid definition names are:

- a NAME starting with PREFIX, a string representing the current
  package prefix,

- a NAME matching `package-lint--sane-prefixes', or

- a NAME whose POSITION in the buffer denotes a global definition."
  (or (string-prefix-p prefix name)
      (string-match-p package-lint--sane-prefixes name)
      (string-match-p (rx-to-string `(seq string-start (or "define" "defun" "defvar" "defface" "with") "-" ,prefix)) name)
      (string-match-p (rx-to-string  `(seq string-start "global-" ,prefix (or "-mode" (seq "-" (* any) "-mode")) string-end)) name)
      (let ((short-prefix (car (cl-remove-if-not (lambda (e) (string-prefix-p e prefix))
                                             (mapcar #'car package-lint--allowed-prefix-mappings)))))
        (when short-prefix
          (package-lint--valid-prefix-mapping-p short-prefix prefix name)))
      (when position
        (goto-char position)
        (looking-at-p (rx (*? space) "(" (*? space)
                          (or "defadvice" "cl-defmethod")
                          symbol-end)))))

(defun package-lint--check-defs-prefix (prefix definitions)
  "Verify that symbol DEFINITIONS start with package PREFIX."
  (pcase-dolist (`(,name . ,position) definitions)
    (unless (package-lint--valid-definition-name-p name prefix position)
      (if (package-lint--added-or-removed-function-p name)
          (package-lint--error-at-point
           'error
           (format "Define compatibility functions with a prefix, e.g. \"%s--%s\", and use `defalias' where they exist."
                   prefix name)
           position)
        (package-lint--error-at-point
         'error
         (format "\"%s\" doesn't start with package's prefix \"%s\"."
                 name prefix)
         position)))))

(defun package-lint--check-minor-mode (def)
  "Offer up concerns about the minor mode definition DEF."
  (when (cl-search '(:global t) def)
    (package-lint--check-globalized-minor-mode def)))

(defun package-lint--check-globalized-minor-mode (def)
  "Offer up concerns about the global minor mode definition DEF."
  (let ((feature-name (package-lint--provided-feature)))
    (when feature-name
      (let ((feature (intern feature-name))
            (autoloaded (save-excursion
                          (forward-line -1)
                          (beginning-of-line)
                          (looking-at ";;;###autoload"))))
        (unless (or autoloaded
                    (cl-search `(:require ',feature) def :test #'equal))
          (package-lint--error-at-point
           'error
           (format
            "Global minor modes should be autoloaded or, rarely, `:require' their defining file (i.e. \":require '%s\"), to support the customization variable of the same name." feature)))))))

(defun package-lint--check-defgroup (def)
  "Offer up concerns about the customization group definition DEF."
  (when (symbolp (cadr def))
    (let ((group-name (symbol-name (cadr def))))
      (when (string-match "\\(.*\\)-mode$" group-name)
        (let ((parent (intern (match-string 1 group-name))))
          (unless (cl-search `(:group ',parent) def :test #'equal)
            (package-lint--error-at-point
             'error
             "Customization groups should not end in \"-mode\" unless that name would conflict with their parent group."))))))

  (unless (memq :group def)
    (package-lint--error-at-point
     'error
     "Customization groups should specify a parent via `:group'.")))

(defun package-lint--check-defalias (prefix def)
  "Offer up concerns about the customization group definition DEF.
PREFIX is the package prefix."
  (pcase (cadr def)
    ((and `(quote ,alias) (guard (symbolp alias)))
     (unless (package-lint--valid-definition-name-p (symbol-name alias) prefix)
       (package-lint--error-at-point
        'error
        (concat "Aliases should start with the package's prefix \"" prefix "\"."))))))

(defun package-lint--check-format-string (valid-deps def)
  "Offer up concerns about the format string used in DEF, depending on VALID-DEPS."
  (let ((emacs-version-dep (or (cadr (assq 'emacs valid-deps)) '(0))))
    (when (consp (cdr def))
      (let ((fmt-str (cadr def)))
        (when (and (version-list-< emacs-version-dep '(26 1))
                   ;; We give up on trying to warn about format strings that are
                   ;; evaluated at runtime.
                   (stringp fmt-str)
                   ;; The usual regexp strategy for finding unescaped matches
                   ;; requires negative lookbehind:
                   ;;  (?<!%)(?:%%)*[0-9]+\$
                   ;; So instead we make sure the count of escape chars is odd
                   (string-match "\\(%+\\)[0-9]+\\$" fmt-str)
                   (cl-oddp (length (match-string 1 fmt-str))))
          (package-lint--error-at-point
           'error
           "You should depend on (emacs \"26.1\") if you need format field numbers."))))))

(defun package-lint--check-lonely-parens ()
  "Warn about dangling closing parens."
  (package-lint--map-regexp-match
   "^\\s-*?\\()\\)"
   (lambda (_)
     ;; Allow dangling parentheses if the preceding line ends with a comment, as
     ;; it's not uncommon even in idiomatic lisp.
     (when (save-excursion
             (end-of-line 0)
             (not (nth 4 (syntax-ppss))))
       (list 'warning
             "Closing parens should not be wrapped onto new lines.")))))



;;; Helpers

(defun package-lint--extract-key-sequence (form)
  "Extract the key sequence from FORM."
  (pcase form
    (`(kbd ,seq)
     (package-lint--extract-key-sequence seq))
    ((or `(global-set-key ,seq ,_) `(local-set-key ,seq ,_))
     (package-lint--extract-key-sequence seq))
    (`(define-key ,_ ,seq ,_)
     (package-lint--extract-key-sequence seq))
    ((pred stringp)
     (listify-key-sequence (read-kbd-macro form)))
    ((pred vectorp)
     (unless (listp (elt form 0))
       (listify-key-sequence form)))))

(defun package-lint--test-keyseq (lks)
  "Return a message if the listified key sequence LKS is invalid, otherwise nil."
  (let* ((modifiers (event-modifiers lks))
         (basic-type (event-basic-type lks)))
    (when (or (and (> (length lks) 1) (equal (car (last lks)) ?\C-g))
              (and (equal (car (last lks)) ?\e)
                   (not (equal (nthcdr (- (length lks) 2) lks)
                               '(?\e ?\e))))
              (equal (car (last lks)) ?\C-h)
              (and (equal modifiers '(control))
                   (equal ?c basic-type)
                   (cdr lks)
                   (let ((v (event-basic-type (cdr lks)))
                         (m (event-modifiers (cdr lks))))
                     (and (numberp v)
                          (<= v ?z)
                          (>= v ?a)
                          (or (null m) (equal '(shift) m)))))
              (member basic-type '(f5 f6 f7 f8 f9)))
      "This key sequence is reserved (see Key Binding Conventions in the Emacs Lisp manual)")))

(defun package-lint--region-empty-p (start end)
  "Return t iff the region between START and END has no non-empty lines.

Lines consisting only of whitespace or empty comments are considered empty."
  (save-excursion
    (save-restriction
      (let ((inhibit-changing-match-data t))
        (narrow-to-region start end)
        (goto-char start)
        (while (and (looking-at "^[[:space:]]*;*[[:space:]]*$")
                    (= 0 (forward-line))))
        (eobp)))))

(defun package-lint--highest-installable-version-of (package)
  "Return the highest version of PACKAGE available for installation."
  (let ((descriptors (cdr (assq package package-archive-contents))))
    (if (fboundp 'package-desc-version)
        (car (sort (mapcar 'package-desc-version descriptors)
                   (lambda (v1 v2) (not (version-list-< v1 v2)))))
      (aref descriptors 0))))

(defun package-lint--goto-header (header-name)
  "Move to the first occurrence of HEADER-NAME in the file.
If the return value is non-nil, then point will be at the end of
the file, and the second and third match groups will contain the name and
value of the header with any leading or trailing whitespace removed."
  (let ((initial-point (point)))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat (lm-get-header-re header-name) "\\(.*?\\) *$") nil t)
          (match-string-no-properties 3)
        (goto-char initial-point)
        nil))))

(defun package-lint--update-or-insert-version (version)
  "Ensure current buffer has a \"Version: VERSION\" header."
  (if (package-lint--goto-header "Version")
      (move-beginning-of-line nil)
    (forward-line))
  (insert (format ";; Version: %s" version))
  (newline))

(defun package-lint--get-header-line-file-local-variables ()
  "Return local variables specified in the -*- line.
Returns an alist of elements (VAR . VAL), where VAR is a variable
and VAL is the specified value.

For details, see `hack-local-variables-prop-line'."
  (cl-letf (((symbol-function #'message) #'ignore))
    (hack-local-variables-prop-line)))

(defun package-lint--lexical-binding-declared-in-header-line-p ()
  "Test if `lexical-binding' is declared in the -*- line."
  ;; Test the `cdr' to see if it's actually true, because
  ;; -*- lexical-binding: nil -*-
  ;; is legal, if silly.
  (cdr (assq 'lexical-binding (package-lint--get-header-line-file-local-variables))))

(defvar semantic-imenu-summary-function)

(defun package-lint--get-defs ()
  "Return a list of all variables and functions defined in the current buffer.

The returned list is of the form (SYMBOL-NAME . POSITION)."
  ;; We probably could use Semantic instead, but it's a *global* minor mode and
  ;; it tends to be quite heavy, so use Imenu instead; if the user has Semantic
  ;; enabled, Imenu will use its index anyway.
  (let ((result '())
        (index
         (save-excursion
           ;; Use the default imenu expression list so that we're not confused
           ;; by user customizations.
           (let ((imenu-generic-expression lisp-imenu-generic-expression)
                 ;; In case it's actually Semantic, tell it not to decorate
                 ;; symbol names.
                 (semantic-imenu-summary-function 'semantic-format-tag-name))
             (funcall imenu-create-index-function)))))
    (dolist (entry index)
      (pcase entry
        ((and `(,submenu-name . ,submenu-elements)
              (guard (consp submenu-elements)))
         (when (member submenu-name '("Variables" "Defuns" "Types"))
           (setq result (nconc (reverse submenu-elements) result))))
        (_
         (push entry result))))
    ;; If it's Semantic, then it returns overlays, not positions. Convert
    ;; them.
    (dolist (entry result)
      (when (overlayp (cdr entry))
        (setcdr entry (overlay-start (cdr entry)))))
    (nreverse result)))

(defun package-lint--provided-feature ()
  "Return the last-provided feature name, as a string, or nil if none."
  (save-excursion
    (goto-char (point-max))
    (cond ((re-search-backward (rx "(provide '" (group (1+ (or (syntax word) (syntax symbol))))) nil t)
           (match-string-no-properties 1))
          ((re-search-backward "(provide-me)" nil t)
           (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))

(defun package-lint--provided-theme ()
  "Return the last-provided theme name, as a string, or nil if none."
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward (rx "(provide-theme '" (group (1+ (or (syntax word) (syntax symbol))))) nil t)
      (match-string-no-properties 1))))

(defun package-lint--get-package-prefix ()
  "Return package prefix string (i.e. the symbol the package `provide's).
Prefix is returned without any `-mode' suffix."
  (let ((feature (package-lint--provided-feature)))
    (when feature
      (replace-regexp-in-string "-mode\\'" "" feature))))

(defun package-lint--check-objects-by-regexp (regexp function)
  "Check all objects with the literal printed form matching REGEXP.

The objects are parsed with `read'.  The FUNCTION is passed the
read object, with the point at the beginning of the match.

S-expressions in comments or comments, partial s-expressions, or
otherwise invalid read forms are ignored."
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (save-excursion
      (goto-char (match-beginning 0))
      (let ((obj (unless (package-lint--inside-comment-or-string-p)
                   (save-excursion
                     (ignore-errors (read (current-buffer)))))))
        (when obj (funcall function obj))))))


;;; Public interface

;;;###autoload
(defun package-lint-buffer (&optional buffer)
  "Get linter errors and warnings for BUFFER.

Returns a list, each element of which is list of

   (LINE COL TYPE MESSAGE)

where TYPE is either 'warning or 'error.

Current buffer is used if none is specified."
  (with-current-buffer (or buffer (current-buffer))
    (unless (derived-mode-p 'emacs-lisp-mode)
      (error "Buffer must be in emacs-lisp-mode"))
    (package-lint--check-all)))

;;;###autoload
(defun package-lint-current-buffer ()
  "Display lint errors and warnings for the current buffer."
  (interactive)
  (let ((errs (package-lint-buffer))
        (buf "*Package-Lint*"))
    (with-current-buffer (get-buffer-create buf)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (cond
         ((null errs) (insert "No issues found."))
         ((null (cdr errs)) (insert "1 issue found:\n\n"))
         (t (insert (format "%d issues found:\n\n" (length errs)))))
        (pcase-dolist (`(,line ,col ,type ,message) errs)
          (insert (format "%d:%d: %s: %s\n" line col type message))))
      (special-mode)
      (view-mode 1))
    (display-buffer buf)))

(defgroup package-lint nil
  "A linting library for elisp package authors"
  :group 'development)

(defcustom package-lint-batch-fail-on-warnings t
  "When non-nil, make warnings fatal for `package-lint-batch-and-exit'."
  :group 'package-lint
  :type 'boolean)

(defun package-lint-batch-and-exit-1 (filenames)
  "Internal helper function for `package-lint-batch-and-exit'.

Checks FILENAMES using package-lint.

The main loop is this separate function so it's easier to test."
  ;; Make sure package.el is initialized so we can query its database.
  (package-initialize)
  (let ((success t) (last-directory nil) (text-quoting-style 'grave))
    (dolist (file filenames success)
      (let* ((file (expand-file-name file))
             (file-directory (file-name-directory file))
             (base (file-name-nondirectory file)))
        (with-temp-buffer
          (insert-file-contents file t)
          (emacs-lisp-mode)
          (let ((checking-result (package-lint-buffer))
                (fail-on (cons 'error (when package-lint-batch-fail-on-warnings '(warning)))))
            (when (cl-some (lambda (err) (memq (nth 2 err) fail-on)) checking-result)
              (setq success nil)
              (unless (equal last-directory file-directory)
                (setq last-directory file-directory)
                (message "Entering directory '%s'" file-directory))
              (pcase-dolist (`(,line ,col ,type ,message) checking-result)
                (message "%s:%d:%d: %s: %s"
                         base line col type message)))))))))

;;;###autoload
(defun package-lint-batch-and-exit ()
  "Run `package-lint-buffer' on the files remaining on the command line.
Use this only with -batch, it won't work interactively.

When done, exit Emacs with status 1 in case of any errors, otherwise exit
with status 0.  The variable `package-lint-batch-fail-on-warnings' controls
whether or not warnings alone produce a non-zero exit code."
  (unless noninteractive
    (error "`package-lint-batch-and-exit' is to be used only with -batch"))
  (let ((success (package-lint-batch-and-exit-1 command-line-args-left)))
    (kill-emacs (if success 0 1))))

;;;###autoload
(defun package-lint-looks-like-a-package-p ()
  "Return non-nil if the current buffer appears to be intended as a package."
  (or package-lint-main-file
      (save-match-data
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (re-search-forward
             (lm-get-header-re (rx (or "Version" "Package-Version" "Package-Requires")))
             nil t))))))

(provide 'package-lint)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; package-lint.el ends here
