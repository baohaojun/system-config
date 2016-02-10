;;; ac-js2.el --- Auto-complete source for Js2-mode, with navigation

;; Copyright (C) 2013  Scott Barnett

;; Author: Scott Barnett <scott.n.barnett@gmail.com>
;; URL: https://github.com/ScottyB/ac-js2
;; Version: 1.0
;; Package-Requires: ((js2-mode "20090723")(skewer-mode "1.4"))

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

;; An attempt to get context sensitive Javascript completion in Emacs.
;; Basic completions are obtained by parsing Javascript code with
;; Js2-mode's parser.
;;
;; Installation
;;
;; Easiest way to get ac-js2 is to install it from MELPA. You may need
;; this snippet
;;
;; `(add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)'
;;
;; if you don't have it already to fetch packages from MELPA.
;;
;; Enable ac-js2 in js2-mode as follows:
;;
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
;;
;; Ac-js2 does not require auto-complete mode but I suggest you grab
;; it anyway as ac-js2 is designed to work with a completion frontend.
;; Support for Company mode is on its way.
;;
;; For more comprehensive completions you can opt to evaluate the code
;; for candidates. A browser needs to be connected to Emacs for the
;; evaluation completions to work. Put this in your init.el file.
;;
;; `(setq ac-js2-evaluate-calls t)'
;;
;; To add completions for external libraries add something like this:
;;
;; (add-to-list 'ac-js2-external-libraries "path/to/lib/library.js")
;;
;; Then connect a browser to Emacs by calling `(run-skewer)'. You may
;; need to save the buffer for completions to start.
;;
;; If auto-complete mode is installed on your system then completions
;; should start showing up otherwise use `completion-at-point'.
;;
;; Note: library completions will only work if `ac-js2-evaluate-calls'
;; is set and a browser is connected to Emacs.
;;
;; Bonus: M-. is bound to `ac-js2-jump-to-definition' which will jump
;; to Javascript definitions found in the same buffer. Given the
;; following proprety reference:
;;
;; foo.bar.baz();
;;
;; placing the cursor on `foo', `bar' or `baz' and executing M-. will
;; take you straight to their respective definitions. Use M-, to jump
;; back to where you were. Also works for object literals.
;;
;; Recently added `ac-js2-expand-function' that will expand a function's
;; parameters bound to `C-c C-c`. Expansion will only work if the cursor
;; is after the function.
;;
;; If you have any issues or suggestions please create an issue on Github:
;; https://github.com/ScottyB/ac-js2

;;; History:

;; Version 1.0
;;   * Navigation within current buffer
;;   * Completion and docstring for objects via Skewer
;;   * External library support
;;   * Basic completions of objects in current buffer

;;; Code:

(require 'js2-mode)
(require 'skewer-mode)
(require 'cl-lib)
(require 'etags)

(defgroup ac-js2 nil
  "Auto-completion for js2-mode."
  :group 'completion
  :prefix "ac-js2-")

;;; Configuration variables

(defcustom ac-js2-add-ecma-262-externs t
  "If non-nil add `js2-ecma-262-externs' to completion candidates.")

(defcustom ac-js2-add-browser-externs t
  "If non-nil add `js2-browser-externs' to completion candidates.")

(defcustom ac-js2-add-keywords t
  "If non-nil add `js2-keywords' to completion candidates.")

(defcustom ac-js2-add-prototype-completions t
  "When non-nil traverse the prototype chain adding to completion candidates.")

(defcustom ac-js2-external-libraries '()
  "List of absolute paths to external Javascript libraries.")

(defcustom ac-js2-evaluate-calls nil
  "Warning. When true function calls will be evaluated in the browser.
This may cause undesired side effects however it will
  provide better completions. Use at your own risk.")

(defcustom ac-js2-force-reparse t
  "Force Js2-mode to reparse buffer before fetching completion candidates.")

;;; Internal variables

(defvar ac-js2-keywords '()
  "Cached string version of `js2-keywords'.")

(defvar ac-js2-candidates '())

;; Types of skewer completion methods available
(defconst ac-js2-method-eval 0)
(defconst ac-js2-method-global 1
  "Return candidates for the global object.
Only keys of the object are returned as the other properties come
  from js2-mode's externs.")

(defvar ac-js2-data-root (file-name-directory load-file-name)
  "Location of data files needed for `ac-js2-on-skewer-load'.")

;;; Skewer integration

(defvar ac-js2-skewer-candidates '()
  "Cadidates obtained from skewering.")

(defun ac-js2-on-skewer-load ()
  "Inject skewer addon and evaluate external libraries in browser."
  (insert-file-contents (expand-file-name "skewer-addon.js" ac-js2-data-root))
  (and ac-js2-evaluate-calls
       (mapcar (lambda (library)
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name library))
                   (skewer-eval (buffer-string)
                                nil
                                :type "complete"))) ac-js2-external-libraries)))

(defun ac-js2-skewer-completion-candidates ()
  "Get completions returned from skewer."
  (mapcar (lambda (candidate) (symbol-name (car candidate))) ac-js2-skewer-candidates))

(defun ac-js2-skewer-document-candidates (name)
  "Return document string for NAME from skewer."
  (let ((doc (cdr (assoc-string name ac-js2-skewer-candidates))))
    (or (ac-js2-format-function doc) doc)))

(defun ac-js2-get-object-properties (name)
  "Find properties of NAME for completion."
  (ac-js2-skewer-eval-wrapper name `((prototypes . ,ac-js2-add-prototype-completions))))

(defun ac-js2-skewer-result-callback (result)
  "Process the RESULT passed from the browser."
  (let ((value (cdr (assoc 'value result))))
    (if (and (skewer-success-p result) value)
        (setq ac-js2-skewer-candidates (append value nil)))))

(defun ac-js2-skewer-eval-wrapper (str &optional extras)
  "Wrap `skewer-eval-synchronously' to check if a skewer-client is avilable.
STR is the text to send to the browser for evaluation. Extra
parameters can be passed to the browser using EXTRAS. EXTRAS must
be of the form (param-string . value) where param-string is the
reference and value is the value that can be retrieved from the
request object in Javacript."
  (setq ac-js2-skewer-candidates nil)
  (if skewer-clients
      (if (or ac-js2-evaluate-calls
              (not (ac-js2-has-function-calls str)))
          (ac-js2-skewer-result-callback
           (skewer-eval-synchronously str
                                      :type "complete"
                                      :extra extras)))
    (setq skewer-queue nil)))

;; Generate candidates
(defun ac-js2-candidates ()
  "Main function called to gather candidates for auto-completion."
  (if ac-js2-force-reparse (js2-reparse))
  (let ((node (js2-node-parent (js2-node-at-point (1- (point)))))
        beg
        (prop-get-regex "[a-zA-Z)]\\.")
        name)
    (setq ac-js2-candidates nil)
    (cond
     ((looking-back "\\.")
      ;; TODO: Need to come up with a better way to extract object than this regex!!
      (save-excursion
        (setq beg (and (skip-chars-backward "[a-zA-Z_$][0-9a-zA-Z_$#\"())]+\\.") (point))))
      (setq name (buffer-substring-no-properties beg (1- (point))))
      (ac-js2-get-object-properties name)
      (setq node (ac-js2-initialized-node (if (string-match prop-get-regex name)
                                              (reverse (split-string name prop-get-regex)) name)))
      (if (js2-object-node-p node)
          (setq ac-js2-candidates
                (mapcar (lambda (elem)
                          (ac-js2-format-node (js2-node-string (js2-object-prop-node-left elem))
                                              elem))
                        (js2-object-node-elems node))))
      (append (mapcar 'first ac-js2-candidates)
              (ac-js2-skewer-completion-candidates)))
     ((js2-prop-get-node-p node)
      (setq node (js2-prop-get-node-left node))
      (setq name (js2-node-string node))
      (ac-js2-get-object-properties name)
      (ac-js2-skewer-completion-candidates))
     (t
      (ac-js2-skewer-eval-wrapper "" `((method . ,ac-js2-method-global)))
      (append (ac-js2-skewer-completion-candidates)
              (ac-js2-add-extra-completions
               (mapcar 'first (ac-js2-get-names-in-scope))))))))

(defun ac-js2-document (name)
  "Show documentation for NAME from local buffer if present
otherwise use documentation obtained from skewer."
  (let* ((docs (cdr (assoc name ac-js2-candidates)))
         (doc (if (listp docs) (first docs) docs)))
    (if doc doc (ac-js2-skewer-document-candidates name))))

;; Auto-complete settings

(defun ac-js2-ac-candidates ()
  "Completion candidates for auto-complete mode."
  (ac-js2-candidates))

(defun ac-js2-ac-document (name)
  "Documentation to be shown for auto-complete mode."
  (ac-js2-document name))

(defun ac-js2-ac-prefix()
  (or (ac-prefix-default) (ac-prefix-c-dot)))

(defun ac-js2-save ()
  "Called on `before-save-hook' to evaluate buffer."
  (interactive)
  (when (string= major-mode "js2-mode")
    (ac-js2-skewer-eval-wrapper (buffer-string)))
  t)

;;;###autoload
(defun ac-js2-expand-function()
  "Expand the function definition left of point.
Expansion will only occur for candidates whose documentation
string contain a function prototype."
  (interactive)
  (let* ((word (progn
                 (if (featurep 'auto-complete) (ac-complete))
                 (substring-no-properties (or (thing-at-point 'word) ""))))
         (candidate (ac-js2-ac-document word)))
    (if (and (looking-back word) (stringp candidate))
        (when (string-match "^function" candidate)
          (cond ((featurep 'yasnippet)
                 (yas-expand-snippet
                  (concat "("
                          (replace-regexp-in-string "\\([a-zA-Z0-9]+\\)"
                                                    (lambda (txt) (concat "${" txt "}"))
                                                    (second (split-string candidate "[()]")))
                          ")$0"))))))))

(defun ac-js2-setup-auto-complete-mode ()
  "Setup ac-js2 to be used with auto-complete-mode."
  (add-to-list 'ac-sources 'ac-source-js2)
  (auto-complete-mode)
  (ac-define-source "js2"
    '((candidates . ac-js2-ac-candidates)
      (document . ac-js2-ac-document)
      (prefix .  ac-js2-ac-prefix)
      (requires . -1))))

;;; Completion at point function

;;;###autoload
(defun ac-js2-completion-function ()
  "Function for `completions-at-point'."
  (save-excursion
    (let ((bounds (if (looking-back "\\.")
                      (cons (point) (point))
                    (bounds-of-thing-at-point 'word))))
      (list (car bounds) (cdr bounds) (ac-js2-candidates)))))

;;; Company

;;;###autoload
(defun ac-js2-company (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (if (not (featurep 'company))
      (message "Company is not installed")
    (case command
      (interactive (company-begin-backend 'ac-js2-company))
      (prefix (when ac-js2-mode
                (or (company-grab-symbol)
                    'stop)))
      (candidates (all-completions arg (ac-js2-candidates)))
      (duplicates t)
      (meta (let ((doc (ac-js2-document arg)))
              (when doc
                (with-temp-buffer
                  (insert doc)
                  (js-mode)
                  (font-lock-fontify-buffer)
                  (buffer-string))))))))

;;; Helper functions

(defun ac-js2-build-prop-name-list (prop-node)
  "Build a list of names from a PROP-NODE."
  (let* (names
         left
         left-node)
    (unless (js2-prop-get-node-p prop-node)
      (error "Node is not a property prop-node"))
    (while (js2-prop-get-node-p prop-node)
      (push (js2-name-node-name (js2-prop-get-node-right prop-node)) names)
      (setq left-node (js2-prop-get-node-left prop-node))
      (when (js2-name-node-p left-node)
        (setq left (js2-name-node-name left-node)))
      (setq prop-node (js2-node-parent prop-node)))
    (append names `(,left))))

(defun ac-js2-prop-names-left (name-node)
  "Create a list of all of the names in the property NAME-NODE.
NAME-NODE must have a js2-prop-get-node as parent. Only adds
properties to the left of point. This is so individual jump
points can be found for each property in the chain."
  (let* (name
         (parent (js2-node-parent name-node))
         left
         names)
    (unless (or (js2-prop-get-node-p parent) (js2-name-node-p name-node))
      (error "Not a name node or doesn't have a prop-get-node as parent"))
    (setq name (js2-name-node-name name-node)
          left (js2-prop-get-node-left parent))
    (if (and (js2-name-node-p left)
             (string= name (js2-name-node-name left)))
        (setq names name)
      (js2-visit-ast
       parent
       (lambda (node endp)
         (unless endp
           (if (js2-name-node-p node)
               (push (js2-name-node-name node) names)
             t))))
      names)))

(defun ac-js2-has-function-calls (string)
  "Check if the Javascript code in STRING has a Js2-call-node."
  (with-temp-buffer
    (insert string)
    (let* ((ast (js2-parse)))
      (catch 'call-node
        (js2-visit-ast-root
         ast
         (lambda (node end-p)
           (unless end-p
             (if (js2-call-node-p node)
                 (throw 'call-node t)
               t))))))))

(defun ac-js2-add-extra-completions (completions)
  "Add extra candidates to COMPLETIONS."
  (append completions
          (if ac-js2-add-keywords (or ac-js2-keywords (setq ac-js2-keywords (mapcar 'symbol-name js2-keywords))))
          (if ac-js2-add-ecma-262-externs js2-ecma-262-externs)
          (if ac-js2-add-browser-externs js2-browser-externs)))

(defun ac-js2-root-or-node ()
  "Return the current node or js2-ast-root node."
  (let ((node (js2-node-at-point)))
    (if (js2-ast-root-p node)
        node
      (js2-node-get-enclosing-scope node))))

(defun ac-js2-get-names-in-scope ()
  "Fetches all symbols in scope and formats them for completion."
  (let* ((scope (ac-js2-root-or-node))
         result)
    (while scope
      (setq result (append result
                           (loop for item in (js2-scope-symbol-table scope)
                                 if (not (assoc (car item) result))
                                 collect item)))
      (setq scope (js2-scope-parent-scope scope)))
    (setq ac-js2-candidates
          (mapcar #'(lambda (x)
                      (let* ((name (symbol-name (car x)))
                             (init (ac-js2-initialized-node name)))
                        (ac-js2-format-node name init)))
                  result))))

(defun ac-js2-initialized-node (name)
  "Return initial value assigned to NAME.
NAME may be either a variable, a function or a variable that
holds a function. NAME may also be a list of names that make up a
object property. Returns nil if no initial value can be found."
  (let* ((node (if (listp name) (ac-js2-find-property name)
                 (ac-js2-name-declaration name)))
         (parent (if node (js2-node-parent node)))
         (init (cond
                ((js2-function-node-p parent)
                 parent)
                ((js2-function-node-p node)
                 node)
                ((js2-var-init-node-p parent)
                 (js2-var-init-node-initializer parent))
                ((js2-assign-node-p parent)
                 (js2-assign-node-right parent))
                (t
                 nil))))
    init))

(defun ac-js2-name-declaration (name)
  "Return the declaration node for node named NAME."
  (let* ((node (ac-js2-root-or-node))
         (scope-def (js2-get-defining-scope node name))
         (scope (if scope-def (js2-scope-get-symbol scope-def name) nil))
         (symbol (if scope (js2-symbol-ast-node scope) nil)))
    (if (not symbol)
        (ac-js2-get-function-node name scope-def)
      symbol)))

;;; Completion candidate formating

(defun ac-js2-format-node (name node)
  "Format NAME and NODE for completion.
Returned format is a list where the first element is the NAME of
the node (shown in completion candidate list) and the last
element is the text to show as documentation."
  (let ((node (if (js2-object-prop-node-p node) (js2-object-prop-node-right node) node))
        (name-format (replace-regexp-in-string "\"" "" name))
        (doc (if (and (js2-function-node-p node)
                      (cl-find name (js2-function-node-params node)
                               :test '(lambda (name param) (string= name (js2-name-node-name param)))))
                 "Function parameter"
               (ac-js2-format-node-doc node))))
    `(,name-format . ,doc)))

(defun ac-js2-format-object-node-doc (obj-node)
  "Format OBJ-NODE to display as documentation."
  (let (elems)
    (unless (js2-object-node-p obj-node)
      (error "Node is not an object node"))
    (setq elems (js2-object-node-elems obj-node))
    (if (not elems)
        "{}"
      (mapconcat #'(lambda (x) (ac-js2-format-js2-object-prop-doc x)) elems "\n"))))

(defun ac-js2-format-node-doc (node)
  "Format NODE for displaying in a document string."
  (let* ((node-above (and node (js2-node-at-point
                                (save-excursion
                                  (goto-char (js2-node-abs-pos node))
                                  (forward-line -1)
                                  (point)))))
         (comment (if (js2-comment-node-p node-above)
                      (ac-js2-format-comment (js2-node-string node-above))))
         (doc (cond
               ((js2-function-node-p node)
                (ac-js2-format-function node))
               ((js2-object-node-p node)
                (ac-js2-format-object-node-doc node))
               ((js2-object-prop-node-p node)
                (ac-js2-format-node-doc (js2-object-prop-node-right node)))
               (t
                (if (js2-node-p node) (js2-node-string node) "")))))
    (if comment (concat comment "\n" doc) doc)))

(defun ac-js2-format-js2-object-prop-doc (obj-prop)
  "Format an OBJ-PROP for displaying as a document string."
  (unless (js2-object-prop-node-p obj-prop)
    (error "Node is not an object property node"))
  (let* ((left (js2-object-prop-node-left obj-prop))
         (right (js2-object-prop-node-right obj-prop)))
    (concat (js2-node-string left) " : "
            (ac-js2-format-node-doc right))))

(defun ac-js2-format-function (func)
  "Formats a function for a document string.
FUNC can be either a function node or a string starting with
'function'. Returns nil if neither."
  (let ((str (or (and (js2-function-node-p func) (js2-node-string func))
                 (and (stringp func) (eq 0 (string-match "function" func)) func))))
    (if str (substring str 0 (1+ (string-match ")" str))))))

(defun ac-js2-format-comment (comment)
  "Prepare a COMMENT node for displaying in a popup."
  (let* ((node-string (if (js2-comment-node-p comment)
                          (js2-node-string comment)
                        comment))
         (string (replace-regexp-in-string "[ \t]$" ""
                                           (replace-regexp-in-string "^[ \t\n*/*]+" "" node-string))))
    string))

;;; Navigation commands for js2-mode

(defun ac-js2-find-property (list-names)
  "Find the property definition that consists of LIST-NAMES.
Supports navigation to 'foo.bar = 3' and 'foo = {bar: 3}'."
  (catch 'prop-found
    (js2-visit-ast-root
     js2-mode-ast
     (lambda (node endp)
       (let ((parent (js2-node-parent node)))
         (unless endp
           (if (or (and (js2-prop-get-node-p node)
                    (not (or (js2-elem-get-node-p parent) (js2-call-node-p parent)))
                    (equal list-names (ac-js2-build-prop-name-list node)))
                   (and (js2-name-node-p node)
                        (js2-object-prop-node-p parent)
                        (string= (js2-name-node-name node)
                                 (first list-names))))
               (throw 'prop-found node))
           t))))))

(defun ac-js2-get-function-node (name scope)
  "Return node of function named NAME in SCOPE."
  (catch 'function-found
    (js2-visit-ast
     scope
     (lambda (node end-p)
       (when (and (not end-p)
                  (string= name (ac-js2-get-function-name node)))
         (throw 'function-found node))
       t))
    nil))

;;;###autoload
(defun ac-js2-jump-to-definition ()
  "Jump to the definition of an object's property, variable or function.
Navigation to a property definend in an Object literal isn't
implemented."
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (let* ((node (js2-node-at-point))
         (parent (js2-node-parent node))
         (prop-names (if (js2-prop-get-node-p parent)
                         (ac-js2-prop-names-left node)))
         (name (if (and (js2-name-node-p node)
                        (not (js2-object-prop-node-p parent)))
                   (js2-name-node-name node)
                 (error "Node is not a supported jump node")))
         (node-init (if (and prop-names (listp prop-names))
                        (ac-js2-find-property prop-names)
                      (ac-js2-name-declaration name))))
    (unless node-init
      (pop-tag-mark)
      (error "No jump location found"))
    (goto-char (js2-node-abs-pos node-init))))

(defun ac-js2-get-function-name (fn-node)
  "Return the name of the function FN-NODE.
Value may be either function name or the variable name that holds
the function."
  (let ((parent (js2-node-parent fn-node)))
    (if (js2-function-node-p fn-node)
        (or (js2-function-name fn-node)
            (if (js2-var-init-node-p parent)
                (js2-name-node-name (js2-var-init-node-target parent)))))))

(defvar ac-js2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'ac-js2-jump-to-definition)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    (define-key map (kbd "C-c C-c") 'ac-js2-expand-function)
    map)
  "Keymap for `ac-js2-mode'.")

;;; Minor mode

;;;###autoload
(define-minor-mode ac-js2-mode
  "A minor mode that provides auto-completion and navigation for Js2-mode."
  :keymap ac-js2-mode-map
  (if (featurep 'auto-complete)
      (ac-js2-setup-auto-complete-mode))
  (set (make-local-variable 'completion-at-point-functions)
       (cons 'ac-js2-completion-function completion-at-point-functions))
  (ac-js2-skewer-eval-wrapper (buffer-string))
  (add-hook 'before-save-hook 'ac-js2-save nil t)
  (add-hook 'skewer-js-hook 'ac-js2-on-skewer-load))


(provide 'ac-js2)

;;; ac-js2.el ends here
