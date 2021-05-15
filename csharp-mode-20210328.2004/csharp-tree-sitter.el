;;; csharp-tree-sitter.el --- tree sitter support for C#  -*- lexical-binding: t; -*-

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Jostein Kjønigsen <jostein@gmail.com>
;;            : Theodor Thornhill <theo@thornhill.no>
;; Created    : September 2020
;; Modified   : 2020
;; Version    : 0.11.0
;; Keywords   : c# languages oop mode
;; X-URL      : https://github.com/emacs-csharp/csharp-mode
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.12.1") (tree-sitter-indent "0.1") (tree-sitter-langs "0.9.1"))

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


;;; Code:
(require 'cl-lib)
(require 'cl-extra)
(require 'seq)

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-indent)
(require 'tree-sitter-langs)

(require 'csharp-compilation)

(defvar csharp-mode-syntax-table)
(defvar csharp-mode-map)

;;; Tree-sitter

(setq csharp-mode-tree-sitter-patterns
  [ ;; Various constructs
   (comment) @comment
   (modifier) @keyword
   (this_expression) @keyword
   (escape_sequence) @keyword

   ;; Literals
   [(real_literal) (integer_literal)] @number
   (null_literal) @constant
   (boolean_literal) @constant
   [(string_literal)
    (verbatim_string_literal)
    (interpolated_string_text)
    (interpolated_verbatim_string_text)
    (character_literal)
    "\""
    "$\""
    "@$\""
    "$@\""] @string

   ;; Keywords
   ["using" "namespace" "class" "if" "else" "throw" "new" "for"
    "return" "await" "struct" "enum" "switch" "case"
    "default" "typeof" "try" "catch" "finally" "break"
    "foreach" "in" "yield" "get" "set" "when" "as" "out"
    "is" "while" "continue" "this" "ref" "goto" "interface"
    "from" "where" "select" "lock" "base" "record" "init"
    "with" "let"
    ] @keyword

   ;; Linq
   (from_clause (identifier) @variable)
   (group_clause)
   (order_by_clause)
   (select_clause (identifier) @variable)
   (query_continuation (identifier) @variable) @keyword

   ;; Enum
   (enum_member_declaration (identifier) @variable)
   (enum_declaration (identifier) @type)

   ;; Interface
   (interface_declaration
    name: (identifier) @type)

   ;; Struct
   (struct_declaration (identifier) @type)

   ;; Record
   (record_declaration (identifier) @type)

   (with_expression
    (with_initializer_expression
     (simple_assignment_expression
      (identifier) @variable)))

   ;; Namespace
   (namespace_declaration
    name: (identifier) @type)

   ;; Class
   (base_list (identifier) @type)
   (property_declaration
    (generic_name))
   (property_declaration
    type: (nullable_type) @type
    name: (identifier) @variable)
   (property_declaration
    type: (predefined_type) @type
    name: (identifier) @variable)
   (property_declaration
    type: (identifier) @type
    name: (identifier) @variable)
   (class_declaration
    name: (identifier) @type)
   (constructor_declaration (identifier) @type)

   ;; Method
   (method_declaration (identifier) @type (identifier) @function)
   (method_declaration (predefined_type) @type (identifier) @function)
   (method_declaration (nullable_type) @type (identifier) @function)
   (method_declaration (void_keyword) @type (identifier) @function)
   (method_declaration (generic_name) (identifier) @function)
   (method_declaration (qualified_name (identifier) @type) (identifier) @function)

   ;; Function
   (local_function_statement (identifier) @type (identifier) @function)
   (local_function_statement (predefined_type) @type (identifier) @function)
   (local_function_statement (nullable_type) @type (identifier) @function)
   (local_function_statement (void_keyword) @type (identifier) @function)
   (local_function_statement (generic_name) (identifier) @function)

   ;; Lambda
   (lambda_expression
    (identifier) @variable)

   ;; Parameter
   (parameter
    type: (qualified_name) @type)
   (parameter
    type: (identifier) @type
    name: (identifier) @variable.parameter)
   (parameter (identifier) @variable.parameter)

   ;; Array
   (array_rank_specifier (identifier) @variable)
   (array_type (identifier) @type)
   (array_creation_expression)

   ;; Attribute
   (attribute (identifier) @variable (attribute_argument_list))
   (attribute (identifier) @variable)

   ;; Object init
   (anonymous_object_creation_expression)
   (object_creation_expression (identifier) @type)
   (initializer_expression (identifier) @variable)

   ;; Typeof
   (type_of_expression (identifier) @variable)

   ;; Member access
   (invocation_expression (member_access_expression (generic_name (identifier) @method.call)))
   (invocation_expression (member_access_expression (identifier)\? @method.call .))
   (member_access_expression (identifier) @variable)

   ;; Variable
   (variable_declaration (identifier) @type)
   (variable_declarator (identifier) @variable)

   ;; Equals value
   (equals_value_clause (identifier) @variable)

   ;; Return
   (return_statement (identifier) @variable)
   (yield_statement (identifier) @variable)

   ;; Type
   (type_parameter
    (identifier) @type)
   (type_argument_list
    (identifier) @type.argument)
   (generic_name
    (identifier) @type)
   (implicit_type) @type
   (predefined_type) @type
   (nullable_type) @type
   ["operator"] @type

   ;; Type constraints
   (type_parameter_constraints_clause
    (identifier) @type)
   ;; (type_parameter_constraint
   ;;  (identifier) @type) ;; causes parsing error in tree-sitter
   (type_constraint
    (identifier) @type)

   ;; Exprs
   (binary_expression (identifier) @variable (identifier) @variable)
   (binary_expression (identifier)* @variable)
   (conditional_expression (identifier) @variable)
   ;; (prefix_unary_expression (identifier)* @variable) ;; crashes tree-sitter c-code with SIGABRT
   (postfix_unary_expression (identifier)* @variable)
   (assignment_expression (identifier) @variable)
   (cast_expression (identifier) @type)

   ;; Preprocessor
   (preprocessor_directive) @constant
   (preprocessor_call (identifier) @string)

   ;; Loop
   (for_each_statement (implicit_type) @type (identifier) @variable)
   (for_each_statement (predefined_type) @type (identifier) @variable)
   (for_each_statement (identifier) @type (identifier) @variable)

   ;; Exception
   (catch_declaration (identifier) @type (identifier) @variable)
   (catch_declaration (identifier) @type)

   ;; Switch
   (switch_statement (identifier) @variable)
   (switch_expression (identifier) @variable)

   ;; If
   (if_statement (identifier) @variable)

   ;; Declaration expression
   (declaration_expression (implicit_type) (identifier) @variable)

   ;; Arrow expression
   (arrow_expression_clause (identifier) @variable)

   ;; Lock statement
   (lock_statement (identifier) @variable)

   ;; Other
   ;; (argument_list
   ;;  (identifier) @variable) ;; causes parsing error in tree-sitter
   (label_name) @variable
   (using_directive (identifier) @type.parameter)
   (using_directive (qualified_name) @type.parameter)
   (using_directive (name_equals (identifier) @type.parameter))
   ;; (await_expression (identifier)* @function) ;; crashes tree-sitter c-code with sigabrt!
   (invocation_expression (identifier) @function)
   (element_access_expression (identifier) @variable)
   (conditional_access_expression (identifier) @variable)
   (member_binding_expression (identifier) @variable)
   (name_colon (identifier)* @variable.special)
   (field_declaration)
   (argument (identifier) @variable)

   ;; Catch-alls
   (identifier) @variable

   ;; Interpolation
   ;; (interpolated_string_expression) @string
   ]
  )

;;; Tree-sitter indentation

(defgroup csharp-mode-indent nil "Indent lines using Tree-sitter as backend"
  :group 'tree-sitter)

(defcustom csharp-tree-sitter-indent-offset 4
  "Indent offset for csharp-tree-sitter-mode."
  :type 'integer
  :group 'csharp)

(defvar tree-sitter-indent-csharp-tree-sitter-scopes
  '((indent-all . ;; these nodes are always indented
                (accessor_declaration
                 break_statement
                 arrow_expression_clause
                 parameter_list
                 conditional_expression
                 constructor_initializer
                 argument_list
                 "."))
    (indent-rest . ;; if parent node is one of these and node is not first → indent
                 (
                  binary_expression
                  switch_section
                  ))
    (indent-body . ;; if parent node is one of these and current node is in middle → indent
                 (enum_member_declaration_list
                  base_list
                  block
                  anonymous_object_creation_expression
                  initializer_expression
                  expression_statement
                  declaration_list
                  attribute_argument_list
                  switch_body))

    (paren-indent . ;; if parent node is one of these → indent to paren opener
                  (parenthesized_expression))
    (align-char-to . ;; chaining char → node types we move parentwise to find the first chaining char
                   ())
    (aligned-siblings . ;; siblings (nodes with same parent) should be aligned to the first child
                      (parameter))

    (multi-line-text . ;; if node is one of these, then don't modify the indent
                     ;; this is basically a peaceful way out by saying "this looks like something
                     ;; that cannot be indented using AST, so best I leave it as-is"
                     (comment
                      preprocessor_call
                      labeled_statement))
    (outdent . ;; these nodes always outdent (1 shift in opposite direction)
             (;; "}"
              case_switch_label

              ))

    (align-to-node-line . ;; this group has lists of alist (node type . (node types... ))
                          ;; we move parentwise, searching for one of the node
                          ;; types associated with the key node type. if found,
                          ;; align key node with line where the ancestor node
                          ;; was found.
             ((block . (lambda_expression))))
    )
  "Scopes for indenting in C#.")

;;; tree-sitter helper-functions. navigation, editing, etc.
;;; may be subject to future upstreaming-effort

(defun csharp-beginning-of-defun ()
  "Replacement-function for `beginning-of-defun' for `csharp-tree-sitter-mode'."
  (interactive)
  (when-let ((declaration
              (cl-some (lambda (decl)
                         (tree-sitter-node-at-point decl))
                       '(method_declaration
                         constructor_declaration
                         class_declaration
                         namespace_declaration))))
    (goto-char (tsc-node-start-position declaration))))

(defun csharp-end-of-defun ()
  "Replacement-function for `end-of-defun' for `csharp-tree-sitter-mode'."
  (interactive)
  (when-let ((declaration
              (cl-some (lambda (decl)
                         (tree-sitter-node-at-point decl))
                       '(method_declaration
                         constructor_declaration
                         class_declaration
                         namespace_declaration))))
    (goto-char (tsc-node-end-position declaration))))

(defun csharp-delete-method-at-point ()
  "Deletes the method at point."
  (interactive)
  (when-let ((method (tree-sitter-node-at-point 'method_declaration)))
    (delete-region (tsc-node-start-position method)
                   (tsc-node-end-position method))))

(defun csharp-change-string-at-point ()
  "Change string at point."
  (interactive)
  (when-let ((method (tree-sitter-node-at-point 'string_literal)))
    (delete-region (1+ (tsc-node-start-position method))
                   (1- (tsc-node-end-position method)))))

;;; end tree-sitter helper-functions.

(defvar csharp-tree-sitter-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in csharp-mode buffers.")

(defvar csharp-tree-sitter-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?@ "_" table)
    table))

;;;###autoload
(define-derived-mode csharp-tree-sitter-mode prog-mode "C#"
  "Major mode for editing Csharp code.

Key bindings:
\\{csharp-tree-sitter-mode-map}"
  :group 'csharp
  :syntax-table csharp-tree-sitter-mode-syntax-table

  (setq-local indent-line-function #'tree-sitter-indent-line)
  (setq-local beginning-of-defun-function #'csharp-beginning-of-defun)
  (setq-local end-of-defun-function #'csharp-end-of-defun)

  ;; https://github.com/ubolonton/emacs-tree-sitter/issues/84
  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))
  (setq-local tree-sitter-hl-default-patterns csharp-mode-tree-sitter-patterns)
  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  (tree-sitter-hl-mode)
  (tree-sitter-indent-mode))

(add-to-list 'tree-sitter-major-mode-language-alist '(csharp-tree-sitter-mode . c-sharp))

(provide 'csharp-tree-sitter)

;;; csharp-tree-sitter.el ends here
