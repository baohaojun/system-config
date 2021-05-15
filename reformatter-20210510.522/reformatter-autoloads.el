;;; reformatter-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "reformatter" "reformatter.el" (0 0 0 0))
;;; Generated autoloads from reformatter.el

(autoload 'reformatter-define "reformatter" "\
Define a reformatter command with NAME.

When called, the reformatter will use PROGRAM and any ARGS to
reformat the current buffer.  The contents of the buffer will be
passed as standard input to the reformatter, which should output
them to standard output.  A nonzero exit code will be reported as
failure, and the output of the command to standard error will be
displayed to the user.

The macro accepts the following keyword arguments:

PROGRAM (required)

  Provides a form which should evaluate to a string at runtime,
  e.g. a literal string, or the name of a variable which holds
  the program path.

ARGS

  Command-line arguments for the program.  If provided, this is a
  form which evaluates to a list of strings at runtime.  Default
  is the empty list.  This form is evaluated at runtime so that
  you can use buffer-local variables to influence the args passed
  to the reformatter program: the variable `input-file' will be
  lexically bound to the path of a file containing the text to be
  reformatted: see the keyword options INPUT-FILE, STDIN and
  STDOUT for more information.

STDIN

  When non-nil (the default), the program is passed the input
  data on stdin.  Set this to nil when your reformatter can only
  operate on files in place.  In such a case, your ARGS should
  include a reference to the `input-file' variable, which will be
  bound to an input path when evaluated.

STDOUT

  When non-nil (the default), the program is expected to write
  the reformatted text to stdout.  Set this to nil if your
  reformatter can only operate on files in place, in which case
  the contents of the temporary input file will be used as the
  replacement text.

INPUT-FILE

  Sometimes your reformatter program might expect files to be in
  a certain directory or have a certain file extension.  This option
  lets you handle that.

  If provided, it is a form which will be evaluated before each
  run of the formatter, and is expected to return a temporary
  file path suitable for holding the region to be reformatted.
  It must not produce the same path as the current buffer's file
  if that is set: you shouldn't be operating directly on the
  buffer's backing file.  The temporary input file will be
  deleted automatically.  You might find the function
  `reformatter-temp-file-in-current-directory' helpful.

MODE

  Unless nil, also generate a minor mode that will call the
  reformatter command from `before-save-hook' when enabled.
  Default is t.

GROUP

  If provided, this is the custom group used for any generated
  modes or custom variables.  Don't forget to declare this group
  using a `defgroup' form.

LIGHTER

  If provided, this is a mode lighter string which will be used
  for the \"-on-save\" minor mode.  It should have a leading
  space.  The supplied value will be used as the default for a
  generated custom variable which specifies the mode lighter.
  Default is nil, ie. no lighter.

KEYMAP

  If provided, this is the symbol name of the \"-on-save\" mode's
  keymap, which you must declare yourself.  Default is no keymap.

EXIT-CODE-SUCCESS-P

  If provided, this is a function object callable with `funcall'
  which accepts an integer process exit code, and returns non-nil
  if that exit code is considered successful.  This could be a
  lambda, quoted symbol or sharp-quoted symbol.  If not supplied,
  the code is considered successful if it is `zerop'.

\(fn NAME &key PROGRAM ARGS (MODE t) (STDIN t) (STDOUT t) INPUT-FILE LIGHTER KEYMAP GROUP (EXIT-CODE-SUCCESS-P \\='zerop))" nil t)

(function-put 'reformatter-define 'lisp-indent-function 'defun)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "reformatter" '("reformatter-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; reformatter-autoloads.el ends here
