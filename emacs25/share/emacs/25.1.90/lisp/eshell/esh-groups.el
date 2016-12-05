;;; esh-groups.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "em-alias" "em-alias.el" "b5d641f823e89046c6639e1964ee9aed")
;;; Generated autoloads from em-alias.el

(defgroup eshell-alias nil "\
Command aliases allow for easy definition of alternate commands." :tag "Command aliases" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-banner" "em-banner.el" "754cabd89e4e2b3f041ff98e599e9a52")
;;; Generated autoloads from em-banner.el

(defgroup eshell-banner nil "\
This sample module displays a welcome banner at login.
It exists so that others wishing to create their own Eshell extension
modules may have a simple template to begin with." :tag "Login banner" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-basic" "em-basic.el" "b691da09f26fc3366892271fb256b5a7")
;;; Generated autoloads from em-basic.el

(defgroup eshell-basic nil "\
The \"basic\" code provides a set of convenience functions which
are traditionally considered shell builtins.  Since all of the
functionality provided by them is accessible through Lisp, they are
not really builtins at all, but offer a command-oriented way to do the
same thing." :tag "Basic shell commands" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-cmpl" "em-cmpl.el" "24d5fa823f4dd3bbdb1d18db96b6e599")
;;; Generated autoloads from em-cmpl.el

(defgroup eshell-cmpl nil "\
This module provides a programmable completion function bound to
the TAB key, which allows for completing command names, file names,
variable names, arguments, etc." :tag "Argument completion" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-dirs" "em-dirs.el" "6bcb8f7a52b3056755e7f4753ba3640b")
;;; Generated autoloads from em-dirs.el

(defgroup eshell-dirs nil "\
Directory navigation involves changing directories, examining the
current directory, maintaining a directory stack, and also keeping
track of a history of the last directory locations the user was in.
Emacs does provide standard Lisp definitions of `pwd' and `cd', but
they lack somewhat in feel from the typical shell equivalents." :tag "Directory navigation" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-glob" "em-glob.el" "403c44f17cb990c084e528b5aa3033e9")
;;; Generated autoloads from em-glob.el

(defgroup eshell-glob nil "\
This module provides extended globbing syntax, similar what is used
by zsh for filename generation." :tag "Extended filename globbing" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-hist" "em-hist.el" "06746f65deb188d2f49072acf137c2cc")
;;; Generated autoloads from em-hist.el

(defgroup eshell-hist nil "\
This module provides command history management." :tag "History list management" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-ls" "em-ls.el" "dc0bb3cddb8ebbdee9aa78b6a27397a9")
;;; Generated autoloads from em-ls.el

(defgroup eshell-ls nil "\
This module implements the \"ls\" utility fully in Lisp.  If it is
passed any unrecognized command switches, it will revert to the
operating system's version.  This version of \"ls\" uses text
properties to colorize its output based on the setting of
`eshell-ls-use-colors'." :tag "Implementation of `ls' in Lisp" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-pred" "em-pred.el" "5fd0b247d38cd8f10a9a534ab5bba5ca")
;;; Generated autoloads from em-pred.el

(defgroup eshell-pred nil "\
This module allows for predicates to be applied to globbing
patterns (similar to zsh), in addition to string modifiers which can
be applied either to globbing results, variable references, or just
ordinary strings." :tag "Value modifiers and predicates" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-prompt" "em-prompt.el" "c8e7eeedd517d7818828e6895d41174a")
;;; Generated autoloads from em-prompt.el

(defgroup eshell-prompt nil "\
This module provides command prompts, and navigation between them,
as is common with most shells." :tag "Command prompts" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-rebind" "em-rebind.el" "1dfeb0ea10e0bb018115a24863dffb0e")
;;; Generated autoloads from em-rebind.el

(defgroup eshell-rebind nil "\
This module allows for special keybindings that only take effect
while the point is in a region of input text.  By default, it binds
C-a to move to the beginning of the input text (rather than just the
beginning of the line), and C-p and C-n to move through the input
history, C-u kills the current input text, etc.  It also, if
`eshell-confine-point-to-input' is non-nil, does not allow certain
commands to cause the point to leave the input area, such as
`backward-word', `previous-line', etc.  This module intends to mimic
the behavior of normal shells while the user editing new input text." :tag "Rebind keys at input" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-script" "em-script.el" "834c3aef6a254387f28aa4169635e97b")
;;; Generated autoloads from em-script.el

(defgroup eshell-script nil "\
This module allows for the execution of files containing Eshell
commands, as a script file." :tag "Running script files." :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-smart" "em-smart.el" "04df4b8c72a776a8941302f0b0887c2c")
;;; Generated autoloads from em-smart.el

(defgroup eshell-smart nil "\
This module combines the facility of normal, modern shells with
some of the edit/review concepts inherent in the design of Plan 9's
9term.  See the docs for more details.

Most likely you will have to turn this option on and play around with
it to get a real sense of how it works." :tag "Smart display of output" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-term" "em-term.el" "f5657c80b6b1c4c17681ce49aaba7b9d")
;;; Generated autoloads from em-term.el

(defgroup eshell-term nil "\
This module causes visual commands (e.g., `vi') to be executed by
the `term' package, which comes with Emacs.  This package handles most
of the ANSI control codes, allowing curses-based applications to run
within an Emacs window.  The variable `eshell-visual-commands' defines
which commands are considered visual in nature." :tag "Running visual commands" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-tramp" "em-tramp.el" "fd5d04d6835e41a66789db424e67683a")
;;; Generated autoloads from em-tramp.el

(defgroup eshell-tramp nil "\
This module defines commands that use TRAMP in a way that is
  not transparent to the user.  So far, this includes only the
  built-in su and sudo commands, which are not compatible with
  the full, external su and sudo commands, and require the user
  to understand how to use the TRAMP sudo method." :tag "TRAMP Eshell features" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-unix" "em-unix.el" "26934c387bcecd3ad96811a9fc1ada5b")
;;; Generated autoloads from em-unix.el

(defgroup eshell-unix nil "\
This module defines many of the more common UNIX utilities as
aliases implemented in Lisp.  These include mv, ln, cp, rm, etc.  If
the user passes arguments which are too complex, or are unrecognized
by the Lisp variant, the external version will be called (if
available).  The only reason not to use them would be because they are
usually much slower.  But in several cases their tight integration
with Eshell makes them more versatile than their traditional cousins
\(such as being able to use `kill' to kill Eshell background processes
by name)." :tag "UNIX commands in Lisp" :group (quote eshell-module))

;;;***

;;;### (autoloads nil "em-xtra" "em-xtra.el" "21e7533f8b8e3916836b5b5bbc0c0082")
;;; Generated autoloads from em-xtra.el

(defgroup eshell-xtra nil "\
This module defines some extra alias functions which are entirely
optional.  They can be viewed as samples for how to write Eshell alias
functions, or as aliases which make some of Emacs's behavior more
naturally accessible within Emacs." :tag "Extra alias functions" :group (quote eshell-module))

;;;***

(provide 'esh-groups)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; esh-groups.el ends here
