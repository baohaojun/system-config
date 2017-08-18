(autoload 'weblogger-mode "weblogger")
;;;### (autoloads nil "../../../../../usr/share/emacs24/site-lisp/w3m/w3m"
;;;;;;  "../../../../../usr/share/emacs24/site-lisp/w3m/w3m.el" (22816
;;;;;;  62977 110958 342000))
;;; Generated autoloads from ../../../../../usr/share/emacs24/site-lisp/w3m/w3m.el

(autoload 'w3m-retrieve "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Retrieve web contents pointed to by URL.
It will put the retrieved contents into the current buffer.

If HANDLER is nil, this function will retrieve web contents, return
the content type of the retrieved data, and then come to an end.  This
behavior is what is called a synchronous operation.  You have to
specify HANDLER in order to make this function show its real ability,
which is called an asynchronous operation.

If HANDLER is a function, this function will come to an end in no time.
In this case, contents will be retrieved by the asynchronous process
after a while.  And after finishing retrieving contents successfully,
HANDLER will be called on the buffer where this function starts.  The
content type of the retrieved data will be passed to HANDLER as a
string argument.

NO-UNCOMPRESS specifies whether this function should not uncompress contents.
NO-CACHE specifies whether this function should not use cached contents.
POST-DATA and REFERER will be sent to the web server with a request.

\(fn URL &optional NO-UNCOMPRESS NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(autoload 'w3m-download "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Download contents of URL to a file named FILENAME.
NO-CACHE (which the prefix argument gives when called interactively)
specifies not using the cached data.

\(fn &optional URL FILENAME NO-CACHE HANDLER POST-DATA)" t nil)

(autoload 'w3m-goto-url "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Visit World Wide Web pages.  This is the primitive function of `w3m'.
If the second argument RELOAD is non-nil, reload a content of URL.
Except that if it is 'redisplay, re-display the page without reloading.
The third argument CHARSET specifies a charset to be used for decoding
a content.
The fourth argument POST-DATA should be a string or a cons cell.
If it is a string, it makes this function request a body as if
the content-type is \"x-www-form-urlencoded\".  If it is a cons cell,
the car of a cell is used as the content-type and the cdr of a cell is
used as the body.
If the fifth argument REFERER is specified, it is used for a Referer:
field for this request.
The remaining HANDLER, ELEMENT[1], NO-POPUP, and SAVE-POS[2] are for
the internal operations of emacs-w3m.
You can also use \"quicksearch\" url schemes such as \"gg:emacs\" which
would search for the term \"emacs\" with the Google search engine.
See the `w3m-search' function and the variable `w3m-uri-replace-alist'.

Notes for the developers:
\[1] ELEMENT is a history element which has already been registered in
the `w3m-history-flat' variable.  It is corresponding to URL to be
retrieved at this time, not for the url of the current page.

\[2] SAVE-POS leads this function to save the current emacs-w3m window
configuration; i.e. to run `w3m-history-store-position'.
`w3m-history-store-position' should be called in a w3m-mode buffer, so
this will be convenient if a command that calls this function may be
invoked in other than a w3m-mode buffer.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER HANDLER ELEMENT NO-POPUP SAVE-POS)" t nil)

(autoload 'w3m-goto-url-new-session "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Visit World Wide Web pages in a new session.
If you invoke this command in the emacs-w3m buffer, the new session
will be created by copying the current session.  Otherwise, the new
session will start afresh.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER)" t nil)

(autoload 'w3m-gohome "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Go to the Home page.

\(fn)" t nil)

(autoload 'w3m-create-empty-session "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Create an empty page as a new session and visit it.

\(fn)" t nil)

(autoload 'w3m "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Visit World Wide Web pages using the external w3m command.

When you invoke this command interactively for the first time, it will
visit a page which is pointed to by a string like url around the
cursor position or the home page specified by the `w3m-home-page'
variable, but you will be prompted for a URL if `w3m-quick-start' is
nil (default t) or `w3m-home-page' is nil.

The variables `w3m-pop-up-windows' and `w3m-pop-up-frames' control
whether this command should pop to a window or a frame up for the
session.

When emacs-w3m sessions have already been opened, this command will
pop to the existing window or frame up, but if `w3m-quick-start' is
nil, (default t), you will be prompted for a URL (which defaults to
`popup' meaning to pop to an existing emacs-w3m buffer up).

In addition, if the prefix argument is given or you enter the empty
string for the prompt, this command will visit a url at the point, or
the home page the `w3m-home-page' variable specifies, or the \"about:\"
page.

You can also run this command in the batch mode as follows:

  emacs -f w3m http://emacs-w3m.namazu.org/ &

In that case, or if this command is called non-interactively, the
variables `w3m-pop-up-windows' and `w3m-pop-up-frames' will be ignored
\(treated as nil) and it will run emacs-w3m at the current (or the
initial) window.

If the optional NEW-SESSION is non-nil, this function makes a new
emacs-w3m buffer.  Besides that, it also makes a new emacs-w3m buffer
if `w3m-make-new-session' is non-nil and a user specifies a url string.

The optional INTERACTIVE-P is for the internal use; it is mainly used
to check whether Emacs 22 or later calls this function as an
interactive command in the batch mode.

\(fn &optional URL NEW-SESSION INTERACTIVE-P)" t nil)

(autoload 'w3m-browse-url "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Ask emacs-w3m to browse URL.
NEW-SESSION specifies whether to create a new emacs-w3m session.  URL
defaults to the string looking like a url around the cursor position.
Pop to a window or a frame up according to `w3m-pop-up-windows' and
`w3m-pop-up-frames'.

\(fn URL &optional NEW-SESSION)" t nil)

(autoload 'w3m-find-file "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Function used to open FILE whose name is expressed in ordinary format.
The file name will be converted into the file: scheme.

\(fn FILE)" t nil)

(autoload 'w3m-region "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Render the region of the current buffer between START and END.
URL specifies the address where the contents come from.  It can be
omitted or nil when the address is not identified.  CHARSET is used
for decoding the contents.  If it is nil, this function attempts to
parse the meta tag to extract the charset.

\(fn START END &optional URL CHARSET)" t nil)

(autoload 'w3m-buffer "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Render the current buffer.
See `w3m-region' for the optional arguments.

\(fn &optional URL CHARSET)" t nil)

;;;***

;;;### (autoloads nil "../../gcode/ajoke/etc/elisp/ajoke" "../../gcode/ajoke/etc/elisp/ajoke.el"
;;;;;;  (22565 50747 892628 16000))
;;; Generated autoloads from ../../gcode/ajoke/etc/elisp/ajoke.el

(autoload 'ajoke--pick-one "../../gcode/ajoke/etc/elisp/ajoke" "\
Pick an item from COLLECTION, which is a list.
ARGS is passed to the supporting function completing-read (or
HELM's or Anything's version of completing-read: you are strongly
advised to use one of these elisp tools).

\(fn PROMPT COLLECTION &rest ARGS)" nil nil)

(autoload 'ajoke-get-imports "../../gcode/ajoke/etc/elisp/ajoke" "\
Write the java import statements automatically.

\(fn)" t nil)

(autoload 'ajoke-get-hierarchy "../../gcode/ajoke/etc/elisp/ajoke" "\
Print the class/interface inheritance hierarchy for the
current class. Output is in compilation-mode for ease of cross
referencing.

\(fn)" t nil)

(autoload 'ajoke-get-override "../../gcode/ajoke/etc/elisp/ajoke" "\
Overide a method defined in super classes/interfaces.

\(fn)" t nil)

(autoload 'ajoke-resolve "../../gcode/ajoke/etc/elisp/ajoke" "\
Resolve the type (class/interface) of ID.

\(fn ID)" t nil)

(autoload 'ajoke-complete-method "../../gcode/ajoke/etc/elisp/ajoke" "\
Complete a method given an ID. First will resolve the
type (class/interface) of ID, then complete using the type's
methods.

\(fn ID)" t nil)

(autoload 'ajoke-search-local-id "../../gcode/ajoke/etc/elisp/ajoke" "\
Search an identifier such as a local variable from the
beginning of current defun.

\(fn)" t nil)

(autoload 'ajoke-get-imports-if-java-mode "../../gcode/ajoke/etc/elisp/ajoke" "\
get imports if java-mode

\(fn)" t nil)

(autoload 'ajoke--pick-output-line "../../gcode/ajoke/etc/elisp/ajoke" "\


\(fn PROMPT COMMAND &rest COMP-READ-ARGS)" nil nil)

(autoload 'ajoke-find-file-using-beagrep "../../gcode/ajoke/etc/elisp/ajoke" "\


\(fn)" t nil)

(autoload 'ajoke-android-add-string "../../gcode/ajoke/etc/elisp/ajoke" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../src/github/twittering-mode/twittering-mode"
;;;;;;  "../../../src/github/twittering-mode/twittering-mode.el"
;;;;;;  (21127 30529 662546 17000))
;;; Generated autoloads from ../../../src/github/twittering-mode/twittering-mode.el

(autoload 'twit "../../../src/github/twittering-mode/twittering-mode" "\
Start twittering-mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../src/github/weibo.emacs/weibo" "../../../src/github/weibo.emacs/weibo.el"
;;;;;;  (22926 30368 24403 465000))
;;; Generated autoloads from ../../../src/github/weibo.emacs/weibo.el

(autoload 'weibo "../../../src/github/weibo.emacs/weibo" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "../skeleton-complete/bbyac" "../skeleton-complete/bbyac.el"
;;;;;;  (22926 36780 679730 327000))
;;; Generated autoloads from ../skeleton-complete/bbyac.el

(autoload 'bbyac-mode "../skeleton-complete/bbyac" "\
Toggle the `bbyac-mode' minor mode.

\(fn &optional ARG)" t nil)

(defvar bbyac-global-mode nil "\
Non-nil if Bbyac-Global mode is enabled.
See the `bbyac-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `bbyac-global-mode'.")

(custom-autoload 'bbyac-global-mode "../skeleton-complete/bbyac" nil)

(autoload 'bbyac-global-mode "../skeleton-complete/bbyac" "\
Toggle Bbyac mode in all buffers.
With prefix ARG, enable Bbyac-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Bbyac mode is enabled in all buffers where
`turn-on-bbyac-mode' would do it.
See `bbyac-mode' for more information on Bbyac mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "ahk-mode" "ahk-mode.el" (21105 487 946563
;;;;;;  464000))
;;; Generated autoloads from ahk-mode.el

(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))

(autoload 'ahk-mode "ahk-mode" "\
Major mode for editing AutoHotKey Scripts.

The hook functions in `ahk-mode-hook' are run after mode initialization.

Key bindings:
\\{ahk-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ajoke" "ajoke.el" (22677 47767 48660 213000))
;;; Generated autoloads from ajoke.el

(autoload 'ajoke--pick-one "ajoke" "\
Pick an item from COLLECTION, which is a list.
ARGS is passed to the supporting function completing-read (or
HELM's or Anything's version of completing-read: you are strongly
advised to use one of these elisp tools).

\(fn PROMPT COLLECTION &rest ARGS)" nil nil)

(autoload 'ajoke-get-imports "ajoke" "\
Write the java import statements automatically.

\(fn)" t nil)

(autoload 'ajoke-get-hierarchy "ajoke" "\
Print the class/interface inheritance hierarchy for the
current class. Output is in compilation-mode for ease of cross
referencing.

\(fn)" t nil)

(autoload 'ajoke-get-override "ajoke" "\
Overide a method defined in super classes/interfaces.

\(fn)" t nil)

(autoload 'ajoke-resolve "ajoke" "\
Resolve the type (class/interface) of ID.

\(fn ID)" t nil)

(autoload 'ajoke-complete-method "ajoke" "\
Complete a method given an ID. First will resolve the
type (class/interface) of ID, then complete using the type's
methods.

\(fn ID)" t nil)

(autoload 'ajoke-search-local-id "ajoke" "\
Search an identifier such as a local variable from the
beginning of current defun.

\(fn)" t nil)

(autoload 'ajoke-get-imports-if-java-mode "ajoke" "\
get imports if java-mode

\(fn)" t nil)

(autoload 'ajoke--pick-output-line "ajoke" "\


\(fn PROMPT COMMAND &rest COMP-READ-ARGS)" nil nil)

(autoload 'ajoke-find-file-using-beagrep "ajoke" "\


\(fn)" t nil)

(autoload 'ajoke-android-add-string "ajoke" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "bbyac" "bbyac.el" (21714 4977 596324 222000))
;;; Generated autoloads from bbyac.el

(autoload 'bbyac-mode "bbyac" "\
Toggle the `bbyac-mode' minor mode.

\(fn &optional ARG)" t nil)

(defvar bbyac-global-mode nil "\
Non-nil if Bbyac-Global mode is enabled.
See the `bbyac-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `bbyac-global-mode'.")

(custom-autoload 'bbyac-global-mode "bbyac" nil)

(autoload 'bbyac-global-mode "bbyac" "\
Toggle Bbyac mode in all buffers.
With prefix ARG, enable Bbyac-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Bbyac mode is enabled in all buffers where
`turn-on-bbyac-mode' would do it.
See `bbyac-mode' for more information on Bbyac mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "bhj-defines" "bhj-defines.el" (22926 30334
;;;;;;  741359 444000))
;;; Generated autoloads from bhj-defines.el

(autoload 'cleanup-buffer-safe "bhj-defines" "\
Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad.

\(fn)" t nil)

(autoload 'bhj-2-window-visit-next-file "bhj-defines" "\
Make there 2 windows, and the other window visit the next buffer in buffer-list

\(fn)" t nil)

(autoload 'confirm-risky-remote-edit "bhj-defines" "\


\(fn)" nil nil)

(autoload 'linux-c-mode "bhj-defines" "\
C mode with adjusted defaults for use with the Linux kernel.

\(fn)" t nil)

(autoload 'linux-c++-mode "bhj-defines" "\
C mode with adjusted defaults for use with the Linux kernel.

\(fn)" t nil)

(autoload 'compout-mode "bhj-defines" "\
compilation mode, which is not buffer readonly for org export

\(fn)" t nil)

(autoload 'grepout-mode "bhj-defines" "\
grep mode, which is not buffer readonly for org export

\(fn)" t nil)

(autoload 'bhj-c-get-includes "bhj-defines" "\


\(fn PREFIX)" t nil)

(autoload 'bhj-indent-region-as-prev-line "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-occur "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-occur-make-errors "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-occur-logcat-errors "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-occur-merge-conflicts "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-w3m-scroll-up-or-next-url "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-w3m-scroll-down-or-previous-url "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-mimedown "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-set-smtp-cred-to-company-mail "bhj-defines" "\


\(fn)" nil nil)

(autoload 'bhj-set-reply "bhj-defines" "\


\(fn)" t nil)

(autoload 'switch-to-devenv "bhj-defines" "\
Jump to VS.NET, at the same file & line as in emacs

\(fn)" t nil)

(autoload 'devenv-toggle-breakpoint "bhj-defines" "\
Toggle a breakpoint at the current line

\(fn)" t nil)

(autoload 'devenv-debug "bhj-defines" "\
Run the debugger in VS.NET

\(fn)" t nil)

(autoload 'poor-mans-csharp-mode "bhj-defines" "\


\(fn)" nil nil)

(autoload 'try-all-themes "bhj-defines" "\


\(fn)" t nil)

(autoload 'try-all-color-themes "bhj-defines" "\


\(fn)" t nil)

(autoload 'where-are-we "bhj-defines" "\


\(fn)" t nil)

(autoload 'android-get-help "bhj-defines" "\


\(fn)" t nil)

(autoload 'visit-code-reading "bhj-defines" "\


\(fn &optional ARG)" t nil)

(autoload 'waw-next-error "bhj-defines" "\


\(fn &optional ARGP RESET)" t nil)

(autoload 'waw-ret-key "bhj-defines" "\


\(fn)" t nil)

(autoload 'waw-mode "bhj-defines" "\
Major mode for output from \\[where-are-we].

\(fn)" t nil)

(autoload 'java-bt-ret-key "bhj-defines" "\


\(fn)" t nil)

(autoload 'java-bt-next-error "bhj-defines" "\


\(fn &optional ARGP RESET)" t nil)

(autoload 'java-bt-mode "bhj-defines" "\
Major mode for output from java back trace.

\(fn)" t nil)

(autoload 'sc--mark-need-merge "bhj-defines" "\
Mark git need merge for system-config.

\(fn)" t nil)

(autoload 'indent-same-space-as-prev-line "bhj-defines" "\


\(fn N-PREV &optional FROM-BOL)" t nil)

(autoload 'back-to-indent-same-space-as-prev-line "bhj-defines" "\


\(fn N-PREV)" t nil)

(autoload 'save-all-buffers-no-check-modified "bhj-defines" "\


\(fn)" t nil)

(autoload 'revert-all-buffers "bhj-defines" "\


\(fn)" t nil)

(autoload 'switch-buffer-same-filename "bhj-defines" "\


\(fn &optional REVERSE)" t nil)

(autoload 'switch-buffer-same-filename-rev "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-sudoedit "bhj-defines" "\


\(fn)" t nil)

(autoload 'localedit "bhj-defines" "\


\(fn)" t nil)

(autoload 'gnus-gmail-search-subject "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-help-it "bhj-defines" "\
open help for the current word

\(fn)" t nil)

(autoload 'bhj-set-working-buffer "bhj-defines" "\
set the current working buffer

\(fn)" t nil)

(autoload 'bhj-help-qt "bhj-defines" "\
open help for the current word for qt

\(fn)" t nil)

(autoload 'bhj-view-mail-external "bhj-defines" "\
open the current maildir file in kmail

\(fn)" t nil)

(autoload 'my-bbdb/gnus-update-records-mode "bhj-defines" "\


\(fn)" nil nil)

(autoload 'bhj-flatten-list "bhj-defines" "\
Return a new, flat list that contains all elements of LIST.

\(bhj-flatten-list '(1 (2 3 (4 5 (6))) 7))
=> (1 2 3 4 5 6 7)

\(fn LIST)" nil nil)

(autoload 'bhj-bbdb-complete-mail "bhj-defines" "\
Complete the user full-name or net-address before point (up to the
preceeding newline, colon, or comma, or the value of START-POS).  If
what has been typed is unique, insert an entry of the form \"User Name
<net-addr>\" (although see documentation for
bbdb-dwim-net-address-allow-redundancy).  If it is a valid completion
but not unique, a list of completions is displayed.

If the completion is done and `bbdb-complete-name-allow-cycling' is
true then cycle through the nets for the matching record.

When called with a prefix arg then display a list of all nets.

Completion behaviour can be controlled with `bbdb-completion-type'.

\(fn &optional START-POS)" t nil)

(autoload 'bhj-org-tasks-closed-last-week "bhj-defines" "\
Produces an org agenda tags view list of the tasks completed
in the specified month and year. Month parameter expects a number
from 1 to 12. Year parameter expects a four digit number. Defaults
to the current month when arguments are not provided. Additional search
criteria can be provided via the optional match-string argument

\(fn &optional MATCH-STRING)" t nil)

(autoload 'bhj-do-code-generation "bhj-defines" "\


\(fn)" t nil)

(autoload 'dos2unix "bhj-defines" "\
Convert this entire buffer from MS-DOS text file format to UNIX.

\(fn)" t nil)

(autoload 'insert-today "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-do-dictionary "bhj-defines" "\
lookup the current word (or region) in dictionary

\(fn WORD)" t nil)

(autoload 'bhj-do-search "bhj-defines" "\
lookup the current word (or region) in dictionary

\(fn WORD)" t nil)

(autoload 'bhj-open-android-doc-on-java-buffer "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-find-missing-file "bhj-defines" "\


\(fn)" t nil)

(autoload 'source-code-help "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-upcase-symbol-or-region "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-downcase-symbol-or-region "bhj-defines" "\


\(fn)" t nil)

(autoload 'weekrep "bhj-defines" "\


\(fn)" t nil)

(autoload 'wiki-local-bhj "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-clt-insert-file-name "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-file-basename "bhj-defines" "\


\(fn)" nil nil)

(autoload 'bhj-insert-pwdw "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-insert-pwdu "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-jdk-help "bhj-defines" "\
start jdk help

\(fn JDK-WORD)" t nil)

(autoload 'ajoke-pop-mark "bhj-defines" "\
Pop back to where ajoke was last invoked.

\(fn)" t nil)

(autoload 'ajoke-pop-mark-back "bhj-defines" "\
Pop back to where ajoke was last invoked.

\(fn)" t nil)

(autoload 'bhj-c-show-current-func "bhj-defines" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "bhj-grep" "bhj-grep.el" (22291 25036 565117
;;;;;;  73000))
;;; Generated autoloads from bhj-grep.el

(autoload 'bhj-grep-tag-default "bhj-grep" "\


\(fn)" nil nil)

(autoload 'bhj-edit-grep-pattern "bhj-grep" "\


\(fn)" t nil)

(autoload 'grep-bhj-dir "bhj-grep" "\


\(fn)" t nil)

(autoload 'grep-beatags "bhj-grep" "\


\(fn &optional HISTORY-VAR DEF-GREP-COMMAND)" t nil)

(autoload 'grep-find-file "bhj-grep" "\


\(fn)" t nil)

(autoload 'grep-func-call "bhj-grep" "\


\(fn)" t nil)

(autoload 'bhj-grep "bhj-grep" "\


\(fn)" t nil)

(autoload 'bhj-rgrep "bhj-grep" "\


\(fn)" t nil)

(autoload 'bhj-abc-grep "bhj-grep" "\


\(fn)" t nil)

(autoload 'bhj-grep-mode "bhj-grep" "\
Toggle the `bhj-grep-mode' minor mode.

\(fn &optional ARG)" t nil)

(defvar bhj-grep-global-mode nil "\
Non-nil if Bhj-Grep-Global mode is enabled.
See the `bhj-grep-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `bhj-grep-global-mode'.")

(custom-autoload 'bhj-grep-global-mode "bhj-grep" nil)

(autoload 'bhj-grep-global-mode "bhj-grep" "\
Toggle Bhj-Grep mode in all buffers.
With prefix ARG, enable Bhj-Grep-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Bhj-Grep mode is enabled in all buffers where
`turn-on-bhj-grep-mode' would do it.
See `bhj-grep-mode' for more information on Bhj-Grep mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "emojis" "emojis.el" (22922 62975 794362 221000))
;;; Generated autoloads from emojis.el

(autoload 'enter-emoji "emojis" "\
Let the user input an emoji interactively

\(fn)" t nil)

;;;***

;;;### (autoloads nil "install-elisp" "install-elisp.el" (20874 19848
;;;;;;  307221 889000))
;;; Generated autoloads from install-elisp.el

(autoload 'install-elisp "install-elisp" "\
Retrieve Emacs Lisp program from URL and save and byte-compile and load.
If optional FILENAME is supplied, save URL as FILENAME, otherwise URL's basename.

\(fn URL &optional FILENAME)" t nil)

(autoload 'install-elisp-from-emacswiki "install-elisp" "\
Install Emacs Lisp program from the EmacsWiki.

\(fn FILENAME)" t nil)

(autoload 'install-elisp-from-gist "install-elisp" "\
Install Emacs Lisp program from gist.

\(fn GISTID &optional FILENAME)" t nil)

(autoload 'dired-install-elisp-from-emacswiki "install-elisp" "\
Upgrade the current Emacs Lisp program from the EmacsWiki.

\(fn &optional FILENAME)" t nil)

;;;***

;;;### (autoloads nil "linkd" "linkd.el" (20874 19848 307221 889000))
;;; Generated autoloads from linkd.el

(autoload 'linkd-version "linkd" "\
Display Linkd version.

\(fn)" t nil)

(autoload 'linkd-back "linkd" "\
Return to the buffer being viewed before the last link was followed.

\(fn)" t nil)

(autoload 'linkd-follow-at-point "linkd" "\
Follow the link at point.

\(fn)" t nil)

(autoload 'linkd-next-link "linkd" "\
Move point to the next link, if any.

\(fn)" t nil)

(autoload 'linkd-previous-link "linkd" "\
Move point to the previous link, if any.

\(fn)" t nil)

(autoload 'linkd-insert-single-arg-link "linkd" "\
Insert a link containing ARGUMENT.

\(fn TYPE-STRING ARGUMENT)" nil nil)

(autoload 'linkd-insert-tag "linkd" "\
Insert a tag.

\(fn TAG-NAME)" t nil)

(autoload 'linkd-insert-star "linkd" "\
Insert a star.

\(fn STAR-NAME)" t nil)

(autoload 'linkd-insert-wiki "linkd" "\
Insert a wiki link.

\(fn WIKI-NAME)" t nil)

(autoload 'linkd-insert-lisp "linkd" "\
Insert a Lisp sexp.

\(fn SEXP)" t nil)

(autoload 'linkd-insert-link "linkd" "\
Insert a link.
Optional arg TYPE is the link type.
Optional arg CURRENT-VALUES is a property list of current values.

\(fn &optional TYPE CURRENT-VALUES)" t nil)

(autoload 'linkd-edit-link-at-point "linkd" "\
Edit the Linkd link at point.

\(fn)" t nil)

(autoload 'linkd-export-default "linkd" "\
Export the current buffer with default settings to all available formats.

\(fn)" t nil)

(autoload 'linkd-latex-export "linkd" "\
Render a buffer as a LaTeX book chapter.

\(fn)" t nil)

(autoload 'linkd-wiki-find-page "linkd" "\
Find Linkd wiki page named PAGE-NAME.

\(fn PAGE-NAME)" t nil)

;;;***

;;;### (autoloads nil "moy-bbdb" "moy-bbdb.el" (21033 52657 866743
;;;;;;  951000))
;;; Generated autoloads from moy-bbdb.el

(autoload 'bbdb/send-ignore-most-messages-hook "moy-bbdb" "\
For use as the value of `bbdb/send-auto-create-p'.
This will automatically create BBDB entries for messages which match
the bbdb/send-ignore-most-messages-alist (which see) and *no* others.

\(fn &optional INVERT-SENSE)" nil nil)

(autoload 'bbdb/send-ignore-some-messages-hook "moy-bbdb" "\
For use as a `bbdb/send-auto-create-hook'.
This will automatically create BBDB entries for messages which do *not*
match the `bbdb/send-ignore-some-messages-alist' (which see).

\(fn)" nil nil)

(autoload 'bbdb/send-auto-notes-hook "moy-bbdb" "\
For use as a `bbdb/send-notice-hook'.  This might automatically add
some text to  the notes field of the BBDB  record corresponding to the
current record  based on the header  of the current  message.  See the
documentation  for   the  variables  `bbdb/send-auto-notes-alist'  and
`bbdb/send-auto-notes-ignore'.

\(fn RECORD)" nil nil)

(autoload 'bbdb/send-hook "moy-bbdb" "\
Parse headers of outgoing message, insert the addresses of the
  recipients one by one into BBDB if they do not exist already

\(fn)" t nil)

;;;***

;;;### (autoloads nil "nsi-mode" "nsi-mode.el" (20874 19848 307221
;;;;;;  889000))
;;; Generated autoloads from nsi-mode.el

(autoload 'nsi-mode "nsi-mode" "\
Major mode for editing Nsi files.
To submit a problem report, enter `\\[nsi-submit-bug-report]' from a
`nsi-mode' buffer.  Do `\\[nsi-describe-mode]' for detailed
documentation.  To see what version of `nsi-mode' you are running,
enter `\\[nsi-version]'.

This mode knows about Nsi indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{nsi-mode-map}
VARIABLES

nsi-indent-offset		indentation increment
nsi-block-comment-prefix		comment string used by `comment-region'
nsi-nsi-command		shell command to invoke Nsi interpreter
nsi-temp-directory		directory used for temp files (if needed)
nsi-beep-if-tab-change		ring the bell if `tab-width' is changed

\(fn)" t nil)

(autoload 'nsi-shell "nsi-mode" "\
Start an interactive Nsi interpreter in another window.
This is like Shell mode, except that Nsi is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Nsi*' buffer.

With optional \\[universal-argument], the user is prompted for the
flags to pass to the Nsi interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CNsi interpreter and the
JNsi interpreter by hitting \\[nsi-toggle-shells].  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*JNsi*' or `*Nsi*' buffers (the
latter is the name used for the CNsi buffer).

Warning: Don't use an interactive Nsi if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `nsi-mode' can't
distinguish your output from Nsi's output, and assumes that `>>> '
at the start of a line is a prompt from Nsi.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Nsi prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Nsi, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Nsi process buffers using the default (Emacs-supplied) process
filter.

\(fn &optional ARGPROMPT)" t nil)

;;;***

;;;### (autoloads nil "org-mime" "org-mime.el" (21799 46161 841928
;;;;;;  385000))
;;; Generated autoloads from org-mime.el

(autoload 'org-mime-htmlize "org-mime" "\
Export a portion of an email body composed using `mml-mode' to
html using `org-mode'.  If called with an active region only
export that region, otherwise export the entire body.

\(fn ARG)" t nil)

(autoload 'org-mime-org-buffer-htmlize "org-mime" "\
Create an email buffer containing the current org-mode file
  exported to html and encoded in both html and in org formats as
  mime alternatives.

\(fn)" t nil)

(autoload 'org-mime-subtree "org-mime" "\
Create an email buffer containing the current org-mode subtree
  exported to a org format or to the format specified by the
  MAIL_FMT property of the subtree.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ox-freemind" "ox-freemind.el" (21122 10352
;;;;;;  596297 883000))
;;; Generated autoloads from ox-freemind.el

(autoload 'org-freemind-export-to-freemind "ox-freemind" "\
Export current buffer to a Freemind Mindmap file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

;;;***

;;;### (autoloads nil "qt-pro" "qt-pro.el" (20874 19848 307221 889000))
;;; Generated autoloads from qt-pro.el

(autoload 'qt-pro-mode "qt-pro" "\
A major mode for editing Qt-pro files.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "revive" "revive.el" (20874 19848 339221 890000))
;;; Generated autoloads from revive.el

(autoload 'current-window-configuration-printable "revive" "\
Return the printable current-window-configuration.
This configuration will be stored by restore-window-configuration.
Returned configurations are list of:
'(Screen-Width Screen-Height Edge-List Buffer-List)

Edge-List is a return value of revive:all-window-edges, list of all
window-edges whose first member is always of north west window.

Buffer-List is a list of buffer property list of all windows.  This
property lists are stored in order corresponding to Edge-List.  Buffer
property list is formed as
'((buffer-file-name) (buffer-name) (point) (window-start)).

\(fn)" nil nil)

(autoload 'restore-window-configuration "revive" "\
Restore the window configuration.
Configuration CONFIG should be created by
current-window-configuration-printable.

\(fn CONFIG)" nil nil)

(autoload 'wipe "revive" "\
Wipe Emacs.

\(fn)" t nil)

(autoload 'save-current-configuration "revive" "\
Save current window/buffer configuration into configuration file.

\(fn &optional NUM)" t nil)

(autoload 'resume "revive" "\
Resume window/buffer configuration.
Configuration should be saved by save-current-configuration.

\(fn &optional NUM)" t nil)

;;;***

;;;### (autoloads nil "screen-lines" "screen-lines.el" (20874 19848
;;;;;;  339221 890000))
;;; Generated autoloads from screen-lines.el

(autoload 'screen-lines-mode "screen-lines" "\
Toggle Screen Lines minor mode for the current buffer.
With ARG, turn the mode on if ARG is positive, off otherwise.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-screen-lines-mode "screen-lines" "\
Turn on Screen Lines minor mode for the current buffer.

\(fn)" t nil)

(autoload 'turn-off-screen-lines-mode "screen-lines" "\
Turn off Screen Lines minor mode for the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "twittering-mode" "twittering-mode.el" (20874
;;;;;;  19848 339221 890000))
;;; Generated autoloads from twittering-mode.el

(autoload 'twit "twittering-mode" "\
Start twittering-mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "windows" "windows.el" (20874 19848 339221
;;;;;;  890000))
;;; Generated autoloads from windows.el

(autoload 'win-switch-to-window "windows" "\
Switch window configurations to a buffer specified by keyboard.
If calling from program, optional second argument WINDOW can specify
the window number.

\(fn ARG &optional WINDOW)" t nil)

(autoload 'win:set-wc "windows" "\
(Windows low level internal) Set the NUM-th windows configuration.
If Windows uses frame(Emacs 19), Select the NUM-th window frame.

\(fn NUM)" nil nil)

(autoload 'win:startup-with-window "windows" "\
Start up Emacs with window[1] selected.

\(fn)" nil nil)

(autoload 'win-save-all-configurations "windows" "\
Save all window configurations into the configuration file.

\(fn)" t nil)

(autoload 'wipe-windows "windows" "\
Kill all buffers.  Optional argument NO-ASK non-nil skips query.

\(fn &optional NO-ASK)" t nil)

(autoload 'win-load-all-configurations "windows" "\
Load all window configurations from the configuration file.
Non-nil for optional argument PRESERVE keeps all current buffers.

\(fn &optional PRESERVE)" t nil)

(autoload 'see-you-again "windows" "\
Save all of the window configurations and kill-emacs.

\(fn)" t nil)

(autoload 'resume-windows "windows" "\
Restore all window configurations reading configurations from a file.
Non-nil for optional argument PRESERVE keeps current buffers.

\(fn &optional PRESERVE)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-complete-clang-async.el" "bhj-darwin.el"
;;;;;;  "bhj-eval-after-load.el" "bhj-fonts.el" "bhj-set-key.el"
;;;;;;  "bhj-setq.el" "color-moccur.el" "color-theme-leuven.el" "cygwin-mount.el"
;;;;;;  "emacs-25.el" "grep-buffers.el" "guess-offset.el" "moinmoin-mode.el"
;;;;;;  "my-erc-config.el" "org-mime-autoloads.el" "org-mime-pkg.el"
;;;;;;  "pink-bliss.el" "point-stack.el" "qmake.el" "sdim.el" "soap-client.el"
;;;;;;  "soap-inspect.el" "subst2-ksc.el" "w32-symlinks.el") (22922
;;;;;;  65337 803088 88000))

;;;***

;;;### (autoloads nil "../weblogger/weblogger" "../weblogger/weblogger.el"
;;;;;;  (20874 19848 339221 890000))
;;; Generated autoloads from ../weblogger/weblogger.el

(autoload 'weblogger-select-configuration "../weblogger/weblogger" "\
Select a previously saved configuration.

\(fn &optional CONFIG)" t nil)

(autoload 'weblogger-setup-weblog "../weblogger/weblogger" "\
Create a profile for a weblog.

\(fn)" t nil)

(autoload 'weblogger-start-entry "../weblogger/weblogger" "\
Start creating a weblog entry in the *weblogger-entry* buffer.
With a prefix, it will check the available weblogs on the server
and prompt for the weblog to post to if multiple ones are
available.

\(fn &optional PROMPT)" t nil)

(autoload 'weblogger-fetch-entries "../weblogger/weblogger" "\
Sync the entry ring with what is on the weblog server.

\(fn)" t nil)

;;;***
