(autoload 'weblogger-mode "weblogger")
;;;### (autoloads (w3m-buffer w3m-region w3m-find-file w3m-browse-url
;;;;;;  w3m w3m-create-empty-session w3m-gohome w3m-goto-url-new-session
;;;;;;  w3m-goto-url w3m-download w3m-retrieve) "../../../../../usr/share/emacs24/site-lisp/w3m/w3m"
;;;;;;  "../../../../../usr/share/emacs24/site-lisp/w3m/w3m.el" (20843
;;;;;;  22906 367956 641000))
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
NO-CHACHE (which the prefix argument gives when called interactively)
specifies not using the cached data.

\(fn &optional URL FILENAME NO-CACHE HANDLER POST-DATA)" t nil)

(autoload 'w3m-goto-url "../../../../../usr/share/emacs24/site-lisp/w3m/w3m" "\
Visit World Wide Web pages.  This is the primitive function of `w3m'.
If the second argument RELOAD is non-nil, reload a content of URL.
Except that if it is 'redisplay, re-display the page without reloading.
The third argument CHARSET specifies a charset to be used for decoding
a content.
The fourth argument POST-DATA should be a string or a cons cell.  If
it is a string, it makes this function request a body as if the
content-type is \"x-www-form-urlencoded\".  If it is a cons cell, the
car of a cell is used as the content-type and the cdr of a cell is
used as the body.
If the fifth argument REFERER is specified, it is used for a Referer:
field for this request.
The remaining HANDLER, ELEMENT[1], and NO-POPUP are for the
internal operations of emacs-w3m.
You can also use \"quicksearch\" url schemes such as \"gg:emacs\" which
would search for the term \"emacs\" with the Google search engine.  See
the `w3m-search' function and the variable `w3m-uri-replace-alist'.

\[1] A note for the developers: ELEMENT is a history element which has
already been registered in the `w3m-history-flat' variable.  It is
corresponding to URL to be retrieved at this time, not for the url of
the current page.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER HANDLER ELEMENT NO-POPUP)" t nil)

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
string for the prompt, it will visit the home page specified by the
`w3m-home-page' variable or the \"about:\" page.

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

;;;### (autoloads (ajoke-search-local-id ajoke-complete-method ajoke-resolve
;;;;;;  ajoke-get-override ajoke-get-hierarchy ajoke-get-imports
;;;;;;  ajoke--pick-one) "../../gcode/ajoke/etc/elisp/ajoke" "../../gcode/ajoke/etc/elisp/ajoke.el"
;;;;;;  (21032 15180 282361 356000))
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

;;;***

;;;### (autoloads (douban-music) "../../src/github/DoubanMusic/douban-music-mode"
;;;;;;  "../../../src/github/DoubanMusic/douban-music-mode.el" (20872
;;;;;;  37462 699706 737000))
;;; Generated autoloads from ../../../src/github/DoubanMusic/douban-music-mode.el

(autoload 'douban-music "../../src/github/DoubanMusic/douban-music-mode" "\
Play douban music in its own buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (twit) "../../src/github/twittering-mode/twittering-mode"
;;;;;;  "../../../src/github/twittering-mode/twittering-mode.el"
;;;;;;  (20871 38424 871391 24000))
;;; Generated autoloads from ../../../src/github/twittering-mode/twittering-mode.el

(autoload 'twit "../../src/github/twittering-mode/twittering-mode" "\
Start twittering-mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-jira-get-issues-from-filter-headonly org-jira-get-issues-from-filter
;;;;;;  org-jira-browse-issue org-jira-progress-issue org-jira-refresh-issue
;;;;;;  org-jira-create-subtask org-jira-create-issue org-jira-get-subtasks
;;;;;;  org-jira-todo-to-jira org-jira-update-issue org-jira-copy-current-issue-key
;;;;;;  org-jira-update-comment org-jira-get-issues org-jira-get-issues-headonly
;;;;;;  org-jira-get-projects org-jira-mode) "../org-jira/org-jira"
;;;;;;  "../../../.emacs_d/org-jira/org-jira.el" (20855 15281 643693
;;;;;;  707000))
;;; Generated autoloads from ../../../.emacs_d/org-jira/org-jira.el

(autoload 'org-jira-mode "../org-jira/org-jira" "\
Toggle org-jira mode.
With no argument, the mode is toggled on/off.
Non-nil argument turns mode on.
Nil argument turns mode off.

Commands:
\\{org-jira-entry-mode-map}

Entry to this mode calls the value of `org-jira-mode-hook'.

\(fn &optional ARG)" t nil)

(autoload 'org-jira-get-projects "../org-jira/org-jira" "\
Get list of projects.

\(fn)" t nil)

(autoload 'org-jira-get-issues-headonly "../org-jira/org-jira" "\
Get list of issues assigned to you and unresolved, head
only. With a prefix argument, allow you to customize the jql. See `org-jira-get-issue-list'

\(fn ISSUES)" t nil)

(autoload 'org-jira-get-issues "../org-jira/org-jira" "\
Get list of issues. Default is get unfinished issues assigned
to you, but you can customize jql with a prefix argument. See
`org-jira-get-issue-list'

\(fn ISSUES)" t nil)

(autoload 'org-jira-update-comment "../org-jira/org-jira" "\
update a comment for the current issue

\(fn)" t nil)

(autoload 'org-jira-copy-current-issue-key "../org-jira/org-jira" "\
Copy the current issue's key into clipboard

\(fn)" t nil)

(autoload 'org-jira-update-issue "../org-jira/org-jira" "\
update an issue

\(fn)" t nil)

(autoload 'org-jira-todo-to-jira "../org-jira/org-jira" "\
convert an ordinary todo item to a jira ticket

\(fn)" t nil)

(autoload 'org-jira-get-subtasks "../org-jira/org-jira" "\
get subtasks for the current issue

\(fn)" t nil)

(autoload 'org-jira-create-issue "../org-jira/org-jira" "\
create an issue

\(fn PROJECT TYPE SUMMARY DESCRIPTION)" t nil)

(autoload 'org-jira-create-subtask "../org-jira/org-jira" "\
create an subtask issue

\(fn PROJECT TYPE SUMMARY DESCRIPTION)" t nil)

(autoload 'org-jira-refresh-issue "../org-jira/org-jira" "\
Refresh issue from jira to org

\(fn)" t nil)

(autoload 'org-jira-progress-issue "../org-jira/org-jira" "\
Progress issue workflow

\(fn)" t nil)

(autoload 'org-jira-browse-issue "../org-jira/org-jira" "\
Open the current issue in external browser.

\(fn)" t nil)

(autoload 'org-jira-get-issues-from-filter "../org-jira/org-jira" "\
Get issues from filter which are jql created and saved on the
server side. Provide this command in case some users are not able
to use client side jql (maybe because of Jira server version?).

\(fn FILTER)" t nil)

(autoload 'org-jira-get-issues-from-filter-headonly "../org-jira/org-jira" "\
Get issues *head only* from saved filter. See `org-jira-get-issues-from-filter'

\(fn FILTER)" t nil)

;;;***

;;;### (autoloads (ahk-mode) "ahk-mode" "ahk-mode.el" (21104 33899
;;;;;;  312653 180000))
;;; Generated autoloads from ahk-mode.el

(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))

(autoload 'ahk-mode "ahk-mode" "\
Major mode for editing AutoHotKey Scripts.

The hook functions in `ahk-mode-hook' are run after mode initialization.

Key bindings:
\\{ahk-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (ajoke-search-local-id ajoke-complete-method ajoke-resolve
;;;;;;  ajoke-get-imports ajoke-get-override ajoke-get-hierarchy)
;;;;;;  "ajoke" "ajoke.el" (20978 9411 730903 403000))
;;; Generated autoloads from ajoke.el

(autoload 'ajoke-get-hierarchy "ajoke" "\
Print the class/interface inheritance hierarchy for the
current class. Output is in compilation-mode for ease of cross
referencing.

\(fn)" t nil)

(autoload 'ajoke-get-override "ajoke" "\
Overide a method defined in super classes/interfaces.

\(fn)" t nil)

(autoload 'ajoke-get-imports "ajoke" "\
Write the java import statements automatically.

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

;;;***

;;;### (autoloads (bhj-c-show-current-func ajoke-pop-mark-back ajoke-pop-mark
;;;;;;  bhj-jdk-help bhj-insert-pwdu bhj-insert-pwdw bhj-clt-insert-file-name
;;;;;;  wiki-local-bhj weekrep bhj-downcase-symbol-or-region bhj-upcase-symbol-or-region
;;;;;;  source-code-help bhj-find-missing-file bhj-open-android-doc-on-java-buffer
;;;;;;  bhj-do-dictionary insert-today dos2unix bhj-do-code-generation
;;;;;;  bhj-org-tasks-closed-last-week bbdb-complete-mail bhj-flatten-list
;;;;;;  my-bbdb-canonicalize my-bbdb/gnus-update-records-mode bhj-view-mail-external
;;;;;;  gnus-gmail-search-subject localedit sudoedit switch-buffer-same-filename-rev
;;;;;;  switch-buffer-same-filename revert-all-buffers save-all-buffers-no-check-modified
;;;;;;  back-to-indent-same-space-as-prev-line indent-same-space-as-prev-line
;;;;;;  java-bt-mode java-bt-next-error java-bt-ret-key waw-mode
;;;;;;  waw-ret-key waw-next-error visit-code-reading android-get-help
;;;;;;  where-are-we try-all-color-themes try-all-themes random-theme
;;;;;;  devenv-debug devenv-toggle-breakpoint switch-to-devenv bhj-set-reply
;;;;;;  bhj-set-smtp-cred-to-company-mail bhj-mimedown bhj-w3m-scroll-down-or-previous-url
;;;;;;  bhj-w3m-scroll-up-or-next-url bhj-occur-merge-conflicts bhj-occur-make-errors
;;;;;;  bhj-occur bhj-indent-region-as-prev-line c-get-includes linux-c++-mode
;;;;;;  linux-c-mode bhj-douban-start confirm-risky-remote-edit fix-latex-cjk
;;;;;;  cleanup-buffer-safe) "bhj-defines" "bhj-defines.el" (21057
;;;;;;  31592 639184 905000))
;;; Generated autoloads from bhj-defines.el

(autoload 'cleanup-buffer-safe "bhj-defines" "\
Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad.

\(fn)" t nil)

(autoload 'fix-latex-cjk "bhj-defines" "\
move the cjk env outmost with the document env

\(fn)" t nil)

(autoload 'confirm-risky-remote-edit "bhj-defines" "\


\(fn)" nil nil)

(autoload 'bhj-douban-start "bhj-defines" "\


\(fn)" nil nil)

(autoload 'linux-c-mode "bhj-defines" "\
C mode with adjusted defaults for use with the Linux kernel.

\(fn)" t nil)

(autoload 'linux-c++-mode "bhj-defines" "\
C mode with adjusted defaults for use with the Linux kernel.

\(fn)" t nil)

(autoload 'c-get-includes "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-indent-region-as-prev-line "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-occur "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-occur-make-errors "bhj-defines" "\


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

(autoload 'random-theme "bhj-defines" "\


\(fn)" t nil)

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

(autoload 'sudoedit "bhj-defines" "\


\(fn)" t nil)

(autoload 'localedit "bhj-defines" "\


\(fn)" t nil)

(autoload 'gnus-gmail-search-subject "bhj-defines" "\


\(fn)" t nil)

(autoload 'bhj-view-mail-external "bhj-defines" "\
open the current maildir file in kmail

\(fn)" t nil)

(autoload 'my-bbdb/gnus-update-records-mode "bhj-defines" "\


\(fn)" nil nil)

(autoload 'my-bbdb-canonicalize "bhj-defines" "\


\(fn ADDR)" nil nil)

(autoload 'bhj-flatten-list "bhj-defines" "\
Return a new, flat list that contains all elements of LIST.

\(bhj-flatten-list '(1 (2 3 (4 5 (6))) 7))
=> (1 2 3 4 5 6 7)

\(fn LIST)" nil nil)

(autoload 'bbdb-complete-mail "bhj-defines" "\
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

;;;### (autoloads (grep-func-call grep-find-file grep-gtags grep-imenu
;;;;;;  grep-bhj-dir bhj-edit-grep-pattern) "bhj-grep" "bhj-grep.el"
;;;;;;  (20855 20629 663417 283000))
;;; Generated autoloads from bhj-grep.el

(autoload 'bhj-edit-grep-pattern "bhj-grep" "\


\(fn)" t nil)

(autoload 'grep-bhj-dir "bhj-grep" "\


\(fn)" t nil)

(autoload 'grep-imenu "bhj-grep" "\


\(fn)" t nil)

(autoload 'grep-gtags "bhj-grep" "\


\(fn &optional HISTORY-VAR DEF-GREP-COMMAND)" t nil)

(autoload 'grep-find-file "bhj-grep" "\


\(fn)" t nil)

(autoload 'grep-func-call "bhj-grep" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (weblogger-fetch-entries weblogger-start-entry
;;;;;;  weblogger-setup-weblog weblogger-select-configuration) "../weblogger/weblogger"
;;;;;;  "../weblogger/weblogger.el" (20622 27199 0 0))
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
