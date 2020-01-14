;;; w3m-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bookmark-w3m" "bookmark-w3m.el" (0 0 0 0))
;;; Generated autoloads from bookmark-w3m.el

(autoload 'bookmark-w3m-bookmark-jump "bookmark-w3m" "\
Default bookmark handler for w3m buffers.

\(fn BOOKMARK)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bookmark-w3m" '("bookmark-w3m-")))

;;;***

;;;### (autoloads nil "mime-w3m" "mime-w3m.el" (0 0 0 0))
;;; Generated autoloads from mime-w3m.el

(autoload 'mime-w3m-preview-text/html "mime-w3m" "\


\(fn ENTITY SITUATION)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mime-w3m" '("mime-w3m-")))

;;;***

;;;### (autoloads nil "w3m" "w3m.el" (0 0 0 0))
;;; Generated autoloads from w3m.el

(autoload 'w3m-retrieve "w3m" "\
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

(autoload 'w3m-download "w3m" "\
Download contents of URL to a file named FILENAME.
NO-CACHE is ignored (always download).

\(fn &optional URL FILENAME NO-CACHE HANDLER POST-DATA)" t nil)

(autoload 'w3m-goto-url "w3m" "\
Visit World Wide Web pages in the current buffer.

This is the primitive function of `w3m'.

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
The remaining HANDLER, ELEMENT[1], BACKGROUND, and SAVE-POS[2] are for
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

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER HANDLER ELEMENT BACKGROUND SAVE-POS)" t nil)

(autoload 'w3m-goto-url-new-session "w3m" "\
Visit World Wide Web pages in a new buffer.
Open a new tab if you use tabs, i.e., `w3m-display-mode' is set to
`tabbed' or `w3m-use-tab' is set to a non-nil value.

The buffer will get visible if BACKGROUND is nil or there is no other
emacs-w3m buffer regardless of BACKGROUND, otherwise (BACKGROUND is
non-nil) the buffer will be created but not appear to be visible.
BACKGROUND defaults to the value of `w3m-new-session-in-background',
but it could be inverted if called interactively with the prefix arg.

\(fn URL &optional RELOAD CHARSET POST-DATA REFERER BACKGROUND)" t nil)

(autoload 'w3m-gohome "w3m" "\
Go to the Home page.

\(fn)" t nil)

(autoload 'w3m-create-empty-session "w3m" "\
Create an empty page as a new session and visit it.

\(fn)" t nil)

(autoload 'w3m "w3m" "\
Visit World Wide Web pages using the external w3m command.

If no emacs-w3m session already exists: If POINT is at a url
string, visit that. Otherwise, if `w3m-home-page' is defined,
visit that. Otherwise, present a blank page. This behavior can be
over-ridden by setting variable `w3m-quick-start' to nil, in
which case you will always be prompted for a URL.

If an emacs-w3m session already exists: Pop to one of its windows
or frames. You can over-ride this behavior by setting
`w3m-quick-start' to nil, in order to always be prompted for a
URL.

In you have set `w3m-quick-start' to nil, but wish to over-ride
default behavior from the command line, either run this command
with a prefix argument or enter the empty string for the prompt.
In such cases, this command will visit a url at the point or,
lacking that, the URL set in variable `w3m-home-page' or, lacking
that, the \"about:\" page.

Any of five display styles are possible. See `w3m-display-mode'
for a description of those options.

You can also run this command in the batch mode as follows:

  emacs -f w3m http://emacs-w3m.namazu.org/ &

In that case, or if this command is called non-interactively, the
variables `w3m-pop-up-windows' and `w3m-pop-up-frames' will be ignored
\(treated as nil) and it will run emacs-w3m at the current (or the
initial) window.

If the optional NEW-SESSION is non-nil, this function creates a new
emacs-w3m buffer.  Besides that, it also makes a new emacs-w3m buffer
if `w3m-make-new-session' is non-nil and a user specifies a url string.

The optional INTERACTIVE-P is for the internal use; it is mainly used
to check whether Emacs calls this function as an interactive command
in the batch mode.

\(fn &optional URL NEW-SESSION INTERACTIVE-P)" t nil)

(autoload 'w3m-browse-url "w3m" "\
Ask emacs-w3m to browse URL.
When called interactively, URL defaults to the string existing around
the cursor position and looking like a url.  If the prefix argument is
given[1] or NEW-SESSION is non-nil, create a new emacs-w3m session.
If REFRESH-IF-EXISTS is non-nil, refresh the page if it already exists
but is older than the site.

\[1] More precisely the prefix argument inverts the boolean logic of
`browse-url-new-window-flag' that defaults to nil.

\(fn URL &optional NEW-SESSION REFRESH-IF-EXISTS)" t nil)

(autoload 'w3m-find-file "w3m" "\
Function used to open FILE whose name is expressed in ordinary format.
The file name will be converted into the file: scheme.

\(fn FILE)" t nil)

(autoload 'w3m-region "w3m" "\
Render the region of the current buffer between START and END.
URL specifies the address where the contents come from.  It can be
omitted or nil when the address is not identified.  CHARSET is used
for decoding the contents.  If it is nil, this function attempts to
parse the meta tag to extract the charset.

\(fn START END &optional URL CHARSET)" t nil)

(autoload 'w3m-buffer "w3m" "\
Render the current buffer.
See `w3m-region' for the optional arguments.

\(fn &optional URL CHARSET)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m" '("w3m-" "emacs-w3m-version")))

;;;***

;;;### (autoloads nil "w3m-antenna" "w3m-antenna.el" (0 0 0 0))
;;; Generated autoloads from w3m-antenna.el

(autoload 'w3m-about-antenna "w3m-antenna" "\


\(fn URL &optional NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(autoload 'w3m-antenna "w3m-antenna" "\
Report changes of WEB sites, which is specified in `w3m-antenna-sites'.

\(fn &optional NO-CACHE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-antenna" '("w3m-antenna-")))

;;;***

;;;### (autoloads nil "w3m-bookmark" "w3m-bookmark.el" (0 0 0 0))
;;; Generated autoloads from w3m-bookmark.el

(autoload 'w3m-bookmark-add-this-url "w3m-bookmark" "\
Add link under cursor to bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-add-current-url "w3m-bookmark" "\
Add a url of the current page to the bookmark.
With prefix, ask for a new url instead of the present one.

\(fn &optional ARG)" t nil)

(autoload 'w3m-bookmark-add-all-urls "w3m-bookmark" "\
Add urls of all pages being visited to the bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-add-current-url-group "w3m-bookmark" "\
Add link of the group of current urls to the bookmark.

\(fn)" t nil)

(autoload 'w3m-bookmark-view "w3m-bookmark" "\
Display the bookmark list in the current buffer.

\(fn &optional RELOAD)" t nil)

(autoload 'w3m-bookmark-view-new-session "w3m-bookmark" "\
Display the bookmark list in a new buffer.

\(fn &optional RELOAD)" t nil)

(autoload 'w3m-about-bookmark "w3m-bookmark" "\


\(fn &rest ARGS)" nil nil)

(autoload 'w3m-setup-bookmark-menu "w3m-bookmark" "\
Setup w3m bookmark items in menubar.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-bookmark" '("w3m-bookmark-")))

;;;***

;;;### (autoloads nil "w3m-bug" "w3m-bug.el" (0 0 0 0))
;;; Generated autoloads from w3m-bug.el

(autoload 'report-emacs-w3m-bug "w3m-bug" "\
Report a bug in emacs-w3m.
Prompts for bug subject.  Leaves you in a mail buffer.

\(fn TOPIC &optional BUFFER)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-bug" '("report-emacs-w3m-bug-")))

;;;***

;;;### (autoloads nil "w3m-cookie" "w3m-cookie.el" (0 0 0 0))
;;; Generated autoloads from w3m-cookie.el

(autoload 'w3m-cookie-shutdown "w3m-cookie" "\
Save cookies, and reset cookies' data.

\(fn &optional INTERACTIVE-P)" t nil)

(autoload 'w3m-cookie-set "w3m-cookie" "\
Register cookies which correspond to URL.
BEG and END should be an HTTP response header region on current buffer.

\(fn URL BEG END)" nil nil)

(autoload 'w3m-cookie-get "w3m-cookie" "\
Get a cookie field string which corresponds to the URL.

\(fn URL)" nil nil)

(autoload 'w3m-cookie "w3m-cookie" "\
Display cookies and enable you to manage them.

\(fn &optional NO-CACHE)" t nil)

(autoload 'w3m-about-cookie "w3m-cookie" "\
Make the html contents to display and to enable you to manage cookies.
To delete all cookies associated with amazon.com for example, do it in
the following two steps:

1. Mark them `unused' (type `C-c C-c' or press any OK button).

Limit to [amazon.com          ] <= [ ]regexp  [OK]
\( )Noop  ( )Use all  (*)Unuse all  ( )Delete unused all  [OK]

2. Delete cookies that are marked `unused'.

Limit to [amazon.com          ] <= [ ]regexp  [OK]
\( )Noop  ( )Use all  ( )Unuse all  (*)Delete unused all  [OK]

You can mark cookies on the one-by-one basis of course.  The `Limit-to'
string is case insensitive and allows a regular expression.

\(fn URL &optional NO-DECODE NO-CACHE POST-DATA &rest ARGS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-cookie" '("w3m-")))

;;;***

;;;### (autoloads nil "w3m-dtree" "w3m-dtree.el" (0 0 0 0))
;;; Generated autoloads from w3m-dtree.el

(autoload 'w3m-about-dtree "w3m-dtree" "\


\(fn URL &optional NODECODE ALLFILES &rest ARGS)" nil nil)

(autoload 'w3m-dtree "w3m-dtree" "\
Display directory tree on local file system.
If called with 'prefix argument', display all directorys and files.

\(fn ALLFILES PATH)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-dtree" '("w3m-dtree-")))

;;;***

;;;### (autoloads nil "w3m-ems" "w3m-ems.el" (0 0 0 0))
;;; Generated autoloads from w3m-ems.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-ems" '("w3m-")))

;;;***

;;;### (autoloads nil "w3m-favicon" "w3m-favicon.el" (0 0 0 0))
;;; Generated autoloads from w3m-favicon.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-favicon" '("w3m-")))

;;;***

;;;### (autoloads nil "w3m-fb" "w3m-fb.el" (0 0 0 0))
;;; Generated autoloads from w3m-fb.el

(defvar w3m-fb-mode nil "\
Non-nil if W3m-Fb mode is enabled.
See the `w3m-fb-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `w3m-fb-mode'.")

(custom-autoload 'w3m-fb-mode "w3m-fb" nil)

(autoload 'w3m-fb-mode "w3m-fb" "\
Toggle W3M Frame Buffer mode.
This allows frame-local lists of buffers (tabs).

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-fb" '("w3m-fb-")))

;;;***

;;;### (autoloads nil "w3m-filter" "w3m-filter.el" (0 0 0 0))
;;; Generated autoloads from w3m-filter.el

(autoload 'w3m-filter "w3m-filter" "\
Apply filtering rule of URL against a content in this buffer.

\(fn URL)" nil nil)

(autoload 'w3m-toggle-filtering "w3m-filter" "\
Toggle whether web pages will have their html modified by w3m's filters before being rendered.
When called with a prefix argument, prompt for a single filter to
toggle with completion (a function toggled last will first appear).

\(fn ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-filter" '("w3m-filter-")))

;;;***

;;;### (autoloads nil "w3m-form" "w3m-form.el" (0 0 0 0))
;;; Generated autoloads from w3m-form.el

(autoload 'w3m-fontify-forms "w3m-form" "\
Process half-dumped data and fontify forms in this buffer.

\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-form" '("w3m-fo")))

;;;***

;;;### (autoloads nil "w3m-hist" "w3m-hist.el" (0 0 0 0))
;;; Generated autoloads from w3m-hist.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-hist" '("w3m-history")))

;;;***

;;;### (autoloads nil "w3m-image" "w3m-image.el" (0 0 0 0))
;;; Generated autoloads from w3m-image.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-image" '("w3m-")))

;;;***

;;;### (autoloads nil "w3m-lnum" "w3m-lnum.el" (0 0 0 0))
;;; Generated autoloads from w3m-lnum.el

(autoload 'w3m-lnum-mode "w3m-lnum" "\
Minor mode to extend point commands by using Conkeror style number selection.
With prefix ARG 0 disable battery included point functions, otherwise
enable them.  With no prefix ARG - toggle.

\(fn &optional ARG)" t nil)

(autoload 'w3m-lnum-goto "w3m-lnum" "\
Turn on link, image and form numbers and ask for one to go to.
0 corresponds to location url.

\(fn)" t nil)

(autoload 'w3m-lnum-follow "w3m-lnum" "\
Turn on link numbers, ask for one and execute appropriate action on it.
If link - visit it, when button - press, when input - activate it,
If image - toggle it.
With prefix ARG visit link in new session or don't move over
field/button/image on activation/push/toggle.
With `-' ARG, for link image - go to it and toggle it, if link,
visit in background.  With -4 ARG, for link image - toggle it.
With double prefix ARG, prompt for url to visit.
With triple prefix ARG, prompt for url to visit in new session.

\(fn ARG)" t nil)

(autoload 'w3m-lnum-universal "w3m-lnum" "\
Turn on link numbers, ask for one and offer actions over it depending on selection type.
Actions may be selected either by hitting corresponding key,
pressing <return> over the action line or left clicking.

\(fn)" t nil)

(autoload 'w3m-lnum-toggle-inline-image "w3m-lnum" "\
If image at point, toggle it.
Otherwise turn on link numbers and toggle selected image.
With prefix ARG open url under image in new session.
If no such url, move over image and toggle it.

\(fn &optional ARG)" t nil)

(autoload 'w3m-lnum-view-image "w3m-lnum" "\
Display the image under point in the external viewer.
If no image at poing, turn on image numbers and display selected.
The viewer is defined in `w3m-content-type-alist' for every type of an
image.

\(fn)" t nil)

(autoload 'w3m-lnum-save-image "w3m-lnum" "\
Save the image under point to a file.
If no image at point, turn on image numbers and save selected.
The default name will be the original name of the image.

\(fn)" t nil)

(autoload 'w3m-lnum-external-view-this-url "w3m-lnum" "\
Launch the external browser and display the link at point.
If no link at point, turn on link numbers and open selected externally.

\(fn)" t nil)

(autoload 'w3m-lnum-edit-this-url "w3m-lnum" "\
Edit the page linked from the anchor under the cursor.
If no such, turn on link numbers and edit selected.

\(fn)" t nil)

(autoload 'w3m-lnum-print-this-url "w3m-lnum" "\
Display the url under point in the echo area and put it into `kill-ring'.
If no url under point, activate numbering and select one.

\(fn)" t nil)

(autoload 'w3m-lnum-download-this-url "w3m-lnum" "\
Download the file or the page pointed to by the link under point.
If no point, activate numbering and select andchor to download.

\(fn)" t nil)

(autoload 'w3m-lnum-bookmark-add-this-url "w3m-lnum" "\
Add link under cursor to bookmarks.
If no link under point, activate numbering and ask for one.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-lnum" '("w3m-")))

;;;***

;;;### (autoloads nil "w3m-mail" "w3m-mail.el" (0 0 0 0))
;;; Generated autoloads from w3m-mail.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-mail" '("w3m-mail")))

;;;***

;;;### (autoloads nil "w3m-namazu" "w3m-namazu.el" (0 0 0 0))
;;; Generated autoloads from w3m-namazu.el

(autoload 'w3m-about-namazu "w3m-namazu" "\


\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-namazu "w3m-namazu" "\
Search indexed files with Namazu.

\(fn INDEX QUERY &optional RELOAD)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-namazu" '("w3m-namazu-")))

;;;***

;;;### (autoloads nil "w3m-perldoc" "w3m-perldoc.el" (0 0 0 0))
;;; Generated autoloads from w3m-perldoc.el

(autoload 'w3m-about-perldoc "w3m-perldoc" "\


\(fn URL &optional NO-DECODE NO-CACHE &rest ARGS)" nil nil)

(autoload 'w3m-perldoc "w3m-perldoc" "\
View Perl documents.

\(fn DOCNAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-perldoc" '("w3m-perldoc-")))

;;;***

;;;### (autoloads nil "w3m-proc" "w3m-proc.el" (0 0 0 0))
;;; Generated autoloads from w3m-proc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-proc" '("w3m-process-")))

;;;***

;;;### (autoloads nil "w3m-rss" "w3m-rss.el" (0 0 0 0))
;;; Generated autoloads from w3m-rss.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-rss" '("w3m-rss-")))

;;;***

;;;### (autoloads nil "w3m-save" "w3m-save.el" (0 0 0 0))
;;; Generated autoloads from w3m-save.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-save" '("w3m-save-buffer")))

;;;***

;;;### (autoloads nil "w3m-search" "w3m-search.el" (0 0 0 0))
;;; Generated autoloads from w3m-search.el

(autoload 'w3m-search "w3m-search" "\
Search QUERY using SEARCH-ENGINE.

Search results will appear in the current buffer.

When called interactively with a prefix argument, you can choose one of
the search engines defined in `w3m-search-engine-alist'.  Otherwise use
`w3m-search-default-engine'.
If Transient Mark mode, use the region as an initial string of query
and deactivate the mark.

\(fn SEARCH-ENGINE QUERY)" t nil)

(autoload 'w3m-search-new-session "w3m-search" "\
Like `w3m-search', but do the search in a new buffer.

\(fn SEARCH-ENGINE QUERY)" t nil)

(autoload 'w3m-search-uri-replace "w3m-search" "\
Generate query string for ENGINE from URI matched by last search.

\(fn URI ENGINE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-search" '("w3m-search-")))

;;;***

;;;### (autoloads nil "w3m-session" "w3m-session.el" (0 0 0 0))
;;; Generated autoloads from w3m-session.el

(autoload 'w3m-session-save "w3m-session" "\
Save the current session (all currently open emacs-w3m buffers).

The user will be prompted for a name for the saved session.  The
saved session information will include, for each currently open
emacs-w3m buffer: the current url and page title, and the
buffer's url history.

\(fn)" t nil)

(autoload 'w3m-session-crash-recovery-remove "w3m-session" "\
Remove crash recovery session set.

\(fn)" nil nil)

(autoload 'w3m-session-select "w3m-session" "\
Select session from session list.
Position point at N-th session if N is given.  With the
prefix-argument, toggles the position of the popup window between
being below or beside the main window.

\(fn &optional N TOGGLE NOMSG)" t nil)

(autoload 'w3m-setup-session-menu "w3m-session" "\
Setup w3m session items in menubar.

\(fn)" nil nil)

(autoload 'w3m-session-last-autosave-session "w3m-session" "\


\(fn)" nil nil)

(autoload 'w3m-session-last-crashed-session "w3m-session" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-session" '("w3m-")))

;;;***

;;;### (autoloads nil "w3m-symbol" "w3m-symbol.el" (0 0 0 0))
;;; Generated autoloads from w3m-symbol.el

(autoload 'w3m-replace-symbol "w3m-symbol" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-symbol" '("w3m-")))

;;;***

;;;### (autoloads nil "w3m-tabmenu" "w3m-tabmenu.el" (0 0 0 0))
;;; Generated autoloads from w3m-tabmenu.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-tabmenu" '("w3m-")))

;;;***

;;;### (autoloads nil "w3m-util" "w3m-util.el" (0 0 0 0))
;;; Generated autoloads from w3m-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-util" '("w3m-")))

;;;***

;;;### (autoloads nil "w3m-weather" "w3m-weather.el" (0 0 0 0))
;;; Generated autoloads from w3m-weather.el

(autoload 'w3m-weather "w3m-weather" "\
Display weather report.

\(fn AREA)" t nil)

(autoload 'w3m-about-weather "w3m-weather" "\


\(fn URL NO-DECODE NO-CACHE POST-DATA REFERER HANDLER)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3m-weather" '("w3m-weather-")))

;;;***

;;;### (autoloads nil "w3mhack" "w3mhack.el" (0 0 0 0))
;;; Generated autoloads from w3mhack.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "w3mhack" '("w3mhack-" "shimbun-module-directory")))

;;;***

;;;### (autoloads nil nil ("w3m-pkg.el") (0 0 0 0))

;;;***

(defconst emacs-w3m-git-revision "6eda382"
  "Git revision string of this package.")

(provide 'w3m-load)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; w3m-autoloads.el ends here
