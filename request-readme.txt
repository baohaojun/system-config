Request.el is a HTTP request library with multiple backends.  It
supports url.el which is shipped with Emacs and curl command line
program.  User can use curl when s/he has it, as curl is more reliable
than url.el.  Library author can use request.el to avoid imposing
external dependencies such as curl to users while giving richer
experience for users who have curl.

Following functions are adapted from GNU Emacs source code.
Free Software Foundation holds the copyright of them.
* `request--process-live-p'
* `request--url-default-expander'
