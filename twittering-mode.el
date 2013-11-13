;;; twittering-mode.el --- Major mode for Twitter   -*- coding: utf-8 -*-

;; Copyright (C) 2007, 2009, 2010 Yuto Hayamizu.
;;               2008 Tsuyoshi CHO
;;               2010, 2011, 2012 William Xu

;; Author: Y. Hayamizu <y.hayamizu@gmail.com>
;;         Tsuyoshi CHO <Tsuyoshi.CHO+develop@Gmail.com>
;;         Alberto Garcia  <agarcia@igalia.com>
;;         William Xu <william.xwl@gmail.com>
;; Created: Sep 4, 2007
;; Version: HEAD
;; Keywords: twitter web
;; URL: http://twmode.sf.net/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; twittering-mode.el is a major mode for Twitter.
;; You can check friends timeline, and update your status on Emacs.

;;; Feature Request:

;; URL : http://code.nanigac.com/source/view/419
;; * update status for region

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)
(require 'parse-time)
(when (> 22 emacs-major-version)
  (setq load-path
        (append (mapcar (lambda (dir)
                          (expand-file-name
                           dir
                           (if load-file-name
                               (or (file-name-directory load-file-name)
                                   ".")
                             ".")))
                        '("url-emacs21" "emacs21"))
                load-path))
  (and (require 'un-define nil t)
       ;; the explicitly require 'unicode to update a workaround with
       ;; navi2ch. see a comment of `twittering-ucs-to-char' for more
       ;; details.
       (require 'unicode nil t)))
(require 'url)
(require 'oauth2)

(defconst twittering-mode-version "HEAD")

(defun twittering-mode-version ()
  "Display a message for twittering-mode version."
  (interactive)
  (let ((version-string
         (format "twittering-mode-v%s" twittering-mode-version)))
    (if (called-interactively-p 'interactive)
        (message "%s" version-string)
      version-string)))

;;;
;;; User Customizable
;;;

(defgroup twittering nil
  "Twitter client."
  :prefix "twittering-"
  :group 'applications)

(defcustom twittering-number-of-tweets-on-retrieval 20
  "*The number of tweets which will be retrieved in one request.
The upper limit is `twittering-max-number-of-tweets-on-retrieval'."
  :type 'integer
  :group 'twittering)

(defcustom twittering-timer-interval 90
  "The interval of auto reloading.
You should use 60 or more seconds for this variable because the number of API
call is limited by the hour."
  :type 'integer
  :group 'twittering)

(defcustom twittering-initial-timeline-spec-string '(":home@twitter")
  "*The initial timeline spec strings."
  :type 'list
  :group 'twittering)

(defcustom twittering-format-thumbnail-prefix "                          "
  "Thumbnail (like in sina weibo) prefix."
  :type 'string
  :group 'twittering)

(defcustom twittering-status-format
  "%FACE[twittering-zebra-1-face,twittering-zebra-2-face]{%i %g %s, from %f%L%r%R:\n%FOLD[       ]{%t}\n}"
  "Format string for rendering statuses.
Ex. \"%i %s,  %@:\\n%FILL{  %t // from %f%L%r%R}\n \"

Items:
 %s - screen_name
 %S - name
 %i - profile_image
 %d - description
 %l - location
 %L - \" [location]\"
 %r - \" sent to user\" (use on direct_messages{,_sent})
 %r - \" in reply to user\" (use on other standard timeline)
 %R - \" (retweeted by user)\"
 %RT{...} - strings rendered only when the tweet is a retweet.
            The braced strings are rendered with the information of the
            retweet itself instead of that of the retweeted original tweet.
            For example, %s for a retweet means who posted the original
            tweet, but %RT{%s} means who retweeted it.
 %u - url
 %j - user.id
 %p - protected?
 %c - created_at (raw UTC string)
 %g - format %c using `gnus-user-date' (Note: this assumes you will not keep
      latest statuses for more than a week)
 %C{time-format-str} - created_at (formatted with time-format-str)
 %@ - X seconds ago
 %t - text filled as one paragraph
 %' - truncated
 %FACE[face-name]{...} - strings decorated with the specified face. You can
                      provide two faces, separated by colon, to create a
                      zebra-like background.

 %FILL[prefix]{...} - strings filled as a paragraph. The prefix is optional.
                      You can use any other specifiers in braces.

 %FOLD[prefix]{...} - strings folded within the frame width.
                      The prefix is optional. This keeps newlines and does not
                      squeeze a series of white spaces.
                      You can use any other specifiers in braces.
 %f - source
 %# - id
"
  :type 'string
  :group 'twittering)

(defcustom twittering-my-status-format
  "%FACE[twittering-zebra-1-face,twittering-zebra-2-face]{%g %s, from %f%L%r%R: %i\n%FOLD[]{%t}\n}"
  "Specific format for my posts.
See `twittering-status-format'. "
  :type 'string
  :group 'twittering)

(defcustom twittering-retweet-format "RT: %t (via @%s)"
  "Format string when retweeting.

Items:
 %s - screen_name
 %t - text
 %% - %
"
  :type 'string
  :group 'twittering)

(defcustom twittering-fill-column nil
  "*The fill-column used for \"%FILL{...}\" in `twittering-status-format'.
If nil, the fill-column is automatically calculated."
  :type 'integer
  :group 'twittering)

(defcustom twittering-my-fill-column nil
  "Similar to `twittering-fill-column', specially for tweets sent by myself."
  :type 'integer
  :group 'twittering)

(defcustom twittering-show-replied-tweets t
  "*The number of replied tweets which will be showed in one tweet.

If the value is not a number and is non-nil, show all replied tweets
which is already fetched.
If the value is nil, doesn't show replied tweets."
  :type 'symbol
  :group 'twittering)

(defcustom twittering-default-show-replied-tweets nil
  "*The number of default replied tweets which will be showed in one tweet.
This value will be used only when showing new tweets.

See `twittering-show-replied-tweets' for more details."
  :type 'symbol
  :group 'twittering)

(defcustom twittering-use-show-minibuffer-length t
  "*Show current length of minibuffer if this variable is non-nil.

We suggest that you should set to nil to disable the showing function
when it conflict with your input method (such as AquaSKK, etc.)"
  :type 'symbol
  :group 'twittering)

(defcustom twittering-notify-successful-http-get t
  "Non-nil will notify successful http GET in minibuffer."
  :type 'symbol
  :group 'twittering)

(defcustom twittering-timeline-most-active-spec-strings '(":home" ":replies")
  "See `twittering-timeline-spec-most-active-p'."
  :type 'list
  :group 'twittering)

(defcustom twittering-request-confirmation-on-posting nil
  "*If *non-nil*, confirmation will be requested on posting a tweet edited in
pop-up buffer."
  :type 'symbol
  :group 'twittering)

(defcustom twittering-timeline-spec-alias nil
  "*Alist for aliases of timeline spec.
Each element is (NAME . SPEC-STRING), where NAME is a string and
SPEC-STRING is a string or a function that returns a timeline spec string.

The alias can be referred as \"$NAME\" or \"$NAME(ARG)\" in timeline spec
string. If SPEC-STRING is a string, ARG is simply ignored.
If SPEC-STRING is a function, it is called with a string argument.
For the style \"$NAME\", the function is called with nil.
For the style \"$NAME(ARG)\", the function is called with a string ARG.

For example, if you specify
 '((\"FRIENDS\" . \"(USER1+USER2+USER3)\")
   (\"to_me\" . \"(:mentions+:retweets_of_me+:direct_messages)\")
   (\"related-to\" .
            ,(lambda (username)
               (if username
                   (format \":search/to:%s OR from:%s OR @%s/\"
                           username username username)
                 \":home\")))),
then you can use \"$to_me\" as
\"(:mentions+:retweets_of_me+:direct_messages)\"."
  :type 'list
  :group 'twittering)

(defcustom twittering-convert-fix-size 48
  "Size of user icon.

When nil, don't convert, simply use original size.

Most default profile_image_url in status is already an
avatar(48x48).  So normally we don't have to convert it at all."
  :type 'number
  :group 'twittering)

(defcustom twittering-new-tweets-count-excluding-me nil
  "Non-nil will exclude my own tweets when counting received new tweets."
  :type 'boolean
  :group 'twittering)

(defcustom twittering-new-tweets-count-excluding-replies-in-home nil
  "Non-nil will exclude replies in home timeline when counting received new
tweets."
  :type 'boolean
  :group 'twittering)

(defcustom twittering-need-to-be-updated-indicator " "
  "A string indicating it is being updated.
The string should not be empty.  "
  :type 'string
  :group 'twittering)

(defcustom twittering-curl-socks-proxy '()
  "Socks parameters for curl session.
Don't use it together with http proxy.  "
  :type 'list
  :group 'twittering)

(defcustom twittering-auto-adjust-fill-column? t
  "If t, adjust `twittering-fill-column' and `twittering-my-fill-column'
automatically when window resizes.  "
  :type 'boolean
  :group 'twittering)

(defcustom twittering-tweet-separator ""
  "A string for separating tweets.  "
  :type 'string
  :group 'twittering)

;;;
;;; Internal Variables
;;;

(defvar twittering-account-authorization '()
  "Alist of state of account authorization for each service.
The value is one of the following symbols:
nil -- The account have not been authorized yet.
queried -- The authorization has been queried, but not finished yet.
authorized -- The account has been authorized.")

(defun twittering-get-account-authorization (&optional service)
  (cadr (assq (or service (twittering-extract-service))
              twittering-account-authorization)))

(defun twittering-update-account-authorization (auth)
  (setq twittering-account-authorization
        `((,(twittering-extract-service) ,auth)
          ,@(assq-delete-all twittering-service-method
                             twittering-account-authorization))))

(defvar twittering-oauth-invoke-browser nil
  "*Whether to invoke a browser on authorization of access key automatically.")

(defconst twittering-max-number-of-tweets-on-retrieval 200
  "The maximum number of `twittering-number-of-tweets-on-retrieval'.")

(defconst twittering-max-number-of-tweets-on-search 100
  "The maximum number for search API: http://search.twitter.com/api/")

(defvar twittering-tinyurl-service 'tinyurl
  "*The service to shorten URI.
This must be one of key symbols of `twittering-tinyurl-services-map'.
To use 'bit.ly or 'j.mp, you have to configure `twittering-bitly-login' and
`twittering-bitly-api-key'.")

(defvar twittering-tinyurl-services-map
  '((bit.ly twittering-make-http-request-for-bitly
            (lambda (service reply)
              (if (string-match "\n\\'" reply)
                  (substring reply 0 (match-beginning 0))
                reply)))
    (goo.gl
     (lambda (service longurl)
       (twittering-make-http-request-from-uri
        "POST" '(("Content-Type" . "application/json"))
        "https://www.googleapis.com/urlshortener/v1/url"
        (concat "{\"longUrl\": \"" longurl "\"}")))
     (lambda (service reply)
       (when (string-match "\"id\"[[:space:]]*:[[:space:]]*\"\\([^\"]*\\)\""
                           reply)
         (match-string 1 reply))))
    (is.gd . "http://is.gd/create.php?format=simple&url=")
    (j.mp twittering-make-http-request-for-bitly
          (lambda (service reply)
            (if (string-match "\n\\'" reply)
                (substring reply 0 (match-beginning 0))
              reply)))
    (tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly
     (lambda (service longurl)
       (twittering-make-http-request-from-uri
        "POST" nil
        "http://to.ly/api.php"
        (concat "longurl=" (twittering-percent-encode longurl))))))
  "Alist of URL shortening services.
The key is a symbol specifying the service.
The value is a string or a list consisting of two elements at most.

If the value is a string, `(concat THE-FIRST-ELEMENT longurl)' is used as the
URL invoking the service.
If the value is a list, it is interpreted as follows.
The first element specifies how to make a HTTP request for shortening a URL.
If the first element is a string, `(concat THE-FIRST-ELEMENT longurl)' is
used as the URL invoking the service.
If the first element is a function, it is called as `(funcall THE-FIRST-ELEMENT
service-symbol longurl)' to obtain a HTTP request alist for invoking the
service, which must be generated by `twittering-make-http-request'.

The second element specifies how to post-process a HTTP reply by the HTTP
request.
If the second element is nil, the reply is directly used as a shortened URL.
If the second element is a function, it is called as `(funcall
THE-SECOND-ELEMENT service-symbol HTTP-reply-string)' and its result is used
as a shortened URL.")

(defvar twittering-bitly-login nil
  "*The login name for URL shortening service bit.ly and j.mp.")
(defvar twittering-bitly-api-key nil
  "*The API key for URL shortening service bit.ly and j.mp.")

(defvar twittering-mode-menu-on-uri-map (make-sparse-keymap "Twittering Mode"))
(defvar twittering-mode-on-uri-map (make-sparse-keymap))

(defvar twittering-tweet-history    nil)
(defvar twittering-user-history     nil)
(defvar twittering-timeline-history nil)
(defvar twittering-hashtag-history  nil)
(defvar twittering-search-history   nil)

(defvar twittering-current-hashtag nil
  "A hash tag string currently set. You can set it by calling
`twittering-set-current-hashtag'.")

(defvar twittering-timer nil
  "Timer object for timeline refreshing will be stored here.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-timer-for-redisplaying nil
  "Timer object for timeline redisplay statuses will be stored here.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-timer-interval-for-redisplaying 5.0
  "The interval of auto redisplaying statuses.
Each time Emacs remains idle for the interval, twittering-mode updates parts
requiring to be redrawn.")

(defvar twittering-timeline-spec nil
  "The timeline spec for the current buffer.")

(defvar twittering-timeline-spec-string ""
  "The timeline spec string for the current buffer.")

(defvar twittering-current-timeline-spec-string nil
  "The current timeline spec string. This variable should not be referred
directly. Use `twittering-current-timeline-spec-string' or
`twittering-current-timeline-spec'.")

(defvar twittering-get-simple-retrieved nil)

(defvar twittering-process-info-alist nil
  "Alist of active process and timeline spec retrieved by the process.")

(defvar twittering-server-info-alist nil
  "Alist of server information.")

(defvar twittering-new-tweets-count 0
  "Number of new tweets when `twittering-new-tweets-hook' is run.")

(defvar twittering-new-tweets-spec nil
  "Timeline spec, which new tweets belong to, when
`twittering-new-tweets-hook' is run.")
(defvar twittering-new-tweets-statuses nil
  "New tweet status messages, when
`twittering-new-tweets-hook' is run.")

(defvar twittering-new-tweets-hook nil
  "*Hook run when new tweets are received.

You can read `twittering-new-tweets-count' or `twittering-new-tweets-spec'
to get the number of new tweets received when this hook is run.")

(defvar twittering-active-mode nil
  "Non-nil if new statuses should be retrieved periodically.
Do not modify this variable directly. Use `twittering-activate-buffer',
`twittering-deactivate-buffer', `twittering-toggle-activate-buffer' or
`twittering-set-active-flag-for-buffer'.")
(defvar twittering-scroll-mode nil)

(defvar twittering-jojo-mode nil)
(defvar twittering-reverse-mode nil
  "*Non-nil means tweets are aligned in reverse order of `http://twitter.com/'.")
(defvar twittering-display-remaining nil
  "*If non-nil, display remaining of rate limit on the mode-line.")
(defvar twittering-display-connection-method t
  "*If non-nil, display the current connection method on the mode-line.")

(defvar twittering-allow-insecure-server-cert nil
  "*If non-nil, twittering-mode allows insecure server certificates.")

(defvar twittering-curl-program nil
  "Cache a result of `twittering-find-curl-program'.
DO NOT SET VALUE MANUALLY.")
(defvar twittering-curl-program-https-capability nil
  "Cache a result of `twittering-start-http-session-curl-https-p'.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-wget-program nil
  "Cache a result of `twittering-find-wget-program'.
DO NOT SET VALUE MANUALLY.")

(defvar twittering-tls-program nil
  "*List of strings containing commands to start TLS stream to a host.
Each entry in the list is tried until a connection is successful.
%h is replaced with server hostname, %p with port to connect to.
Also see `tls-program'.
If nil, this is initialized with a list of valied entries extracted from
`tls-program'.")

(defvar twittering-connection-type-order
  '(curl wget urllib-http native urllib-https)
  "*A list of connection methods in the preferred order.")

(defvar twittering-connection-type-table
  '((native (check . t)
            (https . twittering-start-http-session-native-tls-p)
            (send-http-request . twittering-send-http-request-native)
            (pre-process-buffer . twittering-pre-process-buffer-native))
    (curl (check . twittering-start-http-session-curl-p)
          (https . twittering-start-http-session-curl-https-p)
          (send-http-request . twittering-send-http-request-curl)
          (pre-process-buffer . twittering-pre-process-buffer-curl))
    (wget (check . twittering-start-http-session-wget-p)
          (https . t)
          (send-http-request . twittering-send-http-request-wget)
          (pre-process-buffer . twittering-pre-process-buffer-wget))
    (urllib-http
     (display-name . "urllib")
     (check . twittering-start-http-session-urllib-p)
     (https . nil)
     (send-http-request . twittering-send-http-request-urllib)
     (pre-process-buffer . twittering-pre-process-buffer-urllib))
    (urllib-https
     (display-name . "urllib")
     (check . twittering-start-http-session-urllib-p)
     (https . twittering-start-http-session-urllib-https-p)
     (send-http-request . twittering-send-http-request-urllib)
     (pre-process-buffer . twittering-pre-process-buffer-urllib)))
  "A list of alist of connection methods.")

(defvar twittering-last-status-format ""
  "The status format string that has generated the current
`twittering-format-status-function'.")
(defvar twittering-format-status-function nil
  "The formating function generated from `twittering-last-status-format'.")

(defvar twittering-last-my-status-format "")
(defvar twittering-format-my-status-function nil)

(defvar twittering-timeline-data-table (make-hash-table :test 'equal))

(defface twittering-zebra-1-face `((t (:background "#e6e6fa"))) "" :group 'faces)
(defface twittering-zebra-2-face `((t (:background "white"))) "" :group 'faces)

(defface twittering-verify-face `((t (:inherit 'font-lock-variable-name-face)))
  "Face for decorating the V symbol used by weibo.com.  "
  :group 'faces)

(defvar twittering-use-native-retweet nil
  "Post retweets using native retweets if this variable is non-nil.
This is default value, you can also set it separately for each service in
`twittering-accounts', like (retweet organic) or (retweet native).")

(defvar twittering-update-status-function
  'twittering-update-status-from-pop-up-buffer
  "The function used to posting a tweet. It takes two arguments:
the first argument INIT-STR is initial text to be edited and the
second argument REPLY-TO-ID is a user ID of a tweet to which you
are going to reply.

Twittering-mode provides two functions for updating status:
* `twittering-update-status-from-minibuffer': edit tweets in minibuffer
* `twittering-update-status-from-pop-up-buffer': edit tweets in pop-up buffer")

(defvar twittering-invoke-buffer nil
  "The buffer where we invoke `twittering-get-and-render-timeline'.

If we invoke `twittering-get-and-render-timeline' from a twittering buffer, then
do not display unread notifier on mode line.")

(defvar twittering-use-master-password nil
  "*Whether to store private information encrypted with a master password.")
(defvar twittering-private-info-file
  (expand-file-name "~/.emacs.d/twittering/.twittering.gpg")
  "*File for storing encrypted private information when
`twittering-use-master-password' is non-nil.")
(defvar twittering-variables-stored-with-encryption
  '(twittering-oauth-access-token-alist))

(defvar twittering-oauth-access-token-alist '())

;; buffer local, internal use.
(defvar twittering-service-method nil)

(defcustom twittering-service-method-table
  `((twitter (api        "api.twitter.com")
             (search     "search.twitter.com")
             (web        "twitter.com")
             (stream     "stream.twitter.com")
             (userstream "userstream.twitter.com")

             (api-prefix    "1.1/")
             (search-method "search")

             (oauth-request-token-url-without-scheme
              "://api.twitter.com/oauth/request_token")
             (oauth-authorization-url-base-without-scheme
              "://api.twitter.com/oauth/authorize?oauth_token=")
             (oauth-access-token-url-without-scheme
              "://api.twitter.com/oauth/access_token")

             (oauth-consumer-key
              ,(base64-decode-string
                "bzY1aXZsUXdoeUdQVmdCODVHVFln"))
             (oauth-consumer-secret
              ,(base64-decode-string
                "VjJBYldtYVN0ajFTejB5Q1NBWnBnSVdFOUNFOWtEb1MyaE16a294UVdN"))

             (status-url twittering-get-status-url-twitter)
             (search-url twittering-get-search-url-twitter))

    (statusnet (status-url twittering-get-status-url-statusnet)
               (search-url twittering-get-search-url-statusnet))

    (sina (api "api.weibo.com")
          (web "weibo.com")
          ;; search is restricted by sina..

          (api-prefix    "2/")

          (oauth-authorization-url-base-without-scheme
           "://api.weibo.com/oauth2/authorize")
          (oauth-access-token-url-without-scheme
           "://api.weibo.com/oauth2/access_token")
          
          (oauth-consumer-key
           ,(base64-decode-string "MTg4MjM3MzM0NQ=="))
          (oauth-consumer-secret
           ,(base64-decode-string
             "ZTYxOTM3NWI5N2MzOGFmMGNlZWUxMTg2MzNlODc5ZTM="))

          (status-url twittering-get-status-url-sina)
          (search-url twittering-get-search-url-twitter))

    (douban (api "api.douban.com")
            (web "www.douban.com")

            ,@(mapcar
               (lambda (i)
                 `(,(car i) ,(concat "://www.douban.com/service/auth/" (cadr i))))
               '((oauth-request-token-url-without-scheme "request_token")
                 (oauth-authorization-url-base-without-scheme "authorize?oauth_token=")
                 (oauth-access-token-url-without-scheme "access_token")))

            (oauth-consumer-key
             ,(base64-decode-string "MDNhYjJiM2JiYjY1YWExYTBiMDE4MWIzMzIzMGZlNGU="))
            (oauth-consumer-secret
             ,(base64-decode-string "MjE4OWUxYTdmYmE4ZTkxYw=="))

            (status-url twittering-get-status-url-douban)
            (search-url twittering-get-search-url-twitter))

    (socialcast (status-url twittering-get-status-url-socialcast)
                (search-url twittering-get-search-url-twitter)))
  "A list of alist of service methods.
Following services are supported:
  'twitter    -- http://www.twitter.com
  'statusnet  -- http://status.net
  'sina       -- http://weibo.com
  'douban     -- http://www.douban.com
  'socialcast -- your own socialcast web(http://www.socialcast.com/)"
  :type 'list
  :group 'twittering)

(defun twittering-lookup-service-method-table (attr)
  "Lookup ATTR value for `twittering-service-method'."
  (or (cadr (assq attr (assqref (twittering-extract-service)
                                twittering-service-method-table)))
      ""))

(defun twittering-lookup-oauth-access-token-alist ()
  (assqref (twittering-extract-service) twittering-oauth-access-token-alist))

(defun twittering-update-oauth-access-token-alist (alist)
  (setq twittering-oauth-access-token-alist
        `((,(twittering-extract-service) ,@alist)
          ,@(assq-delete-all (twittering-extract-service)
                             twittering-oauth-access-token-alist))))

;;          (retweet organic)) ; Default Retweet style: `native', `organic'.
(defcustom twittering-accounts '((twitter (ssl t))
                                 (sina (ssl t))
                                 (socialcast (ssl t)))
  "Account settings per service.

    ((service-method
         (auth oauth)               ; Authentication method: `oauth', `basic'
         (ssl nil)                  ; Use SSL connection: `nil', `t'
         (quotation before)         ; Where to place quotation: `before', `after'
         (status-format STRING)
         (my-status-format STRING)

         ;; Only necessary for `basic' auth.
         (username \"FOO\")
         (password \"PASSWORD\")
     )
     ...)

How To Choose Authentication Methods
------------------------------------

The symbol `basic' means Basic Authentication. The symbol `oauth'means OAuth
Authentication. The symbol `xauth' means xAuth Authentication.  OAuth
Authentication requires a consumer-key and a consumer-secret. Additionally, it
requires an external command `curl' or another command included in
`tls-program', which may be `openssl' or `gnutls-cli', for SSL."
  :type 'list
  :group 'twittering-mode)

(defvar twittering-enabled-services nil)

(defun twittering-get-accounts (attr)
  (let ((token-alist (twittering-lookup-oauth-access-token-alist)))
    (or (cadr (assq attr (assqref (twittering-extract-service) twittering-accounts)))
        (case attr
          ((auth) 'oauth)
          ((username) (or (assocref "screen_name" token-alist)
                          (assocref "user_id" token-alist)
                          (assocref "douban_user_id" token-alist)
                          (assqref 'uid (assocref "access-response" token-alist))))
          ((password) (assocref "password" token-alist))))))

(defvar twittering-accounts-internal '((douban (oauth . 1.0))
                                       (twitter (oauth . 1.0)))
  "Internal service wise configuration.  ")

(defun twittering-get-accounts-internal (attr)
  (assqref attr (assqref (twittering-extract-service)
                         twittering-accounts-internal)))

(defun twittering-update-accounts-internal (alist)
  (let ((service (twittering-extract-service)))
    (setq twittering-accounts-internal
          `(,@(remove-if (lambda (i) (eq (car i) service))
                         twittering-accounts-internal)
            (,service ,@alist)))))

;;; Macros

(defmacro list-push (value listvar)
  `(setq ,listvar (cons ,value ,listvar)))

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
         (let ((keylist (car clause))
               (body (cdr clause)))
           `(,(if (listp keylist)
                  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key))
                                 keylist))
                't)
             ,@body)))
       clauses)))

;;; magic Parser
;;;;

(defun twittering-ucs-to-char-internal (code-point)
  ;; Check (featurep 'unicode) is a workaround with navi2ch to avoid
  ;; error "error in process sentinel: Cannot open load file:
  ;; unicode".
  ;;
  ;; Details: navi2ch prior to 1.8.3 (which is currently last release
  ;; version as of 2010-01-18) always define `ucs-to-char' as autoload
  ;; file "unicode(.el)" (which came from Mule-UCS), hence it breaks
  ;; `ucs-to-char' under non Mule-UCS environment. The problem is
  ;; fixed in navi2ch dated 2010-01-16 or later, but not released yet.
  (if (and (featurep 'unicode) (functionp 'ucs-to-char))
      (ucs-to-char code-point)
    ;; Emacs21 have a partial support for UTF-8 text, so it can decode
    ;; only parts of a text with Japanese.
    (decode-char 'ucs code-point)))

(defvar twittering-unicode-replacement-char
  ;; "Unicode Character 'REPLACEMENT CHARACTER' (U+FFFD)"
  (or (twittering-ucs-to-char-internal #xFFFD)
      ??)
  "*Replacement character returned by `twittering-ucs-to-char' when it fails
to decode a code.")

(defun twittering-ucs-to-char (code-point)
  "Return a character specified by CODE-POINT in Unicode.
If it fails to decode the code, return `twittering-unicode-replacement-char'."
  (or (twittering-ucs-to-char-internal code-point)
      twittering-unicode-replacement-char))

(defadvice decode-char (after twittering-add-fail-over-to-decode-char)
  (when (null ad-return-value)
    (setq ad-return-value twittering-unicode-replacement-char)))

;;; ============================================= OAuth
;;;;

(defun twittering-oauth-url-encode (str &optional coding-system)
  "Encode string according to Percent-Encoding defined in RFC 3986."
  (let ((coding-system (or (when (and coding-system
                                      (coding-system-p coding-system))
                             coding-system)
                           'utf-8)))
    (mapconcat
     (lambda (c)
       (cond
        ((or (and (<= ?A c) (<= c ?Z))
             (and (<= ?a c) (<= c ?z))
             (and (<= ?0 c) (<= c ?9))
             (eq ?. c)
             (eq ?- c)
             (eq ?_ c)
             (eq ?~ c))
         (char-to-string c))
        (t (format "%%%02X" c))))
     (encode-coding-string str coding-system)
     "")))

(defun twittering-oauth-unhex (c)
  (cond
   ((and (<= ?0 c) (<= c ?9))
    (- c ?0))
   ((and (<= ?A c) (<= c ?F))
    (+ 10 (- c ?A)))
   ((and (<= ?a c) (<= c ?f))
    (+ 10 (- c ?a)))
   ))

(defun twittering-oauth-url-decode (str &optional coding-system)
  (let* ((coding-system (or (when (and coding-system
                                       (coding-system-p coding-system))
                              coding-system)
                            'utf-8))
         (substr-list (split-string str "%"))
         (head (car substr-list))
         (tail (cdr substr-list)))
    (decode-coding-string
     (concat
      head
      (mapconcat
       (lambda (substr)
         (if (string-match "\\`\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\(.*\\)\\'"
                           substr)
             (let* ((c1 (string-to-char (match-string 1 substr)))
                    (c0 (string-to-char (match-string 2 substr)))
                    (tail (match-string 3 substr))
                    (ch (+ (* 16 (twittering-oauth-unhex c1))
                           (twittering-oauth-unhex c0))))
               (concat (char-to-string ch) tail))
           substr))
       tail
       ""))
     coding-system)))

(defun twittering-oauth-make-signature-base-string (method base-url parameters)
  ;; "OAuth Core 1.0a"
  ;; http://oauth.net/core/1.0a/#anchor13
  (let* ((sorted-parameters (copy-sequence parameters))
         (sorted-parameters
          (sort sorted-parameters
                (lambda (entry1 entry2)
                  (string< (car entry1) (car entry2))))))
    (concat
     method
     "&"
     (twittering-oauth-url-encode base-url)
     "&"
     (mapconcat
      (lambda (entry)
        (let ((key (car entry))
              (value (cdr entry)))
          (concat (twittering-oauth-url-encode key)
                  "%3D"
                  (twittering-oauth-url-encode value))))
      sorted-parameters
      "%26"))))

(defun twittering-oauth-make-random-string (len)
  (let* ((table
          (concat
           "0123456789"
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           "abcdefghijklmnopqrstuvwxyz"))
         (n (length table))
         (l 0)
         (result (make-string len ?0)))
    (while (< l len)
      (aset result l (aref table (random n)))
      (setq l (1+ l)))
    result))

;;;
;; The below function is derived from `hmac-sha1' retrieved
;; from http://www.emacswiki.org/emacs/HmacShaOne.
;;;
(defun twittering-hmac-sha1 (key message)
  "Return an HMAC-SHA1 authentication code for KEY and MESSAGE.

KEY and MESSAGE must be unibyte strings.  The result is a unibyte
string.  Use the function `encode-hex-string' or the function
`base64-encode-string' to produce human-readable output.

See URL:<http://en.wikipedia.org/wiki/HMAC> for more information
on the HMAC-SHA1 algorithm.

The Emacs multibyte representation actually uses a series of
8-bit values under the hood, so we could have allowed multibyte
strings as arguments.  However, internal 8-bit values don't
correspond to any external representation \(at least for major
version 22).  This makes multibyte strings useless for generating
hashes.

Instead, callers must explicitly pick and use an encoding for
their multibyte data.  Most callers will want to use UTF-8
encoding, which we can generate as follows:

  (let ((unibyte-key   (encode-coding-string key   'utf-8 t))
        (unibyte-value (encode-coding-string value 'utf-8 t)))
    (twittering-hmac-sha1 unibyte-key unibyte-value))

For keys and values that are already unibyte, the
`encode-coding-string' calls just return the same string."
  ;; Return an HMAC-SHA1 authentication code for KEY and MESSAGE.
  ;;
  ;; KEY and MESSAGE must be unibyte strings.  The result is a unibyte
  ;; string.  Use the function `encode-hex-string' or the function
  ;; `base64-encode-string' to produce human-readable output.
  ;;
  ;; See URL:<http://en.wikipedia.org/wiki/HMAC> for more information
  ;; on the HMAC-SHA1 algorithm.
  ;;
  ;; The Emacs multibyte representation actually uses a series of
  ;; 8-bit values under the hood, so we could have allowed multibyte
  ;; strings as arguments.  However, internal 8-bit values don't
  ;; correspond to any external representation \(at least for major
  ;; version 22).  This makes multibyte strings useless for generating
  ;; hashes.
  ;;
  ;; Instead, callers must explicitly pick and use an encoding for
  ;; their multibyte data.  Most callers will want to use UTF-8
  ;; encoding, which we can generate as follows:
  ;;
  ;; (let ((unibyte-key   (encode-coding-string key   'utf-8 t))
  ;;       (unibyte-value (encode-coding-string value 'utf-8 t)))
  ;; (hmac-sha1 unibyte-key unibyte-value))
  ;;
  ;; For keys and values that are already unibyte, the
  ;; `encode-coding-string' calls just return the same string.
  ;;
  ;; Author: Derek Upham - sand (at) blarg.net
  ;;
  ;; Copyright: This code is in the public domain.
  (require 'sha1)
  (when (multibyte-string-p key)
    (error "key must be unibyte"))
  (when (multibyte-string-p message)
    (error "message must be unibyte"))

  ;; The key block is always exactly the block size of the hash
  ;; algorithm.  If the key is too small, we pad it with zeroes (or
  ;; instead, we initialize the key block with zeroes and copy the
  ;; key onto the nulls).  If the key is too large, we run it
  ;; through the hash algorithm and use the hashed value (strange
  ;; but true).

  (let ((+hmac-sha1-block-size-bytes+ 64)) ; SHA-1 uses 512-bit blocks
    (when (< +hmac-sha1-block-size-bytes+ (length key))
      (setq key (sha1 key nil nil t)))

    (let ((key-block (make-vector +hmac-sha1-block-size-bytes+ 0)))
      (dotimes (i (length key))
        (aset key-block i (aref key i)))

      (let ((opad (make-vector +hmac-sha1-block-size-bytes+ #x5c))
            (ipad (make-vector +hmac-sha1-block-size-bytes+ #x36)))
        (dotimes (i +hmac-sha1-block-size-bytes+)
          (aset ipad i (logxor (aref ipad i) (aref key-block i)))
          (aset opad i (logxor (aref opad i) (aref key-block i))))

        (when (fboundp 'unibyte-string)
          ;; `concat' of Emacs23 (and later?) generates a multi-byte
          ;; string from a vector of characters with eight bit.
          ;; Since `opad' and `ipad' must be unibyte, we have to
          ;; convert them by using `unibyte-string'.
          ;; We cannot use `string-as-unibyte' here because it encodes
          ;; bytes with the manner of UTF-8.
          (setq opad (apply 'unibyte-string (mapcar 'identity opad)))
          (setq ipad (apply 'unibyte-string (mapcar 'identity ipad))))
        (sha1 (concat opad
                      (sha1 (concat ipad message)
                            nil nil t))
              nil nil t)))))

(defun twittering-oauth-auth-str (method base-url query-parameters oauth-parameters key)
  "Generate the value for HTTP Authorization header on OAuth.
QUERY-PARAMETERS is an alist for query parameters, where name and value
must be encoded into the same as they will be sent."
  (let* ((parameters (append query-parameters oauth-parameters))
         (base-string
          (twittering-oauth-make-signature-base-string method base-url parameters))
         (key (if (multibyte-string-p key)
                  (string-make-unibyte key)
                key))
         (base-string (if (multibyte-string-p base-string)
                          (string-make-unibyte base-string)
                        base-string))
         (signature
          (base64-encode-string (twittering-hmac-sha1 key base-string))))
    (concat
     "OAuth "
     (mapconcat
      (lambda (entry)
        (concat (car entry) "=\"" (cdr entry) "\""))
      ;; TODO, what to set? (xwl)
      `(("realm" . ,(twittering-oauth-url-encode "http://127.0.0.1/"))
        ,@oauth-parameters)
      ",")
     ",oauth_signature=\"" (twittering-oauth-url-encode signature) "\"")))

(defun twittering-oauth-auth-str-request-token (url query-parameters consumer-key consumer-secret &optional oauth-parameters)
  (let ((key (concat consumer-secret "&"))
        (oauth-params
         (or oauth-parameters
             `(("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
               ("oauth_callback" . "oob")
               ("oauth_signature_method" . "HMAC-SHA1")
               ("oauth_timestamp" . ,(format-time-string "%s"))
               ("oauth_consumer_key" . ,consumer-key)
               ("oauth_version" . "1.0")))))
    (twittering-oauth-auth-str "POST" url query-parameters oauth-params key)))

(defun twittering-oauth-auth-str-exchange-token (url query-parameters consumer-key consumer-secret request-token request-token-secret verifier &optional oauth-parameters)
  (let ((key (concat consumer-secret "&" request-token-secret))
        (oauth-params
         (or oauth-parameters
             `(("oauth_consumer_key" . ,consumer-key)
               ("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
               ("oauth_signature_method" . "HMAC-SHA1")
               ("oauth_timestamp" . ,(format-time-string "%s"))
               ("oauth_version" . "1.0")
               ("oauth_token" . ,request-token)
               ,@(unless (string-match "douban.com" url)
                   `(("oauth_verifier" . ,verifier)))))))
    (twittering-oauth-auth-str "POST" url query-parameters oauth-params key)))

(defun twittering-oauth-auth-str-access (method url query-parameters consumer-key consumer-secret access-token access-token-secret &optional oauth-parameters)
  "Generate a string for Authorization in HTTP header on OAuth.
METHOD means HTTP method such as \"GET\", \"POST\", etc. URL means a simple
URL without port number and query parameters.
QUERY-PARAMETERS means an alist of query parameters such as
'((\"status\" . \"test%20tweet\")
  (\"in_reply_to_status_id\" . \"12345678\")),
where name and value must be encoded into the same as they will be sent.
CONSUMER-KEY and CONSUMER-SECRET specifies the consumer.
ACCESS-TOKEN and ACCESS-TOKEN-SECRET must be authorized before calling this
function."
  (let ((key (concat consumer-secret "&" access-token-secret))
        (oauth-params
         (or oauth-parameters
             `(("oauth_consumer_key" . ,consumer-key)
               ("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
               ("oauth_signature_method" . "HMAC-SHA1")
               ("oauth_timestamp" . ,(format-time-string "%s"))
               ("oauth_version" . "1.0")
               ("oauth_token" . ,access-token)))))
    (twittering-oauth-auth-str method url query-parameters oauth-params key)))

;; "Using xAuth | dev.twitter.com"
;; http://dev.twitter.com/pages/xauth
(defun twittering-xauth-auth-str-access-token (url query-parameters consumer-key consumer-secret username password &optional oauth-parameters)
  (let ((key (concat consumer-secret "&"))
        (oauth-params
         (or oauth-parameters
             `(("oauth_nonce" . ,(twittering-oauth-make-random-string 43))
               ("oauth_signature_method" . "HMAC-SHA1")
               ("oauth_timestamp" . ,(format-time-string "%s"))
               ("oauth_consumer_key" . ,consumer-key)
               ("oauth_version" . "1.0"))))
        (query-params
         (append query-parameters
                 `(("x_auth_mode" . "client_auth")
                   ("x_auth_password"
                    . ,(twittering-oauth-url-encode password))
                   ("x_auth_username"
                    . ,(twittering-oauth-url-encode username))))))
    (twittering-oauth-auth-str "POST" url query-params oauth-params key)))

;; "OAuth Core 1.0a"
;; http://oauth.net/core/1.0a/#response_parameters
(defun twittering-oauth-make-response-alist (str)
  (mapcar
   (lambda (entry)
     (let* ((pair (split-string entry "="))
            (name-entry (car pair))
            (value-entry (cadr pair))
            (name (and name-entry (twittering-oauth-url-decode name-entry)))
            (value (and value-entry
                        (twittering-oauth-url-decode value-entry))))
       `(,name . ,value)))
   (split-string str "&")))

(defun twittering-oauth-get-token-alist (url auth-str &optional post-body)
  (let ((additional-info '((sync . t)))
        (request (twittering-make-http-request-from-uri
                  "POST"
                  `(("Authorization" . ,auth-str)
                    ("Accept-Charset" . "us-ascii")
                    ("Content-Type" . "application/x-www-form-urlencoded"))
                  url post-body)))
    (let (result)
      (twittering-send-http-request
       request additional-info
       (lambda (proc status connection-info header-info)
         (let ((status-line (assqref 'status-line header-info))
               (status-code (assqref 'status-code header-info)))
           (case-string
            status-code
            (("200")
             (when twittering-debug-mode
               (let ((buffer (current-buffer)))
                 (with-current-buffer (twittering-debug-buffer)
                   (insert-buffer-substring buffer))))
             (setq result
                   (twittering-oauth-make-response-alist (buffer-string)))
             nil)
            (t
             (format "Response: %s" status-line))))))
      result)))

(defun twittering-oauth-get-request-token (url consumer-key consumer-secret)
  (let ((auth-str
         (twittering-oauth-auth-str-request-token
          url nil consumer-key consumer-secret)))
    (twittering-oauth-get-token-alist url auth-str)))

(defun twittering-oauth-exchange-request-token (url consumer-key consumer-secret request-token request-token-secret verifier)
  (let ((auth-str
         (twittering-oauth-auth-str-exchange-token
          url nil
          consumer-key consumer-secret
          request-token request-token-secret verifier)))
    (twittering-oauth-get-token-alist url auth-str)))

(defun twittering-oauth-get-access-token (request-token-url authorize-url-func access-token-url consumer-key consumer-secret consumer-name)
  "Return an alist of authorized access token.
The function retrieves a request token from the site specified by
REQUEST-TOKEN-URL. Then, The function asks a WWW browser to authorize the
token by calling `browse-url'. The URL for authorization is calculated by
calling AUTHORIZE-URL-FUNC with the request token as an argument.
AUTHORIZE-URL-FUNC is called as `(funcal AUTHORIZE-URL-FUNC request-token)',
where the request-token is a string.
After calling `browse-url', the function waits for user to input the PIN code
that is displayed in the browser. The request token is authorized by the
PIN code, and then it is exchanged for the access token on the site
specified by ACCESS-TOKEN-URL.
CONSUMER-KEY and CONSUMER-SECRET specify the consumer.
CONSUMER-NAME is displayed at the guide of authorization.

The access token is returned as a list of a cons pair of name and value
like following:
 ((\"oauth_token\"
  . \"819797-Jxq8aYUDRmykzVKrgoLhXSq67TEa5ruc4GJC2rWimw\")
  (\"oauth_token_secret\"
   . \"J6zix3FfA9LofH0awS24M3HcBYXO5nI1iYe8EfBA\")
  (\"user_id\" . \"819797\")
  (\"screen_name\" . \"episod\"))
."
  (let* ((request-token-alist
          (twittering-oauth-get-request-token
           request-token-url consumer-key consumer-secret))
         (request-token (assocref "oauth_token" request-token-alist))
         (request-token-secret
          (assocref "oauth_token_secret" request-token-alist))
         (authorize-url (funcall authorize-url-func request-token))
         (str
          (concat
           (propertize "Authorization via OAuth\n" 'face 'bold)
           "\n"
           "1.Allow access by " consumer-name " on the below site.\n"
           "\n  "
           (propertize authorize-url 'url authorize-url 'face 'bold)
           "\n"
           "\n"
           (when twittering-oauth-invoke-browser
             (concat
              "  Emacs invokes your browser by the function `browse-url'.\n"
              "  If the site is not opened automatically, you have to open\n"
              "  the site manually.\n"
              "\n"))
           "2.After allowing access, the site will display the PIN code."
           "\n"
           "  Input the PIN code "
           (propertize "at the below minibuffer." 'face 'bold))))
    (when request-token-alist
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (let* ((str-height (length (split-string str "\n")))
               (height (max 0 (- (/ (- (window-text-height) 1) 2)
                                 (/ str-height 2)))))
          (insert (make-string height ?\n) str)
          (if twittering-oauth-invoke-browser
              (browse-url authorize-url)
            (when (y-or-n-p "Open authorization URL with browser? (using `browse-url')")
              (browse-url authorize-url)))
          (let* ((pin
                  (if (string-match "douban.com" access-token-url)
                      (unless (y-or-n-p "Have you allowed twittering-mode to access douban? ")
                        (error "Access request rejected"))
                    (block pin-input-block
                      (while t
                        (let ((pin-input (read-string "Input PIN code: ")))
                          (when (string-match "^\\s-*\\([0-9]+\\)\\s-*$" pin-input)
                            (return-from pin-input-block
                              (match-string 1 pin-input))))))))
                 (verifier pin))
            (twittering-oauth-exchange-request-token
             access-token-url
             consumer-key consumer-secret
             request-token request-token-secret verifier)))))))

(defun twittering-xauth-get-access-token (access-token-url consumer-key consumer-secret username password)
  (let ((auth-str
         (twittering-xauth-auth-str-access-token
          access-token-url nil consumer-key consumer-secret
          username password))
        (post-body
         (mapconcat (lambda (pair)
                      (format "%s=%s" (car pair)
                              (twittering-oauth-url-encode (cdr pair))))
                    `(("x_auth_mode" . "client_auth")
                      ("x_auth_password" . ,password)
                      ("x_auth_username" . ,username))
                    "&")))
    (twittering-oauth-get-token-alist access-token-url auth-str post-body)))

(defvar twittering-regexp-uri
  "\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)")

;;; ============================================= Timeline spec
;;;;

;; Timeline spec as S-expression
;; - ((SERVICE-METHOD) user USER): timeline of the user whose name is USER. USER is a string.
;; - ((SERVICE-METHOD) list USER LIST):
;;     the list LIST of the user USER. LIST and USER are strings.
;;     specially,
;;       ((SERVICE-METHOD) list USER following): friends that USER is following.
;;       ((SERVICE-METHOD) list USER followers): followers of USER.
;;
;; - ((SERVICE-METHOD) direct_messages): received direct messages.
;; - ((SERVICE-METHOD) direct_messages_sent): sent direct messages.
;; - ((SERVICE-METHOD) friends): friends timeline.
;; - ((SERVICE-METHOD) home): home timeline.
;; - ((SERVICE-METHOD) mentions): mentions timeline.
;;     mentions ((SERVICE-METHOD) status containing @username) for the authenticating user.
;; - ((SERVICE-METHOD) public): public timeline.
;; - ((SERVICE-METHOD) replies): replies.
;; - ((SERVICE-METHOD) retweeted_by_me): retweets posted by the authenticating user.
;; - ((SERVICE-METHOD) retweeted_to_me): retweets posted by the authenticating user's friends.
;; - ((SERVICE-METHOD) retweets_of_me):
;;     tweets of the authenticated user that have been retweeted by others.
;;
;; - ((SERVICE-METHOD) search STRING): the result of searching with query STRING.
;; - ((SERVICE-METHOD) merge SPEC1 SPEC2 ...): result of merging timelines SPEC1 SPEC2 ...
;; - ((SERVICE-METHOD) filter REGEXP SPEC): timeline filtered with REGEXP.
;;

;; Timeline spec string
;;
;; SPEC ::= {PRIMARY | COMPOSITE} "@" SERVICE-METHOD
;; PRIMARY ::= USER | LIST | DIRECT_MESSSAGES | DIRECT_MESSSAGES_SENT
;;             | FRIENDS | HOME | MENTIONS | PUBLIC | REPLIES
;;             | RETWEETED_BY_ME | RETWEETED_TO_ME | RETWEETS_OF_ME
;;             | FOLLOWING | FOLLOWERS
;;             | SEARCH
;; COMPOSITE ::= MERGE | FILTER
;;
;; USER                  ::= /[a-zA-Z0-9_-]+/
;; LIST                  ::= USER "/" LISTNAME
;; LISTNAME              ::= /[a-zA-Z0-9_-]+/
;; DIRECT_MESSSAGES      ::= ":direct_messages"
;; DIRECT_MESSSAGES_SENT ::= ":direct_messages_sent"
;; FRIENDS               ::= ":friends"
;; HOME                  ::= ":home" | "~"
;; MENTIONS              ::= ":mentions"
;; PUBLIC                ::= ":public"
;; REPLIES               ::= ":replies" | "@"
;; RETWEETED_BY_ME       ::= ":retweeted_by_me"
;; RETWEETED_TO_ME       ::= ":retweeted_to_me"
;; RETWEETS_OF_ME        ::= ":retweets_of_me"
;; FOLLOWING             ::= USER "/following"
;; FOLLOWERS             ::= USER "/followers"
;;
;; SEARCH       ::= ":search/" QUERY_STRING "/"
;; QUERY_STRING ::= any string, where "/" is escaped by a backslash.
;; MERGE        ::= "(" MERGED_SPECS ")"
;; MERGED_SPECS ::= SPEC | SPEC "+" MERGED_SPECS
;; FILTER       ::= ":filter/" REGEXP "/" SPEC
;;

(defvar twittering-regexp-hash
  (let ((full-width-number-sign (twittering-ucs-to-char #xff03)))
    ;; Unicode Character 'FULLWIDTH NUMBER SIGN' (U+FF03)
    (concat "\\(?:#\\|" (char-to-string full-width-number-sign) "\\)")))

(defvar twittering-regexp-atmark
  (let ((full-width-commercial-at (twittering-ucs-to-char #xff20)))
    ;; Unicode Character 'FULLWIDTH COMMERCIAL AT' (U+FF20)
    (concat "\\(?:@\\|" (char-to-string full-width-commercial-at) "\\)")))

(defun twittering-timeline-spec-to-string (timeline-spec &optional shorten)
  "Convert TIMELINE-SPEC into a string.
If SHORTEN is non-nil, the abbreviated expression will be used."
  (ignore-errors                 ; TODO, hack, TIMELINE-SPEC should be complete.
    (unless (listp (car timeline-spec))
      (setq timeline-spec `((,(twittering-extract-service)) ,@timeline-spec))))

  (let* ((service (caar timeline-spec))
         (timeline-spec (cdr timeline-spec))
         (type (car timeline-spec))
         (value (cdr timeline-spec)))
    (format
     "%s@%S"
     (cond
      ;; user
      ((eq type 'user) (car value))
      ;; list
      ((eq type 'list) (concat (car value) "/" (cadr value)))
      ;; simple
      ((eq type 'direct_messages)       ":direct_messages")
      ((eq type 'direct_messages_sent)  ":direct_messages_sent")
      ((eq type 'friends)               ":friends")
      ((eq type 'home)                  (if shorten "~" ":home"))
      ((eq type 'mentions)              ":mentions")
      ((eq type 'public)                ":public")
      ((eq type 'replies)               (if shorten "@" ":replies"))
      ((eq type 'retweeted_by_me)       ":retweeted_by_me")
      ((eq type 'retweeted_to_me)       ":retweeted_to_me")
      ((eq type 'retweets_of_me)        ":retweets_of_me")

      ((eq type 'search)
       (let ((query (car value)))
         (concat ":search/"
                 (replace-regexp-in-string "/" "\\/" query nil t)
                 "/")))
      ;; composite
      ((eq type 'filter)
       (let ((regexp (car value))
             (spec (cadr value)))
         (concat ":filter/"
                 (replace-regexp-in-string "/" "\\/" regexp nil t)
                 "/"
                 (twittering-timeline-spec-to-string spec))))
      ((eq type 'merge)
       (concat "("
               (mapconcat 'twittering-timeline-spec-to-string value "+")
               ")")))

     service)))

(defun twittering-extract-timeline-spec (str &optional unresolved-aliases)
  "Extract one timeline spec from STR.
Return cons of the spec and the rest string."
  (let ((re "@\\([a-zA-Z0-9_-]+\\)"))        ; service method
    (cond
     ((null str)
      (error "STR is nil"))

     ((string-match (concat "^\\([a-zA-Z0-9_-]+\\)/\\([a-zA-Z0-9_-]+\\)" re) str)
      (let ((user (match-string 1 str))
            (listname (match-string 2 str))
            (service (intern (match-string 3 str)))
            (rest (substring str (match-end 0))))
        `(((,service) list ,user ,listname) . ,rest)))

     ((string-match (concat "^\\([a-zA-Z0-9_-]+\\)" re) str)
      (let ((user (match-string 1 str))
            (service (intern (match-string 2 str)))
            (rest (substring str (match-end 0))))
        `(((,service) user ,user) . ,rest)))

     ((string-match (concat "^~" re) str)
      (let ((service (intern (match-string 1 str)))
            (rest (substring str (match-end 0))))
        `(((,service) home) . ,rest)))

     ;; Disable this as @ is used for separating timeline name and service
     ;; name. (xwl)
     ;; ((string-match (concat "^" twittering-regexp-atmark) str)
     ;;  `((replies) . ,(substring str (match-end 0))))

     ((string-match (concat "^" twittering-regexp-hash "\\([a-zA-Z0-9_-]+\\)" re)
                    str)
      (let* ((tag (match-string 1 str))
             (service (intern (match-string 2 str)))
             (query (concat "#" tag))
             (rest (substring str (match-end 0))))
        `(((,service) search ,query) . ,rest)))

     ((string-match (concat "^:\\([a-z_/-]+\\)" re) str)
      (let ((type (match-string 1 str))
            (service (intern (match-string 2 str)))
            (following (substring str (match-end 0)))
            (alist '(("direct_messages"      direct_messages)
                     ("direct_messages_sent" direct_messages_sent)
                     ("friends"              friends)
                     ("home"                 home)
                     ("mentions"             mentions)
                     ("public"               public)
                     ("replies"              replies)
                     ("retweeted_by_me"      retweeted_by_me)
                     ("retweeted_to_me"      retweeted_to_me)
                     ("retweets_of_me"       retweets_of_me))))
        (cond
         ((assoc type alist)
          (let ((first-spec (assocref type alist)))
            `(((,service) ,@first-spec) . ,following)))
         ((string-match (concat ;; "^:search/\\(\\(.*?[^\\]\\)??\\(\\\\\\\\\\)*\\)??/"
                         "^:search/\\([^/]+\\)/"
                         re)
                        str)
          (let* ((escaped-query (or (match-string 1 str) ""))
                 (service (intern (match-string 2 str)))
                 (query (replace-regexp-in-string
                         "\\\\/" "/" escaped-query nil t))
                 (rest (substring str (match-end 0))))
            (if (not (string= "" escaped-query))
                `(((,service) search ,query) . ,rest)
              (error "\"%s\" has no valid regexp" str)
              nil)))
         ((string-match (concat "^:filter/\\(\\(.*?[^\\]\\)??\\(\\\\\\\\\\)*\\)??/" re)
                        str)
          (let* ((escaped-regexp (or (match-string 1 str) ""))
                 (service (intern (match-string 3 str)))
                 (regexp (replace-regexp-in-string
                          "\\\\/" "/" escaped-regexp nil t))
                 (following (substring str (match-end 0)))
                 (pair (twittering-extract-timeline-spec
                        following unresolved-aliases))
                 (spec (car pair))
                 (rest (cdr pair)))
            `(((,service) filter ,regexp ,spec) . ,rest)))
         ;; (error "\"%s\" has no valid regexp" str)
         ;; nil))
         (t
          (error "\"%s\" is invalid as a timeline spec" str)
          nil))))

     ;; TODO, check you later. (xwl)
     ((string-match "^\\$\\([a-zA-Z0-9_-]+\\)\\(?:(\\([^)]*\\))\\)?" str)
      (let* ((name (match-string 1 str))
             (rest (substring str (match-end 0)))
             (value (cdr-safe (assoc name twittering-timeline-spec-alias)))
             (arg (match-string 2 str)))
        (if (member name unresolved-aliases)
            (error "Alias \"%s\" includes a recursive reference" name)
          (cond
           ((stringp value)
            (twittering-extract-timeline-spec
             (concat value rest)
             (cons name unresolved-aliases)))
           ((functionp value)
            (twittering-extract-timeline-spec
             (funcall value arg)
             (cons name unresolved-aliases)))
           (t
            (error "Alias \"%s\" is undefined" name))))))
     ((string-match "^(" str)
      (let ((rest (concat "+" (substring str (match-end 0))))
            (result '()))
        (while (and rest (string-match "^\\+" rest))
          (let* ((spec-string (substring rest (match-end 0)))
                 (pair (twittering-extract-timeline-spec
                        spec-string unresolved-aliases))
                 (spec (car pair))
                 (next-rest (cdr pair)))
            (setq result (cons spec result))
            (setq rest next-rest)))
        (if (and rest (string-match "^)" rest))
            (let ((spec-list
                   (apply 'append
                          (mapcar (lambda (x) (if (eq 'merge (car x))
                                                  (cdr x)
                                                (list x)))
                                  (reverse result)))))
              (if (= 1 (length spec-list))
                  `(,(car spec-list) . ,(substring rest 1))
                `((merge ,@spec-list) . ,(substring rest 1))))
          (if rest
              ;; The string following the opening parenthesis `('
              ;; can be interpreted without errors,
              ;; but there is no corresponding closing parenthesis.
              (error "\"%s\" lacks a closing parenthesis" str))
          ;; Does not display additional error messages if an error
          ;; occurred on interpreting the string following
          ;; the opening parenthesis `('.
          nil)))

     ;; (sina) Treat all chinese string as USER. Put this match at back.
     ((string-match (concat "^\\([^@]+\\)" re) str)
      (let ((user (match-string 1 str))
            (service (intern (match-string 2 str)))
            (rest (substring str (match-end 0))))
        `(((,service) user ,user) . ,rest)))
     (t
      (error "\"%s\" is invalid as a timeline spec" str)))))

(defun twittering-extract-service (&optional spec)
  (let ((service (cond ((stringp spec)
                        (when (string-match "@\\(.+\\)" spec)
                          (intern (match-string 1 spec))))
                       ((consp spec)
                        (when (consp (car spec))
                          (caar spec))))))
    (unless service
      (when (twittering-current-timeline-spec)
        (setq service (caar (twittering-current-timeline-spec)))))

    ;; Fall back to twittering-service-method under `let'.
    (unless service
      (setq service twittering-service-method))

    (unless service
      (error "Null service!"))

    service))

(defun twittering-string-to-timeline-spec (spec-str)
  "Convert SPEC-STR into a timeline spec.
Return nil if SPEC-STR is invalid as a timeline spec."
  (unless (string-match "@" spec-str)
    (setq spec-str (format "%s@%S" spec-str (twittering-extract-service))))
  (let ((result-pair (twittering-extract-timeline-spec spec-str)))
    (when (and result-pair (string= "" (cdr result-pair)))
      (car result-pair))))

(defun twittering-timeline-spec-primary-p (spec)
  "Return non-nil if SPEC is a primary timeline spec.
`primary' means that the spec is not a composite timeline spec such as
`filter' and `merge'."
  (let* ((spec (cdr spec))
         (primary-spec-types
          '(user list
                 direct_messages direct_messages_sent
                 friends home mentions public replies
                 search
                 retweeted_by_me retweeted_to_me retweets_of_me))
         (type (car spec)))
    (memq type primary-spec-types)))

(defun twittering-timeline-spec-user-p (spec)
  "Return non-nil if SPEC is a user timeline spec."
  (let ((spec (cdr spec)))
    (and spec (eq (car spec) 'user))))

(defun twittering-timeline-spec-list-p (spec)
  "Return non-nil if SPEC is a list timeline spec."
  (let ((spec (cdr spec)))
    (and spec (eq (car spec) 'list))))

(defun twittering-timeline-spec-direct-messages-p (spec)
  "Return non-nil if SPEC is a timeline spec which is related of
direct_messages."
  (let ((spec (cdr spec)))
    (and spec
         (memq (car spec) '(direct_messages direct_messages_sent)))))

(defun twittering-timeline-spec-user-methods-p (spec)
  "Return non-nil if SPEC belongs to `User Methods' API."
  (let ((spec (cdr spec)))
    (and spec
         (eq (car spec) 'list)
         (member (car (last spec)) '("following" "followers")))))

(defun twittering-timeline-spec-most-active-p (spec)
  "Return non-nil if SPEC is a very active timeline spec.

For less active spec, do not update it every
`twittering-timer-interval', rather, at the start of each hour.
Or we could easily exceed requests limit of Twitter API,
currently 150/hour.  SPEC is such as '(home).  The complete list
is specified in `twittering-timeline-most-active-spec-strings'."
  (and spec
       (string-match
        (regexp-opt twittering-timeline-most-active-spec-strings)
        (twittering-timeline-spec-to-string spec))))

(defun twittering-equal-string-as-timeline (spec-str1 spec-str2)
  "Return non-nil if SPEC-STR1 equals SPEC-STR2 as a timeline spec."
  (when (and (stringp spec-str1) (stringp spec-str2))
    (let ((spec1 (twittering-string-to-timeline-spec spec-str1))
          (spec2 (twittering-string-to-timeline-spec spec-str2)))
      (equal spec1 spec2))))

;;;;
;;;; Retrieved statuses (timeline data)
;;;;

(defun twittering-current-timeline-id-table (&optional spec)
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (if spec
        (elt (gethash spec twittering-timeline-data-table) 0)
      nil)))

(defun twittering-current-timeline-referring-id-table (&optional spec)
  "Return the hash from a ID to the ID of the first observed status
referring the former ID."
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (if spec
        (elt (gethash spec twittering-timeline-data-table) 1)
      nil)))

(defun twittering-current-timeline-data (&optional spec)
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (if spec
        (elt (gethash spec twittering-timeline-data-table) 2)
      nil)))

(defun twittering-remove-timeline-data (&optional spec)
  (let ((spec (or spec (twittering-current-timeline-spec))))
    (remhash spec twittering-timeline-data-table)))

(defun twittering-find-status (id)
  (let ((result nil))
    (maphash
     (lambda (spec pair)
       (let* ((id-table (car pair))
              (entry (gethash id id-table)))
         ;; Take the most detailed status.
         (when (and entry
                    (or (null result) (< (length result) (length entry))))
           (setq result entry))))
     twittering-timeline-data-table)
    result))

(defun twittering-delete-status-from-data-table (id)
  (let ((modified-spec nil))
    (maphash
     (lambda (spec data)
       (let* ((id-table (elt data 0))
              (referring-id-table (elt data 1))
              (timeline-data (elt data 2))
              (status (gethash id id-table)))
         (when status
           (remhash id id-table)
           ;; Here, `referring-id-table' is not modified.
           ;; Therefore, the retweet observed secondly will not appear even
           ;; if the retweet observed first for the same tweet is deleted.
           (setq modified-spec
                 (cons `(,spec
                         ,id-table
                         ,referring-id-table
                         ,(remove status timeline-data))
                       modified-spec)))))
     twittering-timeline-data-table)
    (mapc
     (lambda (data)
       (let* ((spec (car data))
              (buffer (twittering-get-buffer-from-spec spec)))
         (puthash spec (cdr data) twittering-timeline-data-table)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (save-excursion
               (twittering-for-each-property-region
                'id
                (lambda (beg end value)
                  (when (twittering-status-id= id value)
                    (let ((buffer-read-only nil)
                          (separator-pos (min (point-max) (1+ end))))
                      (delete-region beg separator-pos)
                      (goto-char beg))))
                buffer))))))
     modified-spec)))

(defun twittering-get-replied-statuses (id &optional count)
  "Return a list of replied statuses starting from the status specified by ID.
Statuses are stored in ascending-order with respect to their IDs."
  (let ((result nil)
        (status (twittering-find-status id)))
    (case twittering-service-method
      ((sina)
       (maphash
        (lambda (spec pair)
          (let ((id-table (car pair)))
            (maphash
             (lambda (i entry)
               (when (and (assqref 'status entry)
                          (equal (assqref 'id (assqref 'status entry)) id))
                 ;; Only show comment.
                 (let ((st (assqref 'status entry)))
                   (when st
                     (setq entry `((hide-status ,@st)
                                   ,@(remove-if (lambda (i) (eq (car i) 'status))
                                                entry)))))
                 (let ((rc (assqref 'reply-comment entry)))
                   (when rc
                     (setq entry `((hide-reply-comment ,@rc)
                                   ,@(remove-if (lambda (i) (eq (car i) 'reply-comment))
                                                entry)))))
                 (setq result (cons entry result))))
             id-table)))
        twittering-timeline-data-table))
      (t
       (while
           (and (if (numberp count)
                    (<= 0 (setq count (1- count)))
                  t)
                (let ((replied-id (or (assqref 'in-reply-to-status-id status) "")))
                  (unless (string= "" replied-id)
                    (let ((replied-status (twittering-find-status replied-id)))
                      (when replied-status
                        (setq result (cons replied-status result))
                        (setq status replied-status)
                        t))))))))
    result))

(defun twittering-have-replied-statuses-p (id)
  (let ((status (twittering-find-status id)))
    (when status
      (case twittering-service-method
        ((sina)
         (let ((end (or (twittering-get-next-status-head) (point-max)))
               (pos (point))
               has?)
           (save-excursion
             (while (and (not has?)
                         pos
                         (setq pos (next-single-property-change
                                    pos 'need-to-be-updated nil end)))
               (goto-char pos)
               (when (looking-at "轉發.* 評論(\\([0-9]+\\))")
                 (setq has? (match-string 1)))
               (when (eq pos end)
                 (setq pos nil))))
           has?))
        (t
         (let ((replied-id (assqref 'in-reply-to-status-id status)))
           (and replied-id (not (string= "" replied-id)))))))))

(defun twittering-add-statuses-to-timeline-data (statuses &optional spec)
  (let* ((spec (or spec (twittering-current-timeline-spec)))
         (id-table
          (or (twittering-current-timeline-id-table spec)
              (make-hash-table :test 'equal)))
         (referring-id-table
          (or (twittering-current-timeline-referring-id-table spec)
              (make-hash-table :test 'equal)))
         (timeline-data (twittering-current-timeline-data spec)))
    (let ((new-statuses
           (remove nil
                   (mapcar
                    (lambda (status)
                      (ignore-errors
                        (let ((id (assqref 'id status))
                              (retweeted-id
                               (assqref 'id (assqref 'retweeted-status status))))
                          (unless (or (not retweeted-id)
                                      (gethash retweeted-id referring-id-table))
                            ;; Store the id of the first observed tweet
                            ;; that refers `retweeted-id'.
                            (puthash retweeted-id id referring-id-table))
                          (unless (gethash id id-table)
                            (puthash id status id-table)
                            (puthash id id referring-id-table)
                            `((source-spec . ,spec)
                              ,@status)))))
                    statuses))))
      (when new-statuses
        (puthash spec `(,id-table
                        ,referring-id-table
                        ;; Decreasingly by `id' except `followers', which is
                        ;; sorted by recency one starts following me.
                        ,(append new-statuses timeline-data))
                 twittering-timeline-data-table)
        (when (twittering-jojo-mode-p spec)
          (mapc (lambda (status)
                  (twittering-update-jojo (assqref 'screen-name (assqref 'user status))
                                          (assqref 'text status)))
                new-statuses))
        (let* ((twittering-new-tweets-spec spec)
               (twittering-new-tweets-statuses new-statuses)
               (spec-string (twittering-timeline-spec-to-string spec))
               (twittering-new-tweets-count
                (if (twittering-timeline-spec-user-methods-p spec)
                    (twittering-count-unread-for-user-methods spec statuses)
                  (count-if (lambda (st) (twittering-is-unread-status-p st spec))
                            new-statuses))))
          (let ((latest
                 (if (twittering-timeline-spec-user-methods-p spec)
                     (assqref 'screen-name (assqref 'user (car statuses)))
                   (assqref 'id (car new-statuses)))))
            (setq twittering-cache-lastest-statuses
                  `((,spec-string . ,latest)
                    ,@(remove-if (lambda (entry) (equal spec-string (car entry)))
                                 twittering-cache-lastest-statuses))))
          (run-hooks 'twittering-new-tweets-hook)
          (if (and (twittering-timeline-spec-user-methods-p spec)
                   ;; Insert all when buffer is empty.
                   (> (buffer-size) 0))
              (twittering-take twittering-new-tweets-count statuses)
            new-statuses))))))

(defcustom twittering-status-filter nil
  "Filter for whether to show a status in timeline or not.
It will be called with one argument -- `status', only when it returns t, will
the status be shown. ")

(defun twittering-timeline-data-collect (&optional spec timeline-data)
  "Collect visible statuses for `twittering-render-timeline'."
  (let* ((spec (or spec (twittering-current-timeline-spec)))
         (referring-id-table
          (twittering-current-timeline-referring-id-table spec))
         (timeline-data
          (or timeline-data (twittering-current-timeline-data spec))))
    (remove
     nil
     (mapcar
      (lambda (status)
        (let ((id (assqref 'id status))
              (is-retweeting (twittering-status-has-quotation? status))
              (retweeted-id (assqref 'id (assqref 'retweeted-status status))))
          (when (and (or (twittering-timeline-spec-user-methods-p spec)
                         (if is-retweeting
                             (and (twittering-status-id=
                                   id (gethash retweeted-id referring-id-table)))
                           t)
                         (eq (twittering-extract-service spec) 'sina))
                     (or (not twittering-status-filter)
                         (funcall twittering-status-filter status))
                     (not (assqref 'twittering-reply? status)))
            status)))
      timeline-data))))

(defun twittering-timeline-data-is-previous-p (timeline-data)
  "Are TIMELINE-DATA previous statuses?
This is done by comparing statues in current buffer with TIMELINE-DATA."
  (let ((status (car timeline-data)))
    (if (twittering-timeline-spec-user-methods-p
         (twittering-current-timeline-spec))
        (let* ((previous-cursor (cdr-safe (assq 'previous-cursor status)))
               (new-follower-p (string= previous-cursor "0")))
          (not new-follower-p))
      (let* ((buf-id (get-text-property
                      (twittering-get-current-status-head
                       (if twittering-reverse-mode (point-min) (point-max)))
                      'id))
             (id (assqref 'id status)))
        (and buf-id (twittering-status-id< id buf-id))))))

(defun twittering-is-unread-status-p (status &optional spec)
  (let ((spec-string
         (twittering-timeline-spec-to-string
          (or spec (setq spec (twittering-current-timeline-spec))))))
    (cond
     ((or (and twittering-new-tweets-count-excluding-me
               (twittering-my-status-p status)
               (not (equal spec '(retweets_of_me))))
          (and twittering-new-tweets-count-excluding-replies-in-home
               (equal spec '(home))
               (twittering-is-replies-p status)))
      nil)
     (t
      (twittering-status-id<
       (assocref spec-string twittering-cache-lastest-statuses)
       (assqref 'id status))))))

(defun twittering-count-unread-for-user-methods (spec new-statuses)
  (let ((latest-username
         (or (with-current-buffer (twittering-get-buffer-from-spec spec)
               (goto-char (funcall (if twittering-reverse-mode 'point-max 'point-min)))
               (get-text-property (twittering-get-current-status-head) 'username))
             (let ((spec-string (twittering-timeline-spec-to-string spec)))
               (assocref spec-string twittering-cache-lastest-statuses)))))
    (if (not latest-username)
        (length new-statuses)
      (let ((statuses new-statuses)
            (count 0))
        (while (and statuses
                    (not (string= latest-username
                                  (assqref 'screen-name (assqref 'user (car statuses))))))
          (setq count (1+ count)
                statuses (cdr statuses)))
        count))))

;;;;

;;; ============================================= HTTP
;;;; Basic HTTP functions (general)
;;;;

(defun twittering-percent-encode (str &optional coding-system)
  "Encode STR according to Percent-Encoding defined in RFC 3986."
  (twittering-oauth-url-encode str coding-system))

(defun twittering-lookup-connection-type (use-ssl &optional order table)
  "Return available entry extracted fron connection type table.
TABLE is connection type table, which is an alist of type symbol and its
item alist, such as
 '((native (check . t)
           (https . twittering-start-http-session-native-tls-p)
           (start . twittering-start-http-session-native))
   (curl (check . twittering-start-http-session-curl-p)
         (https . twittering-start-http-session-curl-https-p)
         (start . twittering-start-http-session-curl))) .
ORDER means the priority order of type symbols.
If USE-SSL is nil, the item `https' is ignored.
When the type `curl' has priority and is available for the above table,
the function returns
 '((check . twittering-start-http-session-curl-p)
   (https . twittering-start-http-session-curl-https-p)
   (start . twittering-start-http-session-curl)) ."
  (let ((rest (or order twittering-connection-type-order))
        (table (or table twittering-connection-type-table))
        (result nil))
    (while (and rest (null result))
      (let* ((candidate (car rest))
             (entry (cons `(symbol . ,candidate)
                          (assqref candidate table)))
             (entry (if (assq 'display-name entry)
                        entry
                      (cons `(display-name . ,(symbol-name candidate))
                            entry)))
             (validate (lambda (item)
                         (let ((v (assqref item entry)))
                           (or (null v) (eq t v) (functionp v)))))
             (confirm (lambda (item)
                        (let ((v (assqref item entry)))
                          (cond
                           ((null v) nil)
                           ((eq t v) t)
                           ((functionp v) (funcall v)))))))
        (if (and (funcall validate 'check)
                 (or (not use-ssl) (funcall validate 'https)))
            (cond
             ((and (funcall confirm 'check)
                   (or (not use-ssl) (funcall confirm 'https)))
              (setq rest nil)
              (setq result entry))
             (t
              (setq rest (cdr rest))))
          (message "The configuration for conncetion type `%s' is invalid."
                   candidate)
          (setq rest nil))))
    result))

(defun twittering-get-connection-method-name (use-ssl)
  "Return a name of the preferred connection method.
If USE-SSL is non-nil, return a connection method for HTTPS.
If USE-SSL is nil, return a connection method for HTTP."
  (assqref 'display-name (twittering-lookup-connection-type use-ssl)))

(defun twittering-lookup-http-start-function (&optional order table)
  "Decide a connection method from currently available methods."
  (let ((entry
         (twittering-lookup-connection-type (twittering-get-accounts 'ssl) order table)))
    (assqref 'send-http-request entry)))

(defun twittering-ensure-connection-method ()
  "Ensure a connection method with a compromise.
Return nil if no connection methods are available with a compromise."
  (let* ((use-ssl (twittering-get-accounts 'ssl)))
    (twittering-lookup-connection-type use-ssl)))

(defun twittering-make-http-request (method header-list host port path query-parameters post-body use-ssl)
  "Returns an alist specifying a HTTP request.
METHOD specifies HTTP method. It must be \"GET\" or \"POST\".
HEADER-LIST is a list of (field-name . field-value) specifying HTTP header
fields. The fields \"Host\", \"User-Agent\" and \"Content-Length\" are
automatically filled if necessary.
HOST specifies the host.
PORT specifies the port. This must be an integer.
PATH specifies the absolute path in URI (without query string).
QUERY-PARAMTERS is a string or an alist.
If QUERY-PARAMTERS is a string, it is treated as an encoded query string.
If QUERY-PARAMTERS is an alist, it represents a list of cons pairs of
string, (query-key . query-value).
POST-BODY specifies the post body sent when METHOD equals to \"POST\".
If POST-BODY is nil, no body is posted.
If USE-SSL is non-nil, the request is performed with SSL.

The result alist includes the following keys, where a key is a symbol.
  method: HTTP method such as \"GET\" or \"POST\".
  scheme: the scheme name. \"http\" or \"https\".
  host: the host to which the request is sent.
  port: the port to which the request is sent (integer).
  path: the absolute path string. Note that it does not include query string.
  query-string: the query string.
  encoded-query-alist: the alist consisting of pairs of encoded query-name and
    encoded query-value.
  uri: the URI. It includes the query string.
  uri-without-query: the URI without the query string.
  header-list: an alist specifying pairs of a parameter and its value in HTTP
    header field.
  post-body: the entity that will be posted."
  (let* ((scheme (if use-ssl "https" "http"))
         (default-port (if use-ssl 443 80))
         (port (if port port default-port))
         (query-string
          (cond
           ((stringp query-parameters)
            query-parameters)
           ((consp query-parameters)
            (mapconcat (lambda (pair)
                         (cond
                          ((stringp pair)
                           (twittering-percent-encode pair))
                          ((consp pair)
                           (format
                            "%s=%s"
                            (twittering-percent-encode (car pair))
                            (twittering-percent-encode (cdr pair))))
                          (t
                           nil)))
                       query-parameters
                       "&"))
           (t
            nil)))
         (encoded-query-alist
          (cond
           ((stringp query-parameters)
            ;; Query name and its value must be already encoded.
            (mapcar (lambda (str)
                      (if (string-match "=" str)
                          (let ((key (substring str 0 (match-beginning 0)))
                                (value (substring str (match-end 0))))
                            `(,key . ,value))
                        `(,str . nil)))
                    (split-string query-parameters "&")))
           ((consp query-parameters)
            (mapcar (lambda (pair)
                      (cond
                       ((stringp pair)
                        (cons (twittering-percent-encode pair) nil))
                       ((consp pair)
                        (cons (twittering-percent-encode (car pair))
                              (twittering-percent-encode (cdr pair))))
                       (t
                        nil)))
                    query-parameters))
           (t
            nil)))
         (uri-without-query
          (concat scheme "://"
                  host
                  (when (and port (not (= port default-port)))
                    (format ":%d" port))
                  path))
         (uri
          (if query-string
              (concat uri-without-query "?" query-string)
            uri-without-query))
         (header-list
          `(,@(when (and (string= method "POST")
                         (not (assoc "Content-Length" header-list)))
                `(("Content-Length" . ,(format "%d" (length post-body)))))
            ,@(unless (assoc "Host" header-list)
                `(("Host" . ,host)))
            ,@(unless (assoc "User-Agent" header-list)
                `(("User-Agent" . ,(twittering-user-agent))))
            ,@header-list)))
    (cond
     ((not (member method '("POST" "GET")))
      (error "Unknown HTTP method: %s" method)
      nil)
     ((not (string-match "^/" path))
      (error "Invalid HTTP path: %s" path)
      nil)
     (t
      `((method . ,method)
        (scheme . ,scheme)
        (host . ,host)
        (port . ,port)
        (path . ,path)
        (query-string . ,query-string)
        (encoded-query-alist . ,encoded-query-alist)
        (uri . ,uri)
        (uri-without-query . ,uri-without-query)
        (header-list . ,header-list)
        (post-body . ,post-body))))))

(defun twittering-make-http-request-from-uri (method header-list uri &optional post-body)
  "Returns an alist specifying a HTTP request.
The result alist has the same form as an alist generated by
`twittering-make-http-request'.

METHOD specifies HTTP method. It must be \"GET\" or \"POST\".
HEADER-LIST is a list of (field-name . field-value) specifying HTTP header
fields. The fields \"Host\" and \"User-Agent\" are automatically filled
if necessary.
URI specifies the URI including query string.
POST-BODY specifies the post body sent when METHOD equals to \"POST\".
If POST-BODY is nil, no body is posted."
  (let* ((parts-alist
          (let ((parsed-url (url-generic-parse-url uri)))
            ;; This is required for the difference of url library
            ;; distributed with Emacs 22 and 23.
            (cond
             ((and (fboundp 'url-p) (url-p parsed-url))
              ;; Emacs 23 and later.
              `((scheme . ,(url-type parsed-url))
                (host . ,(url-host parsed-url))
                (port . ,(url-portspec parsed-url))
                (path . ,(url-filename parsed-url))))
             ((vectorp parsed-url)
              ;; Emacs 22.
              `((scheme . ,(aref parsed-url 0))
                (host . ,(aref parsed-url 3))
                (port . ,(aref parsed-url 4))
                (path . ,(aref parsed-url 5))))
             (t
              nil))))
         (path (let ((path (assqref 'path parts-alist)))
                 (if (string-match "\\`\\(.*\\)\\?" path)
                     (match-string 1 path)
                   path)))
         (query-string (let ((path (assqref 'path parts-alist)))
                         (if (string-match "\\?\\(.*\\)\\'" path)
                             (match-string 1 path)
                           nil))))
    (twittering-make-http-request method header-list
                                  (assqref 'host parts-alist)
                                  (assqref 'port parts-alist)
                                  path
                                  query-string
                                  post-body
                                  (string= "https"
                                           (assqref 'scheme parts-alist)))))

(defun twittering-make-connection-info (request &optional additional order table)
  "Make an alist specifying the information of connection for REQUEST.
REQUEST must be an alist that has the same keys as that generated by
`twittering-make-http-request'.

ADDITIONAL is appended to the tail of the result alist.
Following ADDITIONAL, an entry in TABLE is also appended to the result alist,
where `twittering-lookup-connection-type' determines the entry according to
the priority order ORDER.
If ORDER is nil, `twittering-connection-type-order' is used in place of ORDER.
If TABLE is nil, `twittering-connection-type-table' is used in place of TABLE.

The parameter symbols are following:
  use-ssl: whether SSL is enabled or not.
  allow-insecure-server-cert: non-nil if an insecure server certificate is
    allowed on SSL.
  cacert-fullpath: the full-path of the certificate authorizing a server
    certificate on SSL.
  use-proxy: non-nil if using a proxy.
  proxy-server: a proxy server or nil.
  proxy-port: a port for connecting the proxy (integer) or nil.
  proxy-user: a username for connecting the proxy or nil.
  proxy-password: a password for connecting the proxy or nil.
  request: an alist specifying a HTTP request."
  (let* ((order (or order twittering-connection-type-order))
         (table (or table twittering-connection-type-table))
         (scheme (assqref 'scheme request))
         (use-ssl (string= "https" scheme))
         (use-proxy (twittering-proxy-use-match (assqref 'host request)))
         (entry (twittering-lookup-connection-type use-ssl order table)))
    `((use-ssl . ,use-ssl)
      (allow-insecure-server-cert
       . ,twittering-allow-insecure-server-cert)
      (cacert-fullpath
       . ,(when use-ssl (twittering-ensure-ca-cert)))
      (use-proxy . ,use-proxy)
      ,@(when use-proxy
          `((proxy-server . ,(twittering-proxy-info scheme 'server))
            (proxy-port . ,(twittering-proxy-info scheme 'port))
            (proxy-user . ,(if use-ssl
                               twittering-https-proxy-user
                             twittering-http-proxy-user))
            (proxy-password . ,(if use-ssl
                                   twittering-https-proxy-password
                                 twittering-http-proxy-password))))
      (request . ,request)
      ,@additional
      ,@entry)))

(defun twittering-get-response-header (buffer)
  "Extract HTTP response header from HTTP response.
BUFFER may be a buffer or the name of an existing buffer which contains the HTTP response."
  (with-current-buffer buffer
    (save-excursion
      (let (end prev)
        (goto-char (point-min))
        (when (search-forward-regexp "\r?\n\r?\n" nil t 1)
          (setq end (match-end 0))
          ;; Remove 302 redirection.
          (while (search-backward-regexp "\\`HTTP/1\.[01] \\(302 \\|200 Connection established\\)" nil t 1)
            (setq prev (buffer-substring (point-min) end))
            (delete-region (point-min) end)
            (setq end nil)
            (when (search-forward-regexp "\r?\n\r?\n" nil t 1)
              (setq end (match-end 0))))
          (if end
              (buffer-substring (point-min) end)
            prev))))))

(defun twittering-make-header-info-alist (header-str)
  "Make HTTP header alist from HEADER-STR.
The alist consists of pairs of field-name and field-value, such as
'((\"Content-Type\" . \"application/xml\; charset=utf-8\")
  (\"Content-Length\" . \"2075\"))."
  (let* ((lines (split-string header-str "\r?\n"))
         (status-line (car lines))
         (header-lines (cdr lines)))
    (when (string-match
           "^\\(HTTP/1\.[01]\\) \\([0-9][0-9][0-9]\\)"
           status-line)
      (append `((status-line . ,status-line)
                (http-version . ,(match-string 1 status-line))
                (status-code . ,(match-string 2 status-line))
                (reason-phrase . ,(match-string 3 status-line)))
              (remove nil
                      (mapcar
                       (lambda (line)
                         (when (string-match "^\\([^: ]*\\): *\\(.*\\)$" line)
                           (cons (match-string 1 line) (match-string 2 line))))
                       header-lines))))))

(defun twittering-remove-response-header ()
  (goto-char (point-min))
  (let ((end (point-min))
        (inhibit-read-only t))
    (while (search-forward-regexp "^\\(HTTP/1\.[01]\\) \\([0-9][0-9][0-9]\\)" nil t)
      (setq end (search-forward-regexp "\r?\n\r?\n" nil t 1)))
    (delete-region (point-min) end)))

(defun twittering-send-http-request-internal (request additional-info sentinel)
  "Open a connection and return a subprocess object for the connection.
REQUEST must be an alist that has the same keys as that generated by
`twittering-make-http-request'.
SENTINEL is called as a function when the process changes state.
It gets three arguments: the process, a string describing the change, and
the connection-info, which is generated by `twittering-make-connection-info'
and also includes an alist ADDITIONAL-INFO.

How to perform the request is selected from TABLE according to the priority
order ORDER. ORDER and TABLE are directly sent to
`twittering-make-connection-info'.
If ORDER is nil, `twittering-connection-type-order' is used in place of ORDER.
If TABLE is nil, `twittering-connection-type-table' is used in place of TABLE.
"
  (let* ((order twittering-connection-type-order)
         (table twittering-connection-type-table)
         (connection-info
          (twittering-make-connection-info request additional-info order table))
         (func (assqref 'send-http-request connection-info))
         (stream? (assqref 'stream connection-info))
         (proc-name (if stream?
                        (format "*twmode-stream-%s*" (assqref 'timeline-spec-string connection-info))
                      "*twmode-generic*"))
         (temp-buffer (unless stream? (generate-new-buffer "*twmode-http-buffer*"))))
    (when (and func (functionp func))
      (funcall func proc-name temp-buffer connection-info
               (when (and sentinel (functionp sentinel))
                 (lexical-let ((sentinel sentinel)
                               (connection-info connection-info))
                   (lambda (proc status)
                     (apply sentinel proc status connection-info nil))))))))

(defun twittering-send-http-request (request additional-info func &optional clean-up-func)
  "Send a HTTP request and return a subprocess object for the connection.
REQUEST must be an alist that has the same keys as that generated by
`twittering-make-http-request'.

FUNC is called when a HTTP response has been received without errors.
It is called with the current buffer containing the HTTP response (without
HTTP headers). FUNC is called with four arguments: the process, a symbol
describing the status of the process, a connection-info generated by
`twittering-make-connection-info', and a header-info generated by
`twittering-get-response-header'.
The connection-info also includes an alist ADDITIONAL-INFO.
If FUNC returns non-nil and `twittering-buffer-related-p' is non-nil, the
returned value is displayed as a message.

CLEAN-UP-FUNC is called whenever the sentinel of the subprocess for the
connection is called (as `set-process-sentinel').
It is called with three arguments: the process, a symbol describing the status
of the proess, and a connection-info generated by
`twittering-make-connection-info'.
They are the same as arguments for FUNC.
When a HTTP response has been received, FUNC is called in advance of
CLEAN-UP-FUNC. CLEAN-UP-FUNC can overwrite the message displayed by FUNC.

If the subprocess has exited, the buffer bound to it is automatically killed
after calling CLEAN-UP-FUNC.

The method to perform the request is determined from
`twittering-connection-type-table' according to the priority order
`twittering-connection-type-order'."
  (lexical-let ((func func)
                (clean-up-func clean-up-func)
                (twittering-service-method twittering-service-method)
                (sync? (assqref 'sync additional-info))
                (info "(twmode) Retrieving..."))
    (when sync?
      (message info))
    (twittering-send-http-request-internal
     request additional-info
     (lambda (proc status-code-or-str connection-info)
       (let ((status (cond
                      (sync? status-code-or-str)
                      ((string= status-code-or-str "urllib-finished") 'exit)
                      ((processp proc) (process-status proc))
                      (t nil)))
             (pre-process-func (assqref 'pre-process-buffer connection-info))
             (buffer (if sync? (current-buffer) (process-buffer proc)))
             (mes 'unset)
             (error? t))
         (unwind-protect
             (progn
               (unless sync?
                 (let ((exit-status (cond
                                     ((string= status-code-or-str "urllib-finished") 0)
                                     ((processp proc) (process-exit-status proc))
                                     (t 1)))
                       (command (process-command proc)))
                   (setq mes
                         (cond
                          ((null status)
                           (format "Failure: process %s does not exist." proc))
                          ((or (memq status '(run stop open listen connect))
                               (not (memq status '(exit signal closed failed))))
                           ;; If the process is running, FUNC is not called.
                           nil)
                          ((and command (not (= 0 exit-status)))
                           ;; If the process abnormally exited,
                           (format "Failure: %s exited abnormally (exit-status=%s)."
                                   (car command) exit-status))
                          ((not (buffer-live-p buffer))
                           (format "Failure: the buffer for %s is already killed."
                                   proc))
                          (t mes)))))   ; Leave it unchanged

               (when (or sync? (eq mes 'unset))
                 (setq error? nil)
                 (when (functionp pre-process-func)
                   ;; Pre-process buffer.
                   (funcall pre-process-func proc buffer connection-info))
                 (let* ((header (twittering-get-response-header buffer))
                        (header-info (and header (twittering-update-server-info header))))
                   (with-current-buffer buffer
                     (twittering-remove-response-header)
                     (setq mes (apply func proc status connection-info header-info nil))))))
           ;; unwind-forms
           (when (not error?)
             (when (and sync? (not (stringp mes)))
               (setq mes (concat info "done")))
             ;; CLEAN-UP-FUNC can overwrite a message from the return value
             ;; of FUNC.
             (when (stringp mes)
               (message "%s" mes)))
           (when (functionp clean-up-func)
             (funcall clean-up-func proc status connection-info))
           (when (and (or sync?
                          (memq status '(exit signal closed failed)))
                      (buffer-live-p buffer)
                      ;; (not twittering-debug-mode)
                      )
             (kill-buffer buffer))
           (when error?
             (unless (assqref 'stream connection-info)
                (error "%s" mes)))))))))

;;;;
;;;; Basic HTTP functions with tls and Emacs builtins.
;;;;

(eval-when-compile (require 'tls nil t))
(defun twittering-start-http-session-native-tls-p ()
  (when (and (not twittering-proxy-use)
             (require 'tls nil t))
    (unless twittering-tls-program
      (let ((programs
             (remove nil
                     (mapcar (lambda (cmd)
                               (when (string-match "\\`\\([^ ]+\\) " cmd)
                                 (when (executable-find (match-string 1 cmd))
                                   cmd)))
                             tls-program))))
        (setq twittering-tls-program
              (if twittering-allow-insecure-server-cert
                  (mapcar
                   (lambda (str)
                     (cond
                      ((string-match "^\\([^ ]*/\\)?openssl s_client " str)
                       (concat (match-string 0 str) "-verify 0 "
                               (substring str (match-end 0))))
                      ((string-match "^\\([^ ]*/\\)?gnutls-cli " str)
                       (concat (match-string 0 str) "--insecure "
                               (substring str (match-end 0))))
                      (t
                       str)))
                   programs)
                programs))))
    (not (null twittering-tls-program))))

;; TODO: proxy
(defun twittering-send-http-request-native (name buffer connection-info sentinel)
  (let* ((request (assqref 'request connection-info))
         (method (assqref 'method request))
         (scheme (assqref 'scheme request))
         (host (assqref 'host request))
         (port (assqref 'port request))
         (path (assqref 'path request))
         (query-string (assqref 'query-string request))
         (header-list (assqref 'header-list request))
         (post-body (assqref 'post-body request))
         (use-proxy (assqref 'use-proxy connection-info))
         (proxy-server (assqref 'proxy-server connection-info))
         (proxy-port (assqref 'proxy-port connection-info))
         (proxy-user (assqref 'proxy-user connection-info))
         (proxy-password (assqref 'proxy-password connection-info))
         (use-ssl (assqref 'use-ssl connection-info))
         (allow-insecure-server-cert
          (assqref 'allow-insecure-server-cert connection-info))
         (cacert-fullpath (assqref 'cacert-fullpath connection-info))
         (cacert-dir (when cacert-fullpath
                       (file-name-directory cacert-fullpath)))
         (cacert-filename (when cacert-fullpath
                            (file-name-nondirectory cacert-fullpath)))
         (proxy-info
          (when (twittering-proxy-use-match host)
            (twittering-proxy-info scheme)))
         (connect-host (if proxy-info
                           (assqref 'server proxy-info)
                         host))
         (connect-port (if proxy-info
                           (assqref 'port proxy-info)
                         port))
         (request-str
          (format "%s %s%s HTTP/1.1\r\n%s\r\n\r\n%s\r\n"
                  method path
                  (if query-string
                      (concat "?" query-string)
                    "")
                  (mapconcat (lambda (pair)
                               (format "%s: %s" (car pair) (cdr pair)))
                             header-list "\r\n")
                  (or post-body "")))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (tls-program twittering-tls-program)
         (proc
          (funcall (if use-ssl
                       'open-tls-stream
                     'open-network-stream)
                   "network-connection-process"
                   nil connect-host connect-port)))
    (when proc
      (set-process-buffer proc buffer)
      (set-process-sentinel proc sentinel)
      (when (assqref 'stream connection-info)
        (set-process-filter proc 'twittering-stream-filter))
      (process-send-string proc request-str)
      (when (assqref 'sync connection-info)
        (while (let ((status (process-status proc)))
                 (and (memq status '(run stop open listen connect))
                      (not (memq status '(exit signal closed failed)))))
          (sit-for 0.1)))
      proc)))

(defun twittering-pre-process-buffer-native (proc buffer connection-info)
  (let ((use-ssl (assqref 'use-ssl connection-info))
        (args (process-command proc)))
    (cond
     ((and use-ssl args
           (car
            (remove nil
                    (mapcar (lambda (cmd)
                              (string-match "^\\(.*/\\)?gnutls-cli\\b" cmd))
                            args))))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (when (search-backward-regexp
                 "- Peer has closed the GNUTLS connection\r?\n\\'")
            (let ((beg (match-beginning 0))
                  (end (match-end 0)))
              (delete-region beg end))))))
     ((and use-ssl args
           (car
            (remove nil
                    (mapcar
                     (lambda (cmd)
                       (string-match "^\\(.*/\\)?openssl s_client\\b" cmd))
                     args))))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (when (search-backward-regexp "closed\r?\n\\'")
            (let ((beg (match-beginning 0))
                  (end (match-end 0)))
              (delete-region beg end))))))
     (t
      nil))))

;;;;
;;;; Basic HTTP functions with curl
;;;;

(defun twittering-find-curl-program ()
  "Returns an appropriate `curl' program pathname or nil if not found."
  (or (executable-find "curl")
      (let ((windows-p (memq system-type '(windows-nt cygwin)))
            (curl.exe
             (expand-file-name
              "curl.exe"
              (expand-file-name
               "win-curl"
               (file-name-directory (symbol-file 'twit))))))
        (and windows-p
             (file-exists-p curl.exe) curl.exe))))

(defun twittering-start-http-session-curl-p ()
  "Return t if curl was installed, otherwise nil."
  (unless twittering-curl-program
    (setq twittering-curl-program (twittering-find-curl-program)))
  (not (null twittering-curl-program)))

(defun twittering-start-http-session-curl-https-p ()
  "Return t if curl was installed and the curl support HTTPS, otherwise nil."
  (when (twittering-start-http-session-curl-p)
    (unless twittering-curl-program-https-capability
      (with-temp-buffer
        (call-process twittering-curl-program
                      nil (current-buffer) nil
                      "--version")
        (goto-char (point-min))
        (setq twittering-curl-program-https-capability
              (if (search-forward-regexp "^Protocols: .*https" nil t)
                  'capable
                'incapable))))
    (eq twittering-curl-program-https-capability 'capable)))

(defun twittering-send-http-request-curl (name buffer connection-info sentinel)
  (let* ((request        (assqref 'request        connection-info))
         (method         (assqref 'method         request))
         (uri            (assqref 'uri            request))
         (header-list    (assqref 'header-list    request))
         (post-body      (assqref 'post-body      request))
         (use-proxy      (assqref 'use-proxy      connection-info))
         (proxy-server   (assqref 'proxy-server   connection-info))
         (proxy-port     (assqref 'proxy-port     connection-info))
         (proxy-user     (assqref 'proxy-user     connection-info))
         (proxy-password (assqref 'proxy-password connection-info))
         (use-ssl        (assqref 'use-ssl        connection-info))
         (allow-insecure-server-cert
          (assqref 'allow-insecure-server-cert connection-info))
         (cacert-fullpath (assqref 'cacert-fullpath connection-info))
         (cacert-dir (when cacert-fullpath
                       (file-name-directory cacert-fullpath)))
         (cacert-filename (when cacert-fullpath
                            (file-name-nondirectory cacert-fullpath)))
         (header-list
          `(,@header-list
            ;; Make `curl' remove the HTTP header field "Expect" for
            ;; avoiding '417 Expectation Failed' HTTP response error.
            ;; The header field is automatically added for a HTTP request
            ;; exceeding 1024 byte. See
            ;; http://d.hatena.ne.jp/imait/20091228/1262004813 and
            ;; http://www.escafrace.co.jp/blog/09/10/16/1008
            ("Expect" . "")))
         (curl-args
          `("--include"
            "--location"
            "--request" ,method
            ,@(unless twittering-debug-curl '("--silent"))
            ,@(apply 'append
                     (mapcar
                      (lambda (pair)
                        ;; Do not overwrite internal headers `curl' would use.
                        ;; Thanks to William Xu.
                        ;; "cURL - How To Use"
                        ;; http://curl.haxx.se/docs/manpage.html
                        (unless (string= (car pair) "Host")
                          `("-H" ,(format "%s: %s" (car pair) (cdr pair)))))
                      header-list))
            ,@(when use-ssl `("--cacert" ,cacert-filename))
            ,@(when (and use-ssl allow-insecure-server-cert)
                `("--insecure"))
            ,@(when (and use-proxy proxy-server proxy-port)
                (append
                 `("-x" ,(format "%s:%s" proxy-server proxy-port))
                 (when (and proxy-user proxy-password)
                   `("-U" ,(format "%s:%s" proxy-user proxy-password)))))

             ;; ad-hoc
            ,@(when (and twittering-curl-socks-proxy
                         (string-match twittering-uri-regexp-to-proxy uri))
                twittering-curl-socks-proxy)

            ,@(when (string= "POST" method)
                (let ((opt
                       (if (twittering-is-uploading-file-p post-body)
                           "-F"
                         "-d")))
                  (or (mapcan (lambda (pair)
                                (let ((n (car pair))
                                      (v (cdr pair)))
                                  (when (string= opt "-d")
                                    (setq n (twittering-percent-encode n)
                                          v (twittering-percent-encode v)))
                                  (list opt (format "%s=%s" n v))))
                              post-body)

		      ;; Even if no data to post.. or it will fail for favorite,
		      ;; retweet, etc.  This is to ensure curl will use POST?
		      `(,opt ,(or post-body "")))))
            ,uri))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (default-directory
           ;; If `use-ssl' is non-nil, the `curl' process
           ;; is executed at the same directory as the temporary cert file.
           ;; Without changing directory, `curl' misses the cert file if
           ;; you use Emacs on Cygwin because the path on Emacs differs
           ;; from Windows.
           ;; With changing directory, `curl' on Windows can find the cert
           ;; file if you use Emacs on Cygwin.
           (if use-ssl
               cacert-dir
             default-directory))
         proc)
    (debug-printf "curl args: %S" curl-args)

    (if (or (assqref 'sync connection-info) twittering-debug-curl)
        (with-current-buffer buffer
          (when twittering-debug-curl
            (switch-to-buffer buffer))
          (let ((status
                 (apply 'call-process twittering-curl-program nil t nil curl-args)))
            (when sentinel
              (funcall sentinel nil status))))
      (setq proc (apply 'start-process name buffer
                        twittering-curl-program curl-args))
      (when (and proc (functionp sentinel))
        (when (assqref 'stream connection-info)
          (set-process-filter proc 'twittering-stream-filter))
        (set-process-sentinel proc sentinel))
      proc)))

(defun twittering-pre-process-buffer-curl (proc buffer connection-info)
  (let ((use-ssl (assqref 'use-ssl connection-info))
        (use-proxy (assqref 'use-proxy connection-info)))
    (when (and use-ssl use-proxy)
      ;; When using SSL via a proxy with CONNECT method,
      ;; omit a successful HTTP response and headers if they seem to be
      ;; sent from the proxy.
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (let ((first-regexp
                 ;; successful HTTP response
                 "\\`HTTP/1\.[01] 2[0-9][0-9] .*?\r?\n")
                (next-regexp
                 ;; following HTTP response
                 "^\\(\r?\n\\)HTTP/1\.[01] [0-9][0-9][0-9] .*?\r?\n"))
            (when (and (search-forward-regexp first-regexp nil t)
                       (search-forward-regexp next-regexp nil t))
              (let ((beg (point-min))
                    (end (match-end 1)))
                (delete-region beg end)))))))))

;;;;
;;;; Basic HTTP functions with wget
;;;;

(defun twittering-find-wget-program ()
  "Returns an appropriate `wget' program pathname or nil if not found."
  (executable-find "wget"))

(defun twittering-start-http-session-wget-p ()
  "Return t if `wget' was installed, otherwise nil."
  (unless twittering-wget-program
    (setq twittering-wget-program (twittering-find-wget-program)))
  (not (null twittering-wget-program)))

(defun twittering-send-http-request-wget (name buffer connection-info sentinel)
  (let* ((request (assqref 'request connection-info))
         (method (assqref 'method request))
         (scheme (assqref 'scheme request))
         (uri (assqref 'uri request))
         (header-list (assqref 'header-list request))
         (post-body (assqref 'post-body request))
         (use-proxy (assqref 'use-proxy connection-info))
         (proxy-server (assqref 'proxy-server connection-info))
         (proxy-port (assqref 'proxy-port connection-info))
         (proxy-user (assqref 'proxy-user connection-info))
         (proxy-password (assqref 'proxy-password connection-info))
         (use-ssl (assqref 'use-ssl connection-info))
         (allow-insecure-server-cert
          (assqref 'allow-insecure-server-cert connection-info))
         (cacert-fullpath (assqref 'cacert-fullpath connection-info))
         (cacert-dir (when cacert-fullpath
                       (file-name-directory cacert-fullpath)))
         (cacert-filename (when cacert-fullpath
                            (file-name-nondirectory cacert-fullpath)))
         (args
          `("--save-headers"
            "--quiet"
            "--output-document=-"
            ,@(remove nil
                      (mapcar
                       (lambda (pair)
                         (unless (string= (car pair) "Host")
                           (format "--header=%s: %s" (car pair) (cdr pair))))
                       header-list))
            ,@(when use-ssl
                `(,(format "--ca-certificate=%s" cacert-filename)))
            ,@(when (and use-ssl allow-insecure-server-cert)
                `("--no-check-certificate"))
            ,@(cond
               ((not use-proxy)
                '("--no-proxy"))
               ((and use-proxy proxy-server proxy-port
                     proxy-user proxy-password)
                `(,(format "--proxy-user=%s" proxy-user)
                  ,(format "--proxy-password=%s" proxy-password)))
               (t
                nil))
            ,@(when (string= "POST" method)
                `(,(concat "--post-data=" (or post-body ""))))
            ,uri))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (default-directory
           ;; If `use-ssl' is non-nil, the `wget' process
           ;; is executed at the same directory as the temporary cert file.
           ;; Without changing directory, `wget' misses the cert file if
           ;; you use Emacs on Cygwin because the path on Emacs differs
           ;; from Windows.
           ;; With changing directory, `wget' on Windows can find the cert
           ;; file if you use Emacs on Cygwin.
           (if use-ssl
               cacert-dir
             default-directory))
         (process-environment
          `(,@(when (and use-proxy proxy-server proxy-port)
                `(,(format "%s_proxy=%s://%s:%s/" scheme
                           scheme proxy-server proxy-port)))
            ,@process-environment))
         proc)
    (if (assqref 'sync connection-info)
        (with-current-buffer buffer
          (let ((status
                 (apply 'call-process twittering-wget-program nil t nil args)))
            (when sentinel
              (funcall sentinel nil status))))
      (setq proc (apply 'start-process name buffer
                        twittering-wget-program args))
      (when (and proc (functionp sentinel))
        (when (assqref 'stream connection-info)
          (set-process-filter proc 'twittering-stream-filter))
        (set-process-sentinel proc sentinel))
      proc)))

(defun twittering-pre-process-buffer-wget (proc buffer connection-info)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "\\`[^\n]*?\r\r\n" (point-max) t)
        ;; When `wget.exe' writes HTTP response in text mode,
        ;; CRLF may be converted into CRCRLF.
        (goto-char (point-min))
        (while (search-forward "\r\n" nil t)
          (replace-match "\n" nil t)))
      (goto-char (point-max))
      (when (search-backward-regexp "\nProcess [^\n]* finished\n\\'"
                                    (point-min) t)
        (replace-match "" nil t))
      )))

;;;;
;;;; Basic HTTP functions with url library
;;;;

(defun twittering-start-http-session-urllib-p ()
  "Return t if url library is available, otherwise nil."
  (require 'url nil t))

(defun twittering-start-http-session-urllib-https-p ()
  "Return t if url library can be used for HTTPS, otherwise nil."
  (and (not twittering-proxy-use)
       (require 'url nil t)
       (cond
        ((<= 22 emacs-major-version)
         ;; On Emacs22 and later, `url' requires `tls'.
         (twittering-start-http-session-native-tls-p))
        ((require 'ssl nil t)
         ;; On Emacs21, `url' requires `ssl'.
         t)
        ((or (and (fboundp 'open-ssl-stream)
                  ;; Since `url-gw' (required by `url') defines autoload of
                  ;; `open-ssl-stream' from "ssl",
                  ;; (fboundp 'open-ssl-stream) will be non-nil even if
                  ;; "ssl" cannot be loaded and `open-ssl-stream' is
                  ;; unavailable.
                  ;; Here, the availability is confirmed by `documentation'.
                  (documentation 'open-ssl-stream))
             ;; On Emacs21, `url' requires `ssl' in order to use
             ;; `open-ssl-stream', which is included in `ssl.el'.
             ;; Even if `ssl' cannot be loaded, `open-tls-stream' can be
             ;; used as an alternative of the function.
             (and (twittering-start-http-session-native-tls-p)
                  (defalias 'open-ssl-stream 'open-tls-stream)))
         (provide 'ssl)
         t)
        (t
         nil))))

(defun twittering-send-http-request-urllib (name buffer connection-info sentinel)
  (let* ((request (assqref 'request connection-info))
         (method (assqref 'method request))
         (scheme (assqref 'scheme request))
         (uri (assqref 'uri request))
         (header-list (assqref 'header-list request))
         (post-body (assqref 'post-body request))
         (use-proxy (assqref 'use-proxy connection-info))
         (proxy-server (assqref 'proxy-server connection-info))
         (proxy-port (assqref 'proxy-port connection-info))
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (url-proxy-services
          (when use-proxy
            `((,scheme . ,(format "%s:%s" proxy-server proxy-port)))))
         (url-request-method method)
         (url-request-extra-headers
          ;; Remove some headers that should be configured by url library.
          ;; They may break redirections by url library because
          ;; `url-request-extra-headers' overwrites the new headers
          ;; that are adapted to redirected connection.
          (apply 'append
                 (mapcar (lambda (pair)
                           (if (member (car pair)
                                       '("Host" "Content-Length"))
                               nil
                             `(,pair)))
                         header-list)))
         (url-request-data post-body)
         (url-show-status twittering-url-show-status)
         (url-http-attempt-keepalives (assqref 'stream connection-info))
         (tls-program twittering-tls-program)
         (coding-system-for-read 'binary)
         (coding-system-for-write 'binary))
    (lexical-let ((sentinel sentinel)
                  (buffer buffer))
      (let ((callback
             (lambda (&rest args)
               (let ((proc url-http-process)
                     (url-buffer (current-buffer))
                     (status-str "urllib-finished")
                     ;; TODO: necessary?
                     ;; (if (and (< emacs-major-version 22)
                     ;;          (boundp 'url-http-end-of-headers)
                     ;;          url-http-end-of-headers)
                     ;;     "urllib-finished"
                     ;;   "finished")
                     )
                 ;; Callback may be called multiple times.
                 ;; (as filter and sentinel?)
                 (unless (local-variable-if-set-p 'twittering-retrieved)
                   (set (make-local-variable 'twittering-retrieved)
                        'not-completed)
                   (with-current-buffer buffer
                     (set-buffer-multibyte nil)
                     (insert-buffer-substring url-buffer))
                   (set-process-buffer proc buffer)
                   (unwind-protect
                       (apply sentinel proc status-str nil)
                     (set-process-buffer proc url-buffer)
                     (if (eq twittering-retrieved 'exited)
                         (url-mark-buffer-as-dead url-buffer)
                       (setq twittering-retrieved 'completed))))
                 (when (memq (process-status proc)
                             '(nil closed exit failed signal))
                   ;; Mark `url-buffer' as dead when the process exited
                   ;; and `sentinel' is completed.
                   ;; If this `lambda' is evaluated via a filter, the
                   ;; process may exit before it is finished to evaluate
                   ;; `(apply sentinel ...)'. In the case, `buffer' should
                   ;; not be killed. It should be killed after the
                   ;; evaluation of `sentinel'.
                   (if (eq twittering-retrieved 'completed)
                       (url-mark-buffer-as-dead url-buffer)
                     (setq twittering-retrieved 'exited))))))
            result-buffer)
        (if (assqref 'sync connection-info)
            (with-current-buffer (url-retrieve-synchronously uri)
              (when sentinel
                (funcall sentinel nil 'exit)))
          (url-retrieve uri callback)
          (when (buffer-live-p result-buffer)
            (when (assqref 'stream connection-info)
              (set-process-filter (get-buffer-process result-buffer)
                                  'twittering-stream-filter))
            (with-current-buffer result-buffer
              (set (make-local-variable 'url-show-status)
                   twittering-url-show-status)
              ;; Make `url-http-attempt-keepalives' buffer-local
              ;; in order to send the current value of the variable
              ;; to the sentinel invoked for HTTP redirection,
              (make-local-variable 'url-http-attempt-keepalives))
            (get-buffer-process result-buffer)))))))

(defun twittering-pre-process-buffer-urllib (proc buffer connection-info)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (cond
       ((search-backward-regexp
         "- Peer has closed the GNUTLS connection\r?\n\\'"
         nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (delete-region beg end)))
       ((search-backward-regexp "closed\r?\n\\'" nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (delete-region beg end)))
       (t nil)))))

;;;;
;;;; HTTP functions for twitter-like serivce
;;;;

(defun twittering-http-application-headers (&optional method headers)
  "Return an assoc list of HTTP headers for twittering-mode."
  (unless method
    (setq method "GET"))

  (let ((headers headers))
    (push (cons "User-Agent" (twittering-user-agent)) headers)
    (when (string= "GET" method)
      (push (cons "Accept"
                  (concat
                   "text/xml"
                   ",application/xml"
                   ",application/xhtml+xml"
                   ",application/html;q=0.9"
                   ",text/plain;q=0.8"
                   ",image/png,*/*;q=0.5"))
            headers)
      (push (cons "Accept-Charset" "utf-8;q=0.7,*;q=0.7")
            headers))
    (when (string= "POST" method)
      (push (cons "Content-Type"
                  (if (eq (twittering-extract-service) 'douban)
                      "application/atom+xml"
                    "text/plain"))
            headers))
    ;; This makes update-profile-image fail.
    ;; (when (string= "POST" method)
    ;;   (push (cons "Content-Length" "0") headers)
    ;;   (push (cons "Content-Type" "text/plain") headers))
    (when twittering-proxy-use
      (let* ((scheme (if (twittering-get-accounts 'ssl) "https" "http"))
             (keep-alive (twittering-proxy-info scheme 'keep-alive))
             (user (twittering-proxy-info scheme 'user))
             (password (twittering-proxy-info scheme 'password)))
        (when (twittering-proxy-info scheme 'keep-alive)
          (push (cons "Proxy-Connection" "Keep-Alive")
                headers))
        (when (and user password)
          (push (cons
                 "Proxy-Authorization"
                 (concat "Basic "
                         (base64-encode-string (concat user ":" password))))
                headers))))
    headers
    ))

(defun twittering-add-application-header-to-http-request (request)
  (let* ((method (assqref 'method request))
         (auth-str
          (cond
           ((eq (twittering-get-accounts 'auth) 'basic)
            (concat "Basic "
                    (base64-encode-string
                     (concat (twittering-get-accounts 'username)
                             ":" (twittering-get-accounts 'password)))))
           ((memq (twittering-get-accounts 'auth) '(oauth xauth))
            (let ((access-token
                   (assocref "oauth_token" (twittering-lookup-oauth-access-token-alist)))
                  (access-token-secret
                   (assocref "oauth_token_secret" (twittering-lookup-oauth-access-token-alist))))
              (twittering-oauth-auth-str-access
               method
               (assqref 'uri-without-query request)
               (assqref 'encoded-query-alist request)
               (twittering-lookup-service-method-table 'oauth-consumer-key)
               (twittering-lookup-service-method-table 'oauth-consumer-secret)
               access-token
               access-token-secret)))
           (t
            nil)))
         (application-headers
          `(,@(twittering-http-application-headers method)
            ("Authorization" . ,auth-str))))
    (mapcar (lambda (entry)
              (if (eq (car entry) 'header-list)
                  `(header-list
                    . ,(append (cdr entry) application-headers))
                entry))
            request)))

(defun twittering-get-error-message (header-info buffer)
  "Return an error message generated from HEADER-INFO and BUFFER.
HEADER-INFO must be an alist generated by `twittering-get-response-header'.
BUFFER must be a HTTP response body, which includes error messages from
the server when the HTTP status code equals to 400 or 403."
  (let ((status-line (assqref 'status-line header-info))
        (status-code (assqref 'status-code header-info)))
    ;; http://dev.twitter.com/pages/responses_errors
    (when (buffer-live-p buffer)
      (let ((error-msg (ignore-errors (assqref 'error (twittering-construct-statuses)))))
        (if error-msg
            (format "%s (%s)" status-line error-msg)
          status-line)))))

(defun twittering-http-get (host method &optional parameters additional-info sentinel clean-up-sentinel)
  (let* ((service (twittering-extract-service))
         (sentinel (or sentinel 'twittering-http-get-default-sentinel))
         (path (concat "/" method (if (eq service 'douban) "" ".json")))
         (request
          (if (not (equal (twittering-get-accounts-internal 'oauth) 1.0))
              (twittering-make-http-request
               "GET" nil host nil path `(,@parameters ,(assoc "access_token" (twittering-lookup-oauth-access-token-alist)))
               "" (twittering-get-accounts 'ssl))
            (twittering-add-application-header-to-http-request
             (twittering-make-http-request
              "GET" nil host nil path parameters "" (twittering-get-accounts 'ssl))))))
    (twittering-send-http-request
     request additional-info sentinel clean-up-sentinel)))

(defvar twittering-oauth2-wait-user-for-reverifying nil)

(defun twittering-http-get-default-sentinel (proc status connection-info header-info)
  (let ((status-line (assqref 'status-line header-info))
        (status-code (assqref 'status-code header-info))
        (spec (assqref 'timeline-spec connection-info))
        (spec-string (assqref 'timeline-spec-string connection-info)))
    (cond
     ((string= status-code "200")
      (debug-printf "connection-info=%s" connection-info)
      (let ((statuses (twittering-construct-statuses)))
        (ignore-errors
          (when (assqref 'id (car statuses))
            ;; Are we are fetching replies?
            (when (eq (assqref 'command connection-info) 'show)
              (setq statuses
                    (mapcar (lambda (st) `(,@st (twittering-reply? . t)))
                            statuses)))
            (twittering-update-timeline statuses spec)))
        (when (and twittering-notify-successful-http-get
                   (not (assqref 'noninteractive connection-info)))
          (format "Fetching %s.  Success." spec-string))))
     (t
      (let ((twittering-service-method (twittering-extract-service spec-string))
            (error-msg (twittering-get-error-message header-info
                                                     (current-buffer))))
        ;; (unwind-protect
            (if (and ;; (not twittering-oauth2-wait-user-for-reverifying)
                     ;; (setq twittering-oauth2-wait-user-for-reverifying t)
                     (eq twittering-service-method 'sina)
                     (or (and (equal status-code "400")
                              (string-match "expired_token" error-msg))
                         ;; HTTP/1.1 403 Forbidden (invalid_access_token)
                         (and (equal status-code "403")
                              (string-match "invalid_access_token" error-msg)))
                     ;;(y-or-n-p "Access token expired, weibo.com asks you to verify again, OK? ")
                     )
                (message "Sina weibo access token expired, M-x twittering-oauth2-force-verify-credentials to reverify")
                ;; (twittering-oauth2-force-verify-credentials)
              (format "Response from `%s': %s" spec-string error-msg)))))))
          ;; (setq twittering-oauth2-wait-user-for-reverifying nil)))))))
      
(defun twittering-http-post (host method &optional parameters additional-info sentinel clean-up-sentinel)
  "Send HTTP POST request to api.twitter.com (or search.twitter.com)
HOST is hostname of remote side, api.twitter.com (or search.twitter.com).
METHOD must be one of Twitter API method classes
 (statuses, users or direct_messages).
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6
"
  (let* ((service (twittering-extract-service))
         (sentinel (or sentinel 'twittering-http-post-default-sentinel))
         (path (concat "/" method (if (eq service 'douban) "" ".json")))
         (post-body (if (eq service 'douban)
                        (format
                         "<?xml version='1.0' encoding='UTF-8'?>
<entry xmlns:ns0=\"http://www.w3.org/2005/Atom\" xmlns:db=\"http://www.douban.com/xmlns/\">
<content>%s</content></entry>"
                         (encode-coding-string
                          (assocref "status" parameters) 'utf-8))
                      ""))
         ;; TODO
         ;; "POST" url (and (not (twittering-is-uploading-file-p parameters))
         ;;                  parameters))))
         (request
          (if (not (equal (twittering-get-accounts-internal 'oauth) 1.0))
              (twittering-make-http-request "POST" nil host nil path
                                            (unless (eq (twittering-extract-service) 'douban) 
                                              `(,@parameters ,(assoc "access_token" (twittering-lookup-oauth-access-token-alist)))
                                              )
                                            post-body
                                            (twittering-get-accounts 'ssl))
            (twittering-add-application-header-to-http-request
             (twittering-make-http-request "POST" nil host nil path
                                           (unless (eq (twittering-extract-service) 'douban) 
                                             parameters)
                                           post-body
                                           (twittering-get-accounts 'ssl))))))

    (twittering-send-http-request request additional-info
                                  sentinel clean-up-sentinel)))

(defun twittering-http-post-default-sentinel (proc status connection-info header-info)
  (let ((status-line (assqref 'status-line header-info))
        (status-code (assqref 'status-code header-info)))
    (case-string
     status-code
     (("200")
      "Success: Post.")
     (t
      (format "Response from `%s': %s"
              (assqref 'timeline-spec-string connection-info)
              (twittering-get-error-message header-info (current-buffer)))))))

(defun twittering-update-timeline (statuses spec)
  (let* ((twittering-service-method (twittering-extract-service spec))
         (spec-string (twittering-timeline-spec-to-string spec)))
    (when statuses
      (let ((new-statuses (twittering-add-statuses-to-timeline-data statuses spec))
            (buffer (twittering-get-buffer-from-spec spec)))
        ;; FIXME: We should retrieve un-retrieved statuses until
        ;; statuses is nil. twitter server returns nil as
        ;; xmltree with HTTP status-code is "200" when we
        ;; retrieved all un-retrieved statuses.
        (when (and new-statuses buffer)
          (twittering-render-timeline buffer t new-statuses))))
    (twittering-add-timeline-history spec-string)))

;;; ============================================= Commands
;;;

;;;; Commands for changing modes

(defun twittering-scroll-mode (&optional arg)
  (interactive "P")
  (let ((prev-mode twittering-scroll-mode))
    (setq twittering-scroll-mode
          (if (null arg)
              (not twittering-scroll-mode)
            (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-scroll-mode)
      (twittering-update-mode-line))))

(defun twittering-jojo-mode (&optional arg)
  (interactive "P")
  (let ((prev-mode twittering-jojo-mode))
    (setq twittering-jojo-mode
          (if (null arg)
              (not twittering-jojo-mode)
            (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-jojo-mode)
      (twittering-update-mode-line))))

(defun twittering-jojo-mode-p (spec)
  (let ((buffer (twittering-get-buffer-from-spec spec)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        twittering-jojo-mode))))

(defun twittering-toggle-reverse-mode (&optional arg)
  (interactive "P")
  (let ((prev-mode twittering-reverse-mode))
    (setq twittering-reverse-mode
          (if (null arg)
              (not twittering-reverse-mode)
            (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-reverse-mode)
      (let ((id (twittering-get-id-at)))
        (twittering-update-mode-line)
        (twittering-render-timeline (current-buffer))
        (twittering-restore-point id)))))

(defun twittering-set-current-hashtag (&optional tag)
  (interactive)
  (unless tag
    (setq tag (twittering-completing-read "hashtag (blank to clear): #"
                                          twittering-hashtag-history
                                          nil nil
                                          twittering-current-hashtag
                                          'twittering-hashtag-history))
    (message
     (if (eq 0 (length tag))
         (progn (setq twittering-current-hashtag nil)
                "Current hashtag is not set.")
       (progn
         (setq twittering-current-hashtag tag)
         (format "Current hashtag is #%s" twittering-current-hashtag))))))

;;;; Commands for switching buffers
(defun twittering-switch-to-next-timeline ()
  (interactive)
  (when (twittering-buffer-p)
    (let* ((buffer-list (twittering-get-buffer-list))
           (following-buffers (cdr (memq (current-buffer) buffer-list)))
           (next (if following-buffers
                     (car following-buffers)
                   (car buffer-list))))
      (unless (eq (current-buffer) next)
        (switch-to-buffer next)))))

(defun twittering-switch-to-previous-timeline ()
  (interactive)
  (when (twittering-buffer-p)
    (let* ((buffer-list (reverse (twittering-get-buffer-list)))
           (preceding-buffers (cdr (memq (current-buffer) buffer-list)))
           (previous (if preceding-buffers
                         (car preceding-buffers)
                       (car buffer-list))))
      (unless (eq (current-buffer) previous)
        (switch-to-buffer previous)))))

;;;; Commands for visiting a timeline
(defun twittering-visit-timeline (&optional spec initial)
  (interactive)
  (unless spec
    (setq spec (twittering-read-timeline-spec-with-completion
                "timeline: " initial t)))
  (let ((twittering-service-method (twittering-extract-service spec)))
    (cond
     ((stringp spec)
      (unless (string-match "@" spec)
        (setq spec (format "%s@%S" spec twittering-service-method))))
     ((consp spec)
      (unless (consp (car spec))
        (setq spec `((,twittering-service-method) ,@spec)))))

    (cond
     ((twittering-ensure-connection-method)
      (twittering-initialize-global-variables-if-necessary)
      (switch-to-buffer (twittering-get-managed-buffer spec)))
     (t
      (message "No connection methods are available.")
      nil))))

(defun twittering-friends-timeline ()
  (interactive)
  (twittering-visit-timeline '(friends)))

(defun twittering-home-timeline ()
  (interactive)
  (twittering-visit-timeline '(home)))

(defun twittering-replies-timeline ()
  (interactive)
  (twittering-visit-timeline '(replies)))

(defun twittering-public-timeline ()
  (interactive)
  (twittering-visit-timeline '(public)))

(defun twittering-user-timeline ()
  (interactive)
  (twittering-visit-timeline `(user ,(twittering-get-accounts 'username))))

(defun twittering-direct-messages-timeline ()
  (interactive)
  (twittering-visit-timeline '(direct_messages)))

(defun twittering-sent-direct-messages-timeline ()
  (interactive)
  (twittering-visit-timeline '(direct_messages_sent)))

(defun twittering-other-user-timeline ()
  (interactive)
  (let* ((username (get-text-property (point) 'username))
         (goto-spec (get-text-property (point) 'goto-spec))
         (screen-name-in-text
          (get-text-property (point) 'screen-name-in-text))
         (spec ;; Sequence is important.
          (cond (goto-spec
                 goto-spec)	 ; FIXME: better get name for "retweeted by XXX"
                (screen-name-in-text
                 `((,(twittering-extract-service)) user ,screen-name-in-text))
                (username
                 `((,(twittering-extract-service)) user ,username)))))
    (if spec
        (twittering-visit-timeline spec)
      (message "No user selected"))))

(defun twittering-other-user-timeline-interactive ()
  (interactive)
  (let ((username (or (twittering-read-username-with-completion
                       "user: " nil
                       'twittering-user-history)
                      "")))
    (if (string= "" username)
        (message "No user selected")
      (twittering-visit-timeline `(user ,username)))))

(defun twittering-other-user-list-interactive ()
  (interactive)
  (let* ((username (copy-sequence (get-text-property (point) 'username)))
         (username (progn
                     (set-text-properties 0 (length username) nil username)
                     (or (twittering-read-username-with-completion
                          "Whose list: "
                          username
                          'twittering-user-history)
                         ""))))
    (if (string= "" username)
        (message "No user selected")
      (let* ((list-name (twittering-read-list-name username))
             (spec `(list ,username ,list-name)))
        (if list-name
            (twittering-visit-timeline spec)
          ;; Don't show message here to prevent an overwrite of a
          ;; message which is outputted by `twittering-read-list-name'.
          )))))

(defun twittering-search (&optional word)
  (interactive)
  (let ((word (or word
                  (read-from-minibuffer "search: " nil nil nil
                                        'twittering-search-history nil t)
                  "")))
    (if (string= "" word)
        (message "No query string")
      (let ((spec `(search ,word)))
        (twittering-visit-timeline spec)))))

;;;; Commands for retrieving statuses

(defun twittering-current-timeline-noninteractive ()
  (twittering-current-timeline t))

(defun twittering-current-timeline (&optional noninteractive)
  (interactive)
  (when (twittering-buffer-p)
    (twittering-get-and-render-timeline noninteractive)))

;;;; Commands for posting a status

(defun twittering-update-status-interactive (&optional ask)
  "Non-nil ASK will ask user to select a service from `twittering-enabled-services'. "
  (interactive "P")
  (let ((spec (twittering-current-timeline-spec)))
    (when (or ask (null spec))
      (setq spec
            `((,(intern
                 (completing-read
                  "Post to: "
                  `(,@(mapcar 'symbol-name twittering-enabled-services) "all")))))))

    (funcall twittering-update-status-function
             nil nil nil spec)))

(defun twittering-update-lambda ()
  (interactive)
  (when (and (string= "Japanese" current-language-environment)
             (or (< 21 emacs-major-version)
                 (eq 'utf-8 (terminal-coding-system))))
    (let ((text (mapconcat
                 'char-to-string
                 (mapcar 'twittering-ucs-to-char
                         '(955 12363 12431 12356 12356 12424 955)) "")))
      (twittering-call-api 'update-status `((status . ,text))))))

(defun twittering-update-jojo (usr msg)
  (when (and (not (string= usr (twittering-get-accounts 'username)))
             (string= "Japanese" current-language-environment)
             (or (< 21 emacs-major-version)
                 (eq 'utf-8 (terminal-coding-system))))
    (if (string-match
         (mapconcat
          'char-to-string
          (mapcar 'twittering-ucs-to-char
                  '(27425 12395 92 40 12362 21069 92 124 36020 27096
                          92 41 12399 12300 92 40 91 94 12301 93 43 92
                          41 12301 12392 35328 12358)) "")
         msg)
        (let ((text (concat "@" usr " "
                            (match-string-no-properties 2 msg)
                            (mapconcat
                             'char-to-string
                             (mapcar 'twittering-ucs-to-char
                                     '(12288 12399 12387 33 63)) ""))))
          (twittering-call-api 'update-status `((status . ,text)))))))

(defun twittering-direct-message (&optional ask)
  (interactive "P")
  (let ((username (or (and (not ask)
                           (get-text-property (point) 'username))
                      (twittering-read-username-with-completion
                       "Who would you like to receive the DM? "
                       (get-text-property (point) 'username)
                       'twittering-user-history)))
        (spec (or (get-text-property (point) 'source-spec)
                  `((,(twittering-extract-service)) direct_messages))))
    (if (string= "" username)
        (message "No user selected")
      (funcall twittering-update-status-function
               (concat "d " username " ") nil username spec))))

(defun twittering-reply-to-user (&optional quote)
  "Non-nil QUOTE will quote status using `twittering-generate-organic-retweet'.
However, QUOTO has no effect on sina weibo.  "
  (interactive "P")
  (let* ((username (get-text-property (point) 'username))
         (id (get-text-property (point) 'id))
         (spec (get-text-property (point) 'belongs-spec))
         (status (twittering-find-status id))
         (reply-to-quotation nil)
         (init-str (if quote
                       (twittering-generate-organic-retweet)
                     (concat "@" username " ")))
         (quoted-status (twittering-status-has-quotation? status)))

    (when (memq (twittering-extract-service) '(sina socialcast douban))
      (when quoted-status
        (setq username
              (ido-completing-read
               "Reply to: "
               `(,(assqref 'name (assqref 'user status))
                 ,(assqref 'name (assqref 'user quoted-status)))))
        (when (string= username (assqref 'name (assqref 'user quoted-status)))
          (setq reply-to-quotation t
                id (assqref 'id quoted-status))))

      ;; (setq init-str (concat " // @" username))
      ;; (unless reply-to-quotation
      ;;   ;; (sina) Quote by default.
      ;;   (let ((s (assqref 'text (or status quoted-status))))
      ;;     (setq init-str (concat init-str " " s))))
      (setq init-str ""))

    (if username
        (progn
          (funcall twittering-update-status-function init-str id username spec)
          (when (or quote (eq (twittering-extract-service spec) 'sina))
            (goto-char (line-beginning-position))))
      (message "No user selected"))))

(defun twittering-reply-all (&optional quote)
  "Reply(@) all mentioned users in the tweet.
Non-nil QUOTE will quote status using `twittering-generate-organic-retweet'.
However, QUOTO has no effect on sina weibo.  "
  (interactive "P")
  (unless (memq (twittering-extract-service) '(sina twitter))
    (error "twittering-reply-all not yet implemented for %S" (twittering-extract-service)))
  (let* ((username (get-text-property (point) 'username))
         (id (get-text-property (point) 'id))
         (spec (get-text-property (point) 'belongs-spec))
         (status (twittering-find-status id))
         (reply-to-quotation nil)
         (me (assqref 'screen-name (twittering-lookup-user-info-alist 'basic)))
         (init-str (if quote
                       (twittering-generate-organic-retweet)
                     (concat (mapconcat (lambda (u) (concat "@" u))
                                        (remove me (twittering-get-all-usernames-at-pos))
                                        " ")
                             " ")))
         (quoted-status (twittering-status-has-quotation? status)))

    (when (memq (twittering-extract-service) '(sina socialcast douban))
      (when quoted-status
        (setq username
              (ido-completing-read
               "Reply to: "
               `(,(assqref 'name (assqref 'user status))
                 ,(assqref 'name (assqref 'user quoted-status)))))
        (when (string= username (assqref 'name (assqref 'user quoted-status)))
          (setq reply-to-quotation t
                id (assqref 'id quoted-status))))

      ;; (setq init-str (concat " // @" username))
      ;; (unless reply-to-quotation
      ;;   ;; (sina) Quote by default.
      ;;   (let ((s (assqref 'text (or status quoted-status))))
      ;;     (setq init-str (concat init-str " " s))))

      ;; (setq init-str "")
      )

    (if username
        (progn
          (funcall twittering-update-status-function init-str id username spec)
          ;; (when (or quote (eq (twittering-extract-service spec) 'sina))
          ;;   (goto-char (line-beginning-position)))
          )
      (message "No user selected"))))

(defun twittering-erase-all ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;;; Commands for deleting a status

(defun twittering-delete-status (&optional id)
  (interactive)
  (let* ((id (get-text-property (point) 'id))
         (username (get-text-property (point) 'username))
         (text (copy-sequence (get-text-property (point) 'text)))
         (text (progn
                 (set-text-properties 0 (length text) nil text)
                 text))
         (width (max 40 ;; XXX
                     (- (frame-width)
                        1 ;; margin for wide characters
                        11 ;; == (length (concat "Delete \"" "\"? "))
                        9) ;; == (length "(y or n) ")
                     ))
         (mes (format "Delete \"%s\"? "
                      (if (< width (string-width text))
                          (concat
                           (truncate-string-to-width text (- width 3))
                           "...")
                        text))))
    (cond
     ((not (string= username (twittering-get-accounts 'username)))
      (message "The status is not yours!"))
     ((not id)
      (message "No status selected"))
     ((y-or-n-p mes)
      (twittering-call-api 'destroy-status `((id . ,id)))
      (twittering-delete-status-from-data-table id))
     (t
      (message "Request canceled")))))

;;;; Commands for retweet

(defun twittering-retweet (&optional ask)
  (interactive "P")
  (let* ((orig-service (twittering-extract-service))
         (service orig-service))
    (when ask
      (setq service
            (intern
             (ido-completing-read
              "Post to: "
              `(,@(mapcar 'symbol-name twittering-enabled-services) "all")))))
    (mapc (lambda (s)
            (if (eq s orig-service)
                (let ((twittering-service-method s))
                  (if twittering-use-native-retweet
                      (twittering-native-retweet)
                    (twittering-organic-retweet)))
              (twittering-organic-retweet `((,s)))))
          (if (eq service 'all) twittering-enabled-services `(,service)))))

(defun twittering-organic-retweet (&optional spec)
  (interactive)
  (twittering-ensure-retweeting-allowed)
  (if spec
      (funcall twittering-update-status-function
               (format "[%s] %s" (symbol-name (twittering-extract-service))
                       (twittering-generate-organic-retweet t))
               nil
               nil
               spec)
    (funcall twittering-update-status-function
             (twittering-generate-organic-retweet)
             (get-text-property (point) 'id)
             nil
             (twittering-current-timeline-spec)
             t))
  (goto-char (line-beginning-position)))

(defun twittering-native-retweet ()
  (interactive)
  (twittering-ensure-retweeting-allowed)
  (let ((id (or (get-text-property (point) 'retweeted-id)
                (get-text-property (point) 'id)))
        (text (copy-sequence (get-text-property (point) 'text)))
        (user (get-text-property (point) 'username))
        (width (max 40 ;; XXX
                    (- (frame-width)
                       1 ;; margin for wide characters
                       12 ;; == (length (concat "Retweet \"" "\"? "))
                       9) ;; == (length "(y or n) ")
                    )))
    (set-text-properties 0 (length text) nil text)
    (if id
        (if (not (string= user (twittering-get-accounts 'username)))
            (let ((mes (format "Retweet \"%s\"? "
                               (if (< width (string-width text))
                                   (concat
                                    (truncate-string-to-width text (- width 3))
                                    "...")
                                 text))))
              (if (y-or-n-p mes)
                  (if (eq (twittering-extract-service) service)
                      (twittering-call-api 'retweet `((id . ,id)))
                    ;; cross retweet
                    (funcall twittering-update-status-function
                             text nil nil `((,service))))
                (message "Request canceled")))
          (message "Cannot retweet your own tweet"))
      (message "No status selected"))))

(defun twittering-generate-organic-retweet (&optional cross-retweet)
  (let* ((id (get-text-property (point) 'id))
         (status (twittering-find-status id))
         (username (assqref 'screen-name (assqref 'user status)))
         ;; (get-text-property (point) 'username))
         (text (get-text-property (point) 'text))
         (retweet-time (current-time))
         (service (twittering-extract-service))
         (format-str (or twittering-retweet-format "RT: %t (via @%s)")))
    (when username
      (if (and (not cross-retweet) (eq service 'sina))
          (if (twittering-status-has-quotation? status)
              (format " //@%s:%s" username (assqref 'text status))
            "")
        (let* ((prefix "%")
               (replace-table
                `(("%" . "%")
                  ("s" . ,username)
                  ("t" . ,text)
                  ("#" . ,id)
                  ("C{\\([^}]*\\)}" .
                   (lambda (context)
                     (let ((str (assqref 'following-string context))
                           (match-data (assqref 'match-data context)))
                       (store-match-data match-data)
                       (format-time-string (match-string 1 str) ',retweet-time))))))
               (ret (twittering-format-string format-str prefix replace-table)))
          (when (and (eq service 'sina) (assqref 'original-pic status))
            (setq ret (concat ret " " (assqref 'original-pic status))))
          ret)))))

(defun twittering-ensure-retweeting-allowed ()
  (let* ((id (twittering-get-id-at))
         (status (twittering-find-status (twittering-get-id-at))))
    (when (equal "true" (assqref 'protected (assqref 'user status)))
      (error "Cannot retweet protected tweets."))))

;;;; Commands for browsing information related to a status

(defun twittering-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
        (browse-url uri))))

(defun twittering-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
        (id (twittering-get-id-at (point)))
        (uri (get-text-property (point) 'uri))
        (spec (get-text-property (point) 'source-spec))
        (screen-name-in-text
         (get-text-property (point) 'screen-name-in-text))
        (send-message (lambda (name)
                        (funcall twittering-update-status-function
                                 (if (twittering-timeline-spec-direct-messages-p spec)
                                     (concat "d " name " ")
                                   (concat "@" name " "))
                                 id name spec))))

    (cond (screen-name-in-text
           (funcall send-message screen-name-in-text))
          (uri
           (browse-url uri))
          (username
           (funcall send-message username)))))

(defun twittering-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (if uri
        (browse-url uri))))

;;;;
;;;; Commands corresponding to operations on Twitter
;;;;

(defun twittering-follow (&optional remove)
  "Follow a user or a list.

Non-nil optional REMOVE will do the opposite, unfollow. "
  (interactive "P")
  (let ((user-or-list (twittering-read-username-with-completion
                       "who: " "" 'twittering-user-history))
        prompt-format method args)
    (when (string= "" user-or-list)
      (error "No user or list selected"))
    ;; (set-text-properties 0 (length user-or-list) nil user-or-list)
    (if (string-match "/" user-or-list)
        (let ((spec (twittering-string-to-timeline-spec user-or-list)))
          (setq prompt-format (if remove "Unfollowing list `%s'? "
                                "Following list `%s'? ")
                method  (if remove 'unsubscribe-list 'subscribe-list)
                args `((timeline-spec . ,spec))))
      (setq prompt-format (if remove "Unfollowing `%s'? " "Following `%s'? ")
            method (if remove 'destroy-friendships 'create-friendships)
            args `((username . ,user-or-list))))
    (if (y-or-n-p (format prompt-format user-or-list))
        (twittering-call-api method args)
      (message "Request canceled"))))

(defun twittering-unfollow ()
  "Unfollow a user or a list."
  (interactive)
  (twittering-follow t))

(defun twittering-add-list-members (&optional remove)
  "Add a user to a list.

Non-nil optional REMOVE will do the opposite, remove a user from
a list. "
  (interactive "P")
  (let* ((username (twittering-read-username-with-completion
                    (if remove "Remove who from list: " "Add who to list: ")
                    "" 'twittering-user-history))
         (listname
          (unless (string= username "")
            (or (let ((s (twittering-current-timeline-spec-string)))
                  (when (and s
                             (twittering-timeline-spec-list-p
                              (twittering-current-timeline-spec))
                             (y-or-n-p (format "from list: `%s'? " s)))
                    s))
                (twittering-read-list-name (twittering-get-accounts 'username))
                (read-string
                 "Failed to retrieve your list, enter list name manually or retry: ")))))
    (when (or (string= username "") (string= listname ""))
      (error "No user or list selected"))
    (unless (string-match "/" listname)
      (setq listname (concat (twittering-get-accounts 'username) "/" listname)))
    (if (y-or-n-p (format (if remove "Remove `%s' from `%s'? " "Add `%s' to `%s'? ")
                          username listname))
        (twittering-call-api
         (if remove 'delete-list-members 'add-list-members)
         `((id . ,username)
           (timeline-spec . ,(twittering-string-to-timeline-spec listname))))
      (message "Request canceled"))))

(defun twittering-delete-list-members ()
  "Delete a user from a list. "
  (interactive)
  (twittering-add-list-members t))

(defun twittering-favorite (&optional remove)
  (interactive "P")
  (let ((id (get-text-property (point) 'id))
        (text (copy-sequence (get-text-property (point) 'text)))
        (width (max 40 ;; XXX
                    (- (frame-width)
                       1 ;; margin for wide characters
                       15 ;; == (length (concat "Unfavorite \"" "\"? "))
                       9) ;; == (length "(y or n) ")
                    ))
        (method (if remove 'destroy-favorites 'create-favorites)))
    (set-text-properties 0 (length text) nil text)
    (if id
        (let ((mes (format "%s \"%s\"? "
                           (if remove "Unfavorite" "Favorite")
                           (if (< width (string-width text))
                               (concat
                                (truncate-string-to-width text (- width 3))
                                "...")
                             text))))
          (if (y-or-n-p mes)
              (twittering-call-api method `((id . ,id)))
            (message "Request canceled")))
      (message "No status selected"))))

(defun twittering-unfavorite ()
  (interactive)
  (twittering-favorite t))

(defun twittering-update-profile-image (image)
  "Update a new profile image.

Note: the new image might not appear in your timeline immediately (this seems
some limitation of twitter API?), but you can see your new image from web
browser right away."
  (interactive "fUpdate profile image: ")
  (twittering-call-api 'update-profile-image `((image . ,image))))

(defun twittering-block ()
  "Block a user who posted the tweet at the current position."
  (interactive)
  (let* ((id (twittering-get-id-at))
         (status (when id (twittering-find-status id)))
         (username
          (cond
           ((assq 'retweeted-id status)
            (let* ((retweeting-username
                    (cdr (assq 'retweeting-user-screen-name status)))
                   (retweeted-username
                    (cdr (assq 'retweeted-user-screen-name status)))
                   (prompt "Who do you block? ")
                   (candidates (list retweeted-username retweeting-username)))
              (twittering-completing-read prompt candidates nil t)))
           (status
            (assqref 'screen-name (assqref 'user status)))
           (t
            nil))))
    (cond
     ((or (null username) (string= "" username))
      (message "No user selected"))
     ((yes-or-no-p (format "Really block \"%s\"? " username))
      (twittering-call-api 'block `((username . ,username))))
     (t
      (message "Request canceled")))))

(defun twittering-block-and-report-as-spammer ()
  "Report a user who posted the tweet at the current position as a spammer.
The user is also blocked."
  (interactive)
  (let* ((id (twittering-get-id-at))
         (status (when id (twittering-find-status id)))
         (username
          (cond
           ((assq 'retweeted-id status)
            (let* ((retweeting-username
                    (cdr (assq 'retweeting-user-screen-name status)))
                   (retweeted-username
                    (cdr (assq 'retweeted-user-screen-name status)))
                   (prompt "Who do you report as a spammer? ")
                   (candidates (list retweeted-username retweeting-username)))
              (twittering-completing-read prompt candidates nil t)))
           (status
            (assqref 'screen-name (assqref 'user status)))
           (t
            nil))))
    (cond
     ((or (null username) (string= "" username))
      (message "No user selected"))
     ((yes-or-no-p
       (format "Really block \"%s\" and report him or her as a spammer? "
               username))
      (twittering-call-api 'block-and-report-as-spammer
                           `((username . ,username))))
     (t
      (message "Request canceled")))))

;;;; Commands for clearing stored statuses.

(defun twittering-erase-old-statuses ()
  (interactive)
  (when (twittering-buffer-p)
    (let ((spec (twittering-current-timeline-spec)))
      (twittering-remove-timeline-data spec) ;; clear current timeline.
      (twittering-render-timeline (current-buffer)) ;; clear buffer.
      (twittering-get-and-render-timeline))))

;;;; Other commands

(defun twittering-suspend ()
  "Suspend twittering-mode then switch to another buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun twittering-refresh ()
  "Refresh current buffer.

This will invoke `twittering-redisplay-status-on-each-buffer' immediately on
current buffer."
  (interactive)
  (twittering-redisplay-status-on-each-buffer (current-buffer)))

(defun twittering-toggle-thumbnail ()
  (interactive)
  (let ((limit (twittering-get-next-status-head))
        (find-func
         (lambda ()
           (let ((func-args
                  (find-if
                   (lambda (i)
                     (and (consp i)
                          (eq (car i) 'twittering-toggle-thumbnail-1)))
                   (cadr (get-text-property (point) 'keymap)))))
             (when func-args
               (apply (car func-args) (cdr func-args))
               (setq found-pos (point))))))
        found-pos)
    (unless (funcall find-func)         ; FIXME: no do-while in elisp? (xwl)
      (save-excursion
        (while (let ((next (twittering-goto-next-thing)))
                 (and (not found-pos) next (or (not limit) (< next limit))))
          (funcall find-func))))
    (when found-pos
      (recenter-top-bottom 'top))))

(defun twittering-open-url-externally (&optional next-nth)
  "Select and open url inside tweet.
Optionally you may provide a number, we will open NEXT-NTH url
directly.  Specially, if NEXT-NTH is 0, it will open the embeded
image directly.  "
  (interactive "P")
  (let ((limit (or (twittering-get-next-status-head) (point-max)))
        (urls '())
        (start nil)
        (end (point)))
    (save-excursion
      (while (and end (< end limit))
        (goto-char end)
        (setq start (when (get-text-property end 'uri)
                      end)
              end (next-single-property-change end 'uri))
        (if (and start end)
            (if (get-text-property start 'display)
                (setq urls
                      (cons
                       (propertize "IMAGE" 'uri (get-text-property start 'uri))
                       urls))
              (setq urls (cons
                          (replace-regexp-in-string
                           " *\n *" "" (buffer-substring start end))
                          urls)))
          (setq start end))))
    (setq urls (remove-duplicates urls :test 'equal))
    (ignore-errors
      (browse-url
       (get-text-property
        0 'uri
        (if next-nth
            (if (zerop next-nth)
                (find "IMAGE" urls :test 'equal)
              (nth (1- next-nth) (reverse urls)))
          (completing-read "Open url externally: " (reverse urls))))))))

(defun twittering-open-all-thumbnails-externally ()
  "Open all full size of the thumbnails with browse-url.  "
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (twittering-get-next-status-head)
      (twittering-open-url-externally 0)
      (twittering-goto-next-status))
    (when (twittering-get-current-status-head)
      (twittering-open-url-externally 0))))

(defun twittering-restart ()
  (interactive)
  (mapc 'kill-buffer twittering-buffer-info-list)
  (setq twittering-account-authorization nil
        twittering-oauth-access-token-alist nil
        twittering-enabled-services nil
        twittering-url-request-resolving-p nil

        twittering-private-info-file-loaded-p nil
        twittering-private-info-file-dirty nil

        twittering-format-status-function nil
        twittering-format-my-status-function nil)
  (twit))

(defun twittering-switch-to-unread-timeline ()
  (interactive)
  (when twittering-unread-status-info
    (switch-to-buffer (caar twittering-unread-status-info))))

;;;###autoload
(defun twit ()
  "Start twittering-mode."
  (interactive)
  (twittering-mode))

;;; ============================================= Session
;;;; Private storage
;;;;

(defun twittering-load-private-info ()
  (let* ((file twittering-private-info-file)
         (decrypted-str (twittering-read-from-encrypted-file file))
         (loaded-alist
          (when decrypted-str
            (condition-case nil
                (read decrypted-str)
              (error
               nil)))))
    (when loaded-alist
      (remove
       nil
       (mapcar
        (lambda (pair)
          (when (consp pair)
            (let ((sym (car pair))
                  (value (cdr pair)))
              (cond
               ((memq sym twittering-variables-stored-with-encryption)
                (set sym (append (symbol-value sym) value))
                sym)
               (t
                nil)))))
        loaded-alist)))
    (message "The authorized token is loaded.")))

(defun twittering-load-private-info-with-guide ()
  (let ((str (concat
              "Loading authorized access token for OAuth from\n"
              (format "%s.\n" twittering-private-info-file)
              "\n"
              (propertize "Please input the master password.\n" 'face 'bold)
              "\n"
              "To cancel it, you may need to press C-g multiple times.\n"
              )))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((str-height (length (split-string str "\n")))
             (height (max 0 (- (/ (- (window-text-height) 1) 2)
                               (/ str-height 2)))))
        (insert (make-string height ?\n) str)
        (set-buffer-modified-p nil)
        (twittering-load-private-info)))
    (setq twittering-private-info-file-loaded-p t)))

(defun twittering-save-private-info ()
  (let* ((obj (mapcar (lambda (sym)
                        `(,sym . ,(symbol-value sym)))
                      twittering-variables-stored-with-encryption))
         (str (with-output-to-string (pp obj)))
         (file twittering-private-info-file))
    (when (twittering-write-and-encrypt file str)
      (set-file-modes file #o600))))

(defun twittering-save-private-info-with-guide ()
  (let ((str (concat
              "Saving authorized access token for OAuth to "
              (format "%s.\n" twittering-private-info-file)
              "\n"
              (propertize "Please input a master password twice."
                          'face 'bold))))
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((str-height (length (split-string str "\n")))
             (height (max 0 (- (/ (- (window-text-height) 1) 2)
                               (/ str-height 2)))))
        (insert (make-string height ?\n) str)
        (set-buffer-modified-p nil)
        (twittering-save-private-info))))
  (setq twittering-private-info-file-dirty nil))

(defun twittering-capable-of-encryption-p ()
  (and (or (require 'epa nil t) (require 'alpaca nil t))
       (executable-find "gpg")))

(eval-when-compile
  (require 'epa nil t)
  (require 'alpaca nil t))
(defun twittering-read-from-encrypted-file (file)
  (cond
   ((require 'epa nil t)
    (let ((context (epg-make-context epa-protocol)))
      (epg-context-set-passphrase-callback
       context #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback
       context
       (cons #'epa-progress-callback-function
             (format "Decrypting %s..." (file-name-nondirectory file))))
      (message "Decrypting %s..." (file-name-nondirectory file))
      (condition-case err
          (epg-decrypt-file context file nil)
        (error
         (message "%s" (cdr err))
         nil))))
   ((require 'alpaca nil t)
    (with-temp-buffer
      (let ((buffer-file-name file)
            (alpaca-regex-suffix ".*")
            (temp-buffer (current-buffer)))
        (insert-file-contents-literally file)
        (set-buffer-modified-p nil)
        (condition-case nil
            (progn
              (alpaca-after-find-file)
              (if (eq temp-buffer (current-buffer))
                  (buffer-string)
                ;; `alpaca-after-find-file' kills the current buffer
                ;; if the decryption is failed.
                nil))
          (error
           (when (eq temp-buffer (current-buffer))
             (delete-region (point-min) (point-max)))
           nil)))))
   (t
    nil)))

(defun twittering-write-and-encrypt (file str)
  (cond
   ((require 'epg nil t)
    (let ((context (epg-make-context epa-protocol)))
      (epg-context-set-passphrase-callback
       context #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback
       context (cons #'epa-progress-callback-function "Encrypting..."))
      (message "Encrypting...")
      (condition-case err
          (unwind-protect
              ;; In order to prevent `epa-file' to encrypt the file double,
              ;; `epa-file-name-regexp' is temorarily changed into the null
              ;; regexp that never matches any string.
              (let ((epa-file-name-regexp "\\`\\'")
                    (coding-system-for-write 'binary))
                (when (fboundp 'epa-file-name-regexp-update)
                  (epa-file-name-regexp-update))
                (with-temp-file file
                  (set-buffer-multibyte nil)
                  (delete-region (point-min) (point-max))
                  (insert (epg-encrypt-string context str nil))
                  (message "Encrypting...wrote %s" file)
                  t))
            (when (fboundp 'epa-file-name-regexp-update)
              (epa-file-name-regexp-update)))
        (error
         (message "%s" (cdr err))
         nil))))
   ((require 'alpaca nil t)
    ;; Create the file.
    ;; This is required because `alpaca-save-buffer' checks its timestamp.
    (with-temp-file file)
    (with-temp-buffer
      (let ((buffer-file-name file)
            (coding-system-for-write 'binary))
        (insert str)
        (condition-case nil
            (if (alpaca-save-buffer)
                t
              (delete-file file)
              nil)
          (error
           (when (file-exists-p file)
             (delete-file file))
           nil)))))
   (t
    nil)))

;;;; Cache
;;;

(defcustom twittering-cache-file "~/.emacs.d/twittering/cache"
  "File name for caching latest status for each spec."
  :type 'string
  :group 'twittering)

;; '((spec latest-status-id)...)
(defvar twittering-cache-lastest-statuses '())
(defvar twittering-cache-saved-lastest-statuses '())

(defun twittering-cache-save (&optional spec-string)
  "Dump twittering-cache-lastest-statuses.
When SPEC-STRING is non-nil, just save lastest status for SPEC-STRING. "
  (if spec-string
      (setq twittering-cache-saved-lastest-statuses
            (cons (assoc spec-string twittering-cache-lastest-statuses)
                  (remove-if (lambda (entry) (equal spec-string (car entry)))
                             twittering-cache-saved-lastest-statuses)))
    (setq twittering-cache-saved-lastest-statuses
          twittering-cache-lastest-statuses))
  (with-temp-buffer
    (let (print-length
          print-level)
      (insert (format "\
;; automatically generated by twittering-mode -*- emacs-lisp -*-

%S
" twittering-cache-saved-lastest-statuses))
      (write-region (point-min) (point-max) twittering-cache-file nil 'silent)
      (kill-buffer))))

(defun twittering-cache-load ()
  (when (file-exists-p twittering-cache-file)
    (setq twittering-cache-lastest-statuses
          (with-temp-buffer
            (insert-file-contents twittering-cache-file)
            (read (current-buffer))))
    (setq twittering-cache-saved-lastest-statuses
          twittering-cache-lastest-statuses)))


;;; ============================================= Render status
;;;; Format of a status
;;;;

(defun twittering-make-common-properties (status)
  "Generate a property list that tweets should have irrespective of format."
  `(field
    ,(twittering-make-field-id status)

    ,@(apply 'append
             (mapcar (lambda (entry)
                       `(,entry ,(assqref entry status)))
                     '(id retweeted-id source-spec text)))

    retweeted-id ,(assqref 'id (assqref 'retweeted-status status))
    username ,(assqref 'screen-name
                       (assqref 'user
                                (or (twittering-status-has-quotation? status)
                                    status)))))

(defun twittering-get-common-properties (pos)
  "Get a common property list of the tweet rendered at POS.
The common property list is added to each rendered tweet irrespective
of format. The common properties follows:
 properites generated by `twittering-make-common-properties',
 `field' and `rendered-as' generated by `twittering-make-field-properties'."
  (apply 'append
         (mapcar (lambda (prop)
                   (let ((value (get-text-property pos prop)))
                     (when value
                       `(,prop ,value))))
                 '(field id rendered-as retweeted-id source-spec
                         text username))))

(defun twittering-format-string (string prefix replacement-table)
  "Format STRING according to PREFIX and REPLACEMENT-TABLE.
PREFIX is a regexp. REPLACEMENT-TABLE is a list of (FROM . TO) pairs,
where FROM is a regexp and TO is a string or a 2-parameter function.

The pairs in REPLACEMENT-TABLE are stored in order of precedence.
First, search PREFIX in STRING from left to right.
If PREFIX is found in STRING, try to match the following string with
FROM of each pair in the same order of REPLACEMENT-TABLE. If FROM in
a pair is matched, replace the prefix and the matched string with a
string generated from TO.
If TO is a string, the matched string is replaced with TO.
If TO is a function, the matched string is replaced with the
return value of (funcall TO CONTEXT), where CONTEXT is an alist.
Each element of CONTEXT is (KEY . VALUE) and KEY is one of the
following symbols;
  'following-string  --the matched string following the prefix
  'match-data --the match-data for the regexp FROM.
  'prefix --PREFIX.
  'replacement-table --REPLACEMENT-TABLE.
  'from --FROM.
  'processed-string --the already processed string."
  (let ((current-pos 0)
        (result "")
        (case-fold-search nil))
    (while (and (string-match prefix string current-pos)
                (not (eq (match-end 0) current-pos)))
      (let ((found nil)
            (current-table replacement-table)
            (next-pos (match-end 0))
            (matched-string (match-string 0 string))
            (skipped-string
             (substring string current-pos (match-beginning 0))))
        (setq result (concat result skipped-string))
        (setq current-pos next-pos)
        (while (and (not (null current-table))
                    (not found))
          (let ((key (caar current-table))
                (value (cdar current-table))
                (following-string (substring string current-pos))
                (case-fold-search nil))
            (if (string-match (concat "\\`" key) following-string)
                (let ((next-pos (+ current-pos (match-end 0)))
                      (output
                       (if (stringp value)
                           value
                         (funcall value
                                  `((following-string . ,following-string)
                                    (match-data . ,(match-data))
                                    (prefix . ,prefix)
                                    (replacement-table . ,replacement-table)
                                    (from . ,key)
                                    (processed-string . ,result))))))
                  (setq found t)
                  (setq current-pos next-pos)
                  (setq result (concat result output)))
              (setq current-table (cdr current-table)))))
        (if (not found)
            (setq result (concat result matched-string)))))
    (let ((skipped-string (substring string current-pos)))
      (concat result skipped-string))
    ))

(defun twittering-make-string-with-user-name-property (str status)
  (let ((ret ""))
    (when str
      (let* ((screen-name (assqref 'screen-name (assqref 'user status)))
             (uri (twittering-get-status-url
                   (case (twittering-extract-service)
                     ((twitter socialcast)
                      screen-name)
                     ((sina)
                      (assqref 'profile-url (assqref 'user status)))
                     (t
                      (assqref 'id (assqref 'user status))))))
             (spec (twittering-string-to-timeline-spec screen-name)))
        (setq ret (propertize str
                              'mouse-face 'highlight
                              'keymap twittering-mode-on-uri-map
                              'uri uri
                              'screen-name-in-text screen-name
                              'goto-spec spec
                              'face 'twittering-username-face))

        (when (and (eq (twittering-extract-service) 'sina)
                   (equal (assqref 'verified (assqref 'user status)) "t"))
          (setq ret (concat ret
                            (propertize "V"
                                        'mouse-face 'highlight
                                        'keymap twittering-mode-on-uri-map
                                        'uri uri
                                        'screen-name-in-text screen-name
                                        'goto-spec spec
                                        'face 'twittering-verify-face))))))
    ret))

(defun twittering-make-string-with-source-property (str status)
  (if (stringp str)
      (let ((caption str)
            (uri (if (string= str "douban") "www.douban.com" "")))
        (cond
         ((string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>" str)
          (setq uri (match-string 1 str)
                caption (match-string 2 str)))
         ((assqref 'source-uri status)
          (setq uri (assqref 'source-uri status))))
        (propertize caption
                    'mouse-face 'highlight
                    'keymap twittering-mode-on-uri-map
                    'face 'twittering-uri-face
                    'source caption
                    'uri uri))
    ""))

(defun twittering-make-string-with-uri-property (str)
  (when (stringp str)
    (while (string-match "<a href=\"\\([^>]+\\)\">\\([^<]+\\)</a>" str)
      (setq str (replace-match (propertize (match-string 2 str)
                                           'mouse-face 'highlight
                                           'keymap twittering-mode-on-uri-map
                                           'face 'twittering-uri-face
                                           'uri (match-string 1 str))
                               nil nil str)))
    str))

(defun twittering-format-tweet-text-with-quote (quoted-text text status)
  (let ((quoted-status (twittering-status-has-quotation? status)))
    (cond
     (quoted-text
      (if (eq (twittering-get-accounts 'quotation) 'after)
          (format "%s\n\n  『@%s: %s』"
                  text
                  (assqref 'name (assqref 'user quoted-status))
                  quoted-text)
        (if (or (string-match
                 (regexp-opt
                  `(,(replace-regexp-in-string
                      (format "^RT @%s: \\|[.[:blank:]]+$"
                              (assqref 'screen-name (assqref 'user quoted-status)))
                      ""
                      text)))
                 (replace-regexp-in-string
                  "[.[:blank:]]+$" "" quoted-text))

                (some (lambda (i) (string= text i))
                      '("转发微博" "转发微博。" "轉發微博" "轉發微博。")))
            quoted-text
          (format "『%s』\n\n    %s" quoted-text text))))
     (t
      text))))

(defun twittering-make-fontified-tweet-text (str status prefix)
  (if (not str)
      ""
    (case (twittering-extract-service)
      ((sina)
       ;; emotions
       (unless twittering-is-getting-emotions-p
         (setq twittering-is-getting-emotions-p t)
         (twittering-get-simple nil nil nil 'emotions))
       (let (index
             skip-fake-emotion)
         (while (if skip-fake-emotion
                    (progn
                      (setq skip-fake-emotion nil)
                      (setq index (string-match "\\(\\[[^][]+\\]\\)" str (1+ index))))
                  (setq index (string-match "\\(\\[[^][]+\\]\\)" str)))
           (let* ((repl (twittering-make-emotions-string nil nil (match-string 1 str)))
                  (new-str (replace-match repl nil nil str)))
             (if (equal new-str str)
                 (setq skip-fake-emotion t))
             (setq str new-str)))))

      ((douban)
       (setq str (twittering-make-string-with-uri-property str))))

    ;; Wash html tags
    (with-temp-buffer
      (insert str)
      (html2text)
      (setq str (buffer-string)))

    ;; tag, username, uri
    (let* ((regexp-list
            `( ;; Hashtag
              (hashtag . ,(concat twittering-regexp-hash
                                  (if (eq (twittering-extract-service) 'sina)
                                      (format "\\([^%s\n]+%s\\)"
                                              twittering-regexp-hash
                                              twittering-regexp-hash)
                                    "\\([a-zA-Z0-9_-]+\\)")))
              ;; @USER/LIST
              (list-name . ,(concat twittering-regexp-atmark
                                    "\\([a-zA-Z0-9_-]+/[a-zA-Z0-9_-]+\\)"))
              ;; @USER
              (screen-name
               . ,(concat twittering-regexp-atmark
                          (if (eq (twittering-extract-service) 'sina)
                              ;; Exclude some common chinese punctuations.
                              (format "\\([^:,.?!：)）、，。？！…[:space:]%s]+\\)"
                                      twittering-regexp-atmark)
                            "\\([a-zA-Z0-9_-]+\\)")))
              ;; URI
              (uri . ,twittering-regexp-uri)))
           (regexp-str (mapconcat 'cdr regexp-list "\\|"))
           (pos 0))
      (while (string-match regexp-str str pos)
        (let* ((entry
                ;; Find matched entries.
                (let ((rest regexp-list)
                      (counter 1))
                  (while (and rest (not (match-string counter str)))
                    (setq rest (cdr rest))
                    (setq counter (1+ counter)))
                  (when rest
                    (list (caar rest)
                          (match-beginning counter)
                          (match-string counter str)))))
               (sym (elt entry 0))
               (matched-beg (elt entry 1))
               (matched-str (elt entry 2))
               (beg (if (memq sym '(list-name screen-name))
                        ;; Properties are added to the matched part only.
                        ;; The prefixes `twittering-regexp-atmark' will not
                        ;; be highlighted.
                        matched-beg
                      (match-beginning 0)))
               (end (match-end 0))
               (properties
                (cond
                 ((eq sym 'hashtag)
                  (ignore-errors
                    (let* ((hashtag matched-str)
                           (spec
                            (twittering-string-to-timeline-spec
                             (concat "#" hashtag)))
                           (url (twittering-get-search-url (concat "#" hashtag))))
                      (list
                       'mouse-face 'highlight
                       'keymap twittering-mode-on-uri-map
                       'uri url
                       'goto-spec spec
                       'face 'twittering-username-face))))
                 ((eq sym 'list-name)
                  (let ((list-name matched-str))
                    (list
                     'mouse-face 'highlight
                     'keymap twittering-mode-on-uri-map
                     'uri (twittering-get-status-url list-name)
                     'goto-spec (twittering-string-to-timeline-spec list-name)
                     'face 'twittering-username-face)))
                 ((eq sym 'screen-name)
                  (let ((screen-name matched-str))
                    (list
                     'mouse-face 'highlight
                     'keymap twittering-mode-on-uri-map
                     'uri (if (eq (twittering-extract-service) 'sina)
                              ;; TODO: maybe we can get its user_id
                              ;; at background.
                              (concat "http://weibo.com/search/user.php?search="
                                      screen-name)
                            (twittering-get-status-url screen-name))
                     'screen-name-in-text screen-name
                     'goto-spec (twittering-string-to-timeline-spec screen-name)
                     'face 'twittering-username-face)))
                 ((eq sym 'uri)
                  (let ((uri matched-str))
                    (list
                     'mouse-face 'highlight
                     'keymap twittering-mode-on-uri-map
                     'uri uri
                     'face 'twittering-uri-face))))))
          (add-text-properties beg end properties str)
          (setq pos end))))

    ;; (douban) details
    (let ((detail (assqref 'detail status)))
      (when detail
        (setq detail (concat detail
                             "?alt=json&apikey="
                             (twittering-lookup-service-method-table
                              'oauth-consumer-key)))
        (let ((s (twittering-make-douban-detail-string nil nil detail)))
          (setq str (format "%s\n\n%s" str s)))))

    ;; thumbnail picture
    (let* ((st (or (twittering-status-has-quotation? status) status))
           (s (if (and twittering-icon-mode window-system)
                  (let ((thumbnail-pic (assqref 'thumbnail-pic st))
                        (original-pic (assqref 'original-pic st)))
                    (when thumbnail-pic
                      (if original-pic
                          (twittering-toggle-thumbnail-1 thumbnail-pic original-pic t)
                        (twittering-make-original-icon-string nil nil thumbnail-pic))))
                (assqref 'original-pic st))))
      (when s
        (setq str (concat str "\n" twittering-format-thumbnail-prefix s))))

    ;; comment/retweet counts
    (let ((service (twittering-extract-service)))
      (when (memq service '(sina socialcast))
        (let* ((n (- (twittering-calculate-fill-column (length prefix))
                     ;; space for counts string.
                     (if (eq service 'sina) 18 8)))
               (fmt (format "%%%ds" n))
               s)
          (case service
            ((sina)
             (let ((spec-string (twittering-current-timeline-spec-string)))
               (unless (equal spec-string ":replies@sina")
                 (setq s (twittering-get-simple nil nil nil 'counts)))))

            ((socialcast)
             (setq s (format "     Likes(%s) Comments(%s)"
                             (assqref 'likes-count status)
                             (assqref 'comments-count status)))))

          (when s
            (setq str (concat str "\n" (format fmt s)))))))

    str))

(defun twittering-generate-format-table (status-sym prefix-sym)
  `(("%" . "%")
    ("}" . "}")
    ("#" . (assqref 'id ,status-sym))
    ("'" . (when (string-match "true" (assqref 'truncated ,status-sym)) "..."))
    ("c" . (assqref 'created-at ,status-sym))
    ("d" . (assqref 'description (assqref 'user ,status-sym)))
    ("f" .
     (twittering-make-string-with-source-property
      (assqref 'source ,status-sym) ,status-sym))
    ("i" .
     (when (and twittering-icon-mode window-system)
       (let* ((st (or (and (not (eq (twittering-get-accounts 'quotation) 'after))
                           (twittering-status-has-quotation? ,status-sym))
                      ,status-sym))
              (url
               (cond
                ((and twittering-use-profile-image-api
                      (eq (twittering-extract-service) 'twitter)
                      (or (null twittering-convert-fix-size)
                          (member twittering-convert-fix-size '(48 73))))
                 (let ((user (assqref 'screen-name (assqref 'user st)))
                       (size
                        (if (or (null twittering-convert-fix-size)
                                (= 48 twittering-convert-fix-size))
                            "normal"
                          "bigger")))
                   (format "http://%s/%s/%s.xml?size=%s" (twittering-lookup-service-method-table 'api)
                           (twittering-api-path "users/profile_image") user size)))
                (t
                 (assqref 'profile-image-url (assqref 'user st))))))
         (twittering-make-icon-string nil nil url))))
    ("j" . (assqref 'id (assqref 'user ,status-sym)))
    ("L" .
     (let ((location (or (assqref 'location (assqref 'user ,status-sym)) "")))
       (unless (string= "" location)
         (concat "[" location "]"))))
    ("l" . (assqref 'location (assqref 'user ,status-sym)))
    ("p" . (when (string-match "true" (assqref 'protected (assqref 'user ,status-sym))) "[x]"))
    ("r" .
     (unless (eq (twittering-get-accounts 'quotation) 'after)
       (let* ((replied-status (or (assqref 'reply-comment ,status-sym)
                                  (assqref 'status ,status-sym)))
              (reply-id (or (assqref 'in-reply-to-status-id ,status-sym)
                            (assqref 'id replied-status)
                            ""))
              (reply-name (or (assqref 'in-reply-to-screen-name ,status-sym)
                              (assqref 'screen-name replied-status)
                              ""))
              (recipient-screen-name
               (assqref 'recipient-screen-name ,status-sym)))

         (let* ((pair
                 (cond
                  (replied-status
                   (let ((name (assqref 'screen-name (assqref 'user status))))
                     (list (format " (replied by %s)"
                                   (twittering-make-string-with-user-name-property name status)))))
                  (recipient-screen-name
                   (cons (format " sent to %s" recipient-screen-name)
                         (twittering-get-status-url recipient-screen-name)))
                  ((and (not (string= "" reply-id))
                        (not (string= "" reply-name)))
                   (cons (format " in reply to %s" reply-name)
                         (twittering-get-status-url reply-name reply-id)))))
                (str (car pair))
                (url (cdr pair))
                (properties
                 (list 'mouse-face 'highlight 'face 'twittering-uri-face
                       'keymap twittering-mode-on-uri-map
                       'uri url)))
           (when str
             (if url (apply 'propertize str properties) str))))))
    ("R" .
     (when (and (not (eq (twittering-get-accounts 'quotation) 'after))
                (twittering-is-retweet? ,status-sym))
       (concat " (retweeted by "
               (twittering-make-string-with-user-name-property
                (assqref 'screen-name (assqref 'user ,status-sym))
                ,status-sym)
               ")")))
    ("S" .
     (let ((st (or (and (not (eq (twittering-get-accounts 'quotation) 'after))
                        (twittering-status-has-quotation? ,status-sym))
                   ,status-sym)))
       (twittering-make-string-with-user-name-property
        (assqref 'name (assqref 'user st)) st)))
    ("s" .
     (let ((st (or (and (not (eq (twittering-get-accounts 'quotation) 'after))
                        (twittering-status-has-quotation? ,status-sym))
                   ,status-sym)))
       (twittering-make-string-with-user-name-property
        (assqref 'screen-name (assqref 'user st)) st)))
    ("t" .
     (twittering-make-fontified-tweet-text
      (funcall twittering-format-tweet-text-function
               (assqref 'text (twittering-status-has-quotation? ,status-sym))
               (assqref 'text ,status-sym)
               ,status-sym)
      ,status-sym
      ,prefix-sym))
    ("u" .
     (let ((st (or (and (not (eq (twittering-get-accounts 'quotation) 'after))
                        (twittering-status-has-quotation? ,status-sym))
                   ,status-sym)))
       (assqref 'url (assqref 'user st))))))

(defvar twittering-format-tweet-text-function 'twittering-format-tweet-text-with-quote
  "Function to format a tweet text.
It takes three arguments:

  quoted-text -- retweeted or replied text
  text        -- current text
  status      -- the tweet")

(defun twittering-generate-formater-for-first-spec (format-str status-sym prefix-sym)
  (cond
   ((string-match "\\`}" format-str)
    ;; "}" at the first means the end of the current level.
    `(nil . ,(substring format-str (match-end 0))))
   ((string-match "\\`%" format-str)
    (let* ((following (substring format-str 1))
           (table (twittering-generate-format-table status-sym prefix-sym))
           (regexp (concat "\\`\\(" (mapconcat 'car table "\\|") "\\)"))
           (case-fold-search nil))
      (cond
       ((string-match "\\`\\(@\\|g\\)\\({\\([^}]*\\)}\\)?" following)
        (let ((time-format (if (string= (match-string 1 following) "g")
                               "%g"
                             (or (match-string 3 following)
                                 "%I:%M %p %B %d, %Y")))
              (rest (substring following (match-end 0))))
          `((let* ((created-at-str (assqref 'created-at ,status-sym))
                   (st (or (assqref 'status ,status-sym) ,status-sym))
                   (url (twittering-get-status-url
                         (assqref 'id (assqref 'user st))
                         (assqref 'id st)))
                   (properties
                    (list 'mouse-face 'highlight 'face 'twittering-uri-face
                          'keymap twittering-mode-on-uri-map
                          'uri url)))
              (twittering-make-passed-time-string
               nil nil created-at-str ,time-format properties))
            . ,rest)))
       ((string-match "\\`C\\({\\([^}]*\\)}\\)?" following)
        (let ((time-format (or (match-string 2 following) "%H:%M:%S"))
              (rest (substring following (match-end 0))))
          `((let* ((created-at-str (assqref 'created-at ,status-sym))
                   (created-at (date-to-time created-at-str)))
              (format-time-string ,time-format created-at))
            . ,rest)))
       ((string-match "\\`FACE\\[\\([a-zA-Z0-9:-]+\\)\\(, *\\([a-zA-Z0-9:-]+\\)\\)?\\]{" following)
        (let* ((face-name-str-1 (match-string 1 following))
               (face-sym-1 (intern face-name-str-1))
               (face-name-str-2 (match-string 3 following))
               (face-sym-2 (and face-name-str-2 (intern face-name-str-2)))
               (str-after-brace (substring following (match-end 0)))
               (pair (twittering-generate-formater-for-current-level
                      str-after-brace status-sym prefix-sym))
               (braced-body (car pair))
               (rest (cdr pair)))
          `((twittering-decorate-zebra-background (concat ,@braced-body)
                                                  (quote ,face-sym-1)
                                                  (quote ,face-sym-2))
            . ,rest)))
       ((string-match "\\`\\(FILL\\|FOLD\\)\\(\\[\\([^]]*\\)\\]\\)?{"
                      following)
        (let* ((str-after-brace (substring following (match-end 0)))
               (specifier (match-string 1 following))
               (prefix-str (match-string 3 following))
               (pair (twittering-generate-formater-for-current-level
                      str-after-brace status-sym prefix-sym))
               (filled-body (car pair))
               (formater
                `(lambda (,status-sym ,prefix-sym)
                   (let ((,prefix-sym (concat ,prefix-sym ,prefix-str)))
                     (concat ,@filled-body))))
               (keep-newline (string= "FOLD" specifier))
               (rest (cdr pair)))
          `((twittering-update-filled-string
             nil nil ,formater ,status-sym ,prefix-sym ,prefix-str
             ,keep-newline)
            . ,rest)))
       ((string-match "\\`RT{" following)
        (let* ((str-after-brace (substring following (match-end 0)))
               (pair (twittering-generate-formater-for-current-level
                      str-after-brace 'retweeting prefix-sym))
               (braced-body (car pair))
               (rest (cdr pair)))
          `((when (assqref 'retweeted-status ,status-sym)
              (let ((retweeting
                     (mapcar (lambda (entry)
                               (let ((key-str (symbol-name (car entry)))
                                     (value (cdr entry)))
                                 (when (string-match "\\`retweeting-" key-str)
                                   (let ((new-key
                                          (intern (substring key-str
                                                             (match-end 0)))))
                                     (cons new-key value)))))
                             ,status-sym)))
                (concat ,@braced-body)))
            . ,rest)))
       ((string-match regexp following)
        (let ((specifier (match-string 1 following))
              (rest (substring following (match-end 0))))
          `(,(assocref specifier table) . ,rest)))
       (t
        `("%" . ,following)))))
   ((string-match "\\(%\\|}\\)" format-str)
    (let* ((sep (match-beginning 0))
           (first (substring format-str 0 sep))
           (last (substring format-str sep)))
      ;; Split before "%" or "}".
      `(,first . ,last)))
   (t
    `(,format-str . nil))))

(defun twittering-generate-formater-for-current-level (format-str status-sym prefix-sym)
  (let ((result nil)
        (rest format-str)
        (continue t))
    (while (and continue rest)
      (let* ((pair
              (twittering-generate-formater-for-first-spec
               rest status-sym prefix-sym))
             (current-result (car pair)))
        (if current-result
            (setq result (append result `(,current-result)))
          ;; If `result' is nil, it means the end of the current level.
          (setq continue nil))
        (setq rest (cdr pair))))
    `(,result . ,rest)))

(defun twittering-generate-format-status-function (format-str)
  (let* ((status-sym 'status)
         (prefix-sym 'prefix)
         (pair (twittering-generate-formater-for-current-level
                format-str status-sym prefix-sym))
         (body (car pair))
         (rest (cdr pair)))
    (cond
     ((null rest)
      `(lambda (status prefix)
         (let* ((common-properties (twittering-make-common-properties status))
                (col (and (twittering-my-status-p status) twittering-my-fill-column))
                (str (let ((twittering-fill-column (or col twittering-fill-column)))
                       (concat ,@body)))
                (str
                 (if col
                     (progn
                       ;; Padding spaces before icon placed at right.
                       ;; FIXME: matching against `:' is too ad-hoc.
                       (string-match "\\(\\`.*:\\)" str)
                       (let ((face (get-text-property (1- (length str)) 'face str))
                             (repl (format (format "%%-%ds" col) (match-string 1 str))))
                         (twittering-decorate-zebra-background
                          (replace-match repl nil nil str) face nil)))
                   str))
                (str (if prefix (replace-regexp-in-string "^" prefix str) str))
                (next (next-single-property-change 0 'need-to-be-updated str))
                (need-to-be-updated
                 (or (get-text-property 0 'need-to-be-updated str)
                     (and next (< next (length str))))))
           (add-text-properties 0 (length str) common-properties str)
           ;; (when (and prefix need-to-be-updated)
           ;;   ;; With a prefix, redisplay the total status instead of
           ;;   ;; redisplaying partially.
           ;;   (remove-text-properties 0 (length str)
           ;;                           '(need-to-be-updated nil) str)
           ;;   (put-text-property 0 (length str) 'need-to-be-updated
           ;;                      `(twittering-format-status-for-redisplay
           ;;                        ,status ,prefix)
           ;;                      str))
           str)))
     (t
      (message "Failed to generate a status formater for `twittering-mode'.")
      nil))))

(defun twittering-update-status-format ()
  (cond
   ((twittering-get-accounts 'status-format)
    (let ((status-format (twittering-get-accounts 'status-format))
          (last-status-format (twittering-get-accounts-internal 'last-status-format))
          (status-func (twittering-get-accounts-internal 'status-format-func)))
      (unless (and status-func (equal status-format last-status-format))
        (let* ((func
                (byte-compile
                 (twittering-generate-format-status-function status-format)))
               (service (twittering-extract-service))
               (old (assqref service twittering-accounts-internal)))
          (twittering-update-accounts-internal
           `(,@(remove-if (lambda (i)
                            (memq (car i) '(last-status-format status-func)))
                          old)
             (last-status-format . ,status-format)
             (status-func . ,func)))))))

   ((twittering-get-accounts 'my-status-format)
    (let ((my-status-format (twittering-get-accounts 'my-status-format))
          (last-my-status-format (twittering-get-accounts-internal 'last-my-status-format))
          (status-func (twittering-get-accounts-internal 'my-status-format-func)))
      (unless (and status-func (equal my-status-format last-my-status-format))
        (let* ((func
                (byte-compile
                 (twittering-generate-format-status-function my-status-format)))
               (service (twittering-extract-service))
               (old (assqref service twittering-accounts-internal)))
          (twittering-update-accounts-internal
           `(,@(remove-if (lambda (i)
                            (memq (car i) '(last-my-status-format status-func)))
                          old)
             (last-my-status-format . ,my-status-format)
             (status-func . ,func)))))))
   (t
    (unless (and twittering-format-status-function
                 (equal twittering-status-format twittering-last-status-format))
      (setq twittering-last-status-format
            twittering-status-format)
      (setq twittering-format-status-function
            (byte-compile
             (twittering-generate-format-status-function
              twittering-last-status-format))))
    (unless (and twittering-format-my-status-function
                 (equal twittering-my-status-format twittering-last-my-status-format))
      (setq twittering-last-my-status-format
            twittering-my-status-format)
      (setq twittering-format-my-status-function
            (byte-compile
             (twittering-generate-format-status-function
              twittering-last-my-status-format)))))))

(defun twittering-format-status (status &optional prefix)
  "Format a STATUS by using `twittering-format-status-function'.
Specification of FORMAT-STR is described in the document for the
variable `twittering-status-format'."
  (if (and twittering-my-status-format (twittering-my-status-p status))
      (if (twittering-get-accounts 'my-status-format)
          (funcall (twittering-get-accounts-internal 'my-status-func) status prefix)
        (funcall twittering-format-my-status-function status prefix))
    (if (twittering-get-accounts 'status-format)
        (funcall (twittering-get-accounts-internal 'status-func) status prefix)
      (funcall twittering-format-status-function status prefix))))

(defun twittering-format-status-for-redisplay (beg end status &optional prefix)
  (twittering-format-status status prefix))

;;;;
;;;; Rendering
;;;;

(defun twittering-field-id< (field1 field2)
  (string< field1 field2))

(defun twittering-field-id= (field1 field2)
  (string= field1 field2))

(defun twittering-make-field-id (status &optional base-id)
  "Generate a field property for STATUS.
Tweets are rendered in order of the field.

If BASE-ID is non-nil, generate a field id for a tweet rendered
as a popped ancestor tweet by `twittering-show-replied-statuses'.
In the case, BASE-ID means the ID of the descendant."
  (let ((id (assqref 'id status))
        (format-func (lambda (id) (format "%02d-%s" (length id) id))))
    (cond
     (base-id
      (format "O:%s:5:ancestor:%s"
              (funcall format-func base-id)
              (funcall format-func id)))
     (t
      (format "O:%s" (funcall format-func id))))))

(defun twittering-make-field-properties (status &optional rendered-as)
  (let* ((base-id (assqref 'ancestor-of rendered-as))
         (field-id (twittering-make-field-id status base-id)))
    (if rendered-as
        `(field ,field-id rendered-as ,rendered-as)
      `(field ,field-id))))

(defun twittering-make-field-properties-of-popped-ancestors (status base-id)
  (twittering-make-field-properties status `((ancestor-of . ,base-id))))

(defun twittering-rendered-as-ancestor-status-p (&optional pos)
  "Return non-nil if the status at POS is rendered as an ancestor.
Ancestor statuses are rendered by `twittering-show-replied-statuses'."
  (let ((pos (or pos (point))))
    (assq 'ancestor-of (get-text-property pos 'rendered-as))))

(defun twittering-get-base-id-of-ancestor-at (&optional pos)
  "Return the base ID of a popped ancestor status rendered at POS.
If the status at POS is not a popped ancestor status or no status is
rendered at POS, return nil."
  (let ((pos (or pos (point))))
    (cdr (assq 'ancestor-of (get-text-property pos 'rendered-as)))))

(defun twittering-calculate-fill-column (adjustment)
  (when (and (not (boundp 'kinsoku-limit))
             enable-kinsoku)
    ;; `kinsoku-limit' is defined on loading "international/kinsoku.el".
    ;; Without preloading, "kinsoku.el" will be loaded by auto-loading
    ;; triggered by `fill-region-as-paragraph'.
    ;; In that case, the local binding of `kinsoku-limit' conflicts the
    ;; definition by `defvar' in "kinsoku.el".
    ;; The below warning is displayed;
    ;; "Warning: defvar ignored because kinsoku-limit is let-bound".
    ;; So, we load "kinsoku.el" in advance if necessary.
    (load "international/kinsoku"))
  (let* ((kinsoku-limit 1)
         (adjustment (+ (or adjustment 0)
                        (if enable-kinsoku
                            kinsoku-limit
                          0)))
         (min-width
          (apply 'min
                 (or
                  (mapcar 'window-width
                          (get-buffer-window-list (current-buffer) nil t))
                  ;; Use `(frame-width)' if no windows display
                  ;; the current buffer.
                  `(,(frame-width))))))
    (- (or twittering-fill-column (1- min-width)) adjustment)))

(defun twittering-fill-string (str &optional adjustment prefix keep-newline)
  (with-temp-buffer
    (let ((fill-column (twittering-calculate-fill-column adjustment))
          (fill-prefix (or prefix fill-prefix))
          (adaptive-fill-regexp ""))
      (if keep-newline
          (let* ((hard-newline (propertize "\n" 'hard t))
                 (str (mapconcat 'identity (split-string str "\n")
                                 (concat hard-newline fill-prefix))))
            (use-hard-newlines)
            (insert (concat prefix str))
            (fill-region (point-min) (point-max) nil t)
            (remove-text-properties (point-min) (point-max) '(hard nil)))
        (insert (concat prefix str))
        (fill-region-as-paragraph (point-min) (point-max)))
      (buffer-substring (point-min) (point-max)))))

(defun twittering-update-filled-string (beg end formater status prefix local-prefix &optional keep-newline)
  (twittering-fill-string
   (funcall formater status prefix) (length prefix) local-prefix keep-newline))

(defun twittering-make-passed-time-string (beg end created-at-str time-format
                                               &optional additional-properties)
  (when created-at-str
    (let* ((encoded (date-to-time created-at-str))
           (secs (time-to-seconds (time-since encoded)))
           (time-string
            (cond
             ((string= time-format "%g")
              (if (< emacs-major-version 24)
                  (require 'gnus-util)
                (require 'gnus-sum))
              (gnus-user-date created-at-str))

             ((< secs 5) "less than 5 seconds ago")
             ((< secs 10) "less than 10 seconds ago")
             ((< secs 20) "less than 20 seconds ago")
             ((< secs 30) "half a minute ago")
             ((< secs 60) "less than a minute ago")
             ((< secs 150) "1 minute ago")
             ((< secs 2400) (format "%d minutes ago"
                                    (/ (+ secs 30) 60)))
             ((< secs 5400) "about 1 hour ago")
             ((< secs 84600) (format "about %d hours ago"
                                     (/ (+ secs 1800) 3600)))
             (t (format-time-string time-format encoded))))
           (properties (append additional-properties
                               (and beg (text-properties-at beg)))))
      ;; Restore properties.
      (when properties
        (add-text-properties 0 (length time-string) properties time-string))
      (if (and (< secs 84600)
               (if (string= time-format "%g")
                   (= (time-to-day-in-year (current-time))
                      (time-to-day-in-year encoded))
                 t))
          (put-text-property 0 (length time-string)
                             'need-to-be-updated
                             `(twittering-make-passed-time-string
                               ,created-at-str ,time-format)
                             time-string)
        ;; Remove the property required no longer.
        (remove-text-properties 0 (length time-string) '(need-to-be-updated nil)
                                time-string))
      time-string)))

(defun twittering-render-timeline (buffer &optional additional timeline-data)
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (let* ((spec (twittering-get-timeline-spec-for-buffer buffer))
               (timeline-data (twittering-timeline-data-collect spec timeline-data))
               (rendering-entire (or (null (twittering-get-first-status-head))
                                     (not additional)))
               (buffer-read-only nil))
          (let ((twittering-service-method (caar spec))) ; TODO: necessary?  (xwl)
            (twittering-update-status-format)
            (twittering-update-mode-line)
            (when (and (twittering-timeline-spec-user-p spec) timeline-data)
              (let ((locate-separator
                     (lambda ()
                       (let ((p (next-single-property-change (point-min) 'user-profile-separator)))
                         (when p (goto-char p))))))
                (unless (funcall locate-separator)
                  (twittering-render-user-profile timeline-data))
                (funcall locate-separator)
                (if twittering-reverse-mode
                    (narrow-to-region (point-min) (line-beginning-position))
                  (forward-line 1)
                  (narrow-to-region (point) (point-max)))))
            (when rendering-entire
              (delete-region (point-min) (point-max)))
            (let* ((prev-p (twittering-timeline-data-is-previous-p timeline-data))
                   (get-insert-point
                    (if prev-p
                        (if twittering-reverse-mode 'point-min 'point-max)
                      (if twittering-reverse-mode 'point-max 'point-min))))
              (goto-char (funcall get-insert-point))
              (mapc
               (lambda (status)
                 (let ((formatted-status (twittering-format-status status)))
                   (add-text-properties 0 (length formatted-status)
                                        `(belongs-spec ,spec)
                                        formatted-status)
                   (cond
                    ((eobp)
                     ;; Insert a status after the current position.
                     (insert formatted-status twittering-tweet-separator))
                    (t
                     ;; Use `insert-before-markers' in order to keep
                     ;; which status is pointed by each marker.
                     (insert-before-markers formatted-status twittering-tweet-separator)))
                   (when twittering-default-show-replied-tweets
                     (twittering-show-replied-statuses
                      twittering-default-show-replied-tweets))))
               ;; Always insert most adjacent tweet first.
               (if prev-p
                   timeline-data                  ; sorted decreasingly
                 (reverse timeline-data)))))))))) ; sorted increasingly            

;; TODO: this is assuming the user has at least one tweet.
(defun twittering-render-user-profile (timeline-data)
  (if twittering-reverse-mode
      (goto-char (point-max))
    (goto-char (point-min)))
  (let ((insert-separator
         (lambda ()
           (let ((s " "))
             (put-text-property 0 (length s) 'user-profile-separator t s)
             (insert s "\n")))))
    (when twittering-reverse-mode
      (funcall insert-separator))

    (let* ((status (or (car timeline-data)))

           (user-name             (assqref 'name             (assqref 'user status)))
           (user-screen-name      (assqref 'screen-name      (assqref 'user status)))
           (user-id               (assqref 'id               (assqref 'user status)))
           (user-description      (assqref 'description      (assqref 'user status)))
           (user-location         (assqref 'location         (assqref 'user status)))
           (user-url              (assqref 'url              (assqref 'user status)))
           (user-followers-count  (assqref 'followers-count  (assqref 'user status)))
           (user-friends-count    (assqref 'friends-count    (assqref 'user status)))
           (user-statuses-count   (assqref 'statuses-count   (assqref 'user status)))
           (user-favourites-count (assqref 'favourites-count (assqref 'user status)))
           (user-created-at       (assqref 'created-at       (assqref 'user status)))

           (u (substring-no-properties
               (if (eq (twittering-extract-service) 'sina) user-id user-screen-name)))

           (profile
            (concat
             (format "%s, @%s%s\n"
                     (twittering-make-string-with-user-name-property user-name status)
                     (twittering-make-string-with-user-name-property user-screen-name status)
                     ;; #%s
                     ;; (let ((tmp user-id)
                     ;;            (ret ""))
                     ;;   (while (> (length tmp) 3)
                     ;;          (setq ret (concat (substring tmp -3)
                     ;;                            (if (string= ret "") "" ",")
                     ;;                            ret)
                     ;;                tmp (substring tmp 0 -3)))
                     ;;   ret)
                     (if (string= user-screen-name (twittering-get-accounts 'username))
                         ""
                       (twittering-get-simple nil nil user-screen-name 'show-friendships)))

             ;; bio
             (if (string= user-description "")
                 "\n"
               (concat "\n"
                       (twittering-fill-string
                        (if user-description
                            (twittering-decorate-uri user-description)
                          ""))
                       "\n\n"))

             ;; location, web
             (format "location: %s\n" (or user-location ""))
             (progn
               (when user-url
                 (twittering-decorate-uri user-url))
               (format "     web: %s\n\n" (or user-url "")))

             ;; follow info
             (let ((len-list
                    (mapcar
                     (lambda (lst)
                       (apply 'max (mapcar 'length lst)))
                     (list
                      (list user-friends-count user-statuses-count)
                      (list user-followers-count user-favourites-count)))))

               (format
                (format
                 " %%%ds %%s, %%%ds %%s\n %%%ds tweets,    %%%ds %%s\n"
                 (car len-list) (cadr len-list) (car len-list) (cadr len-list))
                user-friends-count
                (twittering-decorate-listname (concat u "/following") "following")
                user-followers-count
                (twittering-decorate-listname (concat u "/followers") "followers")
                user-statuses-count
                user-favourites-count
                (twittering-decorate-listname (concat u "/favorites") "favorites")))

             ;; lists
             "\n"
             (let* ((prompts
                     (mapcar
                      (lambda (f) (format f user-screen-name))
                      '("%s's Lists: "
                        "Lists %s Follows: "
                        "Lists Following %s: "))))
               (mapconcat
                'identity
                (mapcar*
                 (lambda (prompt method)
                   (format
                    (format "%%%ds%%s"
                            (twittering-calculate-list-info-prefix-width
                             user-screen-name))
                    prompt
                    (if (eq (twittering-extract-service) 'sina) "(not supported yet)"
                      (twittering-get-simple nil nil user-screen-name method))))
                 prompts
                 '(get-list-index get-list-subscriptions get-list-memberships))
                "\n\n"))
             "\n"

             ;; join date
             (format "\nJoined on %s.\n"
                     (mapconcat (lambda (i)
                                  (aref (timezone-parse-date user-created-at) i))
                                '(0 1 2)
                                "-"))))

           ;; Note, twitter provides two sizes of icon:
           ;;   1) 48x48 avatar
           ;;   2) original full sized
           ;; The avatar is named by adding a `_normal' after the original filename, but
           ;; before the file extension, e.g., original filename is `foo.jpg', the avatar will
           ;; be named `foo_normal.jpg'."
           (icon-string
            (if (and twittering-icon-mode window-system)
                (let* ((service (twittering-extract-service))
                       (url (cdr-safe (assq 'profile-image-url (assqref 'user status))))
                       (orig-url (case service
                                   ((twitter)
                                    (replace-regexp-in-string "_normal\\." "." url))
                                   ((sina)
                                    (replace-regexp-in-string "/50/" "/180/" url))
                                   (t
                                    url)))
                       (s (let ((twittering-convert-fix-size nil))
                            (twittering-make-icon-string nil nil orig-url))))
                  (add-text-properties 0 (length s)
                                       `(mouse-face highlight uri ,orig-url)
                                       s)
                  s)
              "")))

      ;; FIXME: how to align image and multiple lines text side by side?
      (insert icon-string "\n" profile))

    (unless twittering-reverse-mode
      (funcall insert-separator))))

(defun twittering-calculate-list-info-prefix-width (username)
  (apply 'max (mapcar
               (lambda (f) (length (format f username)))
               '("%s's Lists: "
                 "Lists %s Follows: "
                 "Lists Following %s: "))))

(defun twittering-get-and-render-timeline (&optional noninteractive max-id)
  (let* ((spec (twittering-current-timeline-spec))
         (spec-string (twittering-current-timeline-spec-string))
         (service (twittering-extract-service)))
    (unless (twittering-account-authorized-p)
      (error "No account for `%s' has been authorized" service))
    (cond
     ((and noninteractive (twittering-process-active-p spec))
      (messsage "a process is still waiting for responses"))
     ((twittering-timeline-spec-primary-p spec)
      (let* ((is-search-spec (eq 'search (cadr spec)))
             ;; Assume the list returned by `twittering-current-timeline-data' is sorted.
             (latest-status (car (twittering-current-timeline-data spec)))
             (since-id (assqref 'id latest-status))
             (args
              `((timeline-spec        . ,spec)
                (timeline-spec-string . ,spec-string)
                (since-id             . ,since-id)

                ,@(when is-search-spec
                    `((word . ,(caddr spec))))
                ,@(when (twittering-timeline-spec-user-methods-p spec)
                    `((cursor . ,(or (assqref max-id latest-status) "-1"))))
                ,@(when (and max-id (not (twittering-timeline-spec-user-methods-p spec)))
                    `((max-id . ,max-id)))
                ,@(when (eq service 'douban)
                    `((douban-user-id . ,(assocref "douban_user_id"
                                                   (twittering-lookup-oauth-access-token-alist)))))

                (clean-up-sentinel . ,(lambda (proc status connection-info)
                                        (when (memq status '(exit signal closed failed))
                                          (twittering-release-process proc))))))
             (additional-info
              `((noninteractive       . ,noninteractive)
                (timeline-spec        . ,spec)
                (timeline-spec-string . ,spec-string)))
             (proc
              (twittering-call-api 'retrieve-timeline args additional-info)))
        (when proc
          (twittering-register-process proc spec spec-string))))
     (t
      (error "%s has not been supported yet" spec)))))

(defun twittering-restore-point (id)
  (goto-char (point-min))
  (let ((pos (point))
        done)
    (unless (string= id (get-text-property pos 'id))
      (while (and (setq pos (next-single-property-change pos 'id))
                  (not done))
        (if (string= id (get-text-property pos 'id))
            (setq done pos)
          (goto-char pos)))
      (when done
        (goto-char done)))))

;;;; Display replied statuses

(defun twittering-replied-statuses-visible-p ()
  (let* ((pos (twittering-get-current-status-head))
         (id (twittering-get-id-at pos))
         (prev (twittering-get-previous-status-head pos))
         (next (twittering-get-next-status-head pos)))
    (or
     (twittering-get-base-id-of-ancestor-at pos)
     (and prev
          (twittering-status-id=
           id (twittering-get-base-id-of-ancestor-at prev)))
     (and next
          (twittering-status-id=
           id (twittering-get-base-id-of-ancestor-at next))))))

(defun twittering-show-replied-statuses (&optional count interactive)
  "Use C-u prefix to force getting new replies.  "
  (interactive)
  (if (twittering-replied-statuses-visible-p)
      (when interactive
        (message "The replied statuses were already showed."))
    (let* ((base-id (twittering-get-id-at))
           (statuses (twittering-get-replied-statuses
                      base-id (when (numberp count) count)))
           (force? current-prefix-arg))
      (when force?
        (setq statuses nil))
      (if statuses
          (let ((beg (if (and twittering-reverse-mode
                              (not (eq twittering-service-method 'sina)))
                         (twittering-get-current-status-head)
                       (or (twittering-get-next-status-head)
                           (point-max))))
                (prefix  "  ")
                (buffer-read-only nil))
            (setq statuses (sort statuses (lambda (e1 e2)
                                            (twittering-status-id<
                                             (assqref 'id e1)
                                             (assqref 'id e2)))))
            (save-excursion
              (narrow-to-region (twittering-get-current-status-head)
                                (or (twittering-get-next-status-head)
                                    (point-max)))
              (goto-char beg)
              (mapc
               (lambda (status)
                 (let* ((id (assqref 'id status))
                        (spec (twittering-current-timeline-spec))
                        (formatted-status (twittering-format-status status prefix))
                        (field-properties ; Overwrite field property.
                         `(,@(twittering-make-field-properties-of-popped-ancestors
                              status base-id)
                           belongs-spec ,spec)))
                   (add-text-properties 0 (length formatted-status)
                                        field-properties
                                        formatted-status)
                   (insert formatted-status twittering-tweet-separator)
                   (delete-region (line-beginning-position) (point))))
               (if twittering-reverse-mode
                   (reverse statuses)
                 statuses))))
        (when interactive
          (case twittering-service-method
            ((sina)
             (if (or force? (twittering-have-replied-statuses-p base-id))
                 (progn
                   (twittering-call-api 'show `((id . ,base-id)) '((sync . t)))
                   (setq current-prefix-arg nil)
                   (twittering-show-replied-statuses))
               (message "This status has no replies.")))
            (t
             (if (twittering-have-replied-statuses-p base-id)
                 (let ((replied-id (assqref 'in-reply-to-status-id
                                            (twittering-find-status base-id))))
                   (twittering-call-api 'show `((id . ,replied-id)) '((sync . t)))
                   (twittering-show-replied-statuses))
               (message "This status is not a reply.")))))))))

(defun twittering-hide-replied-statuses (&optional interactive)
  (interactive)
  (cond
   ((twittering-replied-statuses-visible-p)
    (let* ((pos (twittering-get-current-status-head (point)))
           (base-id (or (twittering-get-base-id-of-ancestor-at pos)
                        (twittering-get-id-at pos)))
           (pointing-to-base-status
            (not (twittering-rendered-as-ancestor-status-p pos)))
           (beg
            (if (and pointing-to-base-status (or (not twittering-reverse-mode)
                                                 (eq twittering-service-method 'sina)))
                (twittering-get-next-status-head pos)
              (let ((pos pos))
                (while
                    (let* ((prev (twittering-get-previous-status-head pos))
                           (prev-base-id
                            (when prev
                              (twittering-get-base-id-of-ancestor-at prev))))
                      (and prev
                           (twittering-status-id= base-id prev-base-id)
                           (setq pos prev))))
                (or pos (point-min)))))
           (end
            (if (and pointing-to-base-status (and twittering-reverse-mode
                                                  (not (eq twittering-service-method 'sina))))
                pos
              (let ((pos beg))
                (while
                    (let ((current-base-id
                           (twittering-get-base-id-of-ancestor-at pos)))
                      (and current-base-id
                           (twittering-status-id= base-id current-base-id)
                           (setq pos (twittering-get-next-status-head pos)))))
                (or pos (point-max)))))
           (buffer-read-only nil))
      ;; (unless pointing-to-base-status
      ;;   (goto-char (if twittering-reverse-mode
      ;;                  beg
      ;;                (or (twittering-get-previous-status-head beg)
      ;;                    (point-min)))))
      (delete-region beg end))
    (goto-char (point-max))
    (twittering-goto-previous-status)
    (widen))
   (interactive
    (message "The status this replies to was already hidden."))))

(defun twittering-toggle-show-replied-statuses ()
  (interactive)
  (if (twittering-replied-statuses-visible-p)
      (twittering-hide-replied-statuses (interactive-p))
    (twittering-show-replied-statuses twittering-show-replied-tweets
                                      (interactive-p))))

;;;; Automatic redisplay of statuses on buffer
;;;;

(defun twittering-redisplay-status-on-buffer ()
  (mapc (lambda (buffer)
          (unless (with-current-buffer buffer
                    (or (and (fboundp 'use-region-p) (use-region-p))
                        (and transient-mark-mode mark-active)))
            (twittering-redisplay-status-on-each-buffer buffer)))
        (twittering-get-buffer-list)))

(defun twittering-redisplay-status-on-each-buffer (buffer)
  (let ((deactivate-mark deactivate-mark)
        (window-list (get-buffer-window-list buffer nil t))
        (marker (with-current-buffer buffer (point-marker)))
        (result nil))
    (with-current-buffer buffer
      (save-excursion
        (twittering-for-each-property-region
         'need-to-be-updated
         (lambda (beg end value)
           ;; `beg' and `end' may be changed unexpectedly when `func' inserts
           ;; some texts at front, so store marker here.
           (setq beg (copy-marker beg)
                 end (copy-marker end))

           (let* ((func (car value))
                  (args (cdr value))
                  (current-str (buffer-substring beg end))
                  (updated-str (apply func beg end args))
                  (config (twittering-current-window-config window-list))
                  (buffer-read-only nil))
             ;; Replace `current-str' if it differs to `updated-str' with
             ;; ignoring properties. This is an ad-hoc solution.
             ;; `current-str' is a part of the displayed status, but it has
             ;; properties which are determined by the whole status.
             ;; (For example, the `id' property.)
             ;; Therefore, we cannot compare the strings with their
             ;; properties.
             (unless (string= current-str updated-str)
               ;; If the region to be modified includes the current position,
               ;; the point moves to the beginning of the region.
               (when (and (< beg marker) (< marker end))
                 ;; This is required because the point moves to the center if
                 ;; the point becomes outside of the window by the effect of
                 ;; `set-window-start'.
                 (setq result beg))
               (let ((common-properties (twittering-remove-property
                                         (text-properties-at beg)
                                         'need-to-be-updated))
                     (faces (get-text-property beg 'face)))
                 ;; Restore properties.
                 (delete-region beg end)
                 (goto-char beg)
                 (add-text-properties 0 (length updated-str) common-properties updated-str)
                 (twittering-decorate-zebra-background updated-str faces)
                 (insert updated-str))
               (twittering-restore-window-config-after-modification
                config beg end))))
         buffer)))))

(defun twittering-for-each-property-region (prop func &optional buffer interrupt)
  "Apply FUNC to each region, where property PROP is non-nil, on BUFFER.
If INTERRUPT is non-nil, the iteration is stopped if FUNC returns nil."
  (with-current-buffer (or buffer (current-buffer))
    (let ((beg (point-min))
          (end-marker (make-marker)))
      (set-marker-insertion-type end-marker t)
      (while
          (let ((value (get-text-property beg prop)))
            (if value
                (let* ((end (next-single-property-change beg prop))
                       (end (or end (point-max)))
                       (end-marker (set-marker end-marker end))
                       (func-result (funcall func beg end value))
                       (end (marker-position end-marker)))
                  (when (or (null interrupt) func-result)
                    (if (get-text-property end prop)
                        (setq beg end)
                      (setq beg (next-single-property-change end prop)))))
              (setq beg (next-single-property-change beg prop)))))
      (set-marker end-marker nil))))


(defvar twittering-idle-timer-for-redisplay nil)

(defun twittering-is-uploading-file-p (parameters)
  "Check whether there is a pair '(\"image\" . \"@...\") in PARAMETERS."
  (when parameters
    (string-match "image=\\`@\\([^;&]+\\)" parameters)))

;; (some (lambda (pair)
;;         (and (string= (car pair) "image")
;;              (when (string-match "image=\\`@\\([^;&]+\\)" (cdr pair))
;;                       (let ((f (match-string 1 (cdr pair))))
;;                         (or (file-exists-p f)
;;                             (error "%s doesn't exist" f))))))
;;       parameters))


;;; ============================================= Web tools

;;;; Functions for URL library
;;;;

(defvar twittering-url-show-status nil
  "*Whether to show a running total of bytes transferred.")

(defadvice url-retrieve (around hexify-multibyte-string activate)
  (let ((url (ad-get-arg 0)))
    (if (twittering-multibyte-string-p url)
        (let ((url-unreserved-chars
               (append '(?: ?/ ??) url-unreserved-chars)))
          (ad-set-arg 0 (url-hexify-string url))
          ad-do-it)
      ad-do-it)))

(defun twittering-url-retrieve-synchronously (url)
  (let ((request (twittering-make-http-request-from-uri
                  "GET" nil url))
        (additional-info `((url . ,url) (sync . t)))
        result)
    (twittering-send-http-request
     request additional-info
     (lambda (proc status connection-info header-info)
       (let ((status-line (assqref 'status-line header-info))
             (status-code (assqref 'status-code header-info)))
         (case-string
          status-code
          (("200")
           (setq result (buffer-string))
           nil)
          (t
           (setq result nil)
           (format "Response: %s" status-line))))))
    (unless result
      (error "Failed to retrieve `%s'" url))
    result))

;;;; Proxy setting / functions
;;;;

(defvar twittering-proxy-use nil)

(defvar twittering-proxy-server nil
  "*Proxy server for `twittering-mode'.
If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twittering-proxy-server'
and `twittering-proxy-port' must be nil.")
(defvar twittering-proxy-port nil
  "*Port number for `twittering-mode'.
If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.

To use individual proxies for HTTP and HTTPS, both `twittering-proxy-server'
and `twittering-proxy-port' must be nil.")
(defvar twittering-proxy-keep-alive nil)
(defvar twittering-proxy-user nil
  "*Username for `twittering-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-proxy-password nil
  "*Password for `twittering-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")

(defvar twittering-http-proxy-server nil
  "*HTTP proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
The port number is specified by `twittering-http-proxy-port'.
For HTTPS connection, the proxy specified by `twittering-https-proxy-server'
and `twittering-https-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-http-proxy-port nil
  "*Port number of a HTTP proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
The server is specified by `twittering-http-proxy-server'.
For HTTPS connection, the proxy specified by `twittering-https-proxy-server'
and `twittering-https-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-http-proxy-keep-alive nil
  "*If non-nil, the Keep-alive is enabled. This is experimental.")
(defvar twittering-http-proxy-user nil
  "*Username for `twittering-http-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-http-proxy-password nil
  "*Password for `twittering-http-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")

(defvar twittering-https-proxy-server nil
  "*HTTPS proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
The port number is specified by `twittering-https-proxy-port'.
For HTTP connection, the proxy specified by `twittering-http-proxy-server'
and `twittering-http-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-https-proxy-port nil
  "*Port number of a HTTPS proxy server for `twittering-mode'.
If nil, it is initialized on entering `twittering-mode'.
The server is specified by `twittering-https-proxy-server'.
For HTTP connection, the proxy specified by `twittering-http-proxy-server'
and `twittering-http-proxy-port' is used.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-https-proxy-keep-alive nil
  "*If non-nil, the Keep-alive is enabled. This is experimental.")
(defvar twittering-https-proxy-user nil
  "*Username for `twittering-https-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")
(defvar twittering-https-proxy-password nil
  "*Password for `twittering-https-proxy-server'.

NOTE: If both `twittering-proxy-server' and `twittering-proxy-port' are
non-nil, the variables `twittering-proxy-*' have priority over other
variables `twittering-http-proxy-*' or `twittering-https-proxy-*'
regardless of HTTP or HTTPS.")

(defcustom twittering-uri-regexp-to-proxy ".*"
  "Matched uri will be retrieved via proxy.
See also `twittering-proxy-use', `twittering-proxy-server' and
`twittering-proxy-port', `twittering-curl-socks-proxy'.  "
  :type 'string
  :group 'twittering)

(defun twittering-proxy-use-match (uri)
  (and twittering-proxy-use
       (string-match twittering-uri-regexp-to-proxy uri)))

(defun twittering-normalize-proxy-vars ()
  "Normalize the type of `twittering-http-proxy-port' and
`twittering-https-proxy-port'."
  (mapc (lambda (sym)
          (let ((value (symbol-value sym)))
            (cond
             ((null value)
              nil)
             ((integerp value)
              nil)
             ((stringp value)
              (set sym (string-to-number value)))
             (t
              (set sym nil)))))
        '(twittering-proxy-port
          twittering-http-proxy-port
          twittering-https-proxy-port)))

(defun twittering-proxy-info (scheme &optional item)
  "Return an alist for proxy configuration registered for SCHEME.
SCHEME must be a string \"http\", \"https\" or a symbol 'http or 'https.
The server name is a string and the port number is an integer."
  (twittering-normalize-proxy-vars)
  (let ((scheme (if (symbolp scheme)
                    (symbol-name scheme)
                  scheme))
        (info-list
         `((("http" "https")
            . ((server . ,twittering-proxy-server)
               (port . ,twittering-proxy-port)
               (keep-alive . ,twittering-proxy-keep-alive)
               (user . ,twittering-proxy-user)
               (password . ,twittering-proxy-password)))
           (("http")
            . ((server . ,twittering-http-proxy-server)
               (port . ,twittering-http-proxy-port)
               (keep-alive . ,twittering-http-proxy-keep-alive)
               (user . ,twittering-http-proxy-user)
               (password . ,twittering-http-proxy-password)))
           (("https")
            . ((server . ,twittering-https-proxy-server)
               (port . ,twittering-https-proxy-port)
               (keep-alive . ,twittering-https-proxy-keep-alive)
               (user . ,twittering-https-proxy-user)
               (password . ,twittering-https-proxy-password))))))
    (let ((info
           (car (remove nil
                        (mapcar
                         (lambda (entry)
                           (when (member scheme (car entry))
                             (let ((info (cdr entry)))
                               (when (and (assqref 'server info)
                                          (assqref 'port info))
                                 info))))
                         info-list)))))
      (if item
          (assqref item info)
        info))))

(defun twittering-url-proxy-services ()
  "Return the current proxy configuration for `twittering-mode' in the format
of `url-proxy-services'."
  (remove nil (mapcar
               (lambda (scheme)
                 (let ((server (twittering-proxy-info scheme 'server))
                       (port (twittering-proxy-info scheme 'port)))
                   (when (and server port)
                     `(,scheme . ,(format "%s:%s" server port)))))
               '("http" "https"))))

(defun twittering-find-proxy (scheme)
  "Find proxy server and its port from the environmental variables and return
a cons pair of them.
SCHEME must be \"http\" or \"https\"."
  (cond
   ((require 'url-methods nil t)
    (url-scheme-register-proxy scheme)
    (let* ((proxy-service (assoc scheme url-proxy-services))
           (proxy (if proxy-service (cdr proxy-service) nil)))
      (if (and proxy
               (string-match "^\\([^:]+\\):\\([0-9]+\\)$" proxy))
          (let ((host (match-string 1 proxy))
                (port (string-to-number (match-string 2 proxy))))
            (cons host port))
        nil)))
   (t
    (let* ((env-var (concat scheme "_proxy"))
           (env-proxy (or (getenv (upcase env-var))
                          (getenv (downcase env-var))))
           (default-port (if (string= "https" scheme) "443" "80")))
      (if (and env-proxy
               (string-match
                "^\\(https?://\\)?\\([^:/]+\\)\\(:\\([0-9]+\\)\\)?/?$"
                env-proxy))
          (let* ((host (match-string 2 env-proxy))
                 (port-str (or (match-string 4 env-proxy) default-port))
                 (port (string-to-number port-str)))
            (cons host port))
        nil)))))

(defun twittering-setup-proxy ()
  (when (require 'url-methods nil t)
    ;; If `url-scheme-registry' is not initialized,
    ;; `url-proxy-services' will be reset by calling
    ;; `url-insert-file-contents' or `url-retrieve-synchronously', etc.
    ;; To avoid it, initialize `url-scheme-registry' by calling
    ;; `url-scheme-get-property' before calling such functions.
    (url-scheme-get-property "http" 'name)
    (url-scheme-get-property "https" 'name))
  (unless (and twittering-http-proxy-server
               twittering-http-proxy-port)
    (let ((info (twittering-find-proxy "http")))
      (setq twittering-http-proxy-server (car-safe info))
      (setq twittering-http-proxy-port (cdr-safe info))))
  (unless (and twittering-https-proxy-server
               twittering-https-proxy-port)
    (let ((info (twittering-find-proxy "https")))
      (setq twittering-https-proxy-server (car-safe info))
      (setq twittering-https-proxy-port (cdr-safe info))))
  (if (and twittering-proxy-use
           (null (twittering-proxy-info "http"))
           (null (twittering-proxy-info "https")))
      (progn
        (error "Proxy enabled, but lack of configuration"))
    t))

(defun twittering-toggle-proxy ()
  (interactive)
  ;; (setq twittering-proxy-use
  ;;       (not twittering-proxy-use))
  ;; (if (twittering-setup-proxy)
  ;;     (message (if twittering-proxy-use "Use Proxy:on" "Use Proxy:off")))
  ;; (twittering-update-mode-line)
  (error "Not implemented yet"))


;;;; URI shortening
;;;;

(defun twittering-tinyurl-get (longurl &optional service)
  "Shorten LONGURL with the service specified by `twittering-tinyurl-service'."
  (let* ((service (or service twittering-tinyurl-service))
         (api (cdr (assq service twittering-tinyurl-services-map)))
         (request-generator (when (listp api) (elt api 0)))
         (post-process (when (listp api) (elt api 1)))
         (encoded-url (twittering-percent-encode longurl))
         (request
          (cond
           ((stringp api)
            (twittering-make-http-request-from-uri
             "GET" nil (concat api encoded-url)))
           ((stringp request-generator)
            (twittering-make-http-request-from-uri
             "GET" nil (concat request-generator encoded-url)))
           ((functionp request-generator)
            (funcall request-generator service longurl))
           (t
            (error "%s is invalid. try one of %s"
                   (symbol-name service)
                   (mapconcat (lambda (x) (symbol-name (car x)))
                              twittering-tinyurl-services-map ", "))
            nil)))
         (additional-info `((longurl . ,longurl) (sync . t))))
    (cond
     ((null request)
      (error "Failed to generate a HTTP request for shortening %s with %s"
             longurl (symbol-name service))
      nil)
     (t
      (let (result)
        (twittering-send-http-request
         request additional-info
         (lambda (proc status connection-info header-info)
           (let ((status-line (assqref 'status-line header-info))
                 (status-code (assqref 'status-code header-info)))
             (case-string
              status-code
              (("200")
               (setq result (buffer-string))
               nil)
              (t
               (setq result nil)
               (format "Response: %s" status-line))))))
        (let ((processed-result (if (and result (functionp post-process))
                                    (funcall post-process service result)
                                  result)))
          (if processed-result
              processed-result
            (error "Failed to shorten a URL %s with %s"
                   longurl (symbol-name service))
            nil)))))))

(defun twittering-tinyurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (twittering-tinyurl-get (thing-at-point 'url))))
        (when url
          (save-restriction
            (narrow-to-region (car url-bounds) (cdr url-bounds))
            (delete-region (point-min) (point-max))
            (insert url)))))))

(defun twittering-make-http-request-for-bitly (service longurl)
  "Make a HTTP request for URL shortening service bit.ly or j.mp.
Before calling this, you have to configure `twittering-bitly-login' and
`twittering-bitly-api-key'."
  (let* ((query-string
          (mapconcat
           (lambda (entry)
             (concat (car entry) "=" (cdr entry)))
           `(("login" . ,twittering-bitly-login)
             ("apiKey" . ,twittering-bitly-api-key)
             ("format" . "txt")
             ("longUrl" . ,(twittering-percent-encode longurl)))
           "&"))
         (prefix
          (cdr (assq service '((bit.ly . "http://api.bit.ly/v3/shorten?")
                               (j.mp . "http://api.j.mp/v3/shorten?")))))
         (uri (concat prefix query-string)))
    (twittering-make-http-request-from-uri "GET" nil uri)))


;;;; Asynchronous retrieval
;;;;

(defvar twittering-url-data-hash (make-hash-table :test 'equal))
(defvar twittering-url-request-list nil)
(defvar twittering-url-request-sentinel-hash (make-hash-table :test 'equal))
(defvar twittering-internal-url-queue nil)
(defvar twittering-url-request-resolving-p nil)
(defvar twittering-url-request-retry-limit 3)
(defvar twittering-url-request-sentinel-delay 1.0
  "*Delay from completing retrieval to invoking associated sentinels.
Sentinels registered by `twittering-url-retrieve-async' will be invoked
after retrieval is completed and Emacs remains idle a certain time, which
this variable specifies. The unit is second.")

(defun twittering-remove-redundant-queries (queue)
  (remove nil
          (mapcar
           (lambda (url)
             (let ((current (gethash url twittering-url-data-hash)))
               (when (or (null current)
                         (and (integerp current)
                              (< current twittering-url-request-retry-limit)))
                 url)))
           (twittering-remove-duplicates queue))))

(defun twittering-resolve-url-request ()
  "Resolve requests of asynchronous URL retrieval."
  (unless twittering-url-request-resolving-p
    (setq twittering-url-request-resolving-p t)
    ;; It is assumed that the following part is not processed
    ;; in parallel.
    (setq twittering-internal-url-queue
          (append twittering-internal-url-queue twittering-url-request-list))
    (setq twittering-url-request-list nil)
    (setq twittering-internal-url-queue
          (twittering-remove-redundant-queries twittering-internal-url-queue))
    (if (null twittering-internal-url-queue)
        (setq twittering-url-request-resolving-p nil)
      (let* ((url (or (find-if-not   ; Get Sina Weibo emotions with low priority
                       (lambda (i) (string-match (regexp-opt '("img.t.sinajs.cn")) i))
                       twittering-internal-url-queue)
                      (car twittering-internal-url-queue)))
             (request (twittering-make-http-request-from-uri "GET" nil url))
             (additional-info `((uri . ,url))))
        (twittering-send-http-request
         request additional-info
         'twittering-url-retrieve-async-sentinel
         'twittering-url-retrieve-async-clean-up-sentinel)))))

(defun twittering-url-retrieve-async-sentinel (proc status connection-info header-info)
  (let ((status-line (assqref 'status-line header-info))
        (status-code (assqref 'status-code header-info))
        (uri (assqref 'uri (assq 'request connection-info))))
    (when (string= status-code "200")
      (let ((body (string-as-unibyte (buffer-string))))
        (puthash uri body twittering-url-data-hash)
        (setq twittering-internal-url-queue
              (remove uri twittering-internal-url-queue))
        (let ((sentinels (gethash uri twittering-url-request-sentinel-hash)))
          (when sentinels
            (remhash uri twittering-url-request-sentinel-hash))
          (twittering-run-on-idle twittering-url-request-sentinel-delay
                                  (lambda (sentinels uri body)
                                    (mapc (lambda (func)
                                            (funcall func uri body))
                                          sentinels)
                                    ;; Resolve the rest of requests.
                                    (setq twittering-url-request-resolving-p
                                          nil)
                                    (twittering-resolve-url-request))
                                  sentinels uri body)
          ;;  Without the following nil, it seems that the value of
          ;; `sentinels' is displayed.
          nil)))))

(defun twittering-url-retrieve-async-clean-up-sentinel (proc status connection-info)
  (when (memq status '(exit signal closed failed))
    (let* ((uri (assqref 'uri connection-info))
           (current (gethash uri twittering-url-data-hash)))
      (when (or (null current) (integerp current))
        ;; Increment the counter on failure and then retry retrieval.
        (puthash uri (1+ (or current 0)) twittering-url-data-hash)
        (setq twittering-url-request-resolving-p nil)
        (twittering-resolve-url-request)))))

(defun twittering-url-retrieve-async (url &optional sentinel)
  "Retrieve URL asynchronously and call SENTINEL with the retrieved data.
The request is placed at the last of queries queue. When the data has been
retrieved and Emacs remains idle a certain time specified by
`twittering-url-request-sentinel-delay', SENTINEL will be called as
 (funcall SENTINEL URL url-data).
The retrieved data can be referred as (gethash URL twittering-url-data-hash)."
  (let ((data (gethash url twittering-url-data-hash)))
    (cond
     ((or (null data) (integerp data))
      (add-to-list 'twittering-url-request-list url t)
      (when sentinel
        (let ((current (gethash url twittering-url-request-sentinel-hash)))
          (unless (member sentinel current)
            (puthash url (cons sentinel current)
                     twittering-url-request-sentinel-hash))))
      (twittering-resolve-url-request)
      nil)
     (t
      ;; URL has been already retrieved.
      (when sentinel
        (twittering-run-on-idle twittering-url-request-sentinel-delay
                                sentinel url data))
      data))))

;;; ============================================= API interface

;;;; Abstract layer for Twitter API

(defun twittering-api-path (&rest params)
  (mapconcat 'identity `(,(twittering-lookup-service-method-table 'api-prefix)
                         ,@params) ""))

(defun twittering-call-api (command args-alist &optional additional-info)
  "Call Twitter API and return the process object for the request.
COMMAND is a symbol specifying API. ARGS-ALIST is an alist specifying
arguments for the API corresponding to COMMAND. Each key of ARGS-ALIST is a
symbol.
ADDITIONAL-INFO is used as an argument ADDITIONAL-INFO of
`twittering-send-http-request'. Sentinels associated to the returned process
receives it as the fourth argument. See also the function
`twittering-send-http-request'.

The valid symbols as COMMAND follows:
retrieve-timeline -- Retrieve a timeline.
  Valid key symbols in ARGS-ALIST:
    timeline-spec -- the timeline spec to be retrieved.
    timeline-spec-string -- the string representation of the timeline spec.
    number -- (optional) how many tweets are retrieved. It must be an integer.
      If nil, `twittering-number-of-tweets-on-retrieval' is used instead.
      The maximum for search timeline is 100, and that for other timelines is
      `twittering-max-number-of-tweets-on-retrieval'.
      If the given number exceeds the maximum, the maximum is used instead.
    max-id -- (optional) the maximum ID of retrieved tweets.
    since-id -- (optional) the minimum ID of retrieved tweets.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request' via
      `twittering-http-get'.
    page -- (optional and valid only for favorites timeline) which page will
      be retrieved.
get-list-index -- Retrieve list names owned by a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username.
    sentinel -- the sentinel that processes retrieved strings. This is used
      as an argument SENTINEL of `twittering-send-http-request'
      via `twittering-http-get'.
    clean-up-sentinel -- (optional) the clean-up sentinel that post-processes
      the buffer associated to the process. This is used as an argument
      CLEAN-UP-SENTINEL of `twittering-send-http-request' via
      `twittering-http-get'.
create-friendships -- Follow a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username which will be followed.
destroy-friendships -- Unfollow a user.
  Valid key symbols in ARGS-ALIST:
    username -- the username which will be unfollowed.
create-favorites -- Mark a tweet as a favorite.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
destroy-favorites -- Remove a mark of a tweet as a favorite.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
update-status -- Post a tweet.
  Valid key symbols in ARGS-ALIST:
    status -- the string to be posted.
    in-reply-to-status-id -- (optional) the ID of a status that this post is
      in reply to.
destroy-status -- Destroy a tweet posted by the authenticated user itself.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
retweet -- Retweet a tweet.
  Valid key symbols in ARGS-ALIST:
    id -- the ID of the target tweet.
verify-credentials -- Verify the current credentials.
  Valid key symbols in ARGS-ALIST:
    sentinel -- the sentinel that processes returned information. This is used
      as an argument SENTINEL of `twittering-send-http-request'
      via `twittering-http-get'.
    clean-up-sentinel -- the clean-up sentinel that post-processes the buffer
      associated to the process. This is used as an argument CLEAN-UP-SENTINEL
      of `twittering-send-http-request' via `twittering-http-get'.
send-direct-message -- Send a direct message.
  Valid key symbols in ARGS-ALIST:
    username -- the username who the message is sent to.
    status -- the sent message.
block -- Block a user.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be blocked.
    username -- the username who will be blocked.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API.
block-and-report-as-spammer -- Block a user and report him or her as a spammer.
  Valid key symbols in ARGS-ALIST:
    user-id -- the user-id that will be blocked.
    username -- the username who will be blocked.
  This command requires either of the above key. If both are given, `user-id'
  will be used in REST API."
  (unless (assqref 'timeline-spec additional-info)
    (add-to-list 'additional-info
                 `(timeline-spec . ,(twittering-current-timeline-spec))))
  (unless (assqref 'timeline-spec-string additional-info)
    (add-to-list 'additional-info
                 `(timeline-spec-string . ,(twittering-current-timeline-spec-string))))

  (let* ((api-host (twittering-lookup-service-method-table 'api))
         (spec     (assqref 'timeline-spec args-alist))
         (id       (assqref 'id            args-alist))
         (username (assqref 'username      args-alist))
         (sentinel (assqref 'sentinel      args-alist))
         (cursor   (assqref 'cursor        args-alist))
         (service  (twittering-extract-service))
         (sina? (eq service 'sina))
         (douban? (eq service 'douban))
         (username-str (if sina? "uid" "screen_name")))
    (case command
      ((retrieve-timeline)
       (let* ((spec-string (assqref 'timeline-spec-string args-alist))
              (spec-type (cadr spec))
              (max-number (if (eq spec-type 'search)
                              twittering-max-number-of-tweets-on-search
                            twittering-max-number-of-tweets-on-retrieval))
              ;; common parameters
              (count (number-to-string
                      (min twittering-number-of-tweets-on-retrieval max-number)))
              (max-id (assqref 'max-id args-alist))
              (since-id (assqref 'since-id args-alist))

              (parameters
               (remove-if
                (lambda (el) (null (cdr el)))
                (case service
                  ((douban)
                   `(("max-results" . ,count)
                     ;; ("start-index" . ,since-id) ; buggy douban.
                     ("alt"         . "json")
                     ("apikey"      . ,(twittering-lookup-service-method-table 'oauth-consumer-key))))

                  ((socialcast)
                   `(("since"    . ,since-id)
                     ("per_page" . ,count)

                     ("comments_limit"      . "0")
                     ("likes_limit"         . "0")
                     ("comment_likes_limit" . "0")))

                  (t
                   `(,(if max-id        ; max-id and since-id can't coexist.
                          `("max_id" . ,max-id)
                        `("since_id" . ,since-id))
                     ("cursor" . ,(assqref 'cursor args-alist))
                     ,@(case spec-type
                         ((search)
                          `(("q" . ,(assqref 'word args-alist))
                            ("rpp" . ,count)))
                         ((list)
                          (let ((username (elt (cdr spec) 1))
                                (list-name (elt (cdr spec) 2)))
                            (if (member list-name '("following" "followers"))
                                `(("count" . ,count)
                                  ("screen_name" . ,username))
                              `(("per_page" . ,count)))))
                         ((user friends mentions public)
                          `(("count" . ,count)
                            ("include_rts" . "true")))
                         (t
                          `(("count" . ,count)))))))))

              (host (if (eq spec-type 'search)
                        (twittering-lookup-service-method-table 'search)
                      api-host))

              (method
               (case service
                 ((douban)
                  (format "people/%s/miniblog/contacts"
                          (assqref 'douban-user-id args-alist)))
                 (t
                  (case spec-type
                    ((user)
                     (let ((username (elt (cdr spec) 1)))
                       (if (eq service 'sina)
                           (progn
                             (setq parameters `(,@parameters ("screen_name" . ,username)))
                             (twittering-api-path "statuses/user_timeline"))
                         (twittering-api-path "statuses/user_timeline/" username))))
                    ((list)
                     (let ((u (elt (cdr spec) 1))
                           (l (elt (cdr spec) 2)))
                       (twittering-api-path
                        (cond
                         ((string= l "followers") "statuses/followers")
                         ((string= l "following") "statuses/friends")
                         ((string= l "favorites") (if (eq service 'sina)
                                                      "favorites"
                                                    (concat "favorites/" u)))
                         (t (concat "" u "/lists/" l "/statuses"))))))
                    ((search)
                     (twittering-lookup-service-method-table 'search-method))
                    (t
                     (when (twittering-get-simple-spec-method spec)
                       (twittering-api-path (twittering-get-simple-spec-method spec))))))))

              (clean-up-sentinel (assqref 'clean-up-sentinel args-alist)))
         (if (and host method)
             (twittering-http-get
              host method parameters additional-info nil clean-up-sentinel)
           (error "Invalid timeline spec: %S" spec))))

      ;; List methods
      ((get-list-index)                        ; Get list names.
       (let ((clean-up-sentinel (assqref 'clean-up-sentinel args-alist)))
         (twittering-http-get api-host
                              (twittering-api-path username "/lists")
                              nil additional-info sentinel clean-up-sentinel)))
      ((get-list-subscriptions)
       (twittering-http-get api-host
                            (twittering-api-path username "/lists/subscriptions")
                            nil additional-info sentinel))

      ((get-list-memberships)
       (twittering-http-get api-host
                            (twittering-api-path username "/lists/memberships")
                            nil additional-info sentinel))

      ;; Friendship Methods
      ((create-friendships)
       (twittering-http-post api-host
                             (twittering-api-path "friendships/create")
                             `(("screen_name" . ,username))
                             additional-info))
      ((destroy-friendships)
       (twittering-http-post api-host
                             (twittering-api-path "friendships/destroy")
                             `(("screen_name" . ,username))
                             additional-info))
      ((show-friendships)
       (twittering-http-get api-host
                            (twittering-api-path "friendships/show")
                            `(("target_screen_name" . ,username))
                            additional-info
                            sentinel))

      ;; Favorite Methods
      ((create-favorites destroy-favorites)
       (let ((action (car (split-string (symbol-name command) "-")))
             method parameters)
         (case service
           ((sina)
            (setq method (format "favorites/%s" action)
                  parameters `(("id" . ,id))))
            (t
             (setq method (format "favorites/%s/%s" action id))))

         (twittering-http-post api-host (twittering-api-path method) parameters additional-info)))

      ;; List Subscribers Methods
      ((subscribe-list)
       (twittering-http-post api-host
                             (twittering-api-path
                              (apply 'format "%s/%s/subscribers" (cddr spec)))
                             nil additional-info))
      ((unsubscribe-list)
       (twittering-http-post api-host
                             (twittering-api-path
                              (apply 'format "%s/%s/subscribers" (cddr spec)))
                             '(("_method" . "DELETE"))
                             additional-info))

      ;; List Members Methods
      ((add-list-members)
       (twittering-http-post api-host
                             (twittering-api-path
                              (apply 'format "%s/%s/members" (cddr spec)))
                             `(("id" . ,id))
                             additional-info))
      ((delete-list-members)
       (twittering-http-post api-host
                             (twittering-api-path
                              (apply 'format "%s/%s/members" (cddr spec)))
                             `(("id" . ,id)
                               ("_method" . "DELETE"))
                             additional-info))

      ((update-status)
       (let* ((status (assqref 'status args-alist))
              (id (assqref 'in-reply-to-status-id args-alist))
              (retweeting? (assqref 'retweeting? args-alist))
              (st (twittering-find-status id))
              (in-reply-to-status (assqref 'status st))
              (quoted-id (assqref 'id in-reply-to-status))
              (retweeted-status  (assqref 'retweeted-status st))
              comment? method parameters)

         (case service
           ((sina)
            (setq comment? (and id (not retweeting?)))
            (setq method (unless comment? "statuses/update"))
            (setq parameters
                  `((,(if comment? "comment" "status") . ,status)
                    ,@(when id
                        (cond
                         (quoted-id     ; comment to comment
                          (setq method "comments/reply")
                          `(("id" . ,quoted-id)
                            ("cid" . ,id)))
                         (retweeting?   ; retweet with comments
                          (setq method "statuses/repost")
                          `(("id" . ,id)))
                         (t             ; comment
                          (setq method "comments/create")
                          `(("id" . ,id))))))))

           ((socialcast)
            (setq comment? id)
            (if comment?
                (setq method (format "messages/%s/comments" id)
                      parameters `(("comment[text]" . ,status)))
              (setq method "messages" ;; TITLE\n\nBODY
                    parameters (let* ((s (split-string status "\n\n"))
                                      (title "")
                                      (body status))
                                 (when (> (length s) 1)
                                   (setq title (car s)
                                         body (mapconcat 'identity ,(cdr s) "\n\n")))
                                 `(("message[title]" . ,title)
                                   ("message[body]" . ,body))))))

           ((douban)
            (setq method (if id
                             (let ((detail (assqref 'detail st)))
                               (cond
                                ((not detail)
                                 (format "miniblog/%s/comments" id))
                                ((string-match "/\\(recommendation/[0-9]+\\)" detail)
                                 (concat (match-string 1 detail) "/comments" ))
                                (t
                                 (error "Can not comment"))))
                           "miniblog/saying"))
            (setq parameters `(("status" . ,status))))

           ((twitter)
            (setq method "statuses/update")
            (setq parameters `(("status" . ,status)
                               ,@(when id
                                   `(("in_reply_to_status_id" . ,id)))
                               ,@(when (eq (twittering-get-accounts 'auth) 'basic)
                                   '(("source" . "twmode")))))))

         (setq method (twittering-api-path method))
         (twittering-http-post api-host method parameters additional-info)))

      ((destroy-status)
       (twittering-http-post api-host
                             (twittering-api-path "statuses/destroy" id)
                             nil
                             additional-info))
      ((retweet)
       (let ((parameters '()))
         (case service
           ((sina)
            (setq method "statuses/repost"
                  parameters `(("id" . ,id))))
           (t
            (setq method (concat "statuses/retweet/" id))))

         (twittering-http-post api-host
                               (twittering-api-path method)
                               parameters
                               additional-info)))
      
      ;; Account Resources
      ((verify-credentials)
       (let ((clean-up-sentinel (assqref 'clean-up-sentinel args-alist)))
         (case service
           ((socialcast)
            (twittering-http-post api-host
                                  (twittering-api-path "authentication")
                                  `(("email"    . ,(twittering-get-accounts 'username))
                                    ("password" . ,(twittering-get-accounts 'password)))
                                  additional-info sentinel clean-up-sentinel))
           (t
            (twittering-http-get api-host
                                 (twittering-api-path "account/verify_credentials")
                                 nil additional-info sentinel clean-up-sentinel)))))

      ((update-profile-image)
       (let* ((image (assqref 'image args-alist))
              (image-type (image-type-from-file-header image)))
         (twittering-http-post api-host
                               (twittering-api-path "account/update_profile_image")
                               `(("image" . ,(format "@%s;type=image/%s" image image-type)))
                               additional-info)))

      ((send-direct-message)
       ;; Send a direct message.
       (let ((parameters
              `(("screen_name" . ,(assqref 'username args-alist))
                ("text" . ,(assqref 'status args-alist)))))
         (twittering-http-post api-host
                               (twittering-api-path "direct_messages/new")
                               parameters
                               additional-info)))

      ;; Get a single tweet
      ((show)
       (let (path parameters)
         (case service
           ((sina)
            (setq path "comments/show"
                  parameters `(("id" . ,id)
                               ("count" . "200"))))
           (t
            (setq path (concat "statuses/show/" id))))
         (twittering-http-get api-host
                              (twittering-api-path path)
                              parameters
                              `(,@additional-info (command . show)))))
      ;; Get user info
      ((show-user)
       (twittering-http-get api-host
                            (twittering-api-path "users/show")
                            `((,username-str . ,username))
                            additional-info
                            sentinel))

      ((show-friends show-followers)
       (twittering-http-get api-host
                            (twittering-api-path
                             (assqref service
                                      `((twitter . ,(concat "statuses/"
                                                            (replace-regexp-in-string
                                                             "show-" "" (symbol-name command))))
                                        (sina . ,(concat "friendships/"
                                                         (replace-regexp-in-string
                                                          "show-" "" (symbol-name command)))))))
                            `((,username-str . ,username)
                              ("count" . "200")
                              ("cursor" . ,cursor))
                            additional-info
                            sentinel))

      ((block)
       ;; Block a user.
       (let* ((user-id (assqref 'user-id args-alist))
              (parameters (if user-id
                              `(("user_id" . ,user-id))
                            `(("screen_name" . ,username)))))
         (twittering-http-post api-host
                               (twittering-api-path "blocks/create")
                               parameters
                               additional-info)))

      ((block-and-report-as-spammer)
       ;; Report a user as a spammer and block him or her.
       (let* ((user-id (cdr (assq 'user-id args-alist)))
              (parameters (if user-id
                              `(("user_id" . ,user-id))
                            `(("screen_name" . ,username)))))
         (twittering-http-post api-host
                               (twittering-api-path "report_spam")
                               parameters
                               additional-info)))
      ;; Twitter Stream API
      ;; ------------------
      ((userstream)
       (let ((twittering-accounts       ; FIXME: why twitter enforce ssl for stream?
              `((twitter
                 ,@(remove-if (lambda (i) (eq (car i) 'ssl))
                              (assqref 'twitter twittering-accounts))
                 (ssl t))

                ,@(remove-if (lambda (i) (eq (car i) 'twitter)) twittering-accounts))))
         (twittering-http-get
          (twittering-lookup-service-method-table 'userstream)
          "2/user" nil `(,@additional-info (stream . t)))))

      ((stream)
       (let ((twittering-accounts   ; FIXME: why twitter enforce ssl for stream?
              `((twitter
                 ,@(remove-if (lambda (i) (eq (car i) 'ssl))
                              (assqref 'twitter twittering-accounts))
                 (ssl nil))

                ,@(remove-if (lambda (i) (eq (car i) 'twitter))
                             twittering-accounts)))

             (predicates
              (let (value)
                (while (not value)
                  (let* ((parameters '("follow" "track" "locations" "annotations"))
                         (p (completing-read "Select parameter to set: "
                                             `(,@parameters "MULTIPLE"))))
                    (if (equal (downcase p) "multiple")
                        (setq value
                              (mapcar (lambda (i) `(,i . ,(read-string (format "Set `%s' to: " i))))
                                      parameters))
                      (setq value `((,p . ,(read-string (format "Set `%s' to: " p))))))
                    (setq value (remove-if (lambda (i) (equal (cdr i) "")) value))))
                value)))
         (twittering-http-post (twittering-lookup-service-method-table 'stream)
                               "1/statuses/filter"
                               predicates
                               `(,@additional-info (stream . t)))))

      ;; Sina Weibo Specific Methods
      ;; ---------------------------
      ((counts)
       (twittering-http-get api-host
                            (twittering-api-path "statuses/count")
                            ;; USERNAME treated as IDS here
                            `(("ids" . ,(assqref 'ids args-alist)))
                            additional-info
                            sentinel))

      ((emotions)
       (twittering-http-get api-host
                            (twittering-api-path "emotions")
                            nil
                            additional-info
                            sentinel))

      ;; mid<->id
      ;;   http://api.weibo.com/queryid.json?mid=5KD0TZiyL24&isBase62=1&type=1
      ;;   http://api.weibo.com/querymid.json?id=6401236707&type=1
      ;; the other way (id->mid)
      ;;   http://api.weibo.com/$uid/statuses/$tid?s=6cm7D0"

      ;; ((get-tweet-url)                        ; FIXME: doesn't work??
      ;;  (twittering-http-get (twittering-lookup-service-method-table 'api)
      ;;                             (twittering-api-path username "/statuses/" id)
      ;;                             nil
      ;;                             additional-info
      ;;                             sentinel))

      ;; ((query-mid)
      ;;  (twittering-http-get (twittering-lookup-service-method-table 'api)
      ;;                             (twittering-api-path "querymid")
      ;;                             `(("id" . ,id)
      ;;                               ("type" . "1"))
      ;;                             additional-info
      ;;                             sentinel))
      ;; ((query-id)
      ;;  (twittering-http-get (twittering-lookup-service-method-table 'api)
      ;;                             (twittering-api-path "id")
      ;;                             `(("id" . ,id)
      ;;                               ("type" . "1")
      ;;                               ("isBase62" . "1"))
      ;;                             additional-info
      ;;                             sentinel))

      )))

(defun twittering-get-simple-spec-method (spec)
  (let ((service (twittering-extract-service spec))
        (spec-type (cadr spec)))
    (assqref
     spec-type
     (case service
       ((socialcast)
        '((home        . "messages")
          (mentions    . "streams/to_me/messages")
          (public      . "streams/company/messages")
          (recommended . "streams/recommended/messages")))
       (t
        `((direct_messages      . "direct_messages")
          (direct_messages_sent . "direct_messages/sent")

          (friends         . "statuses/friends_timeline")
          (home            . "statuses/home_timeline")
          (mentions        . ,(if (eq service 'sina)
                                  "statuses/mentions"
                                "statuses/mentions_timeline"))
          (public          . "statuses/public_timeline")
          (replies         . ,(if (eq service 'sina)
                                  "comments/timeline"
                                "statuses/mentions_timeline"))
          (retweeted_by_me . "statuses/retweeted_by_me")
          (retweeted_to_me . "statuses/retweeted_to_me")
          (retweets_of_me  . "statuses/retweets_of_me")

          (search . "search")))))))

;;;; Other API methods

(defvar twittering-simple-hash (make-hash-table :test 'equal)
  "Hash table for storing results retrieved by `twittering-get-simple'.
The key is a list of username/id and method(such as get-list-index), the value is
string.")

(defconst twittering-counts-request-max 100
  "Max counts that could be retrieved in one api call.")
(defvar twittering-counts-last-timestamp nil)

(defun twittering-get-simple (beg end username method)
  (let ((retrieved (gethash (list username method) twittering-simple-hash))
        (s (if (and (eq method 'counts) beg end)
               (buffer-substring beg end)
             (copy-sequence twittering-need-to-be-updated-indicator))))
    (cond
     (retrieved
      (remove-text-properties 0 (length s) '(need-to-be-updated nil) s)
      (case method
        ((show-friendships)
         (let ((target (or (assqref 'target retrieved)
                           (assqref 'target (assqref 'relationship retrieved)))))
           (setq s (concat (if (assqref 'followed-by target) " <" " ")
                           "--"
                           (if (assqref 'following target) ">" "")
                           " you"))))

        ((get-list-index get-list-subscriptions get-list-memberships)
         (setq s (mapconcat (lambda (l) (concat "@" (twittering-decorate-listname l)))
                            retrieved
                            (concat "\n" (make-string
                                          (twittering-calculate-list-info-prefix-width
                                           username)
                                          ? )))))

        ((counts)
         (let* ((id (twittering-get-id-at beg))
                (st (twittering-find-status id))
                (pred (lambda (i) (equal (assqref 'id i) id)))
                (count (find-if pred retrieved)))
           (when count
             (let* ((rt (assqref 'reposts count))
                    (cm (assqref 'comments count))
                    (rt-str (if (equal rt "0") "" (concat "(" rt ")")))
                    (cm-str (if (equal cm "0") "" (concat "(" cm ")"))))
               (puthash `(,username ,method) (remove-if pred retrieved)
                        twittering-simple-hash)
               (setq s (concat
                        (format "轉發%s " rt-str)
                        (propertize (format "評論%s" cm-str)
                                    'mouse-face 'highlight
                                    'face 'twittering-uri-face
                                    'keymap twittering-mode-on-uri-map
                                    'uri (twittering-get-status-url
                                          (assqref 'id (assqref 'user st))
                                          (assqref 'id st))))))))
         ;; Do it forcefully when timer expires, since some status may be
         ;; deleted before it updates twittering-simple-hash.
         (when (and twittering-counts-last-timestamp
                    (> (time-to-seconds (time-since twittering-counts-last-timestamp))
                       twittering-timer-interval))
           (puthash `(,username ,method) nil twittering-simple-hash))

         (setq s (propertize s 'need-to-be-updated
                             `(twittering-get-simple ,username ,method))))
        (t
         (setq s retrieved))))

     (t
      (setq s (propertize s 'need-to-be-updated
                          `(twittering-get-simple ,username ,method)))

      (if (eq method 'counts)
          (when (or (not twittering-counts-last-timestamp)
                    (> (time-to-seconds (time-since twittering-counts-last-timestamp))
                       twittering-timer-interval))
            (setq twittering-counts-last-timestamp (current-time))
            (let (ids)
              ;; Collect visible statuses' ids.
              (mapc (lambda (buf)
                      (with-current-buffer buf
                        (save-excursion
                          (goto-char (point-min))
                          (when (twittering-get-id-at)
                            (add-to-list 'ids (twittering-get-id-at)))
                          (while (twittering-get-next-status-head)
                            (goto-char (twittering-get-next-status-head))
                            (add-to-list 'ids (twittering-get-id-at))))))
                    (remove-if-not (lambda (buf)
                                     (with-current-buffer buf
                                       (eq (twittering-extract-service) 'sina)))
                                   (twittering-get-active-buffer-list)))
              ;; Process 80% latest, 20% random old.
              (when ids
                (setq ids (sort ids 'twittering-status-id>))
                (let* ((len (length ids))
                       (n (min len (round (* twittering-counts-request-max 0.8))))
                       (new (twittering-take n ids))
                       (m (- (min len twittering-counts-request-max) n))
                       old)
                  (dotimes (i m)
                    (setq old `(,(elt ids (+ (random (- m i)) n)) ,@old)))
                  (setq ids `(,@new ,@old)))
                (twittering-get-simple-1 method `((username . ,username)
                                                  (ids . ,(mapconcat 'identity ids ",")))))))
        (twittering-get-simple-1 method `((username . ,username))))))

    s))

(defun twittering-get-simple-sync (method args-alist)
  (setq twittering-get-simple-retrieved nil)
  (twittering-get-simple-1 method args-alist '((sync . t)))
  (if (not twittering-get-simple-retrieved)
      (progn
        (setq twittering-get-simple-retrieved 'error)
        (message "twittering-get-simple-sync failed"))
    (case method
      ((get-list-index get-list-subscriptions get-list-memberships)
       (cond
        ((stringp twittering-get-simple-retrieved)
         (if (string= "" twittering-get-simple-retrieved)
             (error
              "%s does not have a %s."
              (assqref 'username args-alist)
              (assqref method
                       '((get-list-index         . "list")
                         (get-list-subscriptions . "list subscription")
                         (get-list-memberships   . "list membership"))))
           (message "%s" twittering-get-simple-retrieved))
         nil)
        ((listp twittering-get-simple-retrieved)
         twittering-get-simple-retrieved)))
      ((emotions)
       (setq twittering-emotions-phrase-url-alist
             twittering-get-simple-retrieved))
      ((show-user)
       (car twittering-get-simple-retrieved))
      (t
       twittering-get-simple-retrieved))))

(defun twittering-get-simple-1 (method args-alist &optional additional-info)
  (twittering-call-api
   method
   `(,@args-alist
     (sentinel . (lambda (&rest args)
                   (apply 'twittering-http-get-simple-sentinel
                          (append args '(((method . ,method)
                                          ,@args-alist)))))))
   additional-info))

(defun twittering-followed-by-p (username)
  (twittering-get-simple-sync 'show-friendships `((username . ,username)))
  (and (stringp twittering-get-simple-retrieved)
       (string= twittering-get-simple-retrieved "true")))



;;; ============================================= Operations on tweet

;;;; Predicates

(defun twittering-my-status-p (status)
  "Is STATUS sent by myself? "
  (or (equal (twittering-get-accounts 'username)
             (assqref 'screen-name (assqref 'user status)))
      (equal (assocref "user_id" (twittering-lookup-oauth-access-token-alist))
             (assqref 'id (assqref 'user status)))))

(defun twittering-is-replies-p (status)
  (or (assqref 'reply-comment status)
      (assqref 'status status)))

(defun twittering-is-retweet? (status)
  (assqref 'retweeted-status status))

(defun twittering-status-has-quotation? (status)
  "Return quoted status if STATUS is retweeting or replying."
  (or (assqref 'retweeted-status status)
      (assqref 'reply-comment status)
      (assqref 'status status)))


;;;; URIs related to a tweet

(defun twittering-get-status-url (username &optional id)
  "Generate a URL of a user or a specific status."
  (let ((scheme ;; (if (twittering-get-accounts 'ssl) "https" "http"))
         "http")
        (path (funcall (twittering-lookup-service-method-table 'status-url)
                       username id)))
    (concat scheme "://" path)))

(defun twittering-get-status-url-twitter (username &optional id)
  "Generate status URL for Twitter."
  (let ((web (twittering-lookup-service-method-table 'web)))
    (if id
        (format "%s/%s/status/%s" web username id)
      (format "%s/%s" web username))))

(defun twittering-get-status-url-statusnet (username &optional id)
  "Generate status URL for StatusNet."
  (let ((web (twittering-lookup-service-method-table 'web))
        (web-prefix (twittering-lookup-service-method-table 'web-prefix)))
    (if id
        (format "%s/%s/notice/%s" web web-prefix id)
      (format "%s/%s/%s" web web-prefix username))))

(defun twittering-get-status-url-sina (username &optional id)
  "Generate status URL for Sina."
  (let ((web (twittering-lookup-service-method-table 'web))
        (api (twittering-lookup-service-method-table 'api)))
    (if id
        ;; (twittering-get-simple-sync 'query-mid
        ;;                                   `((username . ,username)
        ;;                                     (id . ,id)))
        ;; (format "%s/%s/statuses/%s?s=6cm7D0" api username id) ; oauth 1.0
        (ignore-errors                  ; ignore deleted status
          (format "%s/%s/%s" web username
                  (mapconcat (lambda (s)
                               (apply 'string (reverse (string-to-list s))))
                             (mapcar 'twittering-int-to-radix62
                                     (mapcar 'string-to-int
                                             `(,(substring id 0 -14)
                                               ,(substring id -14 -7)
                                               ,(substring id -7))))
                             "")))
      (format "%s/%s" web username))))

(defun twittering-get-status-url-douban (username &optional id)
  (let ((web (twittering-lookup-service-method-table 'web)))
    (if id
        web
      (format "%s/people/%s" web username))))

(defun twittering-get-status-url-socialcast (username &optional id)
  (let ((web (twittering-lookup-service-method-table 'web)))
    (if id
        (format "%s/messages/%s" web id)
      (format "%s/users/%s" web username))))

(defun twittering-get-search-url (query-string)
  "Generate a URL for searching QUERY-STRING."
  (let ((scheme (if (twittering-get-accounts 'ssl) "https" "http"))
        (path (funcall (twittering-lookup-service-method-table 'search-url)
                       query-string)))
    (concat scheme "://" path)))

(defun twittering-get-search-url-twitter (query-string)
  (format "%s/search?q=%s"
          (twittering-lookup-service-method-table 'web)
          (twittering-percent-encode query-string)))

(defun twittering-get-search-url-statusnet (query-string)
  (let ((web (twittering-lookup-service-method-table 'web))
        (web-prefix (twittering-lookup-service-method-table 'web-prefix)))
    (if (string-match "^#\\(.+\\)" query-string)
        (format "%s/%s/tag/%s"
                web
                web-prefix
                (twittering-percent-encode (match-string 1 query-string)))
      (format "%s/search?q=%s"
              web (twittering-percent-encode query-string)))))

;;; ============================================= Douban details

(defun twittering-make-douban-detail-string (beg end detail-url &optional html)
  (let* ((detail-data (gethash detail-url twittering-url-data-hash))
         (properties (and beg (text-properties-at beg)))
         (detail (apply 'propertize
                        (copy-sequence twittering-need-to-be-updated-indicator)
                        properties)))

    ;;    (when properties
    ;;      (add-text-properties 0 (length detail) properties detail))
    (cond
     (detail-data
      (let (json
            html-url
            get-photo)
        (with-temp-buffer
          (insert detail-data)
          (goto-char (point-min))
          (if html
              (when (re-search-forward "\\(http://img3.douban.com/view/photo/thumb/public/p[0-9]+\\.[^\"]+\\)\""
                                       nil t 1)
                (setq html-url (match-string 1)))
            (setq json (twittering-wash-json-douban
                        (ignore-errors (twittering-json-read))))))

        (if html
            (setq detail
                  (if html-url
                      (twittering-make-original-icon-string beg end html-url)
                    ""))
          (let ((type (when (string-match (regexp-opt
                                           '("note" "book" "movie" "music" "review"
                                             "collection" "event" "recommendation"))
                                          detail-url)
                        (intern (match-string 0 detail-url)))))
            (case type
              ((recommendation)
               (let* ((status (twittering-get-status-at-pos beg))
                      (text (assqref 'text status))
                      url)
                 (if text
                     (if (not (string-match "<a href=\"\\(http://www.douban.com/photos/album/[0-9]+/\\)\">"
                                            text))
                         (setq detail "")
                       (setq url (match-string 1 text))
                       (setq detail (concat detail detail)) ; should be different.
                       (put-text-property 0 (length detail)
                                          'need-to-be-updated
                                          `(twittering-make-douban-detail-string ,url t)
                                          detail)
                       (twittering-url-retrieve-async url)
                       (setq get-photo t))
                   (put-text-property 0 (length detail)
                                      'need-to-be-updated
                                      `(twittering-make-douban-detail-string ,detail-url)
                                      detail))))
              ((note)
               (setq detail (assqref 'summary json)))
              ((event)
               (setq detail
                     (apply 'format "时间：%s ~ %s\n地点：%s\n参数人数：%s"
                            `(,@(mapcar (lambda (i)
                                          (format-time-string
                                           "%Y-%m-%d %H:%M"
                                           ;; douban timezone bug?
                                           (date-to-time (replace-regexp-in-string
                                                          "+08:00" "+0800" i))))
                                        (let ((date (assqref 'gd:when json)))
                                          `(,(symbol-name (car date)) ,(cdr date))))
                              ,(assqref 'gd:where json)
                              ,(assqref 'participants (assqref 'db:attribute json))))))
              ((movie)
               (let* ((db:attribute (assqref 'db:attribute json))
                      (take (lambda (symbol)
                              (mapconcat
                               'cdr
                               (remove-if-not (lambda (i) (eq (car i) symbol))
                                              db:attribute)
                               " / "))))
                 (setq detail
                       (format "片名：%s\n评分：%s\n导演：%s\n主演：%s\n上映日期：%s"
                               (assqref 'title db:attribute)
                               (twittering-make-rating-string (nth 1 (assqref 'gd:rating json)))
                               (funcall take 'director)
                               (funcall take 'cast)
                               (assqref 'pubdate db:attribute)))))

              ((music)
               (let* ((db:attribute (assqref 'db:attribute json))
                      (title (assqref 'title db:attribute))
                      (singer (assqref 'singer db:attribute))
                      (rating (nth 1 (assqref 'gd:rating json)))
                      (version (assqref 'version db:attribute))
                      (publisher (assqref 'publisher db:attribute))
                      (pubdate (assqref 'pubdate db:attribute)))
                 (setq detail
                       (mapconcat (lambda (i) (mapconcat 'identity i "："))
                                  `(("曲名"    ,title)
                                    ("表演者"   ,singer)
                                    ("评分"     ,(twittering-make-rating-string rating))
                                    ("版本特性" ,version)
                                    ("发行时间" ,pubdate)
                                    ("出版者"   ,publisher))
                                  "\n"))))

              ((book)
               (let* ((db:attribute (assqref 'db:attribute json))
                      (title (assqref 'title db:attribute))
                      (author (assqref 'author db:attribute))
                      (rating (nth 1 (assqref 'gd:rating json)))
                      (publisher (assqref 'publisher db:attribute))
                      (pubdate (assqref 'pubdate db:attribute)))
                 ;; too few people, TODO: handle other possible empty item.
                 (unless (stringp rating)
                   (setq rating ""))
                 (setq detail
                       (mapconcat (lambda (i) (mapconcat 'identity i "："))
                                  `(("书名"    ,title)
                                    ("作者"   ,author)
                                    ("评分"     ,(twittering-make-rating-string rating))
                                    ("发行时间" ,pubdate)
                                    ("出版者"   ,publisher))
                                  "\n"))))

              ((review collection)
               (setq detail (assqref 'summary json))
               (unless detail           ; sounds like douban's bug.
                 (setq detail (car (assqref 'summary json)))))
              (t
               (setq detail "")))

            (unless get-photo
              (unless (string= detail "")
                (let ((prefix ""))
                  (when beg
                    (setq prefix (make-string
                                  (save-excursion (goto-char beg) (current-column)) ? )))
                  (setq prefix (concat prefix "    ") ; indent
                        detail (concat "    " detail))
                  (setq detail (replace-regexp-in-string "\r" "" detail)
                        detail (twittering-fill-string detail nil prefix t)
                        detail (substring detail (length prefix))
                        detail (apply 'propertize detail properties))
                  (remove-text-properties 0 (length detail)
                                          '(need-to-be-updated nil)
                                          detail))))))))
     (t
      (put-text-property 0 (length detail)
                         'need-to-be-updated
                         `(twittering-make-douban-detail-string ,detail-url)
                         detail)
      (twittering-url-retrieve-async detail-url)))

    detail))

(defun twittering-make-rating-string (rating)
  (when rating
    (when (stringp rating)
      (setq rating (string-to-number rating)))
    (concat (make-string (ceiling (/ rating 2.0)) ?★)
            (make-string (- 5 (ceiling (/ rating 2.0))) ?☆))))

;;; ============================================= Mode


;;;; Initialization
;;;;

(defvar twittering-mode-hook nil
  "Twittering-mode hook.")

(defvar twittering-initialized nil)
(defvar twittering-mode-syntax-table nil "")

(unless twittering-mode-syntax-table
  (setq twittering-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" twittering-mode-syntax-table)
  (modify-syntax-entry ?\" "w" twittering-mode-syntax-table)
  )

(defun twittering-initialize-global-variables-if-necessary ()
  "Initialize global variables for `twittering-mode' if they have not
been initialized yet."
  (unless twittering-initialized
    (defface twittering-username-face
      `((t ,(append ;; '(:underline t)
             (face-attr-construct
              (if (facep 'font-lock-string-face)
                  'font-lock-string-face
                'bold)))))
      "" :group 'faces)
    (defface twittering-uri-face `((t (:underline t))) "" :group 'faces)
    (twittering-update-status-format)
    (when twittering-use-convert
      (if (null twittering-convert-program)
          (setq twittering-use-convert nil)
        (with-temp-buffer
          (call-process twittering-convert-program nil (current-buffer) nil
                        "-version")
          (goto-char (point-min))
          (if (null (search-forward-regexp "\\(Image\\|Graphics\\)Magick"
                                           nil t))
              (setq twittering-use-convert nil)))))
    (twittering-setup-proxy)
    (when twittering-use-icon-storage
      (cond
       ((require 'jka-compr nil t)
        (twittering-load-icon-properties)
        (add-hook 'kill-emacs-hook 'twittering-save-icon-properties))
       (t
        (setq twittering-use-icon-storage nil)
        (error "Disabled icon-storage because it failed to load jka-compr."))))
    (setq twittering-initialized t)))

(defun twittering-mode-setup (spec-string)
  "Set up the current buffer for `twittering-mode'."
  (kill-all-local-variables)
  (setq major-mode 'twittering-mode)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq mode-name "twittering")
  (setq mode-line-buffer-identification
        `(,(default-value 'mode-line-buffer-identification)
          (:eval (twittering-mode-line-buffer-identification))))

  ;; Prevent `global-font-lock-mode' enabling `font-lock-mode'.
  ;; This technique is derived from `lisp/bs.el' distributed with Emacs 22.2.
  (make-local-variable 'font-lock-global-modes)
  (setq font-lock-global-modes '(not twittering-mode))

  (make-local-variable 'twittering-service-method)
  (make-local-variable 'twittering-timeline-spec)
  (make-local-variable 'twittering-timeline-spec-string)
  (make-local-variable 'twittering-active-mode)
  (make-local-variable 'twittering-icon-mode)
  (make-local-variable 'twittering-jojo-mode)
  (make-local-variable 'twittering-reverse-mode)
  (make-local-variable 'twittering-scroll-mode)
  (make-local-variable 'twittering-stream-mode)

  (setq twittering-service-method (twittering-extract-service spec-string))
  (setq twittering-timeline-spec-string spec-string)
  (setq twittering-timeline-spec
        (twittering-string-to-timeline-spec spec-string))
  (setq twittering-active-mode t)
  (use-local-map twittering-mode-map)
  (twittering-update-mode-line)
  (set-syntax-table twittering-mode-syntax-table)
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (font-lock-mode -1))
  (add-to-list 'twittering-buffer-info-list (current-buffer) t)
  (if twittering-auto-adjust-fill-column?
      (add-hook 'window-configuration-change-hook
                'twittering-window-configuration-change)
    (remove-hook 'window-configuration-change-hook
                 'twittering-window-configuration-change))
  (run-hooks 'twittering-mode-hook))

(defun twittering-mode ()
  "Major mode for Twitter.
\\{twittering-mode-map}"
  (interactive)
  (unless (file-exists-p "~/.emacs.d/twittering")
    (mkdir "~/.emacs.d/twittering" t))
  (twittering-cache-load)
  ;; Initial twittering-enabled-services first.
  (mapc (lambda (spec)
          (add-to-list 'twittering-enabled-services
                       (twittering-extract-service spec)))
        twittering-initial-timeline-spec-string)
  (mapc 'twittering-visit-timeline
        (if (listp twittering-initial-timeline-spec-string)
            twittering-initial-timeline-spec-string
          (list twittering-initial-timeline-spec-string))))

(defun twittering-window-configuration-change ()
  (when (and (eq major-mode 'twittering-mode)
             (not (twittering-replied-statuses-visible-p)))
    (let ((col (round (window-width)))
          (padding 8))
      (unless (= col (or twittering-fill-column 0))
        (setq twittering-fill-column col
              twittering-my-fill-column (- twittering-fill-column padding))
        (let ((data (twittering-timeline-data-collect))
              (from (save-excursion (goto-char (point-min))
                                    (twittering-get-id-at)))
              (to (save-excursion (goto-char (point-max))
                                  (twittering-get-id-at)))
              (curr (twittering-get-id-at)))
          (when from
            (when (twittering-status-id> from to)
              (setq from to))
            (setq data (remove-if
                        (lambda (i)
                          (twittering-status-id< (assqref 'id i) from))
                        (twittering-current-timeline-data)))
            (twittering-render-timeline (current-buffer) nil data)
            (twittering-jump-to-status curr)))))))

;;;; Account authorization
;;;;

(defun twittering-account-authorized-p ()
  (eq (twittering-get-account-authorization) 'authorized))
(defun twittering-account-authorization-queried-p ()
  (eq (twittering-get-account-authorization) 'queried))

(defun twittering-prepare-account-info ()
  "Return a pair of username and password from `twittering-accounts'.
If nil, read it from the minibuffer."
  (let* ((service (twittering-extract-service))
         (username (or (twittering-get-accounts 'username)
                       (read-string (format "your %S username: " service))))
         (password (or (twittering-get-accounts 'password)
                       (read-passwd (format "%s's %S password: " username service))))
         (token-alist `(,@(twittering-lookup-oauth-access-token-alist)
                        ("screen_name" . ,username)
                        ("password" .  ,password))))
    (twittering-update-oauth-access-token-alist token-alist)
    `(,username . ,password)))

(defun twittering-has-oauth-access-token-p ()
  (if (eq (twittering-get-accounts 'auth) 'basic)
      (assocref "password" (twittering-lookup-oauth-access-token-alist))
    (let ((required-entries '("oauth_token" "oauth_token_secret"))
          (any-entries '("user_id" "screen_name" "douban_user_id"))
          (pred (lambda (key)
                  (assocref key (twittering-lookup-oauth-access-token-alist)))))
      (and (every pred required-entries)
           (some pred any-entries)))))

(defvar twittering-private-info-file-loaded-p nil)
(defvar twittering-private-info-file-dirty nil)

(defun twittering-verify-credentials ()
  (let* ((service (twittering-extract-service))
         (scheme (if (twittering-get-accounts 'ssl) "https" "http")))
    (if (not (equal (twittering-get-accounts-internal 'oauth) 1.0))
        (progn
          (setq scheme "https")         ; oauth 2.0 requirement.  
          (when (or (assocref "access_token" (twittering-lookup-oauth-access-token-alist))
                    (let ((token (oauth2-auth-and-store
                                  (concat scheme (twittering-lookup-service-method-table
                                                  'oauth-authorization-url-base-without-scheme))
                                  (concat scheme (twittering-lookup-service-method-table
                                                  'oauth-access-token-url-without-scheme))
                                  ""
                                  (twittering-lookup-service-method-table 'oauth-consumer-key)
                                  (twittering-lookup-service-method-table 'oauth-consumer-secret)
                                  (base64-decode-string "aHR0cDovL3h3bC5hcHBzcG90LmNvbS9jYWxsYmFjaw=="))))
                      (twittering-update-oauth-access-token-alist
                       `(("access_token" . ,(oauth2-token-access-token token))
                         ("access-response" . ,(oauth2-token-access-response token))))))
            (twittering-update-account-authorization 'authorized)))
      (cond
       ((or (twittering-account-authorized-p)
            (twittering-account-authorization-queried-p))
        nil)
       ((and (memq (twittering-get-accounts 'auth) '(oauth xauth))
             (or (null (twittering-lookup-service-method-table 'oauth-consumer-key))
                 (null (twittering-lookup-service-method-table 'oauth-consumer-secret))))
        (message "Consumer for OAuth is not specified."))
       ((and twittering-use-master-password
             (not (twittering-capable-of-encryption-p)))
        (message "You need GnuPG and (EasyPG or alpaca.el) for master password!"))

       ((memq (twittering-get-accounts 'auth) '(oauth xauth basic))
        (let ((ok nil))
          ;; 1. Read token from saved file
          (when (and twittering-use-master-password
                     (twittering-capable-of-encryption-p)
                     (not twittering-private-info-file-loaded-p)
                     (file-exists-p twittering-private-info-file)
                     (twittering-load-private-info-with-guide)
                     (twittering-has-oauth-access-token-p))
            (twittering-update-account-authorization 'queried)
            (setq ok (twittering-has-oauth-access-token-p))
            (if ok
                (twittering-update-account-authorization 'authorized)
              (unless (eq (twittering-get-accounts 'auth) 'basic)
                (let ((oauth-alist (twittering-lookup-oauth-access-token-alist)))
                  (twittering-call-api 'verify-credentials
                                       `((sentinel
                                          . twittering-http-get-verify-credentials-sentinel)
                                         (clean-up-sentinel
                                          . twittering-http-get-verify-credentials-clean-up-sentinel))
                                       `(,(case service
                                            ((sina)
                                             `(user_id . ,(assocref "user_id" oauth-alist)))
                                            ((douban)
                                             `(user_id . ,(assocref "douban_user_id" oauth-alist)))
                                            (t
                                             `(username . ,(assocref "screen_name" oauth-alist))))
                                         (password . nil)
                                         (service . ,service)
                                         (sync . t)))
                  (setq ok t)))))

          ;; 2. Maybe info file has already been read when loading other accounts.
          (unless ok
            (setq ok (twittering-has-oauth-access-token-p)))

          ;; 3. Get/Renew token
          (unless ok
            (let* ((access-token-url (concat scheme
                                             (twittering-lookup-service-method-table
                                              'oauth-access-token-url-without-scheme)))
                   (consumer-key (twittering-lookup-service-method-table 'oauth-consumer-key))
                   (consumer-secret (twittering-lookup-service-method-table 'oauth-consumer-secret)))

              (case (twittering-get-accounts 'auth)
                ((oauth)
                 (let* ((request-token-url (concat scheme
                                                   (twittering-lookup-service-method-table
                                                    'oauth-request-token-url-without-scheme)))
                        (token-alist (twittering-oauth-get-access-token
                                      request-token-url
                                      (lambda (token)
                                        (concat scheme
                                                (twittering-lookup-service-method-table
                                                 'oauth-authorization-url-base-without-scheme)
                                                token))
                                      access-token-url
                                      consumer-key
                                      consumer-secret
                                      "twittering-mode")))
                   (when (and (assoc "oauth_token"        token-alist)
                              (assoc "oauth_token_secret" token-alist)
                              (or (assoc "screen_name" token-alist)
                                  (assoc "user_id" token-alist)
                                  (assoc "douban_user_id" token-alist)))
                     (twittering-update-oauth-access-token-alist token-alist)
                     (setq ok t))))

                ((xauth)
                 (let* ((account-info (twittering-prepare-account-info))
                        (token-alist (twittering-xauth-get-access-token
                                      access-token-url
                                      consumer-key
                                      consumer-secret
                                      (car account-info)
                                      (cdr account-info))))
                   ;; Dispose of password as recommended by Twitter.
                   ;; http://dev.twitter.com/pages/xauth
                   (setcdr account-info nil)
                   (when (and token-alist
                              (assoc "oauth_token" token-alist)
                              (assoc "oauth_token_secret" token-alist))
                     (twittering-update-oauth-access-token-alist token-alist)
                     (setq ok t))))

                ((basic)
                 (let ((account-info (twittering-prepare-account-info)))
                   (twittering-call-api 'verify-credentials
                                        `((sentinel . twittering-http-get-verify-credentials-sentinel)
                                          (clean-up-sentinel
                                           . twittering-http-get-verify-credentials-clean-up-sentinel))
                                        `((username . ,(car account-info))
                                          (password . ,(cdr account-info))
                                          (service  . ,service)
                                          (sync . t)))
                   (setq ok (twittering-get-account-authorization))))))

            (setq twittering-private-info-file-dirty t))

          ;; 4. Update info file when changed.
          (if (not ok)
              (error "Authorization `%s' via OAuth failed. Type M-x twittering-restart to retry."
                     service)
            (twittering-update-account-authorization 'authorized)
            (message "Authorization for \"%s\" succeeded." service)
            ;; Only save when we authenticate all enabled services and have updated
            ;; anyone of them.
            (when (and twittering-use-master-password
                       (twittering-capable-of-encryption-p)
                       twittering-private-info-file-dirty
                       (>= (length twittering-oauth-access-token-alist)
                           (length twittering-enabled-services))
                       (every (lambda (service)
                                (eq (twittering-get-account-authorization service)
                                    'authorized))
                              twittering-enabled-services))
              (twittering-save-private-info-with-guide)))))
       (t
        (message "%s is invalid as an authorization method."
                 (twittering-get-accounts 'auth)))))))

(defun twittering-http-get-verify-credentials-sentinel (proc status connection-info header-info)
  (let ((status-line (assqref 'status-line header-info))
        (status-code (assqref 'status-code header-info))
        (twittering-service-method (assqref 'service connection-info)))
    (case-string
     status-code
     (("200")
      (twittering-update-account-authorization 'authorized)
      (twittering-start)
      (format "Authorization for the account \"%s\" succeeded."
              (twittering-get-accounts 'username)))
     (t
      (twittering-update-account-authorization nil)
      (let ((error-mes
             (format "Authorization for the account \"%s\" failed. Type M-x twittering-restart to retry."
                     (twittering-get-accounts 'username))))
        (cond
         ((memq (twittering-get-accounts 'auth) '(oauth xauth))
          (twittering-update-oauth-access-token-alist nil))
         ((eq (twittering-get-accounts 'auth) 'basic)))
        error-mes)))))

(defun twittering-http-get-verify-credentials-clean-up-sentinel (proc status connection-info)
  (let ((twittering-service-method (assqref 'service connection-info)))
    (when (and (memq status '(exit signal closed failed))
               (eq (twittering-get-account-authorization) 'queried))
      (twittering-update-account-authorization nil)
      (message "Authorization failed. Type M-x twittering-restart to retry."))))

(defun twittering-oauth2-force-verify-credentials ()
  "Some evil site like weibo.com, doesn't open refresh api.  It forces user to
authenticate again.  "
  (interactive)
  (let* ((twittering-service-method 'sina)
         (scheme "https")
         (token (twittering-oauth2-auth-and-store-force
                 (concat scheme (twittering-lookup-service-method-table
                                 'oauth-authorization-url-base-without-scheme))
                 (concat scheme (twittering-lookup-service-method-table
                                 'oauth-access-token-url-without-scheme))
                 ""
                 (twittering-lookup-service-method-table 'oauth-consumer-key)
                 (twittering-lookup-service-method-table 'oauth-consumer-secret)
                 (base64-decode-string "aHR0cDovL3h3bC5hcHBzcG90LmNvbS9jYWxsYmFjaw=="))))
    (twittering-update-oauth-access-token-alist
     `(("access_token" . ,(oauth2-token-access-token token))
       ("access-response" . ,(oauth2-token-access-response token))))
    (twittering-update-account-authorization 'authorized)))

(defun twittering-oauth2-auth-and-store-force (auth-url token-url resource-url client-id client-secret &optional redirect-uri)
  "Request access to a resource and store it using `plstore'."
  ;; We store a MD5 sum of all URL
  (let* ((inhibit-read-only t)
         (plstore (plstore-open oauth2-token-file))
         (id (oauth2-compute-id auth-url token-url resource-url))
         (plist (cdr (plstore-get plstore id))))
    ;; Check if we found something matching this access
    (let ((token (oauth2-auth auth-url token-url
                              client-id client-secret resource-url nil redirect-uri)))
      ;; Set the plstore
      (setf (oauth2-token-plstore token) plstore)
      (setf (oauth2-token-plstore-id token) id)
      (plstore-put plstore id nil `(:access-token
                                    ,(oauth2-token-access-token token)
                                    :refresh-token
                                    ,(oauth2-token-refresh-token token)
                                    :access-response
                                    ,(oauth2-token-access-response token)))
      (plstore-save plstore)
      token)))

;;;; Start/Stop
;;;

(defun twittering-timer-action (func)
  (let ((buf (twittering-get-active-buffer-list)))
    (if (null buf)
        (twittering-stop)
      (funcall func)
      )))

(defun twittering-run-on-idle (idle-interval func &rest args)
  "Run FUNC the next time Emacs is idle for IDLE-INTERVAL.
Even if Emacs has been idle longer than IDLE-INTERVAL, run FUNC immediately.
Since immediate invocation requires `current-idle-time', it is available
on Emacs 22 and later.
FUNC is called as (apply FUNC ARGS)."
  (let ((sufficiently-idling
         (and (fboundp 'current-idle-time)
              (current-idle-time)
              (time-less-p (seconds-to-time idle-interval)
                           (current-idle-time)))))
    (if (not sufficiently-idling)
        (apply 'run-with-idle-timer idle-interval nil func args)
      (apply func args)
      nil)))

(defun twittering-run-repeatedly-on-idle (check-interval var idle-interval func &rest args)
  "Run FUNC every time Emacs is idle for IDLE-INTERVAL.
Even if Emacs remains idle longer than IDLE-INTERVAL, run FUNC every
CHECK-INTERVAL seconds. Since this behavior requires `current-idle-time',
invocation on long idle time is available on Emacs 22 and later.
VAR is a symbol of a variable to which the idle-timer is bound.
FUNC is called as (apply FUNC ARGS)."
  (apply 'run-at-time "0 sec"
         check-interval
         (lambda (var idle-interval func &rest args)
           (let ((registerd (symbol-value var))
                 (sufficiently-idling
                  (and (fboundp 'current-idle-time)
                       (current-idle-time)
                       (time-less-p (seconds-to-time idle-interval)
                                    (current-idle-time)))))
             (when (or (not registerd) sufficiently-idling)
               (when (and registerd sufficiently-idling)
                 (cancel-timer (symbol-value var))
                 (apply func args))
               (set var (apply 'run-with-idle-timer idle-interval nil
                               (lambda (var func &rest args)
                                 (set var nil)
                                 (apply func args))
                               var func args)))))
         var idle-interval func args))

(defun twittering-start (&optional action)
  (interactive)
  (if (null action)
      (setq action #'twittering-update-active-buffers))
  (unless twittering-timer
    (setq twittering-timer
          (run-at-time "0 sec"
                       twittering-timer-interval
                       #'twittering-timer-action action)))
  (unless twittering-timer-for-redisplaying
    (setq twittering-timer-for-redisplaying
          (twittering-run-repeatedly-on-idle
           (* 2 twittering-timer-interval-for-redisplaying)
           'twittering-idle-timer-for-redisplay
           twittering-timer-interval-for-redisplaying
           #'twittering-redisplay-status-on-buffer))))

(defun twittering-stop ()
  (interactive)
  (when twittering-timer
    (cancel-timer twittering-timer)
    (setq twittering-timer nil))
  (when twittering-timer-for-redisplaying
    (when twittering-idle-timer-for-redisplay
      (cancel-timer twittering-idle-timer-for-redisplay)
      (setq twittering-idle-timer-for-redisplay))
    (cancel-timer twittering-timer-for-redisplaying)
    (setq twittering-timer-for-redisplaying nil)))

(defun twittering-update-active-buffers (&optional noninteractive)
  "Invoke `twittering-get-and-render-timeline' for each active buffer
managed by `twittering-mode'."
  (mapc
   (lambda (buffer)
     (with-current-buffer buffer
       (let ((spec (twittering-get-timeline-spec-for-buffer buffer)))
         (when (or (twittering-timeline-spec-most-active-p spec)
                   ;; first run
                   (not (member (twittering-timeline-spec-to-string spec)
                                twittering-timeline-history))
                   ;; hourly TODO: make it customizable? (xwl)
                   (let ((min-and-sec
                          (+ (* (string-to-number
                                 (format-time-string "%M" (current-time))) 60)
                             (string-to-number
                              (format-time-string "%S" (current-time))))))
                     (<= min-and-sec twittering-timer-interval)))
           (when (and (twittering-account-authorized-p)
                      (not twittering-stream-mode))
             (twittering-get-and-render-timeline t))))))
   (twittering-get-active-buffer-list)))

;;;; Edit mode
;;;;

(defvar twittering-edit-buffer "*twittering-edit*")
(defvar twittering-pre-edit-window-configuration nil)
(defvar twittering-edit-history nil)
(defvar twittering-edit-local-history nil)
(defvar twittering-edit-local-history-idx nil)
(defvar twittering-help-overlay nil)
(defvar twittering-warning-overlay nil)

(define-derived-mode twittering-edit-mode text-mode "twmode-status-edit"
  (use-local-map twittering-edit-mode-map)

  (make-local-variable 'twittering-help-overlay)
  (setq twittering-help-overlay nil)
  (make-local-variable 'twittering-warning-overlay)
  (setq twittering-warning-overlay (make-overlay 1 1 nil nil nil))
  (overlay-put twittering-warning-overlay 'face 'font-lock-warning-face)

  (make-local-variable 'twittering-edit-local-history)
  (setq twittering-edit-local-history (cons (buffer-string)
                                            twittering-edit-history))
  (make-local-variable 'twittering-edit-local-history-idx)
  (setq twittering-edit-local-history-idx 0)

  (make-local-variable 'after-change-functions)
  (add-to-list 'after-change-functions 'twittering-edit-length-check)

  (make-local-variable 'twittering-service-method))

(when twittering-edit-mode-map
  (let ((km twittering-edit-mode-map))
    (define-key km (kbd "C-c C-c") 'twittering-edit-post-status)
    (define-key km (kbd "C-c C-k") 'twittering-edit-cancel-status)
    (define-key km (kbd "M-n") 'twittering-edit-next-history)
    (define-key km (kbd "M-p") 'twittering-edit-previous-history)
    (define-key km (kbd "<f4>") 'twittering-edit-replace-at-point)
    (define-key km (kbd "@") 'twittering-edit-@)))

(defun twittering-edit-length-check (&optional beg end len)
  (let* ((status (twittering-edit-extract-status))
         (sign-str (twittering-sign-string))
         (maxlen (- 140 (length sign-str)))
         (length (length status)))
    (setq mode-name
          (format "twmode-status-edit[%d/%d/140]" length maxlen))
    (force-mode-line-update)
    (if (< maxlen length)
        (move-overlay twittering-warning-overlay (1+ maxlen) (1+ length))
      (move-overlay twittering-warning-overlay 1 1))))

(defun twittering-edit-extract-status ()
  (if (eq major-mode 'twittering-edit-mode)
      (buffer-substring-no-properties (point-min) (point-max))
    ""))

(defun twittering-edit-setup-help (&optional username spec)
  (let* ((item (if (twittering-timeline-spec-direct-messages-p spec)
                   (format "a direct message to %s" username)
                 "a tweet"))
         (help-str (format (substitute-command-keys "Keymap:
  \\[twittering-edit-post-status]: send %s
  \\[twittering-edit-cancel-status]: cancel %s
  \\[twittering-edit-next-history]: next history element
  \\[twittering-edit-previous-history]: previous history element
  \\[twittering-edit-replace-at-point]: shorten URL at point

---- text above this line is ignored ----
") item item))
         (help-overlay
          (or twittering-help-overlay
              (make-overlay 1 1 nil nil nil))))
    (add-text-properties 0 (length help-str) '(face font-lock-comment-face)
                         help-str)
    (overlay-put help-overlay 'before-string help-str)
    (setq twittering-help-overlay help-overlay)))

(defun twittering-edit-close ()
  (kill-buffer (current-buffer))
  (when twittering-pre-edit-window-configuration
    (set-window-configuration twittering-pre-edit-window-configuration)
    (setq twittering-pre-edit-window-configuration nil)))

(defvar twittering-reply-recipient nil)

(defun twittering-update-status-from-pop-up-buffer (&optional init-str reply-to-id username spec retweeting?)
  (interactive)
  (let ((buf (get-buffer twittering-edit-buffer))
        (twittering-service-method (twittering-extract-service spec)))
    (if buf
        (pop-to-buffer buf)
      (setq buf (twittering-get-or-generate-buffer twittering-edit-buffer))
      (setq twittering-pre-edit-window-configuration
            (current-window-configuration))
      ;; This is required because the new window generated by `pop-to-buffer'
      ;; may hide the region following the current position.
      (let ((win (selected-window)))
        (split-window (selected-window) (round (* 0.8 (window-height))))
        (other-window 1)
        (switch-to-buffer buf)
        ;; (pop-to-buffer buf)
        (twittering-ensure-whole-of-status-is-visible win))
      (twittering-edit-mode)
      (setq twittering-service-method (twittering-extract-service spec))
      (unless (twittering-timeline-spec-direct-messages-p spec)
        (and (null init-str)
             twittering-current-hashtag
             (setq init-str (format " #%s " twittering-current-hashtag))))
      (message "%s to `%s': C-c C-c to send, C-c C-k to cancel"
               (cond (retweeting? "Retweet")
                     (username "Reply")
                     (t "Post"))
               (or username (symbol-name (twittering-extract-service spec))))
      (when init-str
        (insert init-str)
        (set-buffer-modified-p nil))
      (make-local-variable 'twittering-reply-recipient)
      (setq twittering-reply-recipient `(,reply-to-id ,username ,spec ,retweeting?)))))

(defun twittering-ensure-whole-of-status-is-visible (&optional window)
  "Ensure that the whole of the tweet on the current point is visible."
  (interactive)
  (let ((window (or window (selected-window))))
    (with-current-buffer (window-buffer window)
      (save-excursion
        (let* ((next-head (or (twittering-get-next-status-head) (point-max)))
               (current-tail (max (1- next-head) (point-min))))
          (when (< (window-end window t) current-tail)
            (twittering-set-window-end window current-tail)))))))

(defun twittering-edit-post-status ()
  (interactive)
  (let ((status (replace-regexp-in-string
                 "\n" "" (replace-regexp-in-string
                          "\\([[:ascii:]]\\) ?\n"
                          "\\1 \n"
                          (twittering-edit-extract-status))))
        (reply-to-id (nth 0 twittering-reply-recipient))
        (username (nth 1 twittering-reply-recipient))
        (spec (or (nth 2 twittering-reply-recipient)
                  `((,(twittering-extract-service)))))
        (retweeting? (nth 3 twittering-reply-recipient))
        (twittering-service-method twittering-service-method))
    (cond
     ((not (twittering-status-not-blank-p status))
      (message "Empty tweet!"))
     (t
      (when (< 140 (length status))
        (unless (y-or-n-p "Too long tweet!  Truncate trailing? ")
          (error "Abort"))
        (setq status (concat (substring status 0 138) "..")))

      (when (or (not twittering-request-confirmation-on-posting)
                (y-or-n-p "Send this tweet? "))
        (setq twittering-edit-history
              (cons status twittering-edit-history))
        (mapc
         (lambda (service)
           (let ((twittering-service-method service))
             (cond
              ;; ((twittering-timeline-spec-direct-messages-p spec)
              ;;  (if username
              ;;      (twittering-call-api 'send-direct-message
              ;;                           `((username . ,username)
              ;;                             (status . ,status)))
              ;;    (message "No username specified")))
              (t
               (twittering-call-api
                'update-status
                `((status . ,status)
                  ,@(when reply-to-id
                      `((in-reply-to-status-id
                         . ,(format "%s" reply-to-id))))
                  (retweeting? . ,retweeting?)))))))
         (if (equal spec '((all))) twittering-enabled-services (car spec)))
        (twittering-edit-close))))))

(defun twittering-edit-cancel-status ()
  (interactive)
  (when (or (not (buffer-modified-p))
            (prog1 (if (y-or-n-p "Cancel this tweet? ")
                       (message "Request canceled")
                     (message nil))))
    (twittering-edit-close)))

(defun twittering-edit-next-history ()
  (interactive)
  (if (>= 0 twittering-edit-local-history-idx)
      (message "End of history.")
    (let ((current-history (nthcdr twittering-edit-local-history-idx
                                   twittering-edit-local-history)))
      (setcar current-history (buffer-string))
      (decf twittering-edit-local-history-idx)
      (erase-buffer)
      (insert (nth twittering-edit-local-history-idx
                   twittering-edit-local-history))
      ;; (twittering-edit-setup-help)
      (goto-char (point-min)))))

(defun twittering-edit-previous-history ()
  (interactive)
  (if (>= twittering-edit-local-history-idx
          (- (length twittering-edit-local-history) 1))
      (message "Beginning of history.")
    (let ((current-history (nthcdr twittering-edit-local-history-idx
                                   twittering-edit-local-history)))
      (setcar current-history (buffer-string))
      (incf twittering-edit-local-history-idx)
      (erase-buffer)
      (insert (nth twittering-edit-local-history-idx
                   twittering-edit-local-history))
      ;; (twittering-edit-setup-help)
      (goto-char (point-min)))))

(defun twittering-edit-replace-at-point ()
  (interactive)
  (when (eq major-mode 'twittering-edit-mode)
    (twittering-tinyurl-replace-at-point)
    (twittering-edit-length-check)))

(defun twittering-read-friends (friend)
  (interactive
   `(,(completing-read "Select a friend: " (twittering-friends))))
  friend)

(defun twittering-edit-@ ()
  (interactive)
  (if (and (memq (twittering-extract-service) '(sina twitter))
           (or (bolp) (looking-back "[ \t]+")))
      (insert "@" (call-interactively 'twittering-read-friends) " ")
    (insert "@")))

;;;;
;;;; Edit a status on minibuffer
;;;;

(defun twittering-show-minibuffer-length (&optional beg end len)
  "Show the number of characters in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (if (and transient-mark-mode deactivate-mark)
        (deactivate-mark))
    (let* ((deactivate-mark deactivate-mark)
           (status-len (- (buffer-size) (minibuffer-prompt-width)))
           (sign-len (length (twittering-sign-string)))
           (mes (if (< 0 sign-len)
                    (format "%d=%d+%d"
                            (+ status-len sign-len) status-len sign-len)
                  (format "%d" status-len))))
      (if (<= 23 emacs-major-version)
          (minibuffer-message mes) ;; Emacs23 or later
        (minibuffer-message (concat " (" mes ")")))
      )))

(defun twittering-setup-minibuffer ()
  (add-hook 'post-command-hook 'twittering-show-minibuffer-length t t))

(defun twittering-finish-minibuffer ()
  (remove-hook 'post-command-hook 'twittering-show-minibuffer-length t))

(defun twittering-status-not-blank-p (status)
  (with-temp-buffer
    (insert status)
    (goto-char (point-min))
    ;; skip user name
    (re-search-forward "\\`[[:space:]]*@[a-zA-Z0-9_-]+\\([[:space:]]+@[a-zA-Z0-9_-]+\\)*" nil t)
    (re-search-forward "[^[:space:]]" nil t)))

(defun twittering-update-status-from-minibuffer (&optional init-str reply-to-id username spec)
  (and (not (twittering-timeline-spec-direct-messages-p spec))
       (null init-str)
       twittering-current-hashtag
       (setq init-str (format " #%s " twittering-current-hashtag)))
  (let ((status init-str)
        (sign-str (if (twittering-timeline-spec-direct-messages-p spec)
                      nil
                    (twittering-sign-string)))
        (not-posted-p t)
        (prompt "status: ")
        (map minibuffer-local-map)
        (minibuffer-message-timeout nil))
    (define-key map (kbd "<f4>") 'twittering-tinyurl-replace-at-point)
    (when twittering-use-show-minibuffer-length
      (add-hook 'minibuffer-setup-hook 'twittering-setup-minibuffer t)
      (add-hook 'minibuffer-exit-hook 'twittering-finish-minibuffer t))
    (unwind-protect
        (while not-posted-p
          (setq status (read-from-minibuffer prompt status map nil 'twittering-tweet-history nil t))
          (let ((status-with-sign (concat status sign-str)))
            (if (< 140 (length status-with-sign))
                (setq prompt "status (too long): ")
              (setq prompt "status: ")
              (when (twittering-status-not-blank-p status)
                (cond
                 ((twittering-timeline-spec-direct-messages-p spec)
                  (if username
                      (twittering-call-api 'send-direct-message
                                           `((username . ,username)
                                             (status . ,status)))
                    (message "No username specified")))
                 (t
                  (let ((parameters `(("status" . ,status-with-sign)))
                        (as-reply
                         (and reply-to-id
                              username
                              (string-match
                               (concat "\\`@" username "\\(?:[\n\r \t]+\\)*")
                               status))))
                    ;; Add in_reply_to_status_id only when a posting
                    ;; status begins with @username.
                    (twittering-call-api
                     'update-status
                     `((status . ,status-with-sign)
                       ,@(when as-reply
                           `((in-reply-to-status-id
                              . ,(format "%s" reply-to-id))))))
                    )))
                (setq not-posted-p nil))
              )))
      ;; unwindforms
      (when (memq 'twittering-setup-minibuffer minibuffer-setup-hook)
        (remove-hook 'minibuffer-setup-hook 'twittering-setup-minibuffer))
      (when (memq 'twittering-finish-minibuffer minibuffer-exit-hook)
        (remove-hook 'minibuffer-exit-hook 'twittering-finish-minibuffer))
      )))
;;;; Keymap
;;;;

(defvar twittering-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "C-c C-f") 'twittering-friends-timeline)
    (define-key km (kbd "C-c C-r") 'twittering-replies-timeline)
    (define-key km (kbd "C-c C-u") 'twittering-user-timeline)
    (define-key km (kbd "C-c C-d") 'twittering-direct-messages-timeline)
    (define-key km (kbd "C-c C-s") 'twittering-update-status-interactive)
    (define-key km (kbd "C-c C-e") 'twittering-erase-old-statuses)
    (define-key km (kbd "C-c C-w") 'twittering-erase-all)
    (define-key km (kbd "C-c C-t") 'twittering-set-current-hashtag)
    (define-key km (kbd "C-m") 'twittering-enter)
    (define-key km (kbd "C-c C-l") 'twittering-update-lambda)
    (define-key km (kbd "<mouse-1>") 'twittering-click)
    (define-key km (kbd "C-<down-mouse-3>") 'mouse-set-point)
    (define-key km (kbd "C-<mouse-3>") 'twittering-push-tweet-onto-kill-ring)
    (define-key km (kbd "C-c C-v") 'twittering-view-user-page)
    (define-key km (kbd "C-c D") 'twittering-delete-status)
    (define-key km (kbd "a") 'twittering-toggle-activate-buffer)
    (define-key km (kbd "g") 'twittering-current-timeline)
    (define-key km (kbd "u") 'twittering-update-status-interactive)
    (define-key km (kbd "C-c r") 'twittering-reply-to-user)
    (define-key km (kbd "U") 'twittering-push-uri-onto-kill-ring)
    (define-key km (kbd "d") 'twittering-direct-message)
    (define-key km (kbd "v") 'twittering-other-user-timeline)
    (define-key km (kbd "V") 'twittering-visit-timeline)
    (define-key km (kbd "L") 'twittering-other-user-list-interactive)
    (define-key km (kbd "f") 'twittering-switch-to-next-timeline)
    (define-key km (kbd "b") 'twittering-switch-to-previous-timeline)
    (define-key km (kbd "0") 'beginning-of-line)
    (define-key km (kbd "^") 'beginning-of-line-text)
    (define-key km (kbd "$") 'end-of-line)
    (define-key km (kbd "n") 'twittering-goto-next-status)
    (define-key km (kbd "p") 'twittering-goto-previous-status)
    (define-key km (kbd "C-i") 'twittering-goto-next-thing)
    (define-key km (kbd "M-C-i") 'twittering-goto-previous-thing)
    (define-key km (kbd "<backtab>") 'twittering-goto-previous-thing)
    (define-key km (kbd "<backspace>") 'twittering-scroll-down)
    (define-key km (kbd "M-v") 'twittering-scroll-down)
    (define-key km (kbd "SPC") 'twittering-scroll-up)
    (define-key km (kbd "C-v") 'twittering-scroll-up)
    (define-key km (kbd "G") 'end-of-buffer)
    (define-key km (kbd "H") 'twittering-goto-first-status)
    (define-key km (kbd "i") 'twittering-icon-mode)
    (define-key km (kbd "r") 'twittering-retweet)
    (define-key km (kbd "s") 'twittering-scroll-mode)
    (define-key km (kbd "t") 'twittering-toggle-proxy)
    (define-key km (kbd "C-c C-p") 'twittering-toggle-proxy)
    (define-key km (kbd "q") 'twittering-kill-buffer)
    (define-key km (kbd "C-c C-q") 'twittering-search)
    (define-key km (kbd "o") 'twittering-open-url-externally)
    km))

(let ((km twittering-mode-menu-on-uri-map))
  (when km
    (define-key km [ct] '("Copy tweet" . twittering-push-tweet-onto-kill-ring))
    (define-key km [cl] '("Copy link" . twittering-push-uri-onto-kill-ring))
    (define-key km [ll] '("Load link" . twittering-click))
    (let ((km-on-uri twittering-mode-on-uri-map))
      (when km-on-uri
        (define-key km-on-uri (kbd "C-<down-mouse-3>") 'mouse-set-point)
        (define-key km-on-uri (kbd "C-<mouse-3>") km)))))

(defun twittering-keybind-message ()
  (let ((important-commands
         '(("Timeline" . twittering-friends-timeline)
           ("Replies" . twittering-replies-timeline)
           ("Update status" . twittering-update-status-interactive)
           ("Next" . twittering-goto-next-status)
           ("Prev" . twittering-goto-previous-status))))
    (mapconcat (lambda (command-spec)
                 (let ((descr (car command-spec))
                       (command (cdr command-spec)))
                   (format "%s: %s" descr (key-description
                                           (where-is-internal
                                            command
                                            overriding-local-map t)))))
               important-commands ", ")))

;; (run-with-idle-timer
;;  0.1 t
;;  '(lambda ()
;;     (when (equal (buffer-name (current-buffer)) twittering-buffer)
;;       (message (twittering-keybind-message)))))


;;;; Icon mode
;;;;

(defvar twittering-icon-mode t
  "You MUST NOT CHANGE this variable directly.
You should change through function `twittering-icon-mode'.")

(defun twittering-icon-mode (&optional arg)
  "Toggle display of icon images on timelines.
With a numeric argument, if the argument is positive, turn on
icon mode; otherwise, turn off icon mode."
  (interactive "P")
  (let ((prev-mode twittering-icon-mode))
    (setq twittering-icon-mode
          (if (null arg)
              (not twittering-icon-mode)
            (< 0 (prefix-numeric-value arg))))
    (unless (eq prev-mode twittering-icon-mode)
      (let ((id (twittering-get-id-at)))
        (twittering-update-mode-line)
        (twittering-render-timeline (current-buffer))
        (twittering-restore-point id)))))

(defvar twittering-icon-prop-hash (make-hash-table :test 'equal)
  "Hash table for storing display properties of icon. The key is the size of
icon and the value is a hash. The key of the child hash is URL and its value
is the display property for the icon.")

(defvar twittering-convert-program (executable-find "convert"))
(defvar twittering-convert-fix-size 48)
(defvar twittering-use-convert (not (null twittering-convert-program))
  "*This variable makes a sense only if `twittering-convert-fix-size'
is non-nil. If this variable is non-nil, icon images are converted by
invoking \"convert\". Otherwise, cropped images are displayed.")

(defvar twittering-use-profile-image-api nil
  "*Whether to use `profile_image' API for retrieving scaled icon images.
NOTE: This API is rate limited.")

(defvar twittering-icon-storage-file
  (expand-file-name "~/.emacs.d/twittering/icons.gz")
  "*The file to which icon images are stored.
`twittering-icon-storage-limit' determines the number icons stored in the
file.
The file is loaded with `with-auto-compression-mode'.")

(defvar twittering-use-icon-storage nil
  "*Whether to use the persistent icon storage.
If this variable is non-nil, icon images are stored to the file specified
by `twittering-icon-storage-file'.")

(defvar twittering-icon-storage-recent-icons nil
  "List of recently rendered icons.")

(defvar twittering-icon-storage-limit nil
  "*How many icons are stored in the persistent storage.
If `twittering-use-icon-storage' is nil, this variable is ignored.
If a positive integer N, `twittering-save-icon-properties' saves N icons that
have been recently rendered.
If nil, the function saves all icons.")

(defconst twittering-error-icon-data-pair
  '(xpm . "/* XPM */
static char * yellow3_xpm[] = {
\"16 16 2 1\",
\"         c None\",
\".        c #FF0000\",
\"................\",
\".              .\",
\". .          . .\",
\".  .        .  .\",
\".   .      .   .\",
\".    .    .    .\",
\".     .  .     .\",
\".      ..      .\",
\".      ..      .\",
\".     .  .     .\",
\".    .    .    .\",
\".   .      .   .\",
\".  .        .  .\",
\". .          . .\",
\".              .\",
\"................\"};
")
  "Image used when the valid icon cannot be retrieved.")

(defun twittering-update-icon-storage-recent-icons (size image-url spec)
  (unless (null twittering-icon-storage-limit)
    (let ((dummy-icon-properties (twittering-make-display-spec-for-icon
                                  twittering-error-icon-data-pair)))
      (unless (equal spec dummy-icon-properties)
        (let ((history-delete-duplicates t))
          (twittering-add-to-history 'twittering-icon-storage-recent-icons
                                     (list size image-url)
                                     twittering-icon-storage-limit))))))

(defun twittering-get-display-spec-for-icon (image-url)
  (let ((hash
         (gethash twittering-convert-fix-size twittering-icon-prop-hash)))
    (when hash
      (let ((spec (gethash image-url hash))
            (size twittering-convert-fix-size))
        (when spec
          (twittering-update-icon-storage-recent-icons size image-url spec)
          spec)))))

(defun twittering-convert-image-data (image-data dest-type &optional src-type)
  "Convert IMAGE-DATA into XPM format and return it. If it fails to convert,
return nil."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (buffer-disable-undo)
    (let ((coding-system-for-read 'binary)
          (coding-system-for-write 'binary)
          (require-final-newline nil))
      (insert image-data)
      (let* ((args
              `(,(if src-type (format "%s:-" src-type) "-")
                ,@(when (integerp twittering-convert-fix-size)
                    `("-resize"
                      ,(format "%dx%d" twittering-convert-fix-size
                               twittering-convert-fix-size)))
                ,(format "%s:-" dest-type)))
             (exit-status
              (apply 'call-process-region (point-min) (point-max)
                     twittering-convert-program t t nil args)))
        (if (equal 0 exit-status)
            (buffer-string)
          ;; failed to convert the image.
          nil)))))

(defun twittering-create-image-pair (image-data)
  "Return a pair of image type and image data.
IMAGE-DATA is converted by `convert' if the image type of IMAGE-DATA is not
available and `twittering-use-convert' is non-nil."
  (let* ((image-type (ignore-errors
                       (and image-data (image-type-from-data image-data))))
         (image-pair `(,image-type . ,image-data))
         (converted-size
          `(,twittering-convert-fix-size . ,twittering-convert-fix-size)))
    (cond
     ((null image-data)
      twittering-error-icon-data-pair)
     ((and (image-type-available-p image-type)
           (or (not (integerp twittering-convert-fix-size))
               (equal (image-size (create-image image-data image-type t) t)
                      converted-size)))
      image-pair)
     (twittering-use-convert
      (let* ((dest-type (if (image-type-available-p image-type) image-type 'xpm))
             (converted-data
              (twittering-convert-image-data image-data dest-type image-type)))
        (if converted-data
            `(,dest-type . ,converted-data)
          twittering-error-icon-data-pair)))
     (t
      ;; twittering-error-icon-data-pair
      image-pair))))

(defun twittering-register-image-spec (image-url spec size)
  (let ((hash (gethash size twittering-icon-prop-hash)))
    (unless hash
      (setq hash (make-hash-table :test 'equal))
      (puthash size hash twittering-icon-prop-hash))
    (puthash image-url spec hash)))

(defun twittering-register-image-data (image-url image-data &optional size)
  (let ((image-pair (twittering-create-image-pair image-data))
        (size (or size twittering-convert-fix-size)))
    (when image-pair
      (let ((spec (twittering-make-display-spec-for-icon image-pair)))
        (twittering-register-image-spec image-url spec size)
        spec))))

(defun twittering-make-slice-spec (image-spec)
  "Return slice property for reducing the image size by cropping it."
  (let* ((size (image-size image-spec t))
         (width (car size))
         (height (cdr size))
         (fixed-length twittering-convert-fix-size)
         (half-fixed-length (/ fixed-length 2)))
    (if (or (< fixed-length width) (< fixed-length height))
        `(slice ,(max 0 (- (/ width 2) half-fixed-length))
                ,(max 0 (- (/ height 2) half-fixed-length))
                ,fixed-length ,fixed-length)
      `(slice 0 0 ,fixed-length ,fixed-length))))

(defun twittering-make-display-spec-for-icon (image-pair)
  "Return the specification for `display' text property, which
limits the size of an icon image IMAGE-PAIR up to FIXED-LENGTH. If
the type of the image is not supported, nil is returned.

If the size of the image exceeds FIXED-LENGTH, the center of the
image are displayed."
  (let* ((type (car-safe image-pair))
         (data (cdr-safe image-pair))
         (raw-image-spec ;; without margins
          (ignore-errors (create-image data type t)))
         slice-spec
         image-spec)
    (if raw-image-spec
        (progn
          (setq slice-spec
                (when (and twittering-convert-fix-size (not twittering-use-convert))
                  (twittering-make-slice-spec raw-image-spec))
                image-spec
                (if (fboundp 'create-animated-image) ;; Emacs24 or later
                    (create-animated-image data type t :margin 2 :ascent 'center)
                  (create-image data type t :margin 2 :ascent 'center)))
          (if slice-spec
              `(display (,image-spec ,slice-spec))
            `(display ,image-spec)))
      (message "Unknown image format")
      '(display))))

(defun twittering-make-icon-string (beg end image-url &optional sync)
  (let ((display-spec (twittering-get-display-spec-for-icon image-url))
        (image-data (gethash image-url twittering-url-data-hash))
        (properties (and beg (text-properties-at beg)))
        (icon-string (copy-sequence twittering-need-to-be-updated-indicator)))
    (when properties
      (add-text-properties 0 (length icon-string) properties icon-string))
    (cond
     (display-spec
      (if (cadr display-spec)
          (let ((icon-string (apply 'propertize "_"
                                    (append properties display-spec))))
            ;; Remove the property required no longer.
            (remove-text-properties 0 (length icon-string)
                                    '(need-to-be-updated nil)
                                    icon-string)
            icon-string)
        ""))
     ((and (integerp image-data)
           (<= twittering-url-request-retry-limit image-data))
      ;; Try to retrieve the image no longer.
      (twittering-register-image-data image-url nil)
      (twittering-make-icon-string beg end image-url))
     ((and image-data (not (integerp image-data)))
      (twittering-register-image-data image-url image-data)
      (twittering-make-icon-string beg end image-url))
     (t
      (put-text-property 0 (length icon-string)
                         'need-to-be-updated
                         `((lambda (&rest args)
                             (let ((twittering-convert-fix-size
                                    ,twittering-convert-fix-size))
                               (apply 'twittering-make-icon-string
                                      (append args (list ,image-url))))))
                         icon-string)
      (if sync
          (progn
            (twittering-register-image-data
             image-url (string-as-unibyte
                        (twittering-url-retrieve-synchronously image-url)))
            (twittering-make-icon-string beg end image-url sync))
        (twittering-url-retrieve-async
         image-url
         `(lambda (&rest args)
            (let ((twittering-convert-fix-size ,twittering-convert-fix-size))
              (apply 'twittering-register-image-data args))))
        icon-string)))))

(defun twittering-make-original-icon-string (beg end image-url &optional sync)
  (let ((twittering-convert-fix-size nil))
    (twittering-make-icon-string beg end image-url sync)))

(defun twittering-make-emotions-string (beg end emotion)
  (let ((ret (if (and beg end)
                 (buffer-substring-no-properties beg end)
               (copy-sequence twittering-need-to-be-updated-indicator))))
    (if (consp twittering-emotions-phrase-url-alist)
        (let ((url (some (lambda (i)
                           (when (string= (assqref 'phrase i) emotion)
                             (assqref 'url i)))
                         twittering-emotions-phrase-url-alist)))
          (if url
              (setq ret (twittering-make-original-icon-string nil nil url))
            (setq ret emotion)))
      (setq ret (propertize ret
                            'need-to-be-updated
                            `(twittering-make-emotions-string ,emotion))))
    ret))

(defun twittering-insert-emotion (phrase)
  (interactive
   (progn
     (unless twittering-is-getting-emotions-p
       (setq twittering-is-getting-emotions-p t)
       (twittering-get-simple nil nil nil 'emotions))
     `(,(ido-completing-read
         "Select emotion: "
         (mapcar (lambda (e)
                   (apply 'propertize
                          (assqref 'phrase e)
                          (let (twittering-convert-fix-size)
                            (twittering-get-display-spec-for-icon
                             (assqref 'url e)))))
                 twittering-emotions-phrase-url-alist)))))
  (let ((url (some (lambda (e)
                     (when (equal (assqref 'phrase e) phrase)
                       (assqref 'url e)))
                   twittering-emotions-phrase-url-alist)))
    (insert (apply 'propertize
                   phrase
                   (let (twittering-convert-fix-size)
                     (twittering-get-display-spec-for-icon url))))))

(defcustom twittering-image-height-threshold 40
  "For images whose height is bigger than this, we will show it in a separate
image buffer during `twittering-toggle-thumbnail-1'. "
  :type 'integer
  :group 'twittering)

(defcustom twittering-image-external-viewer-command nil
  "External command for viewing image.
You can set it to \"\" on some OS like Windows, leaving the OS to call a proper
handler. "
  :type 'string
  :group 'twittering)

(defvar twittering-tmp-image "~/.emacs.d/twittering/twmode-image")

(defun twittering-toggle-thumbnail-1 (thumbnail-pic bmiddle-pic &optional initial)
  (let ((uri (get-text-property (point) 'uri))
        next-uri)
    (when (or uri initial)
      (if initial
          (setq uri thumbnail-pic
                next-uri bmiddle-pic)
        (if (string= uri bmiddle-pic)
            (setq next-uri thumbnail-pic)
          (setq next-uri bmiddle-pic)))

      (let* ((s (twittering-make-original-icon-string nil nil uri (not initial)))
             (image (and s (get-text-property 0 'display s)))
             (height (and image (cdr (image-size image))))
             (image-data (and image (plist-get (cdr image) :data)))
             (common-properties (twittering-get-common-properties (point)))
             (faces (get-text-property (point) 'face)))
        (if (and height (> height twittering-image-height-threshold))
            (if twittering-image-external-viewer-command
                (let ((f (file-truename twittering-tmp-image))
                      (coding-system-for-write 'binary))
                  (when (eq system-type 'windows-nt)
                    (setq f (format "%s.%S" f (plist-get (cdr image) :type))))
                  (with-temp-file f
                    (insert image-data))
                  (let ((cmd (format "%s \"%s\"" twittering-image-external-viewer-command f)))
                    (start-process-shell-command cmd nil cmd)))
              (switch-to-buffer "*twmode-image*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert image-data)
                (image-mode))
              (setq buffer-read-only t))
          (add-text-properties 0 (length s)
                               `(,@common-properties
                                 mouse-face highlight
                                 uri ,next-uri
                                 keymap
                                 ,(let ((map (make-sparse-keymap))
                                        (func `(lambda ()
                                                 (interactive)
                                                 (twittering-toggle-thumbnail-1 ,uri ,next-uri))))
                                    (define-key map (kbd "<mouse-1>") func)
                                    (define-key map (kbd "RET") func)
                                    map))
                               s)
          (twittering-decorate-zebra-background s faces)
          (if initial
              s
            (let ((inhibit-read-only t))
              (save-excursion
                (delete-char (length s))
                (insert s)))))))))

(defun twittering-http-get-simple-sentinel (proc status connection-info
                                                 header-info
                                                 &optional args suc-msg)
  (let ((status-line (assqref 'status-line header-info))
        (status-code (assqref 'status-code header-info))
        ret mes)
    (cond
     ((string= status-code "200")
      (let ((json-data (twittering-construct-statuses)))
        (when json-data
          (setq ret json-data)
          (case (assqref 'method args)
            ((counts)
             (setq twittering-counts-last-timestamp (current-time)))

            ((emotions)
             (setq twittering-emotions-phrase-url-alist ret)
             (mapc (lambda (e)
                     (twittering-make-original-icon-string
                      nil nil (assqref 'url e)))
                   twittering-emotions-phrase-url-alist))

            ((get-list-index get-list-subscriptions get-list-memberships)
             (setq ret (mapcar (lambda (i) (substring (assqref 'full-name i) 1))
                               (assqref 'lists ret))))

            ;; ((query-mid)
            ;;  (setq ret (assqref 'mid (car json-data))))

            )

          (puthash `(,(assqref 'username args) ,(assqref 'method args))
                   (or ret "")
                   twittering-simple-hash))))
     (t
      (let ((error-mes (twittering-get-error-message header-info (current-buffer))))
        (if error-mes
            (setq mes (format "Response: %s" error-mes))
          (setq mes (format "Response: %s" status-line))))))
    (setq twittering-get-simple-retrieved
          (or ret
              mes
              "")) ;; set "" explicitly if user does not have a list.
    nil))

(defun twittering-save-icon-properties (&optional filename)
  (let ((filename (or filename twittering-icon-storage-file))
        (stored-data
         (cond
          ((null twittering-icon-storage-limit)
           (let ((result nil)
                 (dummy-icon-properties (twittering-make-display-spec-for-icon
                                         twittering-error-icon-data-pair)))
             (maphash
              (lambda (size hash)
                (maphash (lambda (url properties)
                           (unless (equal properties dummy-icon-properties)
                             (setq result (cons (list size url) result))))
                         hash))
              twittering-icon-prop-hash)
             result))
          (t
           (reverse twittering-icon-storage-recent-icons))))
        ;; Bind `default-directory' to the temporary directory
        ;; because it is possible that the directory pointed by
        ;; `default-directory' has been already removed.
        (default-directory temporary-file-directory))
    (when (require 'jka-compr nil t)
      (with-auto-compression-mode
        (let ((coding-system-for-write 'binary))
          (with-temp-file filename
            (insert "( 2 ")
            (prin1 (cons 'emacs-version emacs-version) (current-buffer))
            (insert "(icon-list ")
            (mapc
             (lambda (entry)
               (let* ((size (elt entry 0))
                      (url (elt entry 1))
                      properties)
                 ;; Do not save embeded images inside tweet.
                 (unless (string-match "ww[0-9]+.sinaimg.cn" url)
                   (setq properties
                         (gethash url
                                  (gethash size twittering-icon-prop-hash)))
                   (insert (if size
                               (format "(%d " size)
                             "(nil "))
                   (prin1 url (current-buffer))
                   (insert " ")
                   (prin1 properties (current-buffer))
                   (insert ")\n"))))
             stored-data)
            (insert "))")))))))

(defun twittering-load-icon-properties (&optional filename)
  (let* ((filename (or filename twittering-icon-storage-file))
	 ;; Bind `default-directory' to the temporary directory
	 ;; because it is possible that the directory pointed by
	 ;; `default-directory' has been already removed.
	 (default-directory temporary-file-directory)
	 (data
	  (with-temp-buffer
	    (condition-case err
		(cond
		 ((and (require 'jka-compr)
		       (file-exists-p filename))
		  (with-auto-compression-mode
		    (let ((coding-system-for-read 'binary)
			  (coding-system-for-write 'binary))
		      (insert-file-contents filename)))
		  (read (current-buffer)))
		 (t
		  nil))
	      (error
	       (message "Failed to load icon images. %s" (cdr err))
	       nil)))))
    (cond
     ((equal 2 (car data))
      (let ((version (cdr (assq 'emacs-version data))))
        (cond
         ((or (equal version emacs-version)
              (y-or-n-p
               (format "%s is generated by Emacs %s! Use it?"
                       filename version)))
          (mapc (lambda (entry)
                  (let ((size (elt entry 0))
                        (url (elt entry 1))
                        (properties (elt entry 2)))
                    (twittering-update-icon-storage-recent-icons size url
                                                                 properties)
                    (twittering-register-image-spec url properties size)))
                (cdr (assq 'icon-list data))))
         (t
          (message "Stopped loading icons")))))
     (t
      (mapc (lambda (entry)
              (let ((size (car entry))
                    (prop-alist (cdr entry)))
                (mapc (lambda (entry)
                        (let ((url (car entry))
                              (properties (cdr entry)))
                          (twittering-update-icon-storage-recent-icons
                           size url properties)
                          (twittering-register-image-spec url properties
                                                          size)))
                      prop-alist)))
            data)))))

;;;;
;;;; SSL indicator on modeline
(defconst twittering-ssl-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
            :ascent center
            :data
            "/* XPM */
/*
 * Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>
 * Copyright (C) 2003 Kazu Yamamoto <kazu@Mew.org>
 * Copyright (C) 2004 Yoshifumi Nishida <nishida@csl.sony.co.jp>
 * Copyright notice is the same as Mew's one.
 */
static char * yellow3_xpm[] = {
\"14 14 7 1\",
\"         c None\",
\".        c #B07403\",
\"+        c #EFEE38\",
\"@        c #603300\",
\"#        c #D0A607\",
\"$        c #FAFC90\",
\"%        c #241100\",
\"    .++++@    \",
\"   .+@...+@   \",
\"  .+@    .+@  \",
\"  .+@    .+@  \",
\"  .+@    .+@  \",
\"++########@@@@\",
\"+$$++++++++#@@\",
\"+$++++%@+++#@@\",
\"+$+++%%%@++#@@\",
\"+$+++%%%@++#@@\",
\"+$++++%@+++#@@\",
\"+$++++%@+++#@@\",
\"+$+++++++++#@@\",
\"++@@@@@@@@@@@@\"};
"
            ;; The above image is copied from `mew-lock.xpm' distributed with Mew.
            ;; The copyright of the image is below, which is copied from `mew.el'.

            ;; Copyright Notice:

            ;; Copyright (C) 1994-2009 Mew developing team.
            ;; All rights reserved.

            ;; Redistribution and use in source and binary forms, with or without
            ;; modification, are permitted provided that the following conditions
            ;; are met:
            ;;
            ;; 1. Redistributions of source code must retain the above copyright
            ;;    notice, this list of conditions and the following disclaimer.
            ;; 2. Redistributions in binary form must reproduce the above copyright
            ;;    notice, this list of conditions and the following disclaimer in the
            ;;    documentation and/or other materials provided with the distribution.
            ;; 3. Neither the name of the team nor the names of its contributors
            ;;    may be used to endorse or promote products derived from this software
            ;;    without specific prior written permission.
            ;;
            ;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
            ;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
            ;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
            ;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
            ;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
            ;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
            ;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
            ;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
            ;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
            ;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
            ;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
            ))
  "Image for indicator of SSL state.")

(defconst twittering-modeline-ssl
  (if twittering-ssl-indicator-image
      (propertize "SSL"
                  'display twittering-ssl-indicator-image
                  'help-echo "SSL is enabled.")
    "SSL"))

;; ACTIVE/INACTIVE
(defconst twittering-active-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
            :ascent center
            :data
            "/* XPM */
static char *plugged[] = {
\"32 12 8 1\",
\"  c None\",
\". c #a6caf0\",
\"# c #8fa5cf\",
\"a c #717171\",
\"b c #5d5d97\",
\"c c #8488ca\",
\"d c #9f9f9f\",
\"e c #7f8080\",
\"            ...                 \",
\"           .ccb....             \",
\"           accb####.            \",
\"          .accb#####..          \",
\"   eeeeeeeeaccb#####.eeeeeeee   \",
\"   dddddddcaccb#####.dedddddd   \",
\"   dddddddcaccb#####.dedddddd   \",
\"   eeeeeeeeaccb#####.eeeeeeee   \",
\"          aaccb####aaa          \",
\"           accbaaaaa            \",
\"           aaaaaaaa             \",
\"            aaa                 \"
};
"))
  "Image for indicator of active state."
  ;; The above image is copied from `plugged.xpm' distributed with Wanderlust
  ;; by Yuuichi Teranishi <teranisi@gohome.org>.
  ;; The copyright of the image is below, which is copied from `COPYING' of
  ;; Wanderlust 2.14.
  ;; Copyright (C) 1998-2001 Yuuichi Teranishi <teranisi@gohome.org>
  ;;
  ;;    This program is free software; you can redistribute it and/or modify
  ;;    it under the terms of the GNU General Public License as published by
  ;;    the Free Software Foundation; either version 2, or (at your option)
  ;;    any later version.
  ;;
  ;;    This program is distributed in the hope that it will be useful,
  ;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
  ;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ;;    GNU General Public License for more details.
  ;;
  ;;    You should have received a copy of the GNU General Public License
  ;;    along with GNU Emacs; see the file COPYING.  If not, write to the
  ;;    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  ;;    Boston, MA 02111-1307, USA.
  )

(defconst twittering-inactive-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
            :ascent center
            :data
            "/* XPM */
static char * unplugged_xpm[] = {
\"32 12 9 1\",
\"         s None        c None\",
\".        c tomato\",
\"X        c #a6caf0\",
\"o        c #8488ca\",
\"O        c #5d5d97\",
\"+        c #8fa5cf\",
\"@        c #717171\",
\"#        c #7f8080\",
\"$        c #9f9f9f\",
\"          XXX......             \",
\"           ...    ...           \",
\"          ..O     ....X         \",
\"         ..oO    ...+..XX       \",
\"   ######.ooO   ...+++.X#####   \",
\"   $$$$$o.ooO  ...@+++.X$#$$$   \",
\"   $$$$$o.ooO ... @+++.X$#$$$   \",
\"   ######.ooO...  @+++.X#####   \",
\"         ..o...   @++..@@       \",
\"          ....    @@..@         \",
\"           ...    ...@          \",
\"             ......             \"
};
"))
  "Image for indicator of inactive state."
  ;; The above image is copied from `unplugged.xpm' distributed with Wanderlust
  ;; by Yuuichi Teranishi <teranisi@gohome.org>.
  ;; The copyright of the image is below, which is copied from `COPYING' of
  ;; Wanderlust 2.14.
  ;; Copyright (C) 1998-2001 Yuuichi Teranishi <teranisi@gohome.org>
  ;;
  ;;    This program is free software; you can redistribute it and/or modify
  ;;    it under the terms of the GNU General Public License as published by
  ;;    the Free Software Foundation; either version 2, or (at your option)
  ;;    any later version.
  ;;
  ;;    This program is distributed in the hope that it will be useful,
  ;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
  ;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ;;    GNU General Public License for more details.
  ;;
  ;;    You should have received a copy of the GNU General Public License
  ;;    along with GNU Emacs; see the file COPYING.  If not, write to the
  ;;    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  ;;    Boston, MA 02111-1307, USA.
  )

(defconst twittering-logo-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
            :ascent center
            :data
            "/* XPM */
static char * twitter_xpm[] = {
\"16 16 104 2\",
\"          c None\",
\".         c #A3A2A2\",
\"+         c #ADACAC\",
\"@         c #64CFFC\",
\"#         c #64D0FC\",
\"$         c #69D1FC\",
\"%         c #6AD1FC\",
\"&         c #6CD2FC\",
\"*         c #6DD2FC\",
\"=         c #6ED3FC\",
\"-         c #70D3FC\",
\";         c #71D3FC\",
\">         c #C2C1C1\",
\",         c #72D3FC\",
\"'         c #71D4FC\",
\")         c #72D4FC\",
\"!         c #73D4FC\",
\"~         c #C3C3C3\",
\"{         c #75D5FC\",
\"]         c #76D5FC\",
\"^         c #78D6FC\",
\"/         c #79D6FC\",
\"(         c #7BD6FC\",
\"_         c #7DD7FC\",
\":         c #7FD8FC\",
\"<         c #80D8FC\",
\"[         c #80D8FD\",
\"}         c #81D8FD\",
\"|         c #C9C8C8\",
\"1         c #CAC9C9\",
\"2         c #CCCAC9\",
\"3         c #85DAFD\",
\"4         c #89DBFD\",
\"5         c #8BDCFD\",
\"6         c #8CDCFD\",
\"7         c #91DDFD\",
\"8         c #92DDFD\",
\"9         c #D3D2D2\",
\"0         c #D7D7D6\",
\"a         c #A8E4FC\",
\"b         c #A5E5FF\",
\"c         c #A7E5FF\",
\"d         c #A6E6FF\",
\"e         c #A7E6FF\",
\"f         c #ABE6FE\",
\"g         c #AEE7FD\",
\"h         c #AFE8FF\",
\"i         c #E1DFDF\",
\"j         c #B9EAFD\",
\"k         c #B9EAFE\",
\"l         c #B8EBFF\",
\"m         c #E5E2E1\",
\"n         c #E6E2E1\",
\"o         c #C7EFFF\",
\"p         c #C8F0FF\",
\"q         c #ECE9E7\",
\"r         c #ECE9E8\",
\"s         c #EDECEB\",
\"t         c #D0F3FF\",
\"u         c #D3F3FF\",
\"v         c #EEEDED\",
\"w         c #D4F3FF\",
\"x         c #EFEDEC\",
\"y         c #D6F4FF\",
\"z         c #D8F6FF\",
\"A         c #D9F6FF\",
\"B         c #F3F0EE\",
\"C         c #F4F0EF\",
\"D         c #DBF6FF\",
\"E         c #DEF6FF\",
\"F         c #DFF6FF\",
\"G         c #E1F6FF\",
\"H         c #DFF7FF\",
\"I         c #DFF8FF\",
\"J         c #E1F8FF\",
\"K         c #E7F7FF\",
\"L         c #F7F4F3\",
\"M         c #F8F4F3\",
\"N         c #E4F9FF\",
\"O         c #EAF8FE\",
\"P         c #EBF8FE\",
\"Q         c #F9F6F5\",
\"R         c #EFFAFE\",
\"S         c #ECFBFF\",
\"T         c #FBF8F7\",
\"U         c #F9F9F9\",
\"V         c #FAF9F9\",
\"W         c #FDF9F8\",
\"X         c #FAFAFA\",
\"Y         c #F3FCFF\",
\"Z         c #FCFAFA\",
\"`         c #FDFAF9\",
\" .        c #F6FCFF\",
\"..        c #FCFBFA\",
\"+.        c #F2FEFF\",
\"@.        c #F8FDFF\",
\"#.        c #FDFCFB\",
\"$.        c #FDFCFC\",
\"%.        c #F9FDFF\",
\"&.        c #FFFCFA\",
\"*.        c #FFFEFC\",
\"=.        c #FFFEFD\",
\"-.        c #FDFFFF\",
\";.        c #FFFFFF\",
\"      0 q r                     \",
\"    v ;.H N ;.~                 \",
\"    =.o ! ( D M                 \",
\"    X b $ % k *.B Q L x         \",
\"    V e & = a G J F E S ;.9     \",
\"    V e * ! { ^ ^ ^ ] _ t `     \",
\"    V e * ! ' = = = = @ b $.    \",
\"    V e * ) / < : : _ 3 z T     \",
\"    V e & = g R P O O Y V .     \",
\"    U d & * j ;.;.;.;.;.>       \",
\"    Z h & = 7 K @. . .%...      \",
\"    W y , - ; [ 6 5 4 8 I `     \",
\"    1 ;.f & = = * * * # c #.    \",
\"      s -.l } ! ! ! ' { p &.    \",
\"        i ;.+.A u w u J ;.|     \",
\"            2 n B B C m +       \"};
"))
  "Image for twitter logo.")

(defconst twittering-modeline-properties
  (when (display-mouse-p)
    `(local-map
      ,(purecopy (make-mode-line-mouse-map
                  'mouse-2 #'twittering-toggle-activate-buffer))
      help-echo "mouse-2 toggles activate buffer")))

(defconst twittering-modeline-active
  (if twittering-active-indicator-image
      (apply 'propertize " "
             `(display ,twittering-active-indicator-image
                       ,@twittering-modeline-properties))
    " "))

(defconst twittering-modeline-inactive
  (if twittering-inactive-indicator-image
      (apply 'propertize "INACTIVE"
             `(display ,twittering-inactive-indicator-image
                       ,@twittering-modeline-properties))
    "INACTIVE"))

(defconst twittering-logo
  (if twittering-logo-image
      (apply 'propertize " "
             `(display ,twittering-logo-image))
    "tw"))

(defun twittering-mode-line-buffer-identification ()
  (let ((active-mode-indicator
         (if twittering-active-mode
             twittering-modeline-active
           twittering-modeline-inactive))
        (enabled-options
         `(,(if twittering-display-connection-method
                (concat
                 (when (twittering-get-accounts 'ssl) (concat twittering-modeline-ssl ":"))
                 (twittering-get-connection-method-name (twittering-get-accounts 'ssl)))
              (when (twittering-get-accounts 'ssl) twittering-modeline-ssl))
           ,@(when twittering-jojo-mode '("jojo"))
           ,@(when twittering-icon-mode '("icon"))
           ,@(when twittering-reverse-mode '("reverse"))
           ,@(when twittering-scroll-mode '("scroll"))
           ,@(when twittering-proxy-use '("proxy")))))
    (concat active-mode-indicator
            (when twittering-display-remaining
              (format " %d/%d"
                      (twittering-get-ratelimit-remaining)
                      (twittering-get-ratelimit-limit)))
            (when enabled-options
              (concat "[" (mapconcat 'identity enabled-options " ") "]")))))

(defun twittering-update-mode-line ()
  "Update mode line."
  (force-mode-line-update))

;;;; Unread statuses info
;;;;

(defvar twittering-unread-status-info nil
  "A list of (buffer unread-statuses-counter), where `unread-statuses-counter'
means the number of statuses retrieved after the last visiting of the buffer.")

(defun twittering-reset-unread-status-info-if-necessary ()
  (when (twittering-buffer-p)
    (twittering-set-number-of-unread (current-buffer) 0)))

(defun twittering-set-number-of-unread (buffer number)
  (let* ((entry (assq buffer twittering-unread-status-info))
         (current (or (cadr entry) 0))
         (spec-string
          (twittering-get-timeline-spec-string-for-buffer buffer)))
    (when (and (zerop number)
               (or (not (= number current))
                   (not (zerop twittering-new-tweets-count))))
      (twittering-cache-save spec-string))
    (unless (= number current)
      (setq twittering-unread-status-info
            (cons
             `(,buffer ,number)
             (if entry
                 (remq entry twittering-unread-status-info)
               twittering-unread-status-info)))
      (force-mode-line-update))))

(defun twittering-make-unread-status-notifier-string ()
  "Generate a string that displays unread statuses."
  (setq twittering-unread-status-info
        (remove nil
                (mapcar (lambda (entry)
                          (when (and (buffer-live-p (car entry))
                                     (not (zerop (cadr entry))))
                            entry))
                        twittering-unread-status-info)))
  (let ((sum (apply '+ (mapcar 'cadr twittering-unread-status-info))))
    (if (zerop sum)
        ""
      (format
       "%s(%s)"
       twittering-logo
       (mapconcat
        'identity
        (let* ((buffers (twittering-get-active-buffer-list))
               (partitioned-buffers
                (mapcar (lambda (sv)
                          (remove-if-not (lambda (b)
                                           (with-current-buffer b
                                             (eq (twittering-extract-service) sv)))
                                         buffers))
                        twittering-enabled-services))
               (partitioned-buffer-prefixes
                (mapcar* (lambda (sv lst)
                           (mapcar (lambda (p)
                                     (concat p "@" sv))
                                   (twittering-build-unique-prefix
                                    (mapcar 'buffer-name lst))))
                         (twittering-build-unique-prefix
                          (mapcar 'symbol-name twittering-enabled-services))
                         partitioned-buffers)))

          (mapcar (lambda (b-u)
                    (some (lambda (b p)
                            (when (eq (car b-u) b)
                              (twittering-decorate-unread-status-notifier
                               b (format "%s:%d" p (cadr b-u)))))
                          (apply 'append partitioned-buffers)
                          (apply 'append partitioned-buffer-prefixes)))
                  twittering-unread-status-info))
        ",")))))

(defun twittering-update-unread-status-info ()
  "Update `twittering-unread-status-info' with new tweets."
  (let* ((spec twittering-new-tweets-spec)
         (buffer (twittering-get-buffer-from-spec spec))
         (current (or (cadr (assq buffer twittering-unread-status-info)) 0))
         (result (+ current twittering-new-tweets-count)))
    (when buffer
      (when (eq buffer (current-buffer))
        (setq result 0))
      (twittering-set-number-of-unread buffer result))))

(defun twittering-enable-unread-status-notifier ()
  "Enable a notifier of unread statuses on `twittering-mode'."
  (interactive)
  (add-hook 'twittering-new-tweets-hook 'twittering-update-unread-status-info)
  (add-hook 'post-command-hook
            'twittering-reset-unread-status-info-if-necessary)
  (add-to-list 'global-mode-string
               '(:eval (twittering-make-unread-status-notifier-string))
               t))

(defun twittering-disable-unread-status-notifier ()
  "Disable a notifier of unread statuses on `twittering-mode'."
  (interactive)
  (setq twittering-unread-status-info nil)
  (remove-hook 'twittering-new-tweets-hook
               'twittering-update-unread-status-info)
  (remove-hook 'post-command-hook
               'twittering-reset-unread-status-info-if-necessary)
  (setq global-mode-string
        (remove '(:eval (twittering-make-unread-status-notifier-string))
                global-mode-string)))

(defun twittering-decorate-unread-status-notifier (buffer notifier)
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line 'mouse-2)
      `(lambda (e)
         (interactive "e")
         (save-selected-window
           (select-window (posn-window (event-start e)))
           (switch-to-buffer ,buffer)
           (twittering-set-number-of-unread ,buffer 0))))
    (define-key map (vector 'mode-line 'mouse-3)
      `(lambda (e)
         (interactive "e")
         (save-selected-window
           (select-window (posn-window (event-start e)))
           (switch-to-buffer-other-window ,buffer)
           (twittering-set-number-of-unread ,buffer 0))))
    (add-text-properties
     0 (length notifier)
     `(local-map ,map mouse-face mode-line-highlight help-echo
                 "mouse-2: switch to buffer, mouse-3: switch to buffer in other window")
     notifier)
    notifier))

;;; ============================================= Stream API: get real time push

(defvar twittering-stream-data-alist nil
  "An alist: ((spec data) ...)

DATA includes:
  (friends ...)
  (partial-push ...) ")

(defun twittering-stream-data-get (spec attr)
  (let ((cache (assocref spec twittering-stream-data-alist)))
    (when cache
      (assqref attr cache))))

(defun twittering-stream-data-put (spec attr value)
  (let ((cache (assocref spec twittering-stream-data-alist)))
    (setq cache `(,spec ,@(assq-delete-all attr cache)))
    (when value
      (setq cache `(,@cache (,attr . ,value))))
    (setq twittering-stream-data-alist
          `(,@(remove-if (lambda (i) (equal (car i) spec))
                         twittering-stream-data-alist)
            ,cache))))

(defun twittering-stream-filter (proc output)
  (let ((spec (twittering-get-timeline-spec-from-process proc)))
    (unless (twittering-get-buffer-from-spec spec)
      (twittering-stream-exit spec))
    (if (string-match "^\\(HTTP/1\.[01]\\) \\([0-9][0-9][0-9]\\)" output)
        (with-temp-buffer
          (insert output)
          ;; 1. check HTTP return
          (let* ((header (twittering-get-response-header (current-buffer)))
                 (header-info (and header (twittering-update-server-info header)))
                 (status-line (assqref 'status-line header-info))
                 (status-code (assqref 'status-code header-info)))
            (unless (equal status-code "200")
              (with-current-buffer (twittering-get-buffer-from-spec spec)
                (twittering-stream-exit spec))
              (error "Stream response: %s, %s" status-code status-line)))
          ;; 2. Remove HTTP header
          (twittering-remove-response-header)
          (setq output (buffer-string)))
      (setq output (concat (twittering-stream-data-get spec 'partial-push) output)))
    (twittering-stream-data-put spec 'partial-push nil)
    (unless (equal output "")
      (let* ((incomplete-end (not (equal (substring output -2) "\r\n")))
             (messages (split-string output "\r\n" t)))
        (when incomplete-end
          (twittering-stream-data-put spec 'partial-push (car (last messages)))
          (setq messages (reverse (cdr (reverse messages)))))
        (mapc (lambda (msg) (twittering-stream-handle-msg msg spec)) messages)))))

(defvar twittering-stream-debug? nil)   ; TODO, will remove.

(defun twittering-stream-handle-msg (msg spec)
  (let (statuses)
    (with-temp-buffer
      (insert msg)
      (goto-char (point-min))
      (setq statuses (twittering-construct-statuses)))
    (cond
     ((let ((st (car statuses)))        ; statuses
        (and (assqref 'id st) (assqref 'text st)))
      (twittering-update-timeline statuses spec))
     (t
      (case (caar statuses)
        ((friends)                      ; first msg
         (twittering-stream-data-put spec 'friends (cdar statuses)))
        ((delete)
         (let* ((id (assqref 'id (assqref 'status (car statuses))))
                (st (twittering-find-status id)))
           (message "%s deleted a status: %s"
                    (assqref 'name (assqref 'user st))
                    (assqref 'text st))
           (twittering-delete-status-from-data-table id)))
        (t
         (with-current-buffer (get-buffer-create "*twmode stream log for TODO*")
           (when twittering-stream-debug?
             (message "Please have a look at buffer: *twmode stream log for TODO*"))
           (insert (format "TODO: %S\n\n" statuses)))))))))

(defvar twittering-stream-mode nil
  "Buffer local.")

(defun twittering-stream-mode (&optional arg)
  "Get real time stream pushed from twitter. "
  (interactive "P")
  (unless (twittering-buffer-p)
    (error "Not a twmode buffer"))
  (unless (eq (twittering-extract-service) 'twitter)
    (error "%S does not support Stream API" (twittering-extract-service)))
  (let ((enable? (if arg
                     (> (prefix-numeric-value arg) 0)
                   (not twittering-stream-mode)))
        (spec (twittering-current-timeline-spec))
        (spec-string (twittering-current-timeline-spec-string)))
    (if enable?
        (let* ((command (cond ((equal spec-string ":home@twitter")
                               'userstream)
                              ((equal spec-string ":public@twitter")
                               'stream)
                              (t
                               (error "Stream mode is only available for :home@twitter and :public@twitter"))))
               (args `((clean-up-sentinel . ,(lambda (proc status connection-info)
                                               (case status
                                                 ((exit signal closed failed)
                                                  (twittering-release-process proc)))))))
               (proc (twittering-call-api command args)))
          (unless proc
            (error "Failed to start stream mode"))
          (twittering-register-process proc spec)
          (setq twittering-stream-mode t)
          ;; TODO add stream notify string on mode line?
          (message "Stream mode enabled"))
      (when (y-or-n-p "Really disable stream mode? ")
        (twittering-stream-exit spec)
        (message "Stream mode disabled")))))

(defun twittering-stream-mode-restart ()
  (interactive)
  (twittering-stream-mode -1)
  (twittering-stream-mode 1))

(defun twittering-stream-exit (spec)
  (twittering-remove-inactive-processes)
  (mapcar 'kill-process (twittering-find-processes-for-timeline-spec spec))
  (setq twittering-stream-data-alist
        (remove-if (lambda (i) (equal (car i) spec))
                   twittering-stream-data-alist))
  (let ((buf(twittering-get-buffer-from-spec spec)))
    (when buf
      (with-current-buffer buf
        (setq twittering-stream-mode nil)))))

;;; ============================================= misc

(defun assocref (item alist)
  (cdr (assoc item alist)))

(defun assqref (item alist)
  (cdr (assq item alist)))

(defun twittering-multibyte-string-p (string)
  "Similar to `multibyte-string-p', but consider ascii only STRING not multibyte. "
  (and (multibyte-string-p string)
       (> (string-bytes string) (length string))))

(defun twittering-take (n lst)
  "Take first N elements from LST."
  (let (ret)
    (dotimes (i n ret)
      (setq ret `(,@ret ,(elt lst i))))))

(defun twittering-drop (n lst)
  (let (ret)
    (dotimes (i n ret)
      (setq ret (cdr lst)))))

(defun twittering-remove-duplicates (list)
  "Return a copy of LIST with all duplicate elements removed.
This is non-destructive version of `delete-dups' which is not
defined in Emacs21."
  (if (fboundp 'delete-dups)
      (delete-dups (copy-sequence list))
    (let ((rest list)
          (result nil))
      (while rest
        (unless (member (car rest) result)
          (setq result (cons (car rest) result)))
        (setq rest (cdr rest)))
      (nreverse result))))

(defun twittering-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read a string in the minibuffer, with completion.
This is a modified version of `completing-read' and accepts candidates
as a list of a string on Emacs21."
  ;; completing-read() of Emacs21 does not accepts candidates as
  ;; a list. Candidates must be given as an alist.
  (let* ((collection (twittering-remove-duplicates collection))
         (collection
          (if (and (> 22 emacs-major-version)
                   (listp collection)
                   (stringp (car collection)))
              (mapcar (lambda (x) (cons x nil)) collection)
            collection)))
    (completing-read prompt collection predicate require-match
                     initial-input hist def inherit-input-method)))

(defun twittering-add-to-history (history-var elt &optional maxelt keep-all)
  (if (functionp 'add-to-history)
      (add-to-history history-var elt maxelt keep-all)
    (let* ((added (cons elt
                        (if (and (not keep-all)
                                 (boundp 'history-delete-duplicates)
                                 history-delete-duplicates)
                            (delete elt (symbol-value history-var))
                          (symbol-value history-var))))
           (maxelt (or maxelt history-length))
           (len (length added)))
      (set history-var
           (if (<= len maxelt)
               added
             (butlast added (- len maxelt)))))))

(defun twittering-build-unique-prefix (str-list)
  "Create an unique shortest prefix to identify each element of STR-LIST.
The sequence of STR-LIST is not changed.  e.g.,
  '(\"abcdef\" \"ab\" \"c\" \"def\")
       => '(\"abc\" \"ab\" \"c\" \"d\")

Duplicated elements should not exist in STR-LIST."
  (mapcar
   (lambda (el)
     (let ((pre "\\`")
           (c "")
           (str el))
       (while (and str (not (string= str "")))
         (setq c (substring str 0 1)
               str (substring str 1)
               pre (concat pre c))
         (when (= (count-if (lambda (el) (string-match pre el))
                            str-list)
                  1)
           (setq str nil)))
       (substring pre 2)))
   str-list))

(defun twittering-decorate-zebra-background (object face1 &optional face2)
  "Append zebra background to OBJECT.
The zebra face is decided by looking at adjacent face. "
  (let ((other-faces (get-text-property (point) 'face))
        start end zebra-face)
    
    ;; check previous face in the buffer
    (when (and face1 face2)
      (setq start (point))
      (save-excursion
        (while (and start
                    (setq start (previous-single-property-change start 'face)))
          (let ((faces (get-text-property start 'face)))
            (when (and faces (not (listp faces)))
              (setq faces `(,faces)))
            (cond
             ((memq face1 faces)
              (setq zebra-face face2
                    start nil))
             ((memq face2 faces)
              (setq zebra-face face1
                    start nil)))))))
    
    (unless zebra-face (setq zebra-face face1))
    (when (and zebra-face (atom zebra-face))
      (setq zebra-face `(,zebra-face)))

    ;; decorate string
    (setq start 0
          end nil)
    (while (setq end (next-single-property-change start 'face object))
      (put-text-property start end
                         'face (append zebra-face
                                       (when other-faces
                                         (if (listp other-faces)
                                             other-faces
                                           `(,other-faces))))
                         object)
      (setq start end
            other-faces (get-text-property start 'face object)))
    (put-text-property start (length object) 'face zebra-face object)
    object))

(defun twittering-decorate-uri (object)
  "Decorate uri contained in OBJECT."
  (let ((points-str '()))
    (with-temp-buffer
      (insert object)
      (goto-char (point-min))
      (while (re-search-forward twittering-regexp-uri nil t 1)
        ;; FIXME: why need `-1' here?
        (setq points-str (cons (list (1- (match-beginning 1))
                                     (1- (match-end 1))
                                     (match-string 1))
                               points-str))))
    (mapc (lambda (p-s)
            (add-text-properties
             (nth 0 p-s) (nth 1 p-s)
             `(mouse-face highlight uri ,(nth 2 p-s) face twittering-uri-face)
             object))
          points-str)
    object))

(defun twittering-decorate-listname (listname &optional display-string)
  (unless display-string (setq display-string listname))
  (add-text-properties 0 (length display-string)
                       `(mouse-face
                         highlight
                         uri ,(twittering-get-status-url listname)
                         goto-spec
                         ,(twittering-string-to-timeline-spec
                           listname)
                         face 'twittering-username-face)
                       display-string)
  display-string)

(defun twittering-html-decode-buffer (&optional buffer)
  "Decode html BUFFER(default is current buffer).
Usually used in buffer retrieved by `url-retrieve'. If no charset info
is specified in html tag, default is 'utf-8."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((coding 'utf-8))
      (when (save-excursion
              (goto-char (point-min))
              (re-search-forward "<meta http-equiv.*charset=[[:blank:]]*\\([a-zA-Z0-9_-]+\\)" nil t 1))
        (setq coding (intern (downcase (match-string 1)))))
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max) coding))))

;;;;
;;;; Process info
;;;;

(defun twittering-register-process (proc spec &optional str)
  (let ((str (or str (twittering-timeline-spec-to-string spec))))
    (add-to-list 'twittering-process-info-alist `(,proc ,spec ,str))))

(defun twittering-release-process (proc)
  (let ((pair (assoc proc twittering-process-info-alist)))
    (when pair
      (setq twittering-process-info-alist
            (delq pair twittering-process-info-alist)))))

(defun twittering-get-timeline-spec-from-process (proc)
  (let ((entry (assoc proc twittering-process-info-alist)))
    (if entry
        (elt entry 1)
      nil)))

(defun twittering-get-timeline-spec-string-from-process (proc)
  (let ((entry (assoc proc twittering-process-info-alist)))
    (if entry
        (elt entry 2)
      nil)))

(defun twittering-find-processes-for-timeline-spec (spec)
  (apply 'append
         (mapcar
          (lambda (pair)
            (let ((proc (car pair))
                  (spec-info (cadr pair)))
              (if (equal spec-info spec)
                  `(,proc)
                nil)))
          twittering-process-info-alist)))

(defun twittering-remove-inactive-processes ()
  (let ((inactive-statuses '(nil closed exit failed signal)))
    (setq twittering-process-info-alist
          (apply 'append
                 (mapcar
                  (lambda (pair)
                    (let* ((proc (car pair))
                           (info (cdr pair))
                           (status (process-status proc)))
                      (if (memq status inactive-statuses)
                          nil
                        `((,proc ,@info)))))
                  twittering-process-info-alist)))))

(defun twittering-process-active-p (&optional spec)
  (twittering-remove-inactive-processes)
  (if spec
      (twittering-find-processes-for-timeline-spec spec)
    twittering-process-info-alist))

;;;;
;;;; Server info
;;;;

(defun twittering-update-server-info (header-str)
  (let* ((header-info (twittering-make-header-info-alist header-str))
         (new-entry-list (mapcar 'car header-info)))
    (when (remove t (mapcar
                     (lambda (entry)
                       (equal (assoc entry header-info)
                              (assoc entry twittering-server-info-alist)))
                     new-entry-list))
      (setq twittering-server-info-alist
            (append header-info
                    (remove nil (mapcar
                                 (lambda (entry)
                                   (if (member (car entry) new-entry-list)
                                       nil
                                     entry))
                                 twittering-server-info-alist))))
      (when twittering-display-remaining
        (mapc (lambda (buffer)
                (with-current-buffer buffer
                  (twittering-update-mode-line)))
              (twittering-get-buffer-list))))
    header-info))

(defun twittering-get-server-info (field)
  (let* ((table
          '((ratelimit-remaining . "X-RateLimit-Remaining")
            (ratelimit-limit . "X-RateLimit-Limit")
            (ratelimit-reset . "X-RateLimit-Reset")))
         (numeral-field '(ratelimit-remaining ratelimit-limit))
         (unix-epoch-time-field '(ratelimit-reset))
         (field-name (assqref field table))
         (field-value (assocref field-name twittering-server-info-alist)))
    (when (and field-name field-value)
      (cond
       ((memq field numeral-field)
        (string-to-number field-value))
       ((memq field unix-epoch-time-field)
        (seconds-to-time (string-to-number (concat field-value ".0"))))
       (t
        nil)))))

(defun twittering-get-ratelimit-remaining ()
  (or (twittering-get-server-info 'ratelimit-remaining)
      0))

(defun twittering-get-ratelimit-limit ()
  (or (twittering-get-server-info 'ratelimit-limit)
      0))

;;;; Window configuration
;;;;

(defun twittering-set-window-end (window pos)
  (let* ((height (window-text-height window))
         (n (- (- height 1))))
    (while (progn (setq n (1+ n))
                  (set-window-start
                   window
                   (with-current-buffer (window-buffer window)
                     (save-excursion
                       (goto-char pos)
                       (line-beginning-position n))))
                  (not (pos-visible-in-window-p pos window))))))

(defun twittering-current-window-config (window-list)
  "Return window parameters of WINDOW-LIST."
  (mapcar (lambda (win)
            (let ((start (window-start win))
                  (point (window-point win)))
              `(,win ,start ,point)))
          window-list))

(defun twittering-restore-window-config-after-modification (config beg end)
  "Restore window parameters changed by modification on given region.
CONFIG is window parameters made by `twittering-current-window-config'.
BEG and END mean a region that had been modified."
  (mapc (lambda (entry)
          (let ((win (elt entry 0))
                (start (elt entry 1))
                (point (elt entry 2)))
            (when (and (< beg start) (< start end))
              (set-window-start win start))
            (when (and (< beg point) (< point end))
              (set-window-point win point))))
        config))


;;;; Cursor motion

(defun twittering-get-id-at (&optional pos)
  "Return ID of the status at POS. If a separator is rendered at POS, return
the ID of the status rendered before the separator. The default value of POS
is `(point)'."
  (let ((pos (or pos (point))))
    (or (get-text-property pos 'id)
        (let ((prev (or (twittering-get-previous-status-head pos)
                        (point-min))))
          (and prev (get-text-property prev 'id))))))

(defun twittering-get-current-status-head (&optional pos)
  "Return the head position of the status at POS.
If POS is nil, the value of point is used for POS.
If a separator is rendered at POS, return the head of the status followed
by the separator.
Return POS if no statuses are rendered."
  (let* ((pos (or pos (point)))
         (field-id (get-text-property pos 'field))
         (head (field-beginning pos)))
    (cond
     ((null field-id)
      ;; A separator is rendered at `pos'.
      (if (get-text-property head 'field)
          ;; When `pos' points the head of the separator, `head' points
          ;; to the beginning of the status followed by the separator.
          head
        ;; In the case that `pos' points to a character of the separator,
        ;; but not to the head of the separator.
        (field-beginning head)))
     ((null (get-text-property head 'field))
      ;; When `head' points to a separator, `pos' points to the head
      ;; of a status.
      pos)
     (t
      head))))

(defun twittering-goto-first-status ()
  "Go to the first status."
  (interactive)
  (goto-char (or (twittering-get-first-status-head)
                 (point-min))))

(defun twittering-jump-to-status (id)
  (twittering-goto-first-status)
  (let (tmp)
    (while (and (not (equal tmp (twittering-get-id-at)))
                (not (equal (setq tmp (twittering-get-id-at)) id)))
      (twittering-goto-next-status))))

(defun twittering-get-first-status-head ()
  "Return the head position of the first status in the current buffer.
Return nil if no statuses are rendered."
  (if (get-text-property (point-min) 'field)
      (point-min)
    (twittering-get-next-status-head (point-min))))

(defun twittering-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos (twittering-get-next-status-head)))
    (cond
     (pos
      (goto-char pos)
      (forward-char))
     (twittering-reverse-mode
      (message "The latest status."))
     (t
      (let ((id
             (cond
              ((twittering-timeline-spec-user-methods-p
                (twittering-current-timeline-spec))
               'previous-cursor)
              (t
               (or (get-text-property (point) 'id)
                   (let ((prev (twittering-get-previous-status-head)))
                     (when prev
                       (get-text-property prev 'id))))))))
        (when id
          (message "Get more previous timeline...")
          (twittering-get-and-render-timeline nil id)))))))

(defun twittering-get-next-status-head (&optional pos)
  "Search forward from POS for the nearest head of a status.
Return nil if there are no following statuses.
Otherwise, return a positive integer greater than POS."
  (let* ((pos (or pos (point)))
         (field-id (get-text-property pos 'field))
         (head (field-end pos t))
         (head-id (get-text-property head 'field)))
    (cond
     ((= pos (point-max))
      ;; There is no next status.
      nil)
     ((and (null field-id) head-id)
      ;; `pos' points to a separator and `head' points to a head
      ;; of a status.
      head)
     ((null head-id)
      ;; `head' points to a head of a separator.
      (let ((next-head (field-end head t)))
        (if (get-text-property next-head 'field)
            next-head
          ;; There is no next status.
          nil)))
     (t
      head))))

(defun twittering-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((prev-pos
         (twittering-get-previous-status-head (line-beginning-position))))
    (cond
     (prev-pos
      (goto-char prev-pos)
      (forward-char))
     (twittering-reverse-mode
      (let ((id
             (cond
              ((twittering-timeline-spec-user-methods-p
                (twittering-current-timeline-spec))
               'next-cursor)
              (t
               (or (get-text-property (point) 'id)
                   (let ((next (twittering-get-next-status-head)))
                     (when next
                       (get-text-property next 'id))))))))
        (when id
          (message "Get more previous timeline...")
          (twittering-get-and-render-timeline nil id))))
     (t
      (message "The latest status.")))))

(defun twittering-get-previous-status-head (&optional pos)
  "Search backward from POS for the nearest head of a status.
If POS points to a head of a status, return the head of the *previous* status.
If there are no preceding statuses, return nil.
Otherwise, return a positive integer less than POS."
  (let* ((pos (or pos (point)))
         (field-id (get-text-property pos 'field))
         (head (field-beginning pos t))
         (head-id (get-text-property head 'field)))
    (cond
     ((= pos (point-min))
      ;; There is no previous status.
      nil)
     ((and (null field-id) head-id)
      ;; `pos' points to a separator and `head' points to a head
      ;; of a status.
      head)
     ((null head-id)
      ;; `head' points to a head of a separator.
      (let ((prev-head (field-beginning head t)))
        (if (get-text-property prev-head 'field)
            prev-head
          ;; There is no previous status.
          nil)))
     (t
      head))))

(defun twittering-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (twittering-get-username-at-pos (point)))
        (pos (twittering-get-next-status-head (point))))
    (while (and (not (eq pos nil))
                (not (equal (twittering-get-username-at-pos pos) user-name)))
      (setq pos (twittering-get-next-status-head pos)))
    (if pos
        (progn
          (goto-char pos)
          (forward-char))
      (if user-name
          (message "End of %s's status." user-name)
        (message "Invalid user-name.")))))

(defun twittering-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (twittering-get-username-at-pos (point)))
        (prev-pos (point))
        (pos (twittering-get-previous-status-head
              (line-beginning-position))))
    (while (and (not (eq pos nil))
                (not (eq pos prev-pos))
                (not (equal (twittering-get-username-at-pos pos) user-name)))
      (setq prev-pos pos)
      (setq pos (twittering-get-previous-status-head pos)))
    (if (and pos
             (not (eq pos prev-pos))
             (equal (twittering-get-username-at-pos pos) user-name))
        (progn
          (goto-char pos)
          (forward-char))
      (if user-name
          (message "Start of %s's status." user-name)
        (message "Invalid user-name.")))))

(defun twittering-goto-next-thing (&optional backward)
  "Go to next interesting thing. ex) username, URI, ... "
  (interactive)
  (let* ((property-change-f (if backward
                                'previous-single-property-change
                              'next-single-property-change))
         (pos (funcall property-change-f (point) 'mouse-face)))
    (while (and pos
                (not
                 (let* ((current-face (get-text-property pos 'mouse-face))
                        (face-pred
                         (lambda (face)
                           (cond
                            ((listp current-face) (memq face current-face))
                            ((symbolp current-face) (eq face current-face))
                            (t nil)))))
                   (remove nil (mapcar face-pred '(highlight))))))
      (setq pos (funcall property-change-f pos 'mouse-face)))
    (when pos
      (goto-char pos))))

(defun twittering-goto-previous-thing (&optional backward)
  "Go to previous interesting thing. ex) username, URI, ... "
  (interactive)
  (twittering-goto-next-thing (not backward)))

(defun twittering-get-username-at-pos (pos)
  (or (get-text-property pos 'username)
      (get-text-property (max (point-min) (1- pos)) 'username)
      (let* ((border (or (previous-single-property-change pos 'username)
                         (point-min)))
             (pos (max (point-min) (1- border))))
        (get-text-property pos 'username))))

(defun twittering-get-all-usernames-at-pos (&optional point)
  "Get all usernames contained in tweet at POINT.
If there is an username under POINT, make it as `car', else make
tweet author as the `car'.

See also `twittering-get-username-at-pos'. "
  (unless point (setq point (point)))
  (let ((start (twittering-get-current-status-head point))
        (end (or (twittering-get-next-status-head point) (point-max)))
        n names
        (top2
         (remove nil
                 (list (get-text-property (point) 'screen-name-in-text)
                       (twittering-get-username-at-pos point)))))
    (when (setq n (get-text-property start 'screen-name-in-text))
      (setq names (cons n names)))
    (while (and (setq start (next-single-property-change
                             start 'screen-name-in-text))
                (< start end))
      (when (setq n (get-text-property start 'screen-name-in-text))
        (setq names (cons n names))))
    (setq names (remove-duplicates names :test 'string=))
    (when top2
      (setq names (append top2 (delete-if (lambda (s) (member s top2))
                                          names))))
                                        ; (mapcar 'substring-no-properties names)
    names))

(defun twittering-scroll-up()
  "Scroll up if possible; otherwise invoke `twittering-goto-next-status',
which fetch older tweets on non reverse-mode."
  (interactive)
  (cond
   ((= (point) (point-max))
    (twittering-goto-next-status))
   ((= (window-end) (point-max))
    (goto-char (point-max)))
   (t
    (scroll-up))))

(defun twittering-scroll-down()
  "Scroll down if possible; otherwise invoke `twittering-goto-previous-status',
which fetch older tweets on reverse-mode."
  (interactive)
  (cond
   ((= (point) (point-min))
    (twittering-goto-previous-status))
   ((= (window-start) (point-min))
    (goto-char (point-min)))
   (t
    (scroll-down))))

;;;; Kill ring

(defun twittering-push-uri-onto-kill-ring ()
  "Push URI on the current position onto the kill ring.
If the character on the current position does not have `uri' property,
this function does nothing."
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (cond
     ((not (stringp uri))
      nil)
     ((and kill-ring (string= uri (current-kill 0 t)))
      (message "Already copied %s" uri)
      uri)
     (t
      (kill-new uri)
      (message "Copied %s" uri)
      uri))))

(defun twittering-push-tweet-onto-kill-ring ()
  "Copy the tweet (format: \"username: text\") to the kill-ring."
  (interactive)
  (let* ((username (get-text-property (point) 'username))
         (text (get-text-property (point) 'text))
         (copy (if (and username text)
                   (format "%s: %s" username text)
                 nil)))
    (cond
     ((null copy)
      nil)
     ((and kill-ring (string= copy (current-kill 0 t)))
      (message "Already copied %s" copy))
     (t
      (kill-new copy)
      (message "Copied %s" copy)
      copy))))

;;;; Debug mode
;;;;

(defvar twittering-debug-mode nil)
(defvar twittering-debug-buffer "*debug*")

(defvar twittering-debug-curl nil)

(defun twittering-get-or-generate-buffer (buffer)
  (if (bufferp buffer)
      (if (buffer-live-p buffer)
          buffer
        (generate-new-buffer (buffer-name buffer)))
    (if (stringp buffer)
        (or (get-buffer buffer)
            (generate-new-buffer buffer)))))

(defun twittering-debug-buffer ()
  (twittering-get-or-generate-buffer twittering-debug-buffer))

(defun debug-printf (fmt &rest args)
  (when twittering-debug-mode
    (message "%s\n" (concat "[debug] " (apply 'format fmt args)))
    (with-current-buffer (twittering-debug-buffer)
      (insert "[debug] " (apply 'format fmt args))
      (newline))))

(defun twittering-debug-mode ()
  (interactive)
  (setq twittering-debug-mode
        (not twittering-debug-mode))
  (message (if twittering-debug-mode "debug mode:on" "debug mode:off")))

;;;; CA certificate
;;;;

(defvar twittering-cert-file nil)

(defun twittering-delete-ca-cert-file ()
  (when (and twittering-cert-file
             (file-exists-p twittering-cert-file))
    (delete-file twittering-cert-file)
    (setq twittering-cert-file nil)))

;; FIXME: file name is hard-coded. More robust way is desired.
;; https://www.geotrust.com/resources/root_certificates/certificates/Equifax_Secure_Certificate_Authority.cer
(defun twittering-ensure-ca-cert ()
  "Create a CA certificate file if it does not exist, and return
its file name. The certificate is retrieved from
`https://www.geotrust.com/resources/root_certificates/certificates/Equifax_Secure_Certificate_Authority.cer'."
  (if twittering-cert-file
      twittering-cert-file
    (let ((file-name (make-temp-file "twmode-cacert")))
      (with-temp-file file-name
        (insert "-----BEGIN CERTIFICATE-----
MIIDIDCCAomgAwIBAgIENd70zzANBgkqhkiG9w0BAQUFADBOMQswCQYDVQQGEwJV
UzEQMA4GA1UEChMHRXF1aWZheDEtMCsGA1UECxMkRXF1aWZheCBTZWN1cmUgQ2Vy
dGlmaWNhdGUgQXV0aG9yaXR5MB4XDTk4MDgyMjE2NDE1MVoXDTE4MDgyMjE2NDE1
MVowTjELMAkGA1UEBhMCVVMxEDAOBgNVBAoTB0VxdWlmYXgxLTArBgNVBAsTJEVx
dWlmYXggU2VjdXJlIENlcnRpZmljYXRlIEF1dGhvcml0eTCBnzANBgkqhkiG9w0B
AQEFAAOBjQAwgYkCgYEAwV2xWGcIYu6gmi0fCG2RFGiYCh7+2gRvE4RiIcPRfM6f
BeC4AfBONOziipUEZKzxa1NfBbPLZ4C/QgKO/t0BCezhABRP/PvwDN1Dulsr4R+A
cJkVV5MW8Q+XarfCaCMczE1ZMKxRHjuvK9buY0V7xdlfUNLjUA86iOe/FP3gx7kC
AwEAAaOCAQkwggEFMHAGA1UdHwRpMGcwZaBjoGGkXzBdMQswCQYDVQQGEwJVUzEQ
MA4GA1UEChMHRXF1aWZheDEtMCsGA1UECxMkRXF1aWZheCBTZWN1cmUgQ2VydGlm
aWNhdGUgQXV0aG9yaXR5MQ0wCwYDVQQDEwRDUkwxMBoGA1UdEAQTMBGBDzIwMTgw
ODIyMTY0MTUxWjALBgNVHQ8EBAMCAQYwHwYDVR0jBBgwFoAUSOZo+SvSspXXR9gj
IBBPM5iQn9QwHQYDVR0OBBYEFEjmaPkr0rKV10fYIyAQTzOYkJ/UMAwGA1UdEwQF
MAMBAf8wGgYJKoZIhvZ9B0EABA0wCxsFVjMuMGMDAgbAMA0GCSqGSIb3DQEBBQUA
A4GBAFjOKer89961zgK5F7WF0bnj4JXMJTENAKaSbn+2kmOeUJXRmm/kEd5jhW6Y
7qj/WsjTVbJmcVfewCHrPSqnI0kBBIZCe/zuf6IWUrVnZ9NA2zsmWLIodz2uFHdh
1voqZiegDfqnc1zqcPGUIWVEX/r87yloqaKHee9570+sB3c4
-----END CERTIFICATE-----
"))
      (add-hook 'kill-emacs-hook 'twittering-delete-ca-cert-file)
      (setq twittering-cert-file file-name))))

;;;;
;;;; User agent
;;;;

(defvar twittering-user-agent-function 'twittering-user-agent-default-function)

(defun twittering-user-agent-default-function ()
  "Twittering mode default User-Agent function."
  (format "Emacs/%d.%d Twittering-mode/%s"
          emacs-major-version emacs-minor-version
          twittering-mode-version))

(defun twittering-user-agent ()
  "Return User-Agent header string."
  (funcall twittering-user-agent-function))


;;;; Reading username/listname with completion
;;;;

(defun twittering-get-usernames-from-timeline (&optional timeline-data)
  (let ((timeline-data (or timeline-data (twittering-current-timeline-data))))
    (twittering-remove-duplicates
     (mapcar
      (lambda (status)
        (let* ((base-str (assqref 'screen-name (assqref 'user status)))
               ;; `copied-str' is independent of the string in timeline-data.
               ;; This isolation is required for `minibuf-isearch.el',
               ;; which removes the text properties of strings in history.
               (copied-str (copy-sequence base-str)))
          ;; (set-text-properties 0 (length copied-str) nil copied-str)
          copied-str))
      timeline-data))))

(defun twittering-read-username-with-completion (prompt init-user &optional history)
  (let ((collection (append (twittering-get-all-usernames-at-pos)
                            twittering-user-history
                            (twittering-get-usernames-from-timeline))))
    (twittering-completing-read prompt collection nil nil init-user history)))

(defun twittering-read-list-name (username &optional list-index)
  (let* ((list-index (or list-index
                         (twittering-get-simple-sync
                          'get-list-index `((username . ,username)))))
         (username (prog1 (copy-sequence username)
                     (set-text-properties 0 (length username) nil username)))
         (prompt (format "%s's list: " username))
         (listname
          (if list-index
              (twittering-completing-read
               prompt
               (mapcar
                (lambda (l) (car (last (twittering-string-to-timeline-spec l))))
                list-index)
               nil
               t)
            nil)))
    (if (string= "" listname)
        nil
      listname)))

(defun twittering-read-timeline-spec-with-completion (prompt initial &optional as-string)
  "Return a timeline-spec string with 'goto-spec property."
  (let* ((dummy-hist
          (remove
           nil
           (append
            (twittering-get-all-usernames-at-pos)
            twittering-timeline-history
            (twittering-get-usernames-from-timeline)
            '(":direct_messages" ":direct_messages_sent" ":friends"
              ":home" ":mentions" ":public" ":replies"
              ":retweeted_by_me" ":retweeted_to_me" ":retweets_of_me")
            (mapcar (lambda (tl) (concat (twittering-get-accounts 'username) "/" tl))
                    '("followers" "following" "favorites"))
            )))
         (spec-string (twittering-completing-read
                       prompt dummy-hist nil nil initial 'dummy-hist))
         (spec-string
          (cond
           ((string-match "^:favorites/$" spec-string)
            (let ((username
                   (twittering-read-username-with-completion
                    "Whose favorites: " ""
                    (twittering-get-usernames-from-timeline))))
              (if username
                  (concat ":favorites/" username)
                nil)))
           ((string-match "^\\([a-zA-Z0-9_-]+\\)/$" spec-string)
            (let* ((username (match-string 1 spec-string))
                   (list-index (twittering-get-simple-sync
                                'get-list-index `((username . ,username))))
                   (listname
                    (if list-index
                        (twittering-read-list-name username list-index)
                      nil)))
              (if listname
                  (concat username "/" listname)
                nil)))
           (t
            spec-string)))

         (spec (if (stringp spec-string)
                   (condition-case error-str
                       (or (get-text-property 0 'goto-spec spec-string)
                           (twittering-string-to-timeline-spec spec-string))
                     (error
                      (message "Invalid timeline spec: %s" error-str)
                      nil))
                 nil)))
    (cond
     ((null spec) nil)
     (spec (if as-string
               ;;   (if (string-match "@.+" spec-string)
               ;;            spec-string
               ;;   (format "%s@%S" spec-string twittering-service-method))
               ;; `((,twittering-service-method) ,@spec)))
               spec-string spec))
     ((string= "" spec-string) (message "No timeline specs are specified.") nil)
     (t (message "\"%s\" is invalid as a timeline spec." spec-string) nil))))

;;;; Status retrieval
;;;;

(defun twittering-add-timeline-history (spec-string)
  (when (or (null twittering-timeline-history)
            (not (string= spec-string (car twittering-timeline-history))))
    (twittering-add-to-history 'twittering-timeline-history spec-string)))

(defun twittering-atom-xmltree-to-status-datum (atom-xml-entry)
  (let ((id-str (car (cddr (assq 'id atom-xml-entry))))
        (time-str (car (cddr (assq 'updated atom-xml-entry))))
        (author-str (car (cddr (assq 'name (assq 'author atom-xml-entry))))))
    `((created-at
       ;; ISO 8601
       ;; Twitter -> "2010-05-08T05:59:41Z"
       ;; StatusNet -> "2010-05-08T08:44:39+00:00"
       . ,(if (string-match "\\(.*\\)T\\(.*\\)\\(Z\\|\\([-+][0-2][0-9]\\):?\\([0-5][0-9]\\)\\)" time-str)
              ;; time-str is formatted as
              ;; "Combined date and time in UTC:" in ISO 8601.
              (let ((timezone (match-string 3 time-str)))
                (format "%s %s %s"
                        (match-string 1 time-str) (match-string 2 time-str)
                        (if (string= "Z" timezone)
                            "+0000"
                          (concat (match-string 4 time-str)
                                  (match-string 5 time-str)))))
            ;; unknown format?
            time-str))
      (id . ,(progn
               (string-match ":\\([0-9]+\\)$" id-str)
               (match-string 1 id-str)))
      ,@(let ((source (twittering-decode-html-entities
                       (car (cddr (assq 'twitter:source atom-xml-entry))))))
          `(,@(if (string-match "<a href=\"\\(.*?\\)\".*?>\\(.*\\)</a>"
                                source)
                  (let ((uri (match-string-no-properties 1 source))
                        (caption (match-string-no-properties 2 source)))
                    `((source . ,caption)
                      (source-uri . ,uri)))
                `((source . ,source)
                  (source-uri . "")))))
      (text . ,(twittering-decode-html-entities
                (car (cddr (assq 'title atom-xml-entry)))))
      ,@(cond
         ((and (eq (twittering-extract-service) 'statusnet)
               (string-match "^\\([^ ]+\\)\\( (\\(.*\\))\\)?$" author-str))
          ;; StatusNet
          `((user-screen-name . ,(match-string 1 author-str))
            (user-name . ,(or (match-string 3 author-str) ""))))
         ((string-match "^\\([^ ]+\\) (\\(.*\\))$" author-str)
          ;; Twitter (default)
          `((user-screen-name . ,(match-string 1 author-str))
            (user-name . ,(match-string 2 author-str))))
         (t
          '((user-screen-name . "PARSING FAILED!!")
            (user-name . ""))))
      (user-profile-image-url
       . ,(let* ((link-items
                  (mapcar
                   (lambda (item)
                     (when (eq 'link (car-safe item))
                       (cadr item)))
                   atom-xml-entry))
                 (image-urls
                  (mapcar
                   (lambda (item)
                     (cond
                      ((and (eq (twittering-extract-service) 'statusnet)
                            (member '(rel . "related") item))
                       ;; StatusNet
                       (assqref 'href item))
                      ((member '(rel . "image") item)
                       ;; Twitter (default)
                       (assqref 'href item))
                      (t
                       nil)))
                   link-items)))
            (car-safe (remq nil image-urls)))))))

(defun twittering-atom-xmltree-to-status (atom-xmltree)
  (let ((entry-list
         (apply 'append
                (mapcar (lambda (x)
                          (if (eq (car-safe x) 'entry) `(,x) nil))
                        (cdar atom-xmltree)))))
    (mapcar 'twittering-atom-xmltree-to-status-datum
            entry-list)))

(defvar twittering-emotions-phrase-url-alist nil)
(defvar twittering-is-getting-emotions-p nil)

(defun twittering-decode-html-entities (encoded-str)
  (if (consp encoded-str)
      encoded-str
    (if encoded-str
        (let ((cursor 0)
              (found-at nil)
              (result '()))
          (while (setq found-at
                       (string-match "&\\(#\\([0-9]+\\)\\|\\([a-zA-Z]+\\)\\);"
                                     encoded-str cursor))
            (when (> found-at cursor)
              (list-push (substring encoded-str cursor found-at) result))
            (let ((number-entity (match-string-no-properties 2 encoded-str))
                  (letter-entity (match-string-no-properties 3 encoded-str)))
              (cond (number-entity
                     (list-push
                      (char-to-string
                       (twittering-ucs-to-char
                        (string-to-number number-entity))) result))
                    (letter-entity
                     (cond ((string= "gt" letter-entity) (list-push ">" result))
                           ((string= "lt" letter-entity) (list-push "<" result))
                           ((string= "quot" letter-entity) (list-push "\"" result))
                           (t (list-push "?" result))))
                    (t (list-push "?" result)))
              (setq cursor (match-end 0))))
          (list-push (substring encoded-str cursor) result)
          (apply 'concat (nreverse result)))
      "")))

(defun twittering-wash-json (tree)
  "Convert symbols like `a_b' to `a-b', and stringfy cdr values."
  (let ((listy (lambda (symbol)
                 (if (symbolp symbol)
                     (intern (replace-regexp-in-string
                              "_" "-" (symbol-name symbol)))
                   symbol))))
    (if (atom tree)
        (funcall listy tree)
      (if (and (eq (car tree) 'friends)
               (format-proper-list-p tree)
               (> (length tree) 5))
          ;; FIXME: stream `friends' msg hack, to avoid too deep recursion.
          (mapcar 'twittering-wash-json tree)
        (when (consp (car tree))
          (when (and (assqref 'id_str tree)
                     (assqref 'id tree))
              (setq tree `(,@(assq-delete-all 'id tree)
                           (id . ,(assqref 'id_str tree)))))

          (when (and (assqref 'in_reply_to_status_id_str tree)
                     (assqref 'in_reply_to_status_id tree))
            (setq tree `(,@(assq-delete-all 'in_reply_to_status_id tree)
                         (in_reply_to_status_id . ,(assqref 'in_reply_to_status_id_str tree))))))

        (let ((front (car tree))
              (rear (cdr tree)))
          (cond
           ((atom front)
            ;; wash douban json
            (cond ((and (symbolp front)
                        (string-match "\\`\\(@\\|\\$\\)" (symbol-name front)))
                   rear)
                  (t
                   (cons (case front    ; direct-message
                           ((sender) 'user)
                           ((recipient-id) 'in-reply-to-user-id)
                           (t
                            (funcall listy front)))

                         (if (atom rear)
                             (cond ((and (eq front 'id) (numberp rear))
                                    (twittering-number-to-string rear))
                                   ((or (null rear)
                                        (and (stringp rear) (string= rear ""))
                                        (eq rear ':json-false))
                                    '())
                                   ((eq rear ':json-true)
                                    t)
                                   ((stringp rear) rear)
                                   (t (format "%S" rear)))
                           (twittering-wash-json rear))))))

           ((consp front)
            ;; FIXME: too deep lisp.
            (mapcar 'twittering-wash-json tree)
            ;; (cons (twittering-wash-json front)
            ;;    (twittering-wash-json rear))
            )))))))

(defun twittering-wash-json-douban (tree)
  "(a (b)) => (a b)"
  (cond
   ((atom tree)
    tree)
   (t
    (let ((front (car tree))
          (rear (cdr tree)))
      (cond
       ((atom front)
        (when (and (consp rear) (= (length rear) 1))
          (setq rear (car rear)))
        (when (and (atom rear) (stringp front))
          (when (string-match "\\`https?://" front)
            (let ((tmp front))
              (setq front rear
                    rear tmp)))
          (when (stringp front)
            (setq front (intern front))))

        (cons front (if (atom rear)
                        rear
                      (twittering-wash-json-douban rear))))
       (t
        (mapcar 'twittering-wash-json-douban tree)))))))

(defun twittering-construct-statuses ()
  (let ((statuses (twittering-construct-statuses-1)))
    (mapcar (lambda (st)
              (if (ignore-errors (assqref 'id-str st))
                  `(,@(remove-if (lambda (i) (eq (car i) 'id)) st)
                    (id . ,(assqref 'id-str st)))
                st))
            statuses)))

(defun twittering-construct-statuses-1 ()
  (let ((statuses (twittering-json-read))
        (has (lambda (symbol)
               (find-if (lambda (i) (eq (car i) symbol)) statuses))))
    (cond
     ((funcall has 'entry)         ; douban.com
      (setq statuses (twittering-wash-json-douban statuses))
      (setq statuses
            (mapcar
             (lambda (i)
               (let ((link (assqref 'link i)))
                 (unless (consp (cdr link))
                   (setq link `(,link)))

                 `((id . ,(car (last (split-string (assqref 'id i) "/"))))
                   (created-at . ,(replace-regexp-in-string
                                   "+08:00" "+0800" (assqref 'published i)))
                   (text . ,(concat (cdr (assqref 'content i))
                                    (let ((rating (ignore-errors
                                                    (assqref 'rating (assqref 'db:attribute i)))))
                                      (when rating
                                        (concat "  评分："
                                                (twittering-make-rating-string
                                                 (* (string-to-number rating) 2)))))))
                   (thumbnail-pic . ,(assqref 'image link))
                   (source . "douban")
                   (user
                    ,@(let ((author (assqref 'author i)))
                        `((id . ,(car (last (split-string (assqref 'uri author) "/"))))
                          (name . ,(let ((s (assqref 'alternate (assqref 'link author))))
                                     (nth 1 (reverse (split-string s "/")))))
                          (screen-name . ,(assqref 'name author))
                          (profile-image-url . ,(assqref 'icon (assqref 'link author))))))
                   ;; douban specific
                   (detail . ,(or (assqref 'objective link)
                                  (assqref 'related link))))))
             (assqref 'entry statuses))))

     ((or (funcall has 'user)               ; `show' a single tweet
          (funcall has 'id))                 ; show-user
      `((,@statuses)))

     ((funcall has 'users)              ; followers
      (let ((followers (assqref 'users statuses))
            (cursors (assq-delete-all 'users statuses)))
        (mapcar (lambda (follower)
                  (let ((status (assqref 'status follower))
                        (user (assq-delete-all 'status follower)))
                    (unless status ;; he/she has zero tweets..
                      (setq status `(,@user (text . " "))))
                    `((user ,@user) ,@status ,@cursors)))
                followers)))

     ((funcall has 'messages)           ; socialcast
      (setq statuses
            (mapcar
             (lambda (i)
               `((id         . ,(assqref 'id i))
                 (created-at . ,(assqref 'created-at i))
                 (text       . ,(or (mapconcat 'identity
                                               (remove
                                                nil
                                                `(,(or (assqref 'title i) (assqref 'action i))
                                                  ,(assqref 'body i)))
                                               "\n\n")
                                    "FIXME"))
                 ,@(when (assqref 'thumbnail-url i)
                     `((thumbnail-pic . ,(replace-regexp-in-string ":443/" "/" (assqref 'thumbnail-url i)))
                       (original-pic . ,(replace-regexp-in-string ":443/" "/" (assqref 'url (car (assqref 'media-files i)))))))
                 (source     . ,(assqref 'formal-name (assqref 'source i)))
                 (source-uri . ,(assqref 'url (assqref 'source i)))
                 (likes-count . ,(assqref 'likes-count i))
                 (comments-count . ,(assqref 'comments-count i))
                 (user
                  ,@(let ((u (assqref 'user i)))
                      `((id                . ,(assqref 'id u))
                        (name              . ,(assqref 'name u))
                        (screen-name       . ,(assqref 'username u))
                        (profile-image-url . ,(replace-regexp-in-string
                                               ":443/" "/"
                                               (assqref 'square45 (assqref 'avatars u)))))))))
             (assqref 'messages statuses))))

     ((funcall has 'favorites)
      (mapcar (lambda (fav)
                `(,@(assqref 'status fav)
                  ,(assq 'favorited-time fav)
                  ,(assq 'tags fav)))
              (assqref 'favorites statuses)))
     
     ((funcall has 'comments)
      (assqref 'comments statuses))

     ((funcall has 'statuses)          
      (assqref 'statuses statuses))

     (t
      statuses))))

;;;;
;;;; Buffer info
;;;;

(defvar twittering-buffer-info-list nil
  "List of buffers managed by `twittering-mode'.")

(defun twittering-get-buffer-list ()
  "Return buffers managed by `twittering-mode'."
  (twittering-unregister-killed-buffer)
  twittering-buffer-info-list)

(defun twittering-get-active-buffer-list ()
  "Return active buffers managed by `twittering-mode', where statuses are
retrieved periodically."
  (twittering-unregister-killed-buffer)
  (remove nil
          (mapcar (lambda (buffer)
                    (if (twittering-buffer-active-p buffer)
                        buffer
                      nil))
                  twittering-buffer-info-list)))

(defun twittering-buffer-p (&optional buffer)
  "Return t if BUFFER is managed by `twittering-mode'.
BUFFER defaults to the the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (buffer-live-p buffer)
         (memq buffer twittering-buffer-info-list))))

(defun twittering-buffer-related-p ()
  "Return t if current buffer relates to `twittering-mode'."
  (or (twittering-buffer-p)
      (eq major-mode 'twittering-edit-mode)
      (string= (buffer-name (current-buffer))
               twittering-debug-buffer)))

(defun twittering-buffer-active-p (&optional buffer)
  "Return t if BUFFER is an active buffer managed by `twittering-mode'.
BUFFER defaults to the the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (and (twittering-buffer-p buffer)
         (with-current-buffer buffer
           twittering-active-mode))))

(defun twittering-get-buffer-from-spec (spec)
  "Return the buffer bound to SPEC. If no buffers are bound to SPEC,
return nil."
  (let* ((spec-string (twittering-timeline-spec-to-string spec))
         (buffers
          (remove
           nil
           (mapcar
            (lambda (buffer)
              (if (twittering-equal-string-as-timeline
                   spec-string
                   (twittering-get-timeline-spec-string-for-buffer buffer))
                  buffer
                nil))
            (twittering-get-buffer-list)))))
    (if buffers
        ;; We assume that the buffer with the same spec is unique.
        (car buffers)
      nil)))

(defun twittering-get-buffer-from-spec-string (spec-string)
  "Return the buffer bound to SPEC-STRING. If no buffers are bound to it,
return nil."
  (let ((spec (twittering-string-to-timeline-spec spec-string)))
    (and spec (twittering-get-buffer-from-spec spec))))

(defun twittering-get-timeline-spec-for-buffer (buffer)
  "Return the timeline spec bound to BUFFER. If BUFFER is not managed by
`twittering-mode', return nil."
  (when (twittering-buffer-p buffer)
    (with-current-buffer buffer
      twittering-timeline-spec)))

(defun twittering-get-timeline-spec-string-for-buffer (buffer)
  "Return the timeline spec string bound to BUFFER. If BUFFER is not managed
by `twittering-mode', return nil."
  (when (twittering-buffer-p buffer)
    (with-current-buffer buffer
      twittering-timeline-spec-string)))

(defun twittering-current-timeline-spec ()
  "Return the timeline spec bound to the current buffer. If it is not managed
by `twittering-mode', return nil."
  (twittering-get-timeline-spec-for-buffer (current-buffer)))

(defun twittering-current-timeline-spec-string ()
  "Return the timeline spec string bound to the current buffer. If it is not
managed by `twittering-mode', return nil."
  (twittering-get-timeline-spec-string-for-buffer (current-buffer)))

(defun twittering-unregister-buffer (buffer &optional keep-timer)
  "Unregister BUFFER from `twittering-buffer-info-list'.
If BUFFER is the last managed buffer and KEEP-TIMER is nil, call
`twittering-stop' to stop timers."
  (when (memq buffer twittering-buffer-info-list)
    (setq twittering-buffer-info-list
          (delq buffer twittering-buffer-info-list))
    (when (and (null twittering-buffer-info-list)
               (not keep-timer))
      (twittering-stop))))

(defun twittering-unregister-killed-buffer ()
  "Unregister buffers which has been killed."
  (mapc (lambda (buffer)
          (unless (buffer-live-p buffer)
            (twittering-unregister-buffer buffer)))
        twittering-buffer-info-list))

(defun twittering-replace-spec-string-for-buffer (buffer spec-string)
  "Replace the timeline spec string for BUFFER with SPEC-STRING when
BUFFER is managed by `twittering-mode' and SPEC-STRING is equivalent
to the current one."
  (when (twittering-buffer-p buffer)
    (let ((current (twittering-get-timeline-spec-string-for-buffer buffer)))
      (when (and (not (string= current spec-string))
                 (twittering-equal-string-as-timeline current spec-string))
        (with-current-buffer buffer
          (rename-buffer spec-string t)
          (setq twittering-timeline-spec-string spec-string))))))

(defun twittering-set-active-flag-for-buffer (buffer active)
  "Set ACTIVE to active-flag for BUFFER."
  (when (twittering-buffer-p buffer)
    (let ((current (twittering-buffer-active-p buffer)))
      (when (or (and active (not current))
                (and (not active) current))
        (twittering-toggle-activate-buffer buffer)))))

(defun twittering-toggle-activate-buffer (&optional buffer)
  "Toggle whether to retrieve timeline for the current buffer periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (twittering-buffer-p buffer)
      (with-current-buffer buffer
        (let* ((new-mode (not twittering-active-mode))
               (active-buffer-list (twittering-get-active-buffer-list))
               (start-timer (and new-mode (null active-buffer-list))))
          (setq twittering-active-mode new-mode)
          (when start-timer
            (twittering-start))
          (twittering-update-mode-line))))))

(defun twittering-activate-buffer (&optional buffer)
  "Activate BUFFER to retrieve timeline for it periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (twittering-set-active-flag-for-buffer buffer t)))

(defun twittering-deactivate-buffer (&optional buffer)
  "Deactivate BUFFER not to retrieve timeline for it periodically."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (twittering-set-active-flag-for-buffer buffer nil)))

(defun twittering-activate ()
  (interactive)
  (mapc (lambda (buffer)
          (twittering-set-active-flag-for-buffer buffer t))
        twittering-buffer-info-list))

(defun twittering-deactivate ()
  (interactive)
  (mapc (lambda (buffer)
          (twittering-set-active-flag-for-buffer buffer nil))
        twittering-buffer-info-list))

(defun twittering-kill-buffer (&optional buffer)
  "Kill BUFFER managed by `twittering-mode'."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (twittering-buffer-p buffer)
      (twittering-deactivate-buffer buffer)
      (kill-buffer buffer)
      (twittering-unregister-killed-buffer))))

(defun twittering-get-managed-buffer (spec)
  "Return the buffer bound to SPEC.
If no buffers are bound to SPEC, return newly generated buffer.
SPEC may be a timeline spec or a timeline spec string."
  (let* ((original-spec spec)
         (spec-string (if (stringp spec)
                          spec
                        (twittering-timeline-spec-to-string spec)))
         ;; `spec-string' without text properties is required because
         ;; Emacs21 displays `spec-string' with its properties on mode-line.
         ;; In addition, copying `spec-string' keeps timeline-data from
         ;; being modified by `minibuf-isearch.el'.
         (spec-string (copy-sequence spec-string))
         (spec (if (stringp spec-string)
                   (twittering-string-to-timeline-spec spec-string)
                 nil)))
    (when (null spec)
      (error "\"%s\" is invalid as a timeline spec"
             (or spec-string original-spec)))
    (let ((twittering-service-method (caar spec)))
      (set-text-properties 0 (length spec-string) nil spec-string)
      (let ((buffer (twittering-get-buffer-from-spec spec)))
        (if buffer
            (progn
              (twittering-replace-spec-string-for-buffer buffer spec-string)
              (twittering-render-timeline buffer t)
              buffer)
          (let ((buffer (generate-new-buffer spec-string))
                (start-timer (null twittering-buffer-info-list)))
            ;; (add-to-list 'twittering-buffer-info-list buffer t)
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (twittering-mode-setup spec-string)
                (twittering-verify-credentials)
                (twittering-render-timeline buffer)
                (when (twittering-account-authorized-p)
                  (when twittering-active-mode
                    (twittering-get-and-render-timeline))
                  (when start-timer
                    ;; If `buffer' is the first managed buffer,
                    ;; call `twittering-start' to start timers.
                    (twittering-start)))))
            buffer))))))

;;;;
;;;; Sign
;;;;

(defvar twittering-sign-simple-string nil)
(defvar twittering-sign-string-function 'twittering-sign-string-default-function)

(defun twittering-sign-string-default-function ()
  "Append sign string to tweet."
  (if twittering-sign-simple-string
      (format " [%s]" twittering-sign-simple-string)
    ""))

(defun twittering-sign-string ()
  "Return Tweet sign string."
  (funcall twittering-sign-string-function))

(defun twittering-get-status-at-pos (&optional pos)
  (twittering-find-status (twittering-get-id-at pos)))

(defun twittering-json-read ()
  (twittering-html-decode-buffer)
  (let ((json-array-type 'list))
    (twittering-wash-json
     (if twittering-debug-mode
         (json-read)
       (ignore-errors (json-read))))))

(defun twittering-status-id< (id1 id2)
  (let ((len1 (length id1))
        (len2 (length id2)))
    (cond
     ((= len1 len2) (string< id1 id2))
     ((< len1 len2) t)
     (t nil))))

(defun twittering-status-id> (id1 id2)
  (not (or (twittering-status-id= id1 id2)
           (twittering-status-id< id1 id2))))

(defun twittering-status-id= (id1 id2)
  (equal id1 id2))

(defun twittering-extract-face-property (properties)
  (when (memq 'face properties)
    (let ((lst properties)
          ret)
      (while lst
        (if (eq (car lst) 'face)
            (setq ret (cadr lst)
                  lst nil)
          (setq lst (cdr lst))))
      ret)))

(defun twittering-remove-property (properties prop)
  "Delete PROP and its value from PROPERTIES. "
  (let ((match (memq prop properties)))
    (append (twittering-take (- (length properties) (length match))
                             properties)
            (cddr match))))

(defun twittering-number-to-string (number)
  "Also remove scientific notation, compared with `number-to-string'.
e.g.,
  (number-to-string 3.35730918340096e+015)
=> \"3.35730918340096e+015\"

  (twittering-number-to-string 3.35730918340096e+015)
=> \"3357309183400960\""
  (let ((str (replace-regexp-in-string "\\.0$" "" (number-to-string number))))
    (when (string-match "\\([0-9]\\)\\.\\([0-9]*\\)e\\([-+]\\)\\([0-9]+\\)" str)
      (let* ((m1 (match-string 1 str))
             (m2 (match-string 2 str))
             (m3 (match-string 3 str))
             (m4 (match-string 4 str))
             (n (string-to-number m4)))
        (if (string= m3 "+")
            (setq str (concat m1 m2 (make-string (- n -1 (length m1) (length m2)) ?0)))
          (setq str (concat "0." (make-string (1- n) ?0) m1 m2)))))
    str))

;;;; User Info

(defvar twittering-user-info-alist '())
;; basic -  basic information
;; followers
;; following

(defun twittering-lookup-user-info-alist (attr)
  (let ((alist (assqref (twittering-extract-service)
                        twittering-user-info-alist)))
    (or (assqref attr alist)
        (let* ((username (twittering-get-accounts 'username))
               (args `((username . ,username)))
               api
               ret)
          (case attr
            ((basic)
             (setq api 'show-user)
             (setq ret (twittering-get-simple-sync api args)))
            ((friends followers)
             (setq api (intern (concat "show-" (symbol-name attr))))
             (let (page
                   (cursor "-1"))
               (while (not (equal cursor "0"))
                 (setq page (twittering-get-simple-sync
                             api `(,@args (cursor . ,cursor)))
                       ret (append ret page)
                       cursor (twittering-number-to-string
                               (string-to-number
                                (assqref 'next-cursor (car page)))))))))

          (twittering-update-user-info-alist `(,@alist (,attr . ,ret)))
          ret))))

(defun twittering-update-user-info-alist (alist)
  (setq twittering-user-info-alist
        `((,(twittering-extract-service) ,@alist)
          ,@(remove-if (lambda (i) (eq (car i) (twittering-extract-service)))
                       twittering-user-info-alist))))

(defun twittering-friends ()
  "Return list of friends' screen-names.  "
  (mapcar (lambda (i) (assqref 'screen-name (assqref 'user i)))
          (twittering-lookup-user-info-alist 'friends)))

;; TODO: Fix twitter.
(defun twittering-followers ()
  (mapcar (lambda (i) (assqref 'screen-name (assqref 'user i)))
          (twittering-lookup-user-info-alist 'followers)))

;;;; Sina Utilities

(defun twittering-int-to-radix62 (n)
  "Radix 62 is defined as: 0..9a..zA..Z"
  (if (< n 62)
      (twittering-int-to-radix62-1 n)
    (concat (twittering-int-to-radix62-1 (% n 62))
            (twittering-int-to-radix62 (/ n 62)))))

(defun twittering-int-to-radix62-1 (n)
  "Radix 62 is defined as: 0..9a..zA..Z
N is less than 62.  "
  (cond ((< n 10)
             (number-to-string n))
            ((and (>= n 10) (<= n 35))
             (char-to-string (+ ?a (- n 10))))
            (t
             (char-to-string (+ ?A (- n 36))))))

(defun twittering-radix62-to-int (s)
  (reduce (lambda (a b) (+ a (* b 62)))
          (reverse
           (mapcar 'twittering-radix62-to-int-1 (string-to-list s)))))

(defun twittering-radix62-to-int-1 (c)
  (cond ((and (>= c ?0) (<= c ?9))
         (- c ?0))
        ((and (>= c ?a) (<= c ?z))
         (+ (- c ?a) 10))
        (t
         (+ (- c ?A) 36))))


(provide 'twittering-mode)

;;; twittering.el ends here
