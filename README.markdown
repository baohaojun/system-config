xwl's fork
----------

* Fetch replied statues on request.

* Format tweets sent by myself specially: Inspired by "Twitter for iPhone",
  currently an ad-hoc implementation, screenshot:
  http://xwl.appspot.com/images/twitter_my_tweet.png

* Render user profile: When visiting user timeline, we will place user profile
  info near its most recent tweet.  Currently, including big image, basic, bio,
  location, web, following status, lists, etc.  Screenshot:
  http://xwl.appspot.com/images/twittering_user_profile.png

* Decorate tweets with zebra like background.
    Screenshot: http://xwl.appspot.com/images/twitter.png

* new specs/API
  * User Methods: followers, following
  * Friendship Methods: show-friendships
  * List methods: get-list-subscriptions, get-list-memberships
  * List Subscribers Methods: subscribe-list, unsubscribe-list
  * List Members Methods: add-list-members, delete-list-members
  
* unread tweets
  * Show unread tweets for different buffers in a way similar to
    erc-track-mode.
  * Do not display unread notifier when updating tweets from a twittering
    buffer.
  * Show twitter logo on mode line.
      Screenshot: http://xwl.appspot.com/images/twitter_unread.png
  * Make unread notifier on mode line mouse clickable.
  * Cache latest status for interested specs, so that next time when we restart
    twittering-mode, unread count could be up-to-date.

* misc
  * (twittering-erase-all): New func, binded to "C-c C-w".
  * (twittering-mode-setup): Short mode-name to just "twittering"
  * Use defcustom to separate user customizable variables and internal
    variables.
  * %g: New format specifier, to format time string using `gnus-user-date'.  

 Twittering-mode: a Twitter client for Emacs
=============================================

Twittering-mode enables you to twit on Emacsen.

- web: http://twmode.sf.net
- github: http://github.com/hayamiz/twittering-mode

 Features
----------

* Activities on Twitter
  * Viewing various timelines
    * Friends' timeline
    * Replies
    * User's timeline
    * Public timeline
  * Posting tweets
    * Direct message
    * ReTweet
    * Hash tag
    * Signiture
  * Following and removing users
  * Marking tweets as favorites
* HTTP Proxy support
* Secure connection via HTTPS (cURL is required)

 Supported Emacsen
-------------------

- GNU Emacs 21 (some restrictions)
- GNU Emacs 22, 23

 Authors & Contributors
------------------------

- Y. Hayamizu
- naoya_t
- Tsuyoshi CHO
- Alberto Garcia
- Satoshi Yatagawa
- 高山智也
- 松尾(cvmat)
- 青田(naota)
- Jaemok Jeong(jmjeong)
- Thomas Danckaert
- IMAI Toshiyuki

 See also
----------

- http://www.emacswiki.org/emacs-en/TwitteringMode
