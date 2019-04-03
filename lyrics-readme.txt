`lyrics.el' interface to download and display songs lyrics.

Usage:

Add `lyrics.el' somewhere in your `load-path'

    M-x lyrics

Troubleshooting:

+ `musixmatch' backend shows a incomplete lyrics

  MusixMatch filters requests with an unknown User-Agent header.  Be sure to
  check that `url-privacy-level' is not set to "'paranoid".
