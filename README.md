beagrep
==========

Grep 2G source code in 2 seconds. A speed up of 1000 fold.

It works by using the beagle search engine together with grep, thus
the name.

For e.g., when you work with android source code, and grep readlink,
it will take you more than half an hour. But if you ask beagle first,
"which files contain the word readlink", and then you run grep on
those files only, you are done in less than 2 seconds.

For more details, visit [my github page](http://baohaojun.github.com/beagrep.html) (man page included).

