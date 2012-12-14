beagrep
==========

Grep 2G source tree in 2 seconds. A speed up of more than 100 fold.

It works by using a search engine before using grep. The search
engine's name is beagle, thus I named this little beast beagrep.

For e.g., when I work with Android source code, and grep readlink in
the whole source tree, it takes 5 minutes (on the second run when the
cache is still hot; I used to say 30 minutes here, but thanks to [Camarade_Tux](http://www.reddit.com/r/linux/comments/14tybj/beagrep_grep_2g_source_code_in_2_seconds/c7geb8f)
for pointing it out to me). 

But if you ask the search engine first, "which files contain the word
readlink", and then you run grep on those files only, you are done in
less than 2 seconds.

For more details, visit [my github page](http://baohaojun.github.com/beagrep.html) (man page included).

