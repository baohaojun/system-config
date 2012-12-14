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

# How does it compare to [ag (AKA the silver searcher)](https://github.com/ggreer/the_silver_searcher)

My laptop: Thinkpad T420; CPU: Intel(R) Core(TM) i5-2520M CPU @ 2.50GHz; Memory: 8G.

    cd ~/src/android   
    time ag readlink .

The first time it took 3 minutes, the second time it took 8 seconds, very impressive!

    time beagrep -e readlink

The first time it took 4 seconds, the second time it took 0.8 seconds.

# Pros and cons of beagrep

## Pros

* Very fast

## Cons

* You need build the search engine database beforehand (the first time
  you do this will take a long while, but subsequent updating is
  reasonably fast)

* Works on whole words only: can not use `beagrep -e readli` to find
  `readlink`, but hey, do you really need that? What about `beagrep -e r.*e.*a.*d.*l.*i.*n.*k.*`, which can also find `readlink` you know?


