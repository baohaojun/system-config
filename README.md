beagrep
==========

(I submitted this readme on [reddit](http://redd.it/14tybj), and it
has been updated several times according to comments there. Thanks to
the commenters! I have learned a lot!)

Grep 2G source tree in 2 seconds. A speed up of more than 100 fold.

It works by using a search engine before using grep. The search
engine's name is beagle, thus I named this little beast beagrep.

For e.g., when I work with Android source code, and grep readlink in
the whole source tree, it takes 5 minutes (on the second run when the
cache is still hot; I used to say 30 minutes here, but I fixed it
thanks to [Camarade_Tux](http://redd.it/14tybj) for pointing it out to
me).

But if you ask the search engine first, "which files contain the word
readlink", and then you run grep on those files only, you are done in
less than 2 seconds.

For more details, visit [my github page](http://baohaojun.github.com/beagrep.html) (man page included).

# How does it compare to [ag (AKA the silver searcher)](https://github.com/ggreer/the_silver_searcher)

My laptop: Thinkpad T420; CPU: Intel(R) Core(TM) i5-2520M CPU @ 2.50GHz; Memory: 8G.

    cd ~/src/android   
    time ag readlink .

The first time it took 3 minutes, the second time it took 8 seconds,
very impressive! I think if I had learned about it earlier I probably
wouldn't have started working on beagrep in the first place, because I
would have thought it is quick enough already.

    time beagrep -e readlink

The first time it took 10 seconds, the second time it took 0.8
seconds. (So the 2G in 2 second slogan is a bit underestimated, and
given that `ag` is so fast when cache is hot, I actually think I need
change the slogan to be more precise:-)

Also, an idea has occured to me natually, what if I replace the grep
in beagrep with ag, will it squeeze another tenth of second out? The
name will be beag, of course:-)

Another test is done on Linux kernel source code, where ag takes
1m35s/1.8s for cache hot/cold, while beagrep takes 3s/0.5s
respectively.

# Pros and cons of beagrep

## Pros

* Very fast

* Output format compatibility with grep, so it can be used by, for
  e.g., Emacs grep-mode directly.

## Cons

* You need build the search engine database beforehand (the first time
  you do this will take a long while, but subsequent updating is
  reasonably fast)

* Works on whole words only: can not use `beagrep -e readli` to find
  `readlink`, but hey, do you really need that? What about `beagrep -e r.*e.*a.*d.*l.*i.*n.*k.*`, which can also find `readlink` you know?

# Other grep like tools

ACK's author Andy Lester has written [a nice intro](http://betterthangrep.com/more-tools/) about all kinds of tools for searching source code.

Of particular interest to beagrep is Google's Code Search tool.

# TODO

* Push beagrep into Debian distribution.
