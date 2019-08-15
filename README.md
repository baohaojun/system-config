ngit is an ncurses based UI to quickly select a git commit from a 'git log
--online' output and press a key on it to perform a single action (Checkout,
reword changelog, edit patch, etc). Opens up vim editor when needed. Inspired
by a grep frontend called ngp. It is similar in spirit to tgit but IMO requires
way fewer key strokes and is not that complex of a code base.

It is written purely in bash and python.

Run ngit in any git repository. Type 'h' for usage instructions.

To install
----------
Simply git clone this repository and run the ngit script from any git checkout.
Requires python3.

Screenshots
-----------
![Screen1](/images/screen1.png)

Help screen:
![Screen2](/images/screen2.png)
