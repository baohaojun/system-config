#!/bin/bash
. atexit bash -c 'cd ~/tmp/; nohup setsid xscreensaver -no-splash >/dev/null 2>&1 &'
ps-killall xscreensaver.-no-splash&

command mplayer "$@"
