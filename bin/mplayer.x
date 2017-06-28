#!/bin/bash
ps-killall xscreensaver.-no-splash
. atexit bash -c 'nohup setsid xscreensaver -no-splash >/dev/null 2>&1 &'
/usr/bin/mplayer "$@"
