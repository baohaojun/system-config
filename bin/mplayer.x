#!/bin/bash
ps-killall xscreensaver.-no-splash
. atexit nohup xscreensaver -no-splash >/dev/null 2>&1
/usr/bin/mplayer "$@"
