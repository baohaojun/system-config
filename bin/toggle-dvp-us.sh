#!/bin/bash
set -x
if test $# != 0; then
    exec my-adb am "$@"
fi
(
    flock 9
    if setxkbmap -query | grep 'variant:\s+dvp' -Pq; then
        setxkbmap -layout us
        xmodmap ~/system-config/etc/hardware-mach/.Xmodmap-undo
        do-unnatural-scrolling
    else
        setxkbmap -layout us -variant dvp
        re-xmodmap 2>&1|tee
    fi
) 9> ~/.cache/system-config/logs/$(basename $0).lock
