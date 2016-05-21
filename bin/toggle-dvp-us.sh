#!/bin/bash
set -x

if [[ "$(tty)" =~ /dev/tty ]]; then
    if test "$(cat ~/.config/system-config/am-keyboard)" = en; then
        sudo setupcon dvp
        swap-control-alt
        echo dvp > ~/.config/system-config/am-keyboard
    else
        sudo setupcon en
        echo en > ~/.config/system-config/am-keyboard
    fi
    exit
fi

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
