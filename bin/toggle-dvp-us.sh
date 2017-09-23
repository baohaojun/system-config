#!/bin/bash

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

do-dvp() {
    echo am: dvp layout
    setxkbmap -layout us -variant dvp
    re-xmodmap 2>&1|tee
    do-natural-scrolling
}

(
    flock 9
    if test "$1" = am -o "$1" = dvp; then
        do-dvp
    elif test "$1" = ma || setxkbmap -query | grep 'variant:\s+dvp' -Pq; then
        echo am: normal layout
        setxkbmap -layout us
        xmodmap ~/system-config/etc/hardware-mach/.Xmodmap-undo
        do-unnatural-scrolling
    else
        do-dvp
    fi
) 9> ~/.cache/system-config/logs/$(basename $0).lock
