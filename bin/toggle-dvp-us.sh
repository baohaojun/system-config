#!/bin/bash

set -x
exec >> ~/tmp/am.log 2>&1
(
    date; echo $0
) >> ~/tmp/am.history
set -e

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
    (
        if test -e ~/.config/system-config/using-fcitx || ps.pl fcitx; then
            ps-killall fcitx
            touch ~/.config/system-config/using-fcitx
        fi
    ) >/dev/null 2>&1 || true
    echo am: dvp layout
    setxkbmap -layout us -variant dvp
    re-xmodmap 2>&1|tee
    do-natural-scrolling 9>/dev/null || true
}

(
    if ! flock -n 9; then
        (
            bhj-notify dvp "$(fuser ~/.cache/system-config/logs/$(basename $0).lock | xargs ps)"
        ) 9>/dev/null
        exit 0
    fi
    if test "$1" = am -o "$1" = dvp; then
        do-dvp
    elif test "$1" = ma || setxkbmap -query | grep 'variant:\s+dvp' -Pq; then
        echo am: normal layout
        setxkbmap -layout us
        xmodmap ~/system-config/etc/hardware-mach/.Xmodmap-undo
        xmodmap ~/system-config/etc/hardware-mach/always/.Xmodmap
        do-natural-scrolling
        if test -e ~/.config/system-config/using-fcitx; then
            (
                exec 9>/dev/null
                cd ~/.cache/system-config/logs/
                setsid nohup fcitx
            )&
        fi >/dev/null 2>&1
    else
        do-dvp
    fi
) 9> ~/.cache/system-config/logs/$(basename $0).lock
