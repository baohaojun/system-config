#!/bin/bash
set -x
(
    flock 9
    if setxkbmap -query | grep 'variant:\s+dvp' -Pq; then
        setxkbmap -layout us
        xmodmap ~/system-config/etc/hardware-mach/.Xmodmap-undo
    else
        setxkbmap -layout us -variant dvp
        xmodmap ~/system-config/etc/hardware-mach/.Xmodmap
    fi
) 9> ~/.logs/$(basename $0).lock
