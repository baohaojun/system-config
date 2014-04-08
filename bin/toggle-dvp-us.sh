#!/bin/bash
set -x
(
    flock 9
    if setxkbmap -query | grep 'variant:\s+dvp' -Pq; then
        setxkbmap -layout us
        xmodmap -e 'pointer = 1 2 3 4 5 6 7 8 9 10'
        sawfish-ask-for-input -p 'Switched to qwerty, press any key to continue: '
    else
        setxkbmap -layout us -variant dvp
        re-xmodmap 2>&1 | cat
        sawfish-ask-for-input -p 'Switched to dvp, press any key to continue: '
    fi
) 9> ~/.logs/$(basename $0).lock
