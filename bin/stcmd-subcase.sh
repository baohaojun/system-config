#!/bin/bash
if test "$1" = srem-helper; then
    shift
fi

if my-adb shell stcmd-subcase.sh srem-helper "$@" | tee /dev/stderr | grep -q "nc: can't connect to remote host (127.0.0.1): Connection refused"; then
    my-adb forward tcp:54321 localreserved:scitd
    echo -e srem-helper "$@\r" | nc localhost 54321
fi
