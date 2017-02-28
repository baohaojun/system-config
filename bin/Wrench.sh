#!/bin/bash

export EMACS=t
export ANDROID_SERIAL=$(select-output-line -p "Select the adb device" my-adb devices?|pn 1)
export LD_LIBRARY_PATH=/usr/local/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH
# adb forward --remove tcp:28888

find-or-exec 'Wrench V%Wrench'

what_to_do=$(
    ask-for-input --history -a "Wrench" -p "你要小扳手的什么功能？（可以自己输入比如 baohaojun@@wx）"
          )

if test "$what_to_do" -a "$what_to_do" != Wrench; then
    cat <<EOF > ~/.cache/system-config/wrench-$$.twa
-- -*- mode: lua -*-
-- {%lua%}
t1_call("$what_to_do")
-- {%/lua%}
EOF
    Wrench ~/.cache/system-config/wrench-$$.twa
    rm ~/.cache/system-config/wrench-$$.twa
else
    exec Wrench
fi
