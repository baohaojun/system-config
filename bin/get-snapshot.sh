#!/bin/bash
set -e
if test -e ~/user/Documents/My\ Pictures; then
    dir=~/user/Documents/My\ Pictures
else
    dir=~/shots
fi
cd ${1:-$dir}
find . -type f -mtime -.1 -print0|xargs -0 bash -c '
last=$1;
shift;
for x in "$@"; do
    if test "$last" -ot "$x"; then
        last=$x;
    fi;
done;

readlink -m "$last"
mp >/dev/null 2>&1 "$last"' xx
