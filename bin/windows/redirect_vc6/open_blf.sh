#!/bin/bash
set -x
cd "$(dirname "$1")"
here=$(pwd)
there=~/Downloads/"$(basename "$here")"
if ! touch temp-test.$$ >/dev/null 2>&1 ; then
    there=~/Downloads/"$(basename "$here")"
    mkdir -p "$there"
    find . -maxdepth 1 -type f|grep -v '\.tar$\|\.zip$\|\.tgz$' | xargs -d \\n bash -c 'rsync -av "$@" '"$there"'' true
    cd "$there"
fi
blf=$(wp $(basename "$1"))
cd /cygdrive/c/Program\ Files/marvell/MarsBurner/
./MarsBurner.exe "$blf"



