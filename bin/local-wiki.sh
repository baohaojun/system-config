#!/bin/bash
test -L ~/system-config/gcode/offline.wikipedia/$1.py || set -- en "$@"
lang=$1
shift
host=localhost:34567
if test -e ~/.edictrc; then
    . ~/.edictrc
fi
of 'http://'$host'/'$lang'/keyword/'"`echo \"$@\"|perl -npe 'chomp; s#([^_0-9a-zA-Z ])#sprintf(\"%%%02x\", ord($1))#seg; '`" &
