#!/bin/bash
test -e ~/external/bin/linux/ext/wiki-"$1".txt || set -- en "$@"
lang=$1
shift
host=localhost:8000
if test -e ~/.edictrc; then
    . ~/.edictrc
fi
firefox 'http://'$host'/'$lang'/keyword/'"`echo \"$@\"|perl -npe 'chomp; s#([^_0-9a-zA-Z ])#sprintf(\"%%%02x\", ord($1))#seg; '`" &
