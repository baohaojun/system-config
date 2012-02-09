#!/bin/bash
test -e ~/external/bin/linux/ext/wiki-"$1".txt || set -- en "$@"
lang=$1
shift
firefox 'http://localhost:8000/'$lang'/keyword/'"`echo \"$@\"|perl -npe 'chomp; s#([^_0-9a-zA-Z ])#sprintf(\"%%%02x\", ord($1))#seg; '`" &
