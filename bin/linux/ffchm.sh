#!/bin/bash
FILE="$1"

if ! test -f "$FILE"; then
    FILE=`which "$FILE"`
fi
if test `uname` = Linux; then
    firefox chm:file://"$(readlink -f "$FILE")"
else
    firefox chm:file://"$(cygpath -alw "$FILE")"
fi
