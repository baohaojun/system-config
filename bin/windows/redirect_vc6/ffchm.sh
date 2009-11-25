#!/bin/bash
FILE="$1"

if ! test -f "$FILE"; then
    FILE=`which "$FILE"`
fi
firefox chm:file://"$(cygpath -alw "$FILE")"
