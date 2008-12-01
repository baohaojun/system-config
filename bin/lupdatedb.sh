#!/bin/bash

mkdir -p "${HOME}"/tmp/locate

if ! [[ -z "$1" ]]; then
    LOCATE_DIRS="$1"
else 
    lupdatedb.sh /c
    lupdatedb.sh /d
    lupdatedb.sh /e
fi
mkdir -p ~/tmp/locate
updatedb --localpaths="$LOCATE_DIRS" --prunepaths='.+/.svn .+/CVS .+/tmp .+/temp' --output="${HOME}"/tmp/locate/"`gcachesum \"$LOCATE_DIRS\"`"
