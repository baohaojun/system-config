#!/bin/bash
function edit()
{
    if ! [[ -r "$1" ]]; then
        echo Error: "$1" is not readable
        return 1
    fi

    if ! [[ -w "$1" ]]; then
        echo Warning: "$1" is not writable
    fi
    
    local IFS=$'\n'

    local dir=$( dirname "$1" )
    local file=$( basename "$1" )
    cd -P "$dir" >/dev/null 2>&1
    file=$( readlink -f "$file")

    emacsedit.bat "`cygpath -alw \"$file\"`"
    cd - >/dev/null 2>&1
}

edit "$1"

