#!/bin/bash
src="`/bin/readlink -f \"$1\"`"

if ! [[ -z "$src" ]]; then
    rm "$1"
    ln -sf "$src" "$1"
else
    echo "$1"' has no valid target!'
fi

