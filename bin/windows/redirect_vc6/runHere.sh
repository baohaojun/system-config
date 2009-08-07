#!/bin/bash

echo "$@" >~/runhere.log

CMDNAME=$1
WHERE=$(cygpath -au "$2")

if test -f "$WHERE"
then
    WHERE=$(dirname "$WHERE")
fi

cd "$WHERE"

"$CMDNAME"
