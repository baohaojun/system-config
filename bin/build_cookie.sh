#!/bin/bash

function Usage() 
{
    echo Usage: $0 [URL]
    exit
}

if [[ -z "$1" ]]; 
    then 
    Usage;
fi

COOKIEFILE=~/.mozilla/firefox/zo923kyp.default/cookies.txt
URL="$1"

export HOST=`echo "$URL"|perl -npe 's"(http://)*(.+?)(/.*|$)"\2"'`

echo -n \"; cat "$COOKIEFILE"|awk \
    '{ if ($1 == ENVIRON["HOST"]) {printf "%s", $6 "=" $7 ";"}}'; 
echo -n \"



