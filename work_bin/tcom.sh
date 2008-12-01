#!/bin/sh
HOST=phone
if ! [[ -z $1 ]]; then
HOST="$@"
fi
while true; do ~a22242/bin/.tcom.ep "$HOST"; exit;done
