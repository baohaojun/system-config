#!/bin/sh
HOST=phone
if ! [[ -z $1 ]]; then
HOST="$@"
fi
while true; do ~a22242/bin/.mlj.ep "$HOST"; sleep 1; done
