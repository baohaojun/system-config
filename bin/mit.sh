#!/bin/sh

if [[ ! -z "$1" ]]; then host="$1"; else host=w201; fi
while true; do date; ~a22242/bin/.mit.ep $host; sleep 1;done
