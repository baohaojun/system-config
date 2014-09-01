#!/bin/bash

host=pub
TEMP=$(getopt -o h: --long host: -n $(basename $0) -- "$@")
eval set -- "$TEMP"
while true; do
    case "$1" in
        -h|--host)
            host=$2
            shift 2
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error"
            ;;
    esac
done

if test $(uname) = Linux; then
    psync -d $host .; ssh $host "cd $PWD; $@"
else
    "$@"
fi
