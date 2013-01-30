#!/bin/bash

set -e

function die() {
    echo Error: "$@"
    exit -1
}

in_place=false
TEMP=$(getopt -o i --long in-place -n $(basename $0) -- "$@")
eval set -- "$TEMP"
while true; do
    case "$1" in
        -i|--in-place)
        in_place=true
        shift
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

if test $# != 2; then
    die "Error: Usage $(basename $0) head tail"
fi

headf=$1
tailf=$2
head=$(blf-to-human $headf | perl -ne 'print if 1..m/### Start of human readable image list ###/')
tail=$(blf-to-human $tailf | perl -ne 'print unless 1..m/### Start of human readable image list ###/')
tailn=$(echo -n "$tail"|wc -l) # there is a head line, so use "echo -n" to minus 1

tf=/tmp/$(basename $0).$$
(set +x; echo "$head"; echo "$tail") | human-to-blf | perl -npe 's/^Number_of_Images\s*=\s*\d+$/Number_of_Images = '$tailn'/' 2>/dev/null| tee $tf 

if test $in_place = true; then
    mv $tf "$tailf"
else 
    rm $tf
fi
