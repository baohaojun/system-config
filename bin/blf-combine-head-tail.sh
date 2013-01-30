#!/bin/bash

set -e

function die() {
    echo Error: "$@"
    exit -1
}

if test $# != 2; then
    die "Error: Usage $(basename $0) head tail"
fi

headf=$1
tailf=$2
head=$(blf-to-human $headf | perl -ne 'print if 1..m/### Start of human readable image list ###/')
tail=$(blf-to-human $tailf | perl -ne 'print unless 1..m/### Start of human readable image list ###/')
tailn=$(echo -n "$tail"|wc -l) # there is a head line, so use "echo -n" to minus 1

(echo "$head"; echo "$tail") | human-to-blf | perl -npe 's/^Number_of_Images\s*=\s*\d+$/Number_of_Images = '$tailn'/'
