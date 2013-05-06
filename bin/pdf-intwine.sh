#!/bin/bash

set -e
function die() {
    echo Error: "$@"
    exit -1
}

outfile=out.pdf
TEMP=$(getopt -o o: --long outfile: -n $(basename $0) -- "$@")
eval set -- "$TEMP"
while true; do
    case "$1" in
        -o|--outfile)
            outfile=$2
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

if test $# != 2; then
    die "Error: Usage $(basename $0) pdf1 pdf2"
fi

pdfseparate -f 1 -l 10000 "$1" odd-%d.pdf || true
pdfseparate -f 1 -l 10000 "$2" even-%d.pdf || true

for x in $(seq 1 10000); do
    for y in odd even; do
        if test -e $y-$x.pdf; then
            echo $y-$x.pdf;
        fi
    done
done | xargs pdfjoin --outfile $outfile
