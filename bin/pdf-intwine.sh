#!/bin/bash

set -e
function die() {
    echo Error: "$@"
    exit -1
}

outfile=out.pdf
TEMP=$(getopt -o o: --long out: -n $(basename $0) -- "$@")
eval set -- "$TEMP"
while true; do
    case "$1" in
        -o|--out)
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

odd_pages=$(pdf-get-pages "$1")
even_pages=$(pdf-get-pages "$2")

(
    for n in $(seq 1 $even_pages); do
        echo "$1" $n "$2" $n
    done
    if test $odd_pages -gt $even_pages; then
        echo "$1" $odd_pages
    fi
) | xargs pdfjoin --no-landscape --rotateoversize false  --outfile $outfile
