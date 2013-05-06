#!/bin/bash

set -e

function die() {
    echo Error: "$@"
    exit -1
}

if test $# != 1; then
    die "Error: Usage $(basename $0) pdf"
fi
pdfseparate -f 1 -l 10000 "$1" seq-%d.pdf || true

for x in $(seq 1 2 10000); do
    if test -e seq-$x.pdf; then
        echo seq-$x.pdf;
    else
        break
    fi
done | xargs pdfjoin --outfile odd.pdf

for x in $(seq 2 2 10000); do
    if test -e seq-$x.pdf; then
        echo seq-$x.pdf;
    else
        break
    fi
done|xargs pdfjoin --outfile even.pdf

rm seq-*.pdf
