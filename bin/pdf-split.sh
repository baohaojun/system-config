#!/bin/bash

set -e

function die() {
    echo Error: "$@"
    exit -1
}

if test $# != 1; then
    die "Error: Usage $(basename $0) pdf"
fi
pages=$(pdf-get-pages "$1")
step=1

seq_done=true
for x in $(seq 1 1 $pages); do
    if test ! -e seq-$x.pdf; then
        seq_done=false
        break
    fi
done
if test $seq_done = false || (test $seq_done = true && yes-or-no-p "Redo split?"); then
    run-in-parallel -j 10 -I %N 'pdfjoin --no-landscape --rotateoversize false  '"$(printf %q "$1")"' %N --outfile seq-%N.pdf' $(seq 1 $step $pages)
fi

page_sizes=$(for x in $(seq 1 $step $pages); do
    pdfinfo seq-$x.pdf 2>/dev/null
done | grep 'page size' -i | perl -ne '
    chomp;
    $page_sizes{$_}++;
    END{
        for (keys %page_sizes) {
            print "$_ : $page_sizes{$_}\n"
        }
    }' | sort -n)

size=$(select-output-line -- echo "$page_sizes")
if test "$size" != "$page_sizes"; then
    export selected_width=$(echo $size | pn 3)
    export selected_height=$(echo $size | pn 5)
    export nup_size=$(echo $size|perl -npe 's!.*?(\d+\.\d+) x (\d+\.\d+).*!sprintf "%fin,%fin", $1/72,$2/72!e')
    run-in-parallel -j 10 -I %N '
        if test $(pdf-get-width seq-%N.pdf) != $selected_width -o $(pdf-get-height seq-%N.pdf) != $selected_height; then
            pdfnup --no-landscape --rotateoversize false  --nup 1x1 --papersize "{$nup_size}" seq-%N.pdf --outfile seq-ss-%N.pdf
        else
            cp seq-%N.pdf seq-ss-%N.pdf
        fi
    ' $(seq 1 $pages)
else
    for x in $(seq 1 $pages); do
        cp seq-$x.pdf seq-ss-$x.pdf
    done
fi

pdfjoin --no-landscape --rotateoversize false  --papersize "{${selected_width}pt,${selected_height}pt}" --outfile odd.pdf $(for x in $(seq 1 2 $pages); do echo seq-ss-$x.pdf; done)
pdfjoin --no-landscape --rotateoversize false  --papersize "{${selected_width}pt,${selected_height}pt}" --outfile even.pdf $(for x in $(seq 2 2 $pages); do echo seq-ss-$x.pdf; done)

