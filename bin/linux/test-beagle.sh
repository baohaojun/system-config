#!/bin/bash

tmpd=/tmp/$$.test-beagle
mkdir $tmpd
txt_file=$tmpd/1.cs

for x in "$@"; do echo "$x"; done > $txt_file

mkdir $tmpd/.beagle; beagle-build-index --recursive --deny-pattern .beagle --enable-deletion --target $tmpd/.beagle/ $tmpd
beagle-dump-index --indexdir=$tmpd/.beagle --term-frequencies
#beagle-extract-content $txt_file
#rm $tmpd -rf
#my-beagle "$@"
