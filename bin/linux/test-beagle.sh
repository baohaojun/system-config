#!/bin/bash

tmpd=/tmp/$$.test-beagle
mkdir $tmpd
for x in "$@"; do echo "$x"; done > $tmpd/1.txt
cd $tmpd
mkdir .beagle; beagle-build-index --recursive --deny-pattern .beagle --enable-deletion --target .beagle/ .
beagle-dump-index --indexdir=`pwd`/.beagle --term-frequencies
#rm $tmpd -rf
my-beagle "$@"
