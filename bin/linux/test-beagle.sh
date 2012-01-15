#!/bin/bash

set -e
tmpd=$(wlp /tmp/$$.test-beagle)
mkdir $tmpd
txt_file=$tmpd/${TBNAME:-1.cs}

for x in "$@"; do echo "$x"; done > $txt_file

mkdir $tmpd/.beagle; 
cd $tmpd
beagle-build-index --recursive --deny-directory-pattern /home/bhj/windows-config/gcode/beagrep/outx --enable-deletion --target $(wlp .beagle) "$(cygpath -alm .)"

beagle-dump-index --indexdir=${tmpd:-`pwd`}/.beagle --term-frequencies
beagle-static-query --add-static-backend ${tmpd:-`pwd`}/.beagle --backend none --max-hits 100000 "$@"
beagle-extract-content $txt_file
cd $tmpd
mkbeagleidx >/dev/null
beagrep -e "${TBNAME:-1.cs}"
#rm $tmpd -rf
#my-beagle "$@"
