#!/bin/bash

set -e
tmpd=$(wlp /tmp/$$.test-beagrep)
mkdir $tmpd
txt_file=$tmpd/${TBNAME:-1.cs}

for x in "$@"; do echo "$x"; done > $txt_file

mkdir $tmpd/.beagrep; 
cd $tmpd
beagrep-build-index --recursive --deny-directory-pattern /home/bhj/windows-config/gcode/beagrep/outx --enable-deletion --target $(wlp .beagrep) "$(cygpath -alm .)"

beagrep-dump-index --indexdir=${tmpd:-`pwd`}/.beagrep --term-frequencies
beagrep-static-query --add-static-backend ${tmpd:-`pwd`}/.beagrep --backend none --max-hits 100000 "$@"
beagrep-extract-content $txt_file
cd $tmpd
mkbeagrepidx >/dev/null
beagrep -e "${TBNAME:-1.cs}"
#rm $tmpd -rf
#my-beagrep "$@"
