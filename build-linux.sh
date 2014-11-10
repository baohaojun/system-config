#!/bin/bash
set -e
cd $(dirname $(readlink -f $0))
mkdir -p ~/tmp/build-t1
rsync * ~/tmp/build-t1 -av

oldpwd=$PWD
cd ~/tmp/build-t1
set -o pipefail
qtchooser -qt=5 -run-tool=qmake && make -j8 | perl -npe "s|$PWD|$oldpwd|g"
relative-link -f $oldpwd/release/* .
./T1Wrench
