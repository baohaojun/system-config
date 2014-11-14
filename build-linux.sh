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
relative-link -f $oldpwd/*.lua .
mkdir -p ~/src/github/T1Wrench-linux
rsync -L T1Wrench $oldpwd/release/* $oldpwd/*.lua ~/src/github/T1Wrench-linux
rsync -L $(which the-true-adb) ~/src/github/T1Wrench-linux
(
    cd ~/src/github/
    tar czfv ~/tmp/T1Wrench-linux.tgz T1Wrench-linux --exclude-vcs
    smb-push T1Wrench-linux.tgz
    mv T1Wrench-linux.tgz T1Wrench-linux-$(today).tgz
    smb-push T1Wrench-linux-$(today).tgz
)&
./T1Wrench
