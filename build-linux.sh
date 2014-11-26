#!/bin/bash
set -e
cd $(dirname $(readlink -f $0))
build_dir=~/tmp/build-t1
if test $# = 1 -a "$1" = debug; then
    build_dir=~/tmp/build-t1-debug
fi

mkdir -p $build_dir
rsync * $build_dir -av --exclude=release
rsync release/ $build_dir -av -L --exclude=adb_usb_driver_smartisan

oldpwd=$PWD
cd $build_dir
if test $# = 1 -a "$1" = debug; then
    perl -npe 'print "CONFIG += debug\n" if 1..1' -i *.pro
fi
set -o pipefail
qtchooser -qt=5 -run-tool=qmake && make -j8 | perl -npe "s|$PWD|$oldpwd|g"
relative-link -f $oldpwd/release/* .
relative-link -f $oldpwd/*.lua .
mkdir -p ~/src/github/T1Wrench-linux
command rsync -L T1Wrench $oldpwd/release/ $oldpwd/*.lua ~/src/github/T1Wrench-linux --exclude=adb_usb_driver_smartisan -av
rsync -L $(which the-true-adb) ~/src/github/T1Wrench-linux
(
    if test "$DOING_T1WRENCH_RELEASE"; then
        (
            cd ~/src/github/
            tar czfv ~/tmp/T1Wrench-linux.tgz T1Wrench-linux --exclude-vcs
            cd ~/tmp
            smb-push T1Wrench-linux.tgz ~/smb/share.smartisan.cn/share/baohaojun/T1Wrench
            rsync T1Wrench-linux.tgz rem:/var/www/html/baohaojun/
        )
        exit
    fi

    if test $# = 1 -a "$1" = debug; then
        ps-killall gdb.T1Wrench
        myscr gdb ./T1Wrench
        find-or-exec konsole
    else
        mkfifo /tmp/build-linux.$$
        myscr bash -c "./T1Wrench > /tmp/build-linux.$$ 2>&1"
        cat /tmp/build-linux.$$
        rm /tmp/build-linux.$$
    fi
)
