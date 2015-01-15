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
for x in . download; do
    (
        cd $x
        qtchooser -qt=5 -run-tool=qmake && make -j8 | perl -npe "s|$PWD|$oldpwd|g"
    )
done
relative-link -f $oldpwd/release/* .
relative-link -f $oldpwd/*.lua .
relative-link -f $oldpwd/release/* $oldpwd/*.lua $build_dir
relative-link -f $oldpwd/linux/binaries/* $build_dir
(
    if test "$DOING_T1WRENCH_RELEASE"; then
        mkdir -p ~/src/github/T1Wrench-linux
        command rsync -L T1Wrench download/download $oldpwd/release/ $oldpwd/*.lua ~/src/github/T1Wrench-linux -av
        command rsync -L $oldpwd/linux/binaries/* ~/src/github/T1Wrench-linux -av
        exit
    fi

    destroy-windows T1Wrench
    ps-killall bash.T1Wrench.tmp.build
    if test $# = 1 -a "$1" = debug; then
        ps-killall gdb.T1Wrench
        myscr gdb ./T1Wrench
        find-or-exec konsole
    else
        mkfifo /tmp/build-linux.$$
        myscr bash -c "echo T1Wrench; ./T1Wrench > /tmp/build-linux.$$ 2>&1"
        cat /tmp/build-linux.$$
        rm /tmp/build-linux.$$
    fi
)
