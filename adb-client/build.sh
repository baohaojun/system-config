#!/bin/bash
set -ex
tmpdir=~/tmp/build-adb-client
if test "$1" = debug; then
    tmpdir=$tmpdir.debug
fi

mkdir -p $tmpdir
rsync -av $PWD/* $tmpdir --exclude=.git --exclude=adb-client --exclude=adb-client.d

(
    cd $tmpdir
    if test "$1" = debug; then
        perl -npe 'print "CONFIG += debug\n" if 1..1' -i *.pro
    fi
    qmake
    make
    if test "$1" = debug; then
        myscr "gdb ./adb-client"
    else
        ./adb-client
    fi
)
