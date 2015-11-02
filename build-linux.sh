#!/bin/bash
set -e

## start code-generator "^\\s *#\\s *"
# generate-getopts  r:release_dir
## end code-generator
## start generated code
release_dir=
OPTIND=1
while getopts 'r:' opt; do
    case "$opt" in
        r)    release_dir=$OPTARG;;
        *)    echo Usage Error; exit 2;;
    esac
done
shift $(($OPTIND - 1))

## end generated code

if test -z "$release_dir"; then
    release_dir=T1Wrench-debian
fi

cd $(dirname $(readlink -f $0))
build_dir=~/tmp/build-t1
if test $# = 1 && [[ "$1" =~ debug ]]; then
    build_dir=~/tmp/build-t1-debug
fi

relative-link -f $build_dir/T1Wrench ~/system-config/bin/overide

mkdir -p $build_dir
rsync -L * $build_dir -av --exclude=release --exclude=windows --exclude=macx --exclude=emojis
rsync release/ $build_dir -av -L --exclude=adb_usb_driver_smartisan --exclude=emojis

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
relative-link -f $oldpwd/*.* .
relative-link -f $oldpwd/release/* .
relative-link -f $oldpwd/linux/binaries/* .
ln -s $oldpwd/linux/binaries/the-true-adb . -f
(
    if test "$DOING_T1WRENCH_RELEASE"; then
        mkdir -p ~/src/github/$release_dir
        command rsync -L $oldpwd/linux/binaries/* T1Wrench download/download $oldpwd/release/ $oldpwd/*.lua ~/src/github/$release_dir -av --delete --exclude-from=$HOME/src/github/T1Wrench/release-exclude.txt
        exit
    fi

    destroy-windows T1Wrench || true
    ps-killall T1Wrench.\!pro || true
    if test $# = 1 && [[ "$1" =~ debug ]]; then
        ps-killall gdb.T1Wrench
        myscr gdb ./T1Wrench
        find-or-exec konsole
    else
        rm $build_dir-debug/T1Wrench -f || true
        mkfifo /tmp/build-linux.$$
        myscr bash -c "echo T1Wrench; ./T1Wrench > /tmp/build-linux.$$ 2>&1"
        cat /tmp/build-linux.$$
        rm /tmp/build-linux.$$
    fi
)
