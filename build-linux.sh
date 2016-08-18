#!/bin/bash
set -e

## start code-generator "^\\s *#\\s *"
# generate-getopts  r:release_dir=Wrench-debian b:build-dir=~/tmp/build-wrench
## end code-generator
## start generated code

release_dir=Wrench-debian
build_dir=~/tmp/build-wrench
OPTIND=1
while getopts "r:b:h" opt; do
    case "$opt" in

        r) release_dir=$OPTARG ;;
        b) build_dir=$OPTARG ;;
        h)
            echo
            echo
            printf %06s '-b '
            printf %-24s 'BUILD_DIR'
            echo ''
            printf %06s '-r '
            printf %-24s 'RELEASE_DIR'
            echo ''
            shift
            exit 0
            ;;
        *)
            echo
            echo
            printf %06s '-b '
            printf %-24s 'BUILD_DIR'
            echo ''
            printf %06s '-r '
            printf %-24s 'RELEASE_DIR'
            echo ''
            exit 2
            ;;
    esac
done


## end generated code

cd $(dirname $(readlink -f $0))

if test $# = 1 && [[ "$1" =~ debug ]]; then
    build_dir=~/tmp/build-t1-debug
fi

if type relative-link >/dev/null 2>&1; then
    system_config=true
else
    system_config=false

    function relative-link() {
        true
    }
fi

relative-link -f $build_dir/Wrench ~/system-config/bin/overide

mkdir -p $build_dir
if test "$system_config" = true; then
    rsync -L * $build_dir -av --exclude=release --exclude=windows --exclude=macx --exclude=emojis
    rsync release/ $build_dir -av -L --exclude=adb_usb_driver_smartisan --exclude=emojis
else
    rsync * $build_dir -a -L --exclude=windows --exclude=macx
    rsync release/ $build_dir -a -L
fi

oldpwd=$PWD
cd $build_dir

set -o pipefail
for x in . download; do
    (
        cd $x
        qtchooser -qt=5 -run-tool=qmake && make -j8 | perl -npe "s|$PWD|$oldpwd|g"
    )
done
relative-link -f $(for x in $oldpwd/*.*; do echo $x; done | grep -v '\.pro$' -P) .

if test $# = 1 -a "$1" = debug; then
    perl -npe 'print "CONFIG += debug\n" if 1..1' -i *.pro
fi

relative-link -f $oldpwd/release/* .
relative-link -f $oldpwd/linux/binaries/* .
ln -s $oldpwd/linux/binaries/the-true-adb . -f
(
    if test "$DOING_WRENCH_RELEASE"; then
        mkdir -p ~/src/github/$release_dir
        command rsync -L $oldpwd/linux/binaries/* Wrench download/download $oldpwd/release/ $oldpwd/*.lua ~/src/github/$release_dir -av --delete --exclude-from=$HOME/src/github/Wrench/release-exclude.txt
        exit
    fi

    if test "$system_config" = false; then
        killall Wrench || true
        ./Wrench&
        exit
    fi

    destroy-windows Wrench || true
    ps-killall Wrench.\!pro || true
    if test $# = 1 && [[ "$1" =~ debug ]]; then
        ps-killall gdb.Wrench
        myscr gdb ./Wrench
        find-or-exec konsole
    else
        rm $build_dir-debug/Wrench -f || true
        mkfifo /tmp/build-linux.$$
        myscr bash -c "echo Wrench; ./Wrench > /tmp/build-linux.$$ 2>&1"
        cat /tmp/build-linux.$$
        rm /tmp/build-linux.$$
    fi
)
