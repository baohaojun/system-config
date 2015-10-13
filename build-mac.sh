#!/bin/bash
set -e
cd $(dirname $(readlink -f $0))

if test $(uname) = Linux; then
    rsync --exclude=release -av * bhj-mac:$(up $PWD)
    rsync release bhj-mac:$(up .) -av -L --exclude=*/adb_usb_driver_smartisan
    remote-cmd bhj-mac bash -c "
        export DOING_T1WRENCH_RELEASE=$DOING_T1WRENCH_RELEASE;
        export ReleaseVersion=$ReleaseVersion;
        set -x;
        cd $(up .);
        ./build-mac.sh"

    if test "$DOING_T1WRENCH_RELEASE"; then
        rsync bhj-mac:$(up .)/T1Wrench.app ../T1Wrench-macos/ -av --delete
    fi
else
    set -e
    (
        cd lua
        make macosx
    )
    (
        cd luamd5
        make PLATFORM=macx
    )
    rm T1Wrench.app -rf
    if test ! -d ~/Qt5 -a -d ~/Qt5.bak; then
        mv ~/Qt5.bak ~/Qt5
    fi
    for dir in . download; do
        (
            cd $dir
            qmake
            make -j8
        )
    done
    rsync -L t1wrench.lua macx/binaries/* release/* T1Wrench.app/Contents/MacOS/ -r
    rm T1Wrench.dmg -f
    macdeployqt T1Wrench.app -dmg -verbose=1 -executable=T1Wrench.app/Contents/MacOS/download
    mv ~/Qt5 ~/Qt5.bak
    if test "$DOING_T1WRENCH_RELEASE"; then
        exit
    fi

    myscr bash -c 'ps-killall T1Wrench; of T1Wrench.app; oc T1Wrench.app' || true
fi
