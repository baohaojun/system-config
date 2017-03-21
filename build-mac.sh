#!/bin/bash
set -e
cd $(dirname $(readlink -f $0))

if test $(uname) = Linux; then
    rsync --exclude=release -av * bhj-mac:$(up $PWD)
    rsync release bhj-mac:$(up .) -av -L --exclude=*/adb_usb_driver_smartisan
    remote-cmd bhj-mac bash -c "
        export ReleaseVersion='$ReleaseVersion';
        set -x;
        cd $(up .);
        ./build-mac.sh"
else
    set -e

    rm Wrench.app -rf
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
    rsync -L wrench.lua macx/binaries/* release/* Wrench.app/Contents/MacOS/ -r
    rm Wrench.dmg -f
    macdeployqt Wrench.app -dmg -verbose=1 -executable=Wrench.app/Contents/MacOS/download
    mv ~/Qt5 ~/Qt5.bak
    if test "$DOING_WRENCH_RELEASE"; then
        exit
    fi

    myscr bash -c 'ps-killall Wrench; of Wrench.app; oc Wrench.app' || true
fi
