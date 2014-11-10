#!/bin/bash
set -e
cd $(dirname $(readlink -f $0))

if test $(uname) = Linux; then
    psync bhj-mac .
    remote-cmd bhj-mac bash -c "set -x; cd $(up .); ./build-mac.sh"
    rsync bhj-mac:$(up .)/T1Wrench.app ../T1Wrench-macos/ -av
    rsync bhj-mac:$(up .)/T1Wrench.dmg ~/today/T1Wrench.dmg
    smb-push ~/today/T1Wrench.dmg
else
    set -e
    (
        cd lua
        make macosx
    )
    rm T1Wrench.app -rf
    if test ! -d ~/Qt5 -a -d ~/Qt5.bak; then
        mv ~/Qt5.bak ~/Qt5
    fi
    qmake
    make -j8
    rsync -L t1wrench.lua macx/binaries/* T1Wrench.app/Contents/MacOS/
    rm T1Wrench.dmg -f
    macdeployqt T1Wrench.app -dmg -verbose=1
    mv ~/Qt5 ~/Qt5.bak
    myscr bash -c 'ps-killall T1Wrench; of T1Wrench.app' || true
fi
