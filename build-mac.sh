#!/bin/bash
if test $(uname) = Linux; then
    psync bhj-mac .
    remote-cmd bhj-mac bash -c "cd $(up .); ./build-mac.sh"
    rsync bhj-mac:$(up .)/T1Wrench.app ../t1wrench-macos/ -av
    rsync bhj-mac:adb ../t1wrench-macos/T1Wrench.app/Contents/MacOS/
else
    rm T1Wrench.app -rf
    if test ! -d ~/Qt5 -a -d ~/Qt5.bak; then
        mv ~/Qt5.bak ~/Qt5
    fi
    qmake
    make -j8
    cp emacs-weixin.sh T1Wrench.app/Contents/MacOS/
    rm T1Wrench.dmg -f
    macdeployqt T1Wrench.app -dmg -verbose=1
    mv ~/Qt5 ~/Qt5.bak
fi
