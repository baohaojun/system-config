#!/bin/bash
if test $(uname) = Linux; then
    psync bhj-mac .
    remote-cmd bhj-mac bash -c "cd $(up .); ./build-mac.sh"
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
