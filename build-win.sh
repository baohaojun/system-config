#!/bin/bash

if test $# = 0; then
    set -- pub
fi
if test $(uname) = Linux; then
    psync $1 .; ssh $1 "cd $PWD; ./build-win.sh"
else
    perl -npe "$(grep 's/.*ord' ~/bin/vc-utf8-escape)" -i t1wrenchmainwindow.cpp
    /c/Qt/5.3/msvc2013_64/bin/qmake.exe
    set -e
    set -o pipefail
    /cygdrive/c/Python2/python.exe "C:\cygwin64\home\bhj\system-config\bin\windows\terminateModule.py" T1Wrench || true
    vc2013env nmake | perl -npe 's/\\/\//g'
    for x in icudt52.dll icuin52.dll icuuc52.dll libEGL.dll libGLESv2.dll QT5CORE.DLL QT5GUI.DLL QT5WIDGETS.DLL; do
        rsync /c/Qt/5.3/msvc2013_64/bin/$x ./release -av
        chmod 555 ./release/$x
    done
    mkdir -p ./release/platforms
    rsync /c/Qt/5.3/msvc2013_64/plugins/platforms/qwindows.dll ./release/platforms -av
    chmod 555 ./release/platforms/*
    rsync ~/vcredist_x64.exe ./release/vcredist-vs2013-x86_64.exe
    rsync ~/adb_usb_driver_smartisan ./release -av
    rm ./release/*.obj
    rm ./release/*.cpp
    rm t1wrench-release -rf
    mv release t1wrench-release
    tar czfv t1wrench-release.tgz t1wrench-release
fi
