#!/bin/bash

if test $(uname) = Linux; then
    psync pub .; ssh pub "cd $PWD; ./build-win.sh"
else
    /c/Qt/5.3/msvc2013_64/bin/qmake.exe
    set -e
    set -o pipefail
    /cygdrive/c/Python2/python.exe "C:\cygwin64\home\bhj\system-config\bin\windows\terminateModule.py" T1Wrench || true
    vc2013env nmake | perl -npe 's/\\/\//g'
    for x in icudt52.dll icuin52.dll icuuc52.dll libEGL.dll libGLESv2.dll QT5CORE.DLL QT5GUI.DLL QT5WIDGETS.DLL; do
        rsync /c/Qt/5.3/msvc2013_64/bin/$x ./release
        chmod 555 ./release/$x
    done
fi
