#!/bin/bash

function copy-dlls()
{
    for x in icudt52.dll icuin52.dll icuuc52.dll QT5CORE.DLL QT5GUI.DLL QT5WIDGETS.DLL; do
        rsync $1/bin/$x ./release -av
        chmod 555 ./release/$x
    done

    for x in libEGL.dll libGLESv2.dll libstdc++-6.dll libwinpthread-1.dll libgcc_s_dw2-1.dll; do
        rsync $1/bin/$x ./release -av || continue
        chmod 555 ./release/$x
    done

    mkdir -p ./release/platforms
    rsync $1/plugins/platforms/qwindows.dll ./release/platforms -av
    chmod 555 ./release/platforms/*
    rsync ~/adb_usb_driver_smartisan ./release -av
}

function make-release-tgz()
{
    rm -rf ./release/*.obj
    rm -rf ./release/*.o
    rm -rf ./release/*.cpp
    rm -rf t1wrench-release
    cp readme.* ./release/
    cp *.png ./release/
    cp ~/adb/* ./release/
    cp -av release t1wrench-release
    set -x
    tar czfv ${1:-t1wrench-release.tgz} t1wrench-release
}

if test $# = 0; then
    set -- pub
fi
if test $(uname) = Linux; then
    psync -d $1 .; ssh $1 "cd $PWD; ./$(basename $0)"
    rsync $1:$(up .)/t1wrench-release ../ -av
    rsync $1:$(up .)/t1wrench-windows.tgz ~/today/
    smb_push ~/today/t1wrench-windows.tgz
else
    set -e
    set -o pipefail
    /cygdrive/c/Python2/python.exe "C:\cygwin64\home\bhj\system-config\bin\windows\terminateModule.py" T1Wrench || true

    if test $(basename $0) = build-win-vc.sh; then
        perl -npe "$(grep 's/.*ord' ~/bin/vc-utf8-escape)" -i t1wrenchmainwindow.cpp
        /c/Qt/5.3/msvc2013_64/bin/qmake.exe

        vc2013env nmake | perl -npe 's/\\/\//g'
        copy-dlls /c/Qt/5.3/msvc2013_64/
        rsync ~/vcredist_x64.exe ./release/vcredist-vs2013-x86_64.exe
        make-release-tgz
    else
        /c/Qt-mingw/./Qt5.3.1/5.3/mingw482_32/bin/qmake.exe
        export PATH=/cygdrive/c/Qt-mingw/Qt5.3.1/5.3/mingw482_32/bin:/c/Qt-mingw/Qt5.3.1/Tools/mingw482_32/bin:$PATH
        mingw32-make.exe | perl -npe 's/\\/\//g'
        copy-dlls /c/Qt-mingw/./Qt5.3.1/5.3/mingw482_32
        make-release-tgz t1wrench-windows.tgz
    fi
fi
