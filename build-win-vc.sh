#!/bin/bash

function copy-dlls()
{
    rsync windows/binaries/* ./release -v -L
    rsync wrench.lua ./release -v

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
}

function make-release-tgz()
{
    rsync readme.* ./release/
    rsync -av *.png ./release/
    command rsync -av -d release/ wrench-release --exclude="*.obj" \
            --exclude="*.o" \
            --exclude="*.cpp"


    set -x
    tar czfv ${1:-Wrench-windows.tgz} Wrench-windows -h
}

if test $# = 0; then
    set -- pub
fi
if test $(uname) = Linux; then
    psync $1 .; ssh $1 "cd $PWD; ./$(basename $0)"
    echo doing windows to linux rsync
    command rsync $1:$(up .)/Wrench-windows/ ../Wrench-windows/ -av
    rm -fv ../Wrench-windows/screen-shot.png
    rsync $1:$(up .)/Wrench-windows.tgz ~/today/
    smb-push ~/today/Wrench-windows.tgz
    (
        cd ~/today
        mv Wrench-windows.tgz Wrench-windows-$(today).tgz
        smb-push Wrench-windows-$(today).tgz
    )
else
    set -e
    set -o pipefail
    /cygdrive/c/Python2/python.exe "C:\cygwin64\home\bhj\system-config\bin\windows\terminateModule.py" Wrench.exe || true
    /cygdrive/c/Python2/python.exe "C:\cygwin64\home\bhj\system-config\bin\windows\terminateModule.py" adb.exe || true

    if test $(basename $0) = build-win-vc.sh; then
        perl -npe "$(grep 's/.*ord' ~/bin/vc-utf8-escape)" -i wrenchmainwindow.cpp
        /c/Qt/5.3/msvc2013_64/bin/qmake.exe

        vc2013env nmake | perl -npe 's/\\/\//g'
        copy-dlls /c/Qt/5.3/msvc2013_64/
        rsync ~/vcredist_x64.exe ./release/vcredist-vs2013-x86_64.exe
        make-release-tgz
    else # mingw
        export PATH=/cygdrive/c/Qt-mingw/Qt5.3.1/5.3/mingw482_32/bin:/cygdrive/c/Qt-mingw/Qt5.3.1/Tools/mingw482_32/bin:$PATH
        if test ! -e Makefile; then
            qmake.exe
        fi
        mingw32-make.exe | perl -npe 's/\\/\//g'
        copy-dlls /c/Qt-mingw/./Qt5.3.1/5.3/mingw482_32
        set -x
        rm -f Wrench-windows
        ln -sf wrench-release Wrench-windows
        make-release-tgz Wrench-windows.tgz
        (
            myscr bash -c 'cd Wrench-windows; of ./Wrench.exe'
        )&

    fi
fi
