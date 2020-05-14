#!/usr/bin/env bash

set -e

cd ~/src/github/Wrench/SnoreNotify
mkdir -p cmake-build
cd cmake-build
qt-wine cmake -D ECM_DIR=z:/usr/share/ECM/cmake -G '"MinGW Makefiles"' z:/home/bhj/src/github/Wrench/SnoreNotify
qt-wine mingw32-make -j8 install
perl -npe 's,= (C:/Program Files.*/SnoreNotify/\S*),= "$1",' -i ~/.wine/drive_c/Program\ Files\ \(x86\)/SnoreNotify/mkspecs/modules/*.pri
for x in ~/.wine/drive_c/Qt/*/*/mingw*/mkspecs/modules; do
    relative-link -f ~/.wine/drive_c/Program\ Files\ \(x86\)/SnoreNotify/mkspecs/modules/* "$x";
done
