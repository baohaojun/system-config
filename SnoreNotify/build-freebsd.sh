#!/bin/bash
set -e

umask 0022

me=$(readlink -f $0)
if test ! -e "$me"; then
    me=$(readlink -f "$(which $0)")
    if test ! -e "$me"; then
        die "Can't find out about me"
        exit 1
    fi
fi
b0=$(basename $0)

cd $(dirname $me)

mkdir -p cmake-build
cd cmake-build
cmake $(
    if test ! -d /usr/share/ECM/cmake/ -a ! -d /usr/local/share/ECM/cmake/; then
        echo -D ECM_DIR=${HOME}/src/github/Wrench/extra-cmake-modules/usr/share/ECM/cmake -D CMAKE_INSTALL_PREFIX=/home/bhj/external/local/qt5-ubuntu-1404
    fi
    )  ..
make -j8
sudo make install
(
    cd $(pkg-config --variable=libdir Qt5Core)/mkspecs/modules
    for x in /usr/local/mkspecs/modules/qt_Libsnore*; do
        test -e "$x" || die "Can't open $x?"
    done
    sudo ln -s /usr/local/mkspecs/modules/qt_Libsnore* .
)
