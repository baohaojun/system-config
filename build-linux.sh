#!/bin/bash

set -e

cd ~/src/github/snorenotify
mkdir -p cmake-build
cd cmake-build
cmake ..
make -j8
sudo make install
(
    cd /usr/lib/x86_64-linux-gnu/qt5/mkspecs/modules
    sudo relative-link /usr/local/mkspecs/modules/qt_Libsnore* .
)

