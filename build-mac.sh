#!/bin/bash
set -e
cd $(dirname $(readlink -f $0))

if test $(uname) = Linux; then
    psync bhj-mac . $*
    remote-cmd bhj-mac bash -c "
        export ReleaseVersion='$ReleaseVersion';
        set -x;
        cd $(up .);
        ./build-mac.sh"
else
    set -e
    set -x

    cd ~/src/github/snorenotify/

    should_cmake_qmake=false
    if test ! -e Makefile; then
        should_cmake_qmake=true
        cmake -D ECM_DIR=/usr/local/share/ECM/cmake .
    fi
    make -j8 install
    export QT_LOGGING_RULES="libsnorenotify.debug=true"

    snoresend -t hello1 -m world 2>&1|perl -npe 's/^/1: /'|| true

    echo
    echo '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    echo

    cd ~/src/github/snorenotify/src/snoresend
    rm -rf *.app

    if test $should_cmake_qmake = true; then
        rm Makefile -f
        qmake
    fi
    make
    (
        cd ~/src/github/snorenotify/src/snoresend/WrenchTest.app/Contents/MacOS
        ./WrenchTest -t hello2 -m world 2>&1|perl -npe 's/^/2: /'|| true
    )
    cp /usr/local/lib/plugins/libsnore-qt5/*.so WrenchTest.app/Contents/MacOS -av
    macdeployqt WrenchTest.app -verbose=1 $(for x in WrenchTest.app/Contents/MacOS/*.so; do echo -executable=$x; done) -qmldir=${HOME}/src/github/snorenotify/src/plugins/backends/snore

    echo
    echo '****************************************************************'
    echo

    (
        cd ~/src/github/snorenotify/src/snoresend/WrenchTest.app/Contents/MacOS
        ./WrenchTest -t hello3 -m world || true
    )
fi
