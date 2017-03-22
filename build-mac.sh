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

    if test ! -e Makefile; then
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
    qmake
    make
    (
        cd ~/src/github/snorenotify/src/snoresend/WrenchTest.app/Contents/MacOS
        ./WrenchTest -t hello2 -m world 2>&1|perl -npe 's/^/2: /'|| true
    )
    macdeployqt WrenchTest.app -verbose=1

    echo
    echo '****************************************************************'
    echo

    (
        cd ~/src/github/snorenotify/src/snoresend/WrenchTest.app/Contents/MacOS
        ./WrenchTest -t hello3 -m world || true
    )
fi
