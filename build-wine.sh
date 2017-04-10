#!/bin/bash

cd $(dirname $(readlink -f $0))

if test ! -e ~/src/github/Wrench/windows/binaries/libsnore-qt5.dll; then
    (
        cd ~/src/github/snorenotify
        git clean -xfd
        mkdir -p cmake-build
        qt-wine cmake -D ECM_DIR=z:/usr/share/ECM/cmake -G "MinGW Makefiles" z:/home/bhj/src/github/snorenotify
        qt-wine mingw32-make -j8 install
    )
fi

function deploy-wrench()
{
    rsync windows/binaries/* ./release -v -L -r
    rsync *.lua  ./release -v

    (
        cd ./release
        mkdir -p tmp
        mv adb.exe the-true-adb.exe luac.exe lua.exe tmp/
        qt-wine windeployqt --qmldir z:$HOME/src/github/snorenotify/src/plugins/backends/snore -qml -quick .
        mv tmp/* .
        rmdir tmp
        find . -iname '*.dll' -o -iname '*.exe' |xargs chmod +x
        relative-link ~/src/github/Wrench/*.lua . -f
    )
}

function make-release-tgz()
{
    # rsync readme.* ./release/
    rsync -av *.png ./release/
    command rsync -av -d release/ download/release/ wrench-release --exclude="*.obj" \
            --exclude="*.o" \
            --exclude="*.cpp" \
            --exclude="*.moc" \
            --delete

    set -x
    if test "$DOING_WRENCH_RELEASE"; then
        command rsync wrench-release/ $release_dir -av -L --delete --exclude=.git
    fi

    if TMOUT=5 yes-or-no-p -y "Should run Wrench?"; then
        cd $release_dir
        myscr bash -c "WINEARCH=win32 WINEPREFIX=~/.wine2 wine ./Wrench.exe"
    fi
}


set -e
set -o pipefail

function wine() {
    cat > build.bat<<EOF
set path=$(for x in ~/.wine/drive_c/Qt/Qt*/*/mingw*/bin; do
               echo $x;
           done |
           perl -npe 'chomp; s!$ENV{HOME}/.wine/drive_c!c:!; s!$!;!')%path%
$@
EOF
    command wine cmd.exe /c build.bat
}

build_dir=~/tmp/build-wrench-windows
release_dir=~/src/github/Wrench-windows
rsync * $build_dir -av --exclude release
rsync release $build_dir -av -L
cd $build_dir

if ! which i686-w64-mingw32-nm; then
    sudo apt-get install mingw32
fi
(
    cd lua
    PATH=~/system-config/bin/mingw:$PATH make -j8 mingw
)

(
    cd luamd5
    PATH=~/system-config/bin/mingw/:$PATH make -j8 PLATFORM=mingw
)

for x in . download; do
    (
        cd $x
        if test ! -e Makefile; then
            wine qmake.exe
        fi
        wine mingw32-make.exe -j8 | perl -npe 's/\\/\//g'
    )
done

deploy-wrench
set -x

make-release-tgz
