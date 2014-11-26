#!/bin/bash

cd $(dirname $(readlink -f $0))

function copy-dlls()
{
    rsync windows/binaries/* ./release -v -L -r
    rsync *.lua  ./release -v

    req_dlls=( icudt52.dll icuin52.dll icuuc52.dll
               qt5network.dll QT5CORE.DLL QT5GUI.DLL QT5WIDGETS.DLL
             )

    for x in "${req_dlls[@]}"; do
        rsync $(find $1/bin/ -maxdepth 1 -iname $x | grep . || echo $1/bin/$x) ./release -av
        chmod 555 $(find ./release/ -iname $x)
    done

    rsync ~/.wine/drive_c/OpenSSL-Win32/*.dll ./release/ -av
    chmod 555 ./release/*.dll

    for x in libEGL.dll libGLESv2.dll libstdc++-6.dll libwinpthread-1.dll libgcc_s_dw2-1.dll; do
        rsync $(find $1/bin/ -maxdepth 1 -iname $x | grep . || echo $1/bin/$x) ./release -av || continue
        chmod 555 $(find ./release/ -iname $x)
    done

    mkdir -p ./release/platforms
    rsync $1/plugins/platforms/qwindows.dll ./release/platforms -av
    chmod 555 ./release/platforms/*
}

function make-release-tgz()
{
    rsync readme.* ./release/
    rsync -av *.png ./release/
    command rsync -av -d release/ download/release/ t1wrench-release --exclude="*.obj" \
            --exclude="*.o" \
            --exclude="*.cpp" \
            --exclude="*.moc"

    set -x
    (
        zip -r ${1:-T1Wrench-windows.zip} T1Wrench-windows
        smb-push ${1:-T1Wrench-windows.zip} ~/smb/share.smartisan.cn/share/baohaojun/T1Wrench
        rsync T1Wrench-windows.zip rem:/var/www/html/baohaojun/ -v
    )&
    command rsync T1Wrench-windows/ $release_dir -av -L --delete --exclude=.git
    cd $release_dir
    ./update-md5s.sh&
}


set -e
set -o pipefail

function wine() {
    cat > build.bat<<EOF
set path=c:/Qt-mingw/Qt5.3.1/5.3/mingw482_32/bin;c:/Qt-mingw/Qt5.3.1/Tools/mingw482_32/bin;%path
$@
EOF
    command wine cmd.exe /c build.bat
}

build_dir=~/tmp/build-t1-windows
release_dir=~/src/github/T1Wrench-windows
rsync * $build_dir -av --exclude release
rsync release $build_dir -av -L
cd $build_dir


(
    cd lua
    PATH=~/bin/mingw:$PATH make mingw
)

(
    cd luamd5
    PATH=~/bin/mingw/:$PATH make PLATFORM=mingw
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

copy-dlls /cygdrive/c/Qt-mingw/./Qt5.3.1/5.3/mingw482_32
set -x
rm -f T1Wrench-windows
ln -sf t1wrench-release T1Wrench-windows
make-release-tgz

(
    cd $release_dir
    rm build.bat -f
    mkfifo /tmp/build-wine.$$
    myscr bash -c "wine ./T1Wrench.exe >/tmp/build-wine.$$ 2>&1"
    cat /tmp/build-wine.$$
    rm /tmp/build-wine.$$
)
