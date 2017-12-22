#!/bin/bash

set -e
cd $(dirname $(readlink -f $0))


## start code-generator "^\\s *#\\s *"
# generate-getopt @build-snore
## end code-generator
## start generated code
TEMP=$(getopt -o h \
              --long build-snore,help,no-build-snore \
              -n $(basename -- $0) -- "$@")
build_snore=false
eval set -- "$TEMP"
while true; do
    case "$1" in

        --build-snore|--no-build-snore)
            if test "$1" = --no-build-snore; then
                build_snore=false
            else
                build_snore=true
            fi
            shift
            ;;
        -h|--help)
            set +x
            echo
            echo
            echo Options and arguments:
            printf "%06s" " "
            printf %-24s '--[no-]build-snore'
            echo
            exit
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error"
            ;;
    esac
done


## end generated code


if test ! -e ~/src/github/Wrench/windows/binaries/libsnore-qt5.dll || test "$build_snore" = true; then
    (
        cd ~/src/github/Wrench/SnoreNotify
        ./build-wine.sh
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
        qt-wine windeployqt --qmldir z:$HOME/src/github/Wrench/SnoreNotify/src/plugins/backends/snore -qml -quick .
        mv tmp/* .
        rmdir tmp
        find . -iname '*.dll' -o -iname '*.exe' |xargs chmod +x
        relative-link ~/src/github/Wrench/*.lua . -f
    )
}

function make-release-tgz()
{
    # rsync readme.* ./release/
    rsync -a *.png ./release/
    command rsync -a -d release/ download/release/ wrench-release --exclude="*.obj" \
            --exclude="*.o" \
            --exclude="*.cpp" \
            --exclude="*.moc" \
            --delete

    set -x
    if test "$DOING_WRENCH_RELEASE"; then
        command rsync wrench-release/ $release_dir -a -L --delete --exclude=.git
        exit
    fi

    cd $build_dir/release
    myscr bash -c "WINEARCH=win32 WINEPREFIX=~/.wine2 SNORE_QML=z:$HOME/src/github/Wrench/SnoreNotify/src/plugins/backends/snore/notification.qml wine cmd /c ./Wrench.exe"
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
rsync * $build_dir -a --exclude release
rsync release $build_dir -a -L
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
