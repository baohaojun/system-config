#!/bin/bash

function my-read() {
    echo -- "$@" > my-read.$$.txt
    notepad my-read.$$.txt || true
    rm -f my-read.$$.txt || true
}

function mkdir() {
    command mkdir -p "$@"
}

function get-python() {
    mkdir ~/external/python
    cd ~/external/python
    for x in  ~/system-config/etc/python-win/vers/*;
    do
        x=$(basename $x)
        eval "wget -N http://python.org/ftp/python/$x/python-$x.msi&"
    done
    wget -N http://sourceforge.net/projects/pywin32/files/pywin32/Build216/pywin32-216.win32-py2.7.exe&
    wait

    for x in  ~/system-config/etc/python-win/vers/*;
    do
        x=$(basename $x)
        chmod +x python-$x.msi
        cygstart python-$x.msi
    done

    my-read -p "Press any key to install pywin32..."
    (
        chmod +x *.exe
        cygstart *.exe
    )
}

function download-all()
{
    (
        mkdir -p ~/external/download-all
        cd ~/external/download-all



        file_list=(
            $(
                for x in putty.exe puttytel.exe pscp.exe psftp.exe plink.exe pageant.exe puttygen.exe; do
                    echo http://the.earth.li/~sgtatham/putty/latest/x86/$x;
                done
            )
            http://mirrors.zerg.biz/stunnel/stunnel-4.50-installer.exe
            http://www.imagemagick.org/download/binaries/ImageMagick-6.7.4-Q16-windows.zip
            http://download.sysinternals.com/Files/ProcessMonitor.zip
            http://download.sysinternals.com/Files/ProcessExplorer.zip
            http://download.sysinternals.com/Files/PsTools.zip
            http://download.sysinternals.com/Files/WinObj.zip
            http://download.sysinternals.com/Files/DebugView.zip
            http://www.nirsoft.net/utils/resourcesextract.zip
            http://www.nirsoft.net/utils/iconsext.zip
            http://www.winterdrache.de/freeware/png2ico/data/png2ico-win-2002-12-08.zip
            http://download.sysinternals.com/Files/Handle.zip
        )

        for x in "${file_list[@]}"; do
            if ! [[ -f `basename "$x"` ]]; then
                eval "wget -N $x&"
            fi
        done
        wait
        cygstart ~/system-config/bin/windows/kbddvp.exe

        cd ~/system-config/bin/windows/ext
        for x in ~/external/download-all/*.zip; do
            if [[ -f "$x" ]]; then
                /bin/unzip -o "$x";
            fi
        done
        find . -type f -print0|xargs -0 bash -c 'mv "$@" ./' true
        find . -type f | perl -ne 'chomp; if (lc($_) ne $_) { printf "mv \"%s\" \"%s\"\n", $_, lc($_)}'|bash -x
        mv ./*/* . || true
        chmod a+x ./* || true
    )&
}

function setup-deb-src()
{
    mkdir -p ~/external/emacs-site-lisp/ ~/external/deb-src
    cp /usr/share/emacs/site-lisp/subdirs.el ~/external/emacs-site-lisp/
    cd ~/external/deb-src

    source_list=(
        mirrors.163.com/debian/dists/unstable/main/source/Sources.gz
        mirrors.163.com/debian/dists/unstable/contrib/source/Sources.gz
        mirrors.163.com/debian/dists/unstable/non-free/source/Sources.gz
    )

    y=0
    for x in "${source_list[@]}"; do
        ((y++)) || true
        ( builtin cd ../ && wget -N -r http://$x )
        ln -sf ../$x Sources.gz.$y
    done
}

function get-bbdb()
{
    (
        set -ex
        setup-deb-src
        x=$(get-deb-src-dir bbdb)
        rm *bbdb*/ *BBDB* -rf
        wget -N "$x"
        cd ~/external/emacs-site-lisp/
        if [[ $x =~ .gz$ ]]; then
            tar zxfv ../deb-src/"$(basename "$x")"
        else
            tar jxfv ../deb-src/"$(basename "$x")"
        fi
        builtin cd *BBDB*/
        ./configure --with-emacs=$HOME/external/emacs-nt/bin/emacs --with-gnus-dir=$HOME/external/emacs-nt/lisp/gnus
        find . -iname 'makefile'|xargs.exe dos2unix
        perl -npe 's/`pwd`/`cygpath -alm .`/g' -i lisp/Makefile
        make -C lisp bbdb-autoloads.el || true
        make
    )&
}

function get-boost-vc9() {
    (
        set -e
        cd ~/external/deb-src
        x=http://sourceforge.net/projects/boost/files/boost/1.47.0/boost_1_47_0.7z
        wget -N $x
        7z -x $x
        cd boost_1_47_0
        my-read make sure that you have installed vc9
        cd ./tools/build/v2/
        cmd.exe /c bootstrap.bat
        cd -
        ./tools/build/v2/b2.exe --toolset=msvc-9.0 stage
    )
}

function emacs-site-lisps()
{
    setup-deb-src
    file_list=(
        `get-deb-src-dir dictionary-el`
        `get-deb-src-dir emacs-goodies-el`
        `get-deb-src-dir cscope`
        `get-deb-src-dir muse-el`
        `get-deb-src-dir w3m-el-snapshot`
        `get-deb-src-dir bbdb`
    )

    emacs_zip=http://alpha.gnu.org/gnu/emacs/pretest/windows/emacs-24.0.92-bin-i386.zip
    non_emacs_list=(
        `get-deb-src-dir offlineimap`
        `get-deb-src-dir exuberant-ctags`
        http://www.python.org/ftp/python/3.1.4/Python-3.1.4.tar.bz2
        $emacs_zip
    )

    for x in "${file_list[@]}" "${non_emacs_list[@]}"; do
        eval "wget -N $x&"
    done
    wait

    (
        oldd=$(pwd)
        mkdir -p ~/external/
        cd ~/external/

        unzip $oldd/$(basename $emacs_zip)
        rm -rf emacs-nt
        mv emacs-24*/ emacs-nt
        chmod +x emacs-nt/bin/*
        cd emacs-nt/lisp/gnus
        rm nnmaildir.elc
        patch -p2 < ~/system-config/doc/nnmaildir.patch
        (get-bbdb)
    )&

    (
        cd ~/external/emacs-site-lisp/
        for x in "${file_list[@]}"; do
            if [[ $x =~ .gz$ ]]; then
                tar zxfv ../deb-src/"$(basename "$x")"
            else
                tar jxfv ../deb-src/"$(basename "$x")"
            fi
        done
        cd emacs-goodies-el*/elisp/emacs-goodies-el/ && mkdir themes

    )&

    (
        for x in "${non_emacs_list[@]}"; do
            if [[ $x =~ .gz$ ]]; then
                tar zxfv ../deb-src/"$(basename "$x")"
            elif [[ $x =~ .bz2$ ]]; then
                tar jxfv ../deb-src/"$(basename "$x")"
            fi
        done
    )

    rm  ~/system-config/bin/windows/ext/offlineimap -f
    ln -sf $(readlink -f *offlineimap*/offlineimap.py) ~/system-config/bin/windows/ext/offlineimap
    (
        builtin cd *ctags*/ && ./configure && make -j8 install && ln -sf /usr/local/bin/ctags.exe /usr/bin/ctags-exuberant
    )&
    (
        builtin cd ~/system-config/gcode/global && sh reconf.sh && ./configure && make -j8 install && git clean -xfd
    )&
    (
        set -e;
        builtin cd *python*/ || builtin cd *Python*/
        patch -p1 <<EOF
diff --git a/Modules/main.c b/Modules/main.c
index 4dcc32d..2e6548f 100644
--- a/Modules/main.c
+++ b/Modules/main.c
@@ -14,7 +14,6 @@
 #include <windows.h>
 #ifdef HAVE_FCNTL_H
 #include <fcntl.h>
-#define PATH_MAX MAXPATHLEN
 #endif
 #endif
EOF
        ./configure;
        make -j8
        make install
        ln -sf /usr/local/bin/python3.1.exe /usr/local/bin/python3
    )&

}


function manual-download()
{
    page_list=(
        http://www.microsoft.com/en-hk/download/details.aspx?id=14632
        http://www.microsoft.com/en-us/download/details.aspx?id=8328
        http://www.riverbankcomputing.co.uk/software/pyqt/download
        http://python.net/crew/mhammond/win32/Downloads.html
        http://www.python.org/download/
        http://www.andlinux.org/downloads.php
        http://www.microsoft.com/whdc/devtools/debugging/default.mspx
        http://www.gimp.org/windows/
        http://java.sun.com/javase/downloads/index.jsp
        http://sourceforge.net/projects/com0com/files/
        http://nsis.sourceforge.net/Download
        http://sourceforge.net/projects/py2exe/files/
        http://sourceforge.net/projects/visemacs/files/
        http://code.google.com/p/zscreen/downloads/list
        http://www.avira.com/en/download/
    )
    for x in "${page_list[@]}"; do
        cygstart "$x";
    done
}

function get-ms-addons()
{
    cd ~/external/download-all/
    addon_list=(
        http://download.microsoft.com/download/5/6/7/567758a3-759e-473e-bf8f-52154438565a/dotnetfx.exe
        http://download.microsoft.com/download/0/6/1/061F001C-8752-4600-A198-53214C69B51F/dotnetfx35setup.exe
    )
    for x in "${page_list[@]}"; do
        wget -c "$x";
        `basename "$x"` /q
    done
}

function setup-vc6-env() {
    unset PATHVC
    for x in ~/vc6/path/* ~/vc6/inc/* ~/vc6/lib/*; do
        if test ! -e "$x"; then
            my-read -p "setup-vc6-env: $x is $(readlink -m "$x"); but it does not exist"
        fi
    done
    for x in ~/vc6/path/*; do export PATHVC=$PATHVC:`readlink -m "$x"`; done
    export PATH=~/system-config/bin/:$PATHVC:$PATH
    export INCLUDE=`for x in ~/vc6/inc/*; do readlink -f "$x"; done|u2dpath`
    export LIB=`for x in ~/vc6/lib/*; do readlink -f "$x"; done|u2dpath`
    export TEMP=/tmp
    export TMP=/tmp
}

function build-boost() {
    (
        cd ~/external/boost
        rm -rf boost_1_34_1/
        tar jxfv boost_1_34_1.tar.bz2
        mkdir -p boost_1_34_1/stage/lib

        (
            cd boost_1_34_1/tools/jam/src
            patch -p1 <<EOF
diff --git a/build.bat b/build.bat
index c331d5c..3cd4f42 100755
--- a/build.bat
+++ b/build.bat
@@ -413,7 +413,7 @@ rename y.tab.h jamgram.h
     shift
     goto Set_Args
 )
-.\bootstrap\jam0 -f build.jam --toolset=%BOOST_JAM_TOOLSET% "--toolset-root=%BOOST_JAM_TOOLSET_ROOT% " %args%
+.\bootstrap\jam0 -f build.jam --toolset=%BOOST_JAM_TOOLSET% "--toolset-root=%BOOST_JAM_TOOLSET_ROOT%" %args%
 :Skip_Jam

 :Finish

EOF
            cmd.exe /c build.bat msvc
            cp bin.ntx86/bjam.exe /c/windows/system32/
        )
        cd boost_1_34_1/
        cp /c/Program\ Files/Microsoft\ Visual\ Studio/VC98/Bin/VCVARS32.BAT /c/Program\ Files/Microsoft\ Visual\ Studio/VC98/Bin/VCVARS32.BAT.bak
        echo -n > /c/Program\ Files/Microsoft\ Visual\ Studio/VC98/Bin/VCVARS32.BAT
        bjam -sBOOST_ROOT=. -sTOOLS=msvc 2>&1|tee build.log
        find . -name "*.dll" -o -name "*.lib" > files.txt
        cp `cat files.txt` ./stage/lib
        cd ./stage/lib
        mv libboost_regex-vc6-mt-1_34_1.lib libboost_regex-vc6-mt-p-1_34_1.lib
    )
}

function get-stlport-and-boost()
{
    mkdir -p ~/external/boost
    cd ~/external/boost
    mkdir -p STLport-5.2.1/lib STLport-5.2.1/stlport boost_1_34_1/stage/lib

    wget -N http://sourceforge.net/projects/stlport/files/STLport/STLport-5.2.1/STLport-5.2.1.tar.bz2&
    wget -N http://sourceforge.net/projects/boost/files/boost/1.34.1/boost_1_34_1.tar.bz2&

    setup-vc6-env
    (
        mkdir -p vcrun2010
        cd vcrun2010
        wget -N http://download.microsoft.com/download/5/B/C/5BC5DBB3-652D-4DCE-B14A-475AB85EEF6E/vcredist_x86.exe&
        wait
        chmod +x vcredist_x86.exe
        cygstart vcredist_x86.exe
        wget -N http://download.microsoft.com/download/1/B/E/1BE39E79-7E39-46A3-96FF-047F95396215/dotNetFx40_Full_setup.exe
        chmod +x dotNetFx40_Full_setup.exe
        cygstart dotNetFx40_Full_setup.exe
    ) >/dev/null 2>&1 &

    (
        mkdir -p psdk
        cd psdk
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.1.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.2.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.3.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.4.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.5.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.6.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.7.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.8.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.9.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.10.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.11.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.12.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.13.cab&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.bat&
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/Extract.exe&
        wget -N http://www.avantbrowser.cn/release/absetup.exe&
        wait
        cmd.exe /c PSDK-FULL.bat ./ext
        cygstart ./ext
    ) >/dev/null 2>&1 &

    (
        mkdir -p vc6
        cd vc6
        wget -N http://download.microsoft.com/download/1/9/f/19fe4660-5792-4683-99e0-8d48c22eed74/Vs6sp6.exe
    ) >/dev/null 2>&1 &

    my-read -p "Make sure you have setup vc6/sp6/psdk! Press any key to continue..."
    my-read -t 5 -p "Note: boost build will not work on remote login! Press any key or wait 5 seconds..." || true

    (
        tar jxfv ./STLport-5.2.1.tar.bz2
        cd STLport-5.2.1
        patch -p1 <<EOF
diff --git a/stlport/stl/config/user_config.h b/stlport/stl/config/user_config.h
index 4efd727..06317c7 100644
--- a/stlport/stl/config/user_config.h
+++ b/stlport/stl/config/user_config.h
@@ -194,9 +194,9 @@
  * If you are using new PSDK with VC++ 6.0 or lower,
  * please define this to get correct prototypes for InterlockedXXX functions
  */
-/*
+
 #define _STLP_NEW_PLATFORM_SDK 1
-*/
+

 /*
  * For the same reason as the one above we are not able to detect easily use
EOF
        (set +e; cmd.exe /c configure.bat msvc6; cd build/lib; nmake /fmsvc.mak install)
    )

    build-boost
}


if test "$(basename -- "$BASH_SOURCE")" = "$(basename -- "$0")";
then
    if test "$(basename -- "$0")" != download-external.sh;
    then
        set -e
        set -x
        `basename $0` "$@"
    else
        set -x
        (get-python)
        (manual-download)
        (download-all)
        (setup-deb-src)
        (get-ms-addons)
        (setup-vc6-env)
        (get-stlport-and-boost)
    fi
fi
