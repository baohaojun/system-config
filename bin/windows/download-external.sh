#!/bin/bash
test "$DOWN" == yes && set -e

function download-all()
{
    mkdir -p /c/download
    cd /c/download
    wget -N http://www.kaufmann.no/downloads/winnt/kbddvp-1_2_2-i386.exe
    chmod +x kbddvp-1_2_2-i386.exe
    cygstart kbddvp-1_2_2-i386.exe

    cygstart http://www.kaufmann.no/roland/dvorak/winxp.html

    file_list=( 
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
            wget -N "$x"
        fi
    done
            

    cd ~/bin/windows/lnks
    for x in /c/download/*.zip; do 
        if [[ -f "$x" ]]; then
            /bin/unzip -o "$x";
        fi
    done
    mv ./*/* . || true
    chmod a+x ./* || true
}

test "$DOWN" == yes && download-all
function setup-deb-src()
{
    mkdir -p ~/tools/emacs-site-lisp/
    cd ~/tools/emacs-site-lisp/
    cp /usr/share/emacs/site-lisp/subdirs.el . 
    

    source_list=(
        mirrors.163.com/debian/dists/unstable/main/source/Sources.bz2 
        mirrors.163.com/debian/dists/unstable/contrib/source/Sources.bz2 
        mirrors.163.com/debian/dists/unstable/non-free/source/Sources.bz2
    )

    y=0
    for x in "${source_list[@]}"; do 
        ((y++)) || true
        ( builtin cd ../ && wget -N -r http://$x )
        ln -sf ../$x Sources.bz2.$y
    done
}

function get-bbdb()
{
    (
	set -ex
	setup-deb-src
	x=$(get-deb-src-dir bbdb)
	rm *bbdb*/ -rf
	wget -N "$x"
	if [[ $x =~ .gz$ ]]; then
            tar zxfv "$(basename "$x")"
	else 
	    tar jxfv "$(basename "$x")"
	fi
	builtin cd *BBDB*/
	./configure --with-emacs=$HOME/external/emacs-nt/bin/emacs --with-gnus-dir=$HOME/external/emacs-nt/lisp/gnus 
	find . -iname 'makefile'|xargs.exe dos2unix
	perl -npe 's/`pwd`/`cygpath -alm .`/g' -i lisp/Makefile
	make -C lisp bbdb-autoloads.el || true
	make
    )
}

function emacs-site-lisps()
{
    setup-deb-src
    file_list=(
        `get-deb-src-dir dictionary-el`
	`get-deb-src-dir offlineimap`
        `get-deb-src-dir emacs-goodies-el`
        `get-deb-src-dir cscope`
        `get-deb-src-dir muse-el`
        `get-deb-src-dir w3m-el-snapshot`
        `get-deb-src-dir exuberant-ctags`
	`get-deb-src-dir bbdb`
	http://www.python.org/ftp/python/3.1.4/Python-3.1.4.tar.bz2
    )
    
    for x in "${file_list[@]}"; do
        wget -N "$x"
	if [[ $x =~ .gz$ ]]; then
            tar zxfv "$(basename "$x")"
	else 
	    tar jxfv "$(basename "$x")"
	fi
    done
    
    rm  ~/bin/windows/lnks/offlineimap -f
    relative-link *offlineimap*/offlineimap.py ~/windows-config/bin/windows/lnks/offlineimap
    test -e /usr/bin/ctags-exuberant || (
	builtin cd *ctags*/ && ./configure && make -j8 install && ln -sf /usr/local/bin/ctags.exe /usr/bin/ctags-exuberant
    )
    test -e /usr/local/bin/global.exe || (
	builtin cd ~/gcode/global && sh reconf.sh && ./configure && make -j8 install && git clean -xfd
    )
    test -e /usr/local/bin/python3 ||
    (
        set -e; 
        builtin cd *python*/;
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
    )

    (
	(
	    mkdir -p ~/external &&
	    cd ~/external &&
	    wget -N http://ftp.gnu.org/gnu/emacs/windows/emacs-23.3-bin-i386.zip
	    of emacs-23.3-bin-i386.zip
	    read -p "Press any key when you have installed emacs into ~/external/emacs-nt: "
	    test -e ~/external/emacs-nt/bin/emacs.exe || die "error: can't find emacs anywhere"
	)
	    
    )

    cd emacs-goodies-el*/elisp/emacs-goodies-el/ && mkdir themes
}

test "$DOWN" == yes && emacs-site-lisps

function manual-download()
{
    page_list=(
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
    cd /c/download/
    addon_list=(
        http://download.microsoft.com/download/5/6/7/567758a3-759e-473e-bf8f-52154438565a/dotnetfx.exe
        http://download.microsoft.com/download/0/6/1/061F001C-8752-4600-A198-53214C69B51F/dotnetfx35setup.exe
    )
    for x in "${page_list[@]}"; do 
        wget -c "$x";
        `basename "$x"` /q
    done
}

test "$DOWN" == yes && manual-download

function setup-vc6-env() {
    unset PATHVC
    for x in ~/vc6/path/*; do export PATHVC=$PATHVC:`readlink -m "$x"`; done
    export PATH=~/bin/:$PATHVC:$PATH
    export INCLUDE=`for x in ~/vc6/inc/*; do readlink -f "$x"; done|u2dpath`
    export LIB=`for x in ~/vc6/lib/*; do readlink -f "$x"; done|u2dpath`
}

function build-boost() {
    (
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
    test -e /d/bhj || ln -s ~/external/boost /d/bhj
    cd /d/bhj
    mkdir -p STLport-5.2.1/lib STLport-5.2.1/stlport boost_1_34_1/stage/lib 

    wget -N http://sourceforge.net/projects/stlport/files/STLport/STLport-5.2.1/STLport-5.2.1.tar.bz2
    wget -N http://sourceforge.net/projects/boost/files/boost/1.34.1/boost_1_34_1.tar.bz2

    setup-vc6-env
    echo start downloading vcrun2010, psdk2003, vc6sp6 in the background
    (
        mkdir -p vcrun2010
        cd vcrun2010
        wget -N http://download.microsoft.com/download/5/B/C/5BC5DBB3-652D-4DCE-B14A-475AB85EEF6E/vcredist_x86.exe
        chmod +x vcredist_x86.exe
        cygstart vcredist_x86.exe
    ) >/dev/null 2>&1 &

    (
        mkdir -p psdk
        cd psdk
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.1.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.2.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.3.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.4.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.5.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.6.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.7.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.8.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.9.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.10.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.11.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.12.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.13.cab
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/PSDK-FULL.bat
        wget -N http://download.microsoft.com/download/platformsdk/sdk/update/win98mexp/en-us/3790.0/FULL/Extract.exe
        wget -N http://www.avantbrowser.cn/release/absetup.exe
    ) >/dev/null 2>&1 &

    (
        mkdir -p vc6
        cd vc6
        wget -N http://download.microsoft.com/download/1/9/f/19fe4660-5792-4683-99e0-8d48c22eed74/Vs6sp6.exe
    ) >/dev/null 2>&1 &

    read -p "Make sure you have setup vc6/sp6/psdk! Press any key to continue..."
    read -t 5 -p "Note: boost build will not work on remote login! Press any key or wait 5 seconds..." || true

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
        cmd.exe /c configure.bat msvc6; cd build/lib; nmake /fmsvc.mak install
    )

    build-boost

    (
        mkdir -p python2.5
        cd python2.5
        wget -N http://python.org/ftp/python/2.7.2/python-2.7.2.msi
        chmod +x *.msi
        cygstart *.msi
    )
    read -p "Press any key to download pywin32..."
    (
        mkdir -p pywin32
        cd pywin32
        wget -N http://sourceforge.net/projects/pywin32/files/pywin32/Build216/pywin32-216.win32-py2.7.exe
        chmod +x *.exe
        cygstart *.exe
    )
    read -p "Press any key to install python3..."
    (
        mkdir -p python3.1
        cd python3.1
        wget -N http://python.org/ftp/python/3.1.3/python-3.1.3.msi
        chmod +x *.msi
        cygstart *.msi
    )        
}

if test "$(basename -- "$BASH_SOURCE")" = "$(basename -- "$0")"; 
then 
    if test "$(basename -- "$0")" != download-external.sh; 
    then
	set -e
	set -x
	`basename $0` "$@"
    fi
fi
