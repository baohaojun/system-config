#!/bin/bash
set -e

THIS=$(cygpath -au "$0")
THIS=$(readlink -f "$THIS")
echo "we are executing $THIS"
BIN_WINDOWS=$(dirname "$THIS")

# strlen("system-config/bin/windows") is 25, and -25 means the last
# 25 chars, but we can't simply write -25, because that's another
# expansion syntax. We must write 0-25, and it's arithmetic, which is
# allowed.

if test "${BIN_WINDOWS:0-25}" != "system-config/bin/windows"
then
    read -e -p 'startup.sh is not in ~/system-config/bin/windows! Press any key to exit...'
    #pause and exit, it's all blowed up!
    exit
fi

cd -P "$BIN_WINDOWS"/../../..
export HOME2=`pwd`

function die() {
    echo "$@" 1>&2
    read -e -p 'Press any key to exit...'
    exit
}

if test -e /q -a ! -L /q; then
    die "Error, the /q exist and is not a symlink";
fi

rm -f /q;
ln -s "$HOME2" /q
export HOME=/q

cd /cygdrive/c/
(
    mkdir -p etc/ywb/
    cd etc/ywb/
    cp ~/etc/ywb-disable.rc disable.rc
)

if test ! -e setup-x86_64.exe; then
    if ! which wget && yes-or-no-p -y wget not found, use browser to download; then
        cygstart http://cygwin.com/setup-x86_64.exe
        exit 1
    fi
    wget -N  http://cygwin.com/setup-x86_64.exe
    chmod +x setup-x86_64.exe
fi

pkgs=(nc util-linux git vim rsync inetutils apache2 shutdown make
      expect gnome-common gcc-core gcc-g++ mingw-gcc-core
      mingw-gcc-g++ mingw64-i686-gcc-core mingw64-i686-gcc-g++
      mingw64-x86_64-gcc-core mingw64-x86_64-gcc-g++ screen
      cygutils-extra procps wget git-svn libcrypt-devel flex gperf
      bison)

if test "$NO_INSTALL" = true; then 
    true
else
/cygdrive/c/setup-x86_64.exe -q -n -d -A -P "${pkgs[@]}" || true

if true; then
    for x in "${pkgs[@]}"; do
        /cygdrive/c/setup-x86_64.exe -q -n -d -A -P $x
    done
fi
fi

cpan String::ShellQuote

~/system-config/bin/after-co-ln-s.sh

cygserver-config
. ~/.bashrc-windows
export HOME=/q
cd ~/bin/windows/Imap4Monitor/
function report_error()
{
    echo "$@"
    echo "Sleeping 5 seconds to continue..."
    sleep 5
}
~/bin/windows/mkpyqt.py || report_error 'Error: you have not installed python2.5 and pyqt into "C:\python25"!'

function mkdir () #so that mkdir won't fail if it is already there.
{
    command mkdir -p "$@"
}

cd ~/bin/windows/
mkdir  ~/bin/windows/lnks
for x in {c..z}; do test -e /$x || (rm -f /$x; ln -s /cygdrive/$x /); done
DOWN=${DOWN:-yes} ./download-external.sh
find . -type l -exec relink.sh '{}' \;


cd ~/bin/windows
cpan String::ShellQuote
cpan String::Approx

ln -sf ~/'Application Data/Microsoft/Internet Explorer/Quick Launch' ~/SendTo/ || report_error "Error: you are not doing it from $HOMEPATH"
mkdir -p ~/.fonts
cp ~/doc/monaco-linux.ttf /cygdrive/c/windows/fonts/simsun.ttc /cygdrive/c/windows/fonts/cour.ttf ~/.fonts || true
fc-cache || true
cd ~/doc
regedit /s no-shift-space-toggle.reg
regedit /s fix-windows-share-alias.reg
regedit /s no-control-period.reg
regedit /s keymap-win.reg

mkdir /c/etc/ywb/ -p
(cd ~/etc/ywb/exclude >/dev/null 2>&1; ls -1) > /c/etc/ywb/disable.rc
rm -f ~/user ~/.mozilla ~/Desktop ~/.localw

ln -s "$(cygpath -u "$USERPROFILE")" ~/user
ln -s ~/user/AppData/Local ~/.localw
ln -s "$(cygpath -D)" ~/Desktop
ln -s ~/user/Application\ Data/Mozilla  ~/.mozilla
at 15:00 /every:monday,tuesday,wednesday,thursday,friday  "$(wlp $(which 15-00.bat))"
for x in /c/Python2?; do
    ln -s $x /c/Python2
    break
done

for x in /c/Python3?; do
    ln -s $x /c/Python3
    break
done

echo -n "$(cygpath -alm c:/Python3/python.exe)" \"$(cygpath -aml ~/system-config/gcode/scim-cs/ime-py/ime-server.py)\" > /cygdrive/c/ime-server.rc
echo "After check out success!"
