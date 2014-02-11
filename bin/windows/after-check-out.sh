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
    read -p 'startup.sh is not in ~/system-config/bin/windows! Press any key to exit...'
    #pause and exit, it's all blowed up!
    exit
fi

cd -P "$BIN_WINDOWS"/../../..
export HOME2=`pwd`

function die() {
    echo "$@" 1>&2
    read -p 'Press any key to exit...'
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

wget -N  http://cygwin.com/setup-x86_64.exe
chmod +x setup-x86_64.exe

pkgs=(nc util-linux git vim rsync inetutils apache2 shutdown make screen cygutils-extra procps)
/cygdrive/c/setup-x86_64.exe -q -n -d -A -P "${pkgs[@]}" || true

if yes-or-no-p "Install the packages one by one?"; then
    for x in "${pkgs[@]}"; do
        /cygdrive/c/setup-x86_64.exe -q -n -d -A -P $x
    done
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

echo -n "c:/python31/python.exe" \"$(cygpath -aml ~/system-config/gcode/scim-cs/ime-py/ime-server.py)\" > /cygdrive/c/ime-server.rc
echo "After check out success!"
