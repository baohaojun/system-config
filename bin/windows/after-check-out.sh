#!/bin/bash
set -e

THIS=$(cygpath -au "$0")
THIS=$(readlink -f "$THIS")
echo "we are executing $THIS"
BIN_WINDOWS=$(dirname "$THIS")

# strlen("windows-config/bin/windows") is 26, and -26 means the last
# 26 chars, but we can't simply write -26, because that's another
# expansion syntax. We must write 0-26, and it's arithmetic, which is
# allowed.

if test "${BIN_WINDOWS:0-26}" != "windows-config/bin/windows"
then
    echo 'startup.sh is not in ~/windows-config/bin/windows!'
    #pause and exit, it's all blowed up!
    cat
    exit
fi

cd -P "$BIN_WINDOWS"/../../..
export HOME2=`pwd`

function die() {
    echo "$@" 1>&2
    false
}

if test -e /q -a ! -L /q; then
    die "Error, the /q exist and is not a symlink";
fi

rm -f /q; 
ln -s "$HOME2" /q
export HOME=/q

~/windows-config/bin/after-co-ln-s.sh
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

~/bin/windows/redirect.sh

cd ~/bin/windows
cpan String::ShellQuote
cpan String::Approx

ln -sf ~/'Application Data/Microsoft/Internet Explorer/Quick Launch' ~/SendTo/ || report_error "Error: you are not doing it from $HOMEPATH"
mkdir -p ~/.fonts
cp ~/doc/monaco-linux.ttf /cygdrive/c/windows/fonts/simsun.ttc /cygdrive/c/windows/fonts/cour.ttf ~/.fonts || true
fc-cache || true
cd ~/doc
regedit /s no-shift-space-toggle.reg
regedit /s keymap-win.reg

echo -n "c:/python31/python.exe" \"$(cygpath -aml ~/windows-config/gcode/scim-cs/ime-py/ime-server.py)\" > /cygdrive/c/ime-server.rc
echo "After check out success!"
