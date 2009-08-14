#!/bin/bash
set -e

THIS=$(cygpath -au "$0")
THIS=$(readlink -f "$THIS")
echo "we are executing $THIS"
BIN_WINDOWS=$(dirname "$THIS")

#strlen("/bin/windows") is 12
if test "${BIN_WINDOWS:0-12}" != "/bin/windows"
then
    echo 'startup.sh is not in ~/bin/windows!'
    #pause and exit, it's all blowed up!
    cat
    exit
fi

cd -P "$BIN_WINDOWS"/../..
export HOME2=`pwd`

if test "$HOME2" != /q -a "$HOME2" != /cygdrive/q; then 
    subst /d q: || true #delete it first. FIXME, what if q: is a real drive?
    subst q: $(cygpath -sma "$HOME2") #if it is already /q, we will have problem here, so we put this in a conditional
fi
export HOME=/cygdrive/q

. ~/.bashrc-windows
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
DOWN=yes ./download-external.sh
find . -type l -exec relink.sh '{}' \;

~/bin/windows/redirect.sh

cd ~/bin/windows
./update-password.sh
echo 'after check out update success!'
ln -sf ~/'Application Data/Microsoft/Internet Explorer/Quick Launch' ~/SendTo/ || report_error "Error: you are not doing it from $HOMEPATH"
mkdir -p ~/.fonts
cp ~/doc/monaco-linux.ttf /cygdrive/c/windows/fonts/simsun.ttc /cygdrive/c/windows/fonts/cour.ttf ~/.fonts || true
fc-cache
for x in {c..z}; do test -e /$x || (rm -f /$x; ln -s /cygdrive/$x /); done

echo "After check out success!"
