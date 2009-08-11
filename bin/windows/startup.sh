#!/bin/bash

echo "\$0 is $0"
#make sure PATH is sane
export PATH=/bin:$PATH

#find out about where we are
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
export HOME2=`pwd` #Ah! and we know where we are now!
rm /qq -f
ln -sf "$HOME2" /qq

#modify /etc/passwd so that this is truly our new home.
/bin/perl -F: -nae 'if ($ENV{USER} eq $F[0]) {$F[5] = $ENV{HOME2}}; print join(q(:), @F)' -i /etc/passwd


#so that I can write /q/ anywhere I want to write "$HOME" in .sh; it'll get worse in .emacs!
subst /d q: || true #delete it first FIXME, what if q: is a real drive?
subst q: $(cygpath -sma "$HOME2")
export HOME=/cygdrive/q


#so that C-SPC will not toggle IME, because it's used by emacs set-mark-command
cd ~/doc
regedit /s ImeNoToggle.reg

#setup environment?
. ~/.bashrc-windows

#rm temp files from tramp
cd ~/Local\ Settings/Temp && rm tramp* -rf

#so that shellHelper_vc6.exe can find where bash is
cygpath -alwm `which bash` > /cygdrive/c/.bash-loc

#this is useless now?
export CYGDIR=`cygpath -alwm /`

#start everything in ~/bin/windows/startup
cd ~/bin/windows/startup
rm *.stackdump -f
for x in *; do 
    ./"$x"&
done

#make sure the next time login will run this script again
cd "$HOMEDRIVE$HOMEPATH"
HOME_DRIVE_PATH=`pwd`
ln -sf "$HOME2"/bin/windows/startup.sh ./Start\ Menu/Programs/Startup/

