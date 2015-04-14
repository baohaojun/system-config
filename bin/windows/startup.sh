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

cd -P "$BIN_WINDOWS"/../../..
export HOME2=`pwd` #Ah! and we know where we are now!
rm /qq -f
ln -sf "$HOME2" /qq

#modify /etc/passwd so that this is truly our new home.
if test -z $USER; then
export USER=`whoami`
fi

export OLDHOME=/qhome/$USER;
mkdir /qhome -p
touch "$OLDHOME"
/bin/perl -F: -nae 'if ($ENV{USER} eq $F[0])  {$F[5] = $ENV{OLDHOME}}; print join(q(:), @F)' -i /etc/passwd
if test "$(stat -c %i $OLDHOME/)" != "$(stat -c %i "$HOME2"/)"; then
    rm -f "$OLDHOME"
    ln -sf "$HOME2" "$OLDHOME"
fi
unset OLDHOME

function die() {
    echo "$@" 1>&2
    false
}

if test -e /q -a ! -L /q; then
    die "Error, the /q exist and is not a symlink";
fi

rm -f /q
ln -s "$HOME2" /q
export HOME=/q


#so that C-SPC will not toggle IME, because it's used by emacs set-mark-command
cd ~/doc
regedit /s ime-noctrlshift-noaltshift.reg
regedit /s console-color.reg

#setup environment?
. ~/system-config/.bashrc-windows

echo path "$(IFS=$'\n'; for x in  $(echo $PATH|tr ':' '\n'); do test -d "$x" && cygpath -asm "$x"; done|tr '\n' ';')" > /c/.bashloc.bat


#so that shellHelper_vc6.exe can find where bash is
cygpath -alwm `which bash` > /cygdrive/c/.bash-loc

#start everything in ~/bin/windows/startup/*/
~/bin/windows/substartup.sh ALL

#make sure the next time login will run this script again
cd "$(cygpath -au "$HOMEDRIVE$HOMEPATH")"
startup_dir=`regtool.exe -s get '\HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders\Startup'`
startup_dir=`cygpath -au "$startup_dir"`
ln -sf "$HOME2"/bin/windows/startup.sh "$startup_dir"

set -x
if test $(uname) != CYGWIN_NT-5.1; then
    cat >/c/ywb.txt <<EOF
[RegionalSettings]
InputLocale = 0804:e0350804
EOF

    cat >/c/ywb.bat <<"EOF"
control intl.cpl,,/f:"C:\ywb.txt"
EOF

    cd /c
    cmd.exe /c ywb.bat
fi
