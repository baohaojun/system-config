#!/bin/bash

#remember where old HOME is
cd "$HOMEDRIVE/$HOMEPATH"
HOME2="`pwd`"

#so that I can write /q/ anywhere I want to write "$HOME" in .sh; it'll get worse in .emacs!
subst q: "$HOMEDRIVE"\\"$HOMEPATH"
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
rm *.stackdump
for x in *; do 
    ./"$x"&
done

#make sure the next time login will run this script again
ln -sf "$HOME2"/bin/windows/startup.sh ~/start\ menu/programs/startup
