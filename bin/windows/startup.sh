#!/bin/bash

cd "$HOMEDRIVE/$HOMEPATH"

#cygrunsrv.exe -I startup -p /c/WINDOWS/system32/subst.exe -a 'q: "c:\documents and settings\bhj"'

HOME2="`pwd`"
#psexec -s subst q: 'c:\Documents and Settings\bhj'
subst q: "$HOMEDRIVE"\\"$HOMEPATH"
export HOME=/q

while ! cd ~/Local\ Settings/Temp; do 
    echo "cd failed! subst not run yet?"; sleep 1;
done

rm tramp* -rf
cd ~/doc
regedit /s ImeNoToggle.reg

cd ~/bin/startup

for x in *; do 
    cygstart "$x"
done

ln -sf "$HOME2"/bin/startup.sh ~/start\ menu/programs/startup
