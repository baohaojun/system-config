#!/bin/bash

cygstart --hide ~/bin/windows/NetworkTest.exe& #this is ssh2Emacs
cd ~/doc
regedit /s ime-noctrlshift-noaltshift.reg
helpEmacs.py&
while true; do 
    ssh bhj2 offlineimap
    echo wait for 300 seconds
    sleep 300
done&
MYXWINSH=~/bin/windows/lnks/myxwin.sh
grep -v xterm /usr/bin/startxwin.sh|sed -ne '/^#.*/d; /^exit$/d; /./p' > $MYXWINSH
echo xhost + >> $MYXWINSH
chmod +x $MYXWINSH
myxwin.sh&
