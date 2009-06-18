#!/bin/bash
run fetchmail&

cd ~/gcode/MboxTray/
cygstart --hide /cygdrive/c/python25/python mail.py
cygstart --hide ~/bin/windows/NetworkTest.exe&
cd ~/doc
regedit /s ime-noctrlshift-noaltshift.reg
net start 'vmware host agent'
helpEmacs.py&
while true; do 
    ssh bhj2 offlineimap
    echo wait for 300 seconds
    sleep 300
done&
grep -v xterm /usr/bin/startxwin.sh > ~/bin/windows/lnks/myxwin.sh
echo xhost + >> ~/bin/windows/lnks/myxwin.sh
chmod +x myxwin.sh
myxwin.sh&
