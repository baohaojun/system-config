#!/bin/bash

#~/bin/windows/NetworkTest.exe& #this is ssh2Emacs
cd ~/doc
regedit /s ime-noctrlshift-noaltshift.reg


# MYXWINSH=~/bin/windows/lnks/myxwin.sh
# (echo "#!/bin/bash -l"; grep -v xterm /usr/bin/startxwin.sh|sed -ne '/^#.*/d; /^exit$/d; /./p'|grep -v checkX) > $MYXWINSH
# chmod +x $MYXWINSH
# myxwin.sh&
net start sshd&
cygstart ~/doc/dvorak.ahk&
~/gcode/scim-cs/ime-py/ime-server.py&


