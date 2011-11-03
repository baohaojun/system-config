#!/bin/bash

#~/bin/windows/NetworkTest.exe& #this is ssh2Emacs
cd ~/doc
regedit /s ime-noctrlshift-noaltshift.reg


# MYXWINSH=~/bin/windows/lnks/myxwin.sh
# (echo "#!/bin/bash -l"; grep -v xterm /usr/bin/startxwin.sh|sed -ne '/^#.*/d; /^exit$/d; /./p'|grep -v checkX) > $MYXWINSH
# chmod +x $MYXWINSH
# myxwin.sh&
net start sshd&

cp ~/doc/dvorak.ahk ~/dvorak.ahk
cat >> ~/dvorak.ahk <<EOF
AppsKey::LWin
Control::Alt
Alt::Control
EOF

if test -f ~/.dvorak.ahk; then
	true
else
    cygstart ~/doc/dvorak.ahk
fi&

~/gcode/scim-cs/ime-py/ime-server.py&


