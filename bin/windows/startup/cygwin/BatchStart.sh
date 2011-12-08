#!/bin/bash

#~/bin/windows/NetworkTest.exe& #this is ssh2Emacs
cd ~/doc
regedit /s ime-noctrlshift-noaltshift.reg


# MYXWINSH=~/bin/windows/lnks/myxwin.sh
# (echo "#!/bin/bash -l"; grep -v xterm /usr/bin/startxwin.sh|sed -ne '/^#.*/d; /^exit$/d; /./p'|grep -v checkX) > $MYXWINSH
# chmod +x $MYXWINSH
# myxwin.sh&
net start sshd&
net start cron&
/c/Python31/python "$(cygpath -alw ~/gcode/scim-cs/ime-py/ime-server.py)"&
while true; do
    sleep 2;
    test -e ~/.no-loop && continue;
    if ps.pl ssh|grep bhj@216 -q; 
    then
        true;
    else
        ssh -C2qN -D 8080 bhj@216.194.70.6; 
    fi
done&

rm ~/.no-loop

function loop-start() {
    local dir=$1
    shift
    while true; do 
	sleep 2;
	test -e ~/.no-loop && continue;
	cd "$dir"
	"$@"
    done&
}

bash emacs-nt&
loop-start ~/bin/windows/ command cmd /c hotkey_hook
loop-start ~/bin/windows/Imap4Monitor /c/Python25/python.exe Imap4Monitor.py
loop-start ~/bin/windows/notification-daemon/ /c/Python25/python.exe notification-daemon.py
close-window '\\osk.exe'

