#!/bin/bash

#~/bin/windows/NetworkTest.exe& #this is ssh2Emacs
cd ~/doc
regedit /s ime-noctrlshift-noaltshift.reg

net start sshd&
net start cron&
$(echo /c/Python3?/python | pn 1) "$(cygpath -alw ~/gcode/scim-cs/ime-py/ime-server.py)"&

(
    if test ! -e /c/Python3; then
        ln -sf "$(find /c/ -maxdepth 1 -type d -name 'Python3?*' | pn 1)" /c/Python3
    fi

    if test ! -e /c/Python2; then
        ln -sf "$(find /c/ -maxdepth 1 -type d -name 'Python2?*' | pn 1)" /c/Python2
    fi
)



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
bcdedit.exe /set "{current}" nx AlwaysOff&
wmic OS Get DataExecutionPrevention_SupportPolicy&
loop-start ~/bin/windows/ command cmd /c hotkey_hook
close-window '\\osk.exe'
~/bin/windows/redirect.sh&
