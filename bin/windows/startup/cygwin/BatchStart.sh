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

rm ~/.no-loop

function loop-start-in-dir() {
    local dir=$1
    shift
    cd "$dir"
    while true; do
        sleep 2;
        test -e ~/.no-loop && continue;
        "$@"
    done&
}

if test ! -e ~/.config/system-config/about_me/links-done; then
    touch ~/.config/system-config/about_me/links-done
    fix-links.sh&
fi

(
    cd ~/doc
    regedit /s no-control-period.reg
    regedit /s no-shift-space-toggle.reg
)&

bash emacs-nt&
bcdedit.exe /set "{current}" nx AlwaysOff&
wmic OS Get DataExecutionPrevention_SupportPolicy&
loop-start-in-dir ~/doc ahk sawfish.ahk
close-window '\\osk.exe'
~/bin/windows/redirect.sh&
