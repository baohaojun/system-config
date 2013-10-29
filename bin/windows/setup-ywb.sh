#!/bin/bash

set -- ${1:-35}

if test "$(uname -m)" = x86_64; then
    cp ~/bin/windows/ywbhj-32.dll /c/Windows/SysWow64/ywbhj$1.dll
    cp ~/bin/windows/ywbhj-64.dll /c/Windows/System32/ywbhj$1.dll
else
    cp ~/bin/windows/ywbhj-32.dll /c/Windows/System32/ywbhj$1.dll
fi

setime.pl $1 > ~/1.reg
cd
regedit /s 1.reg
