#!/bin/bash
File=$(cygpath -au "${1:-.}")
if test ${File:0:2} = //; then
    cygpath -au "$File"|tr -d '\r\n'|putclip
else 
cygpath -au "$(readlink -f "$File")"|tr -d '\r\n'|xargs -0 cygpath -au|tr -d '\r\n'|putclip
fi
getclip
