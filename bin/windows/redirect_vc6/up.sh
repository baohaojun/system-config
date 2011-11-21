#!/bin/bash
File=$(cygpath -au "${1:-.}")
if test ${File:0:2} = //; then
    printf %q "$(cygpath -au "$File"|tr -d '\r\n')"|putclip
else 
    printf %q "$(cygpath -au "$(readlink -f "$File")"|tr -d '\r\n'|xargs -d '\n' cygpath -au|tr -d '\r\n')"|putclip
fi
getclip
