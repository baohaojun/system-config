#!/bin/bash
File=$(cygpath -au "${1:-$(getclip)}")
if test -L "$File"; then
    File=$(readlink "$File")
fi
if test ${File:0:2} = //; then
    cygpath -alw "$File"|tr -d '\r\n'|putclip
else 
cygpath -au "$(readlink -m "$File")"|tr -d '\r\n'|xargs -0 cygpath -alw|tr -d '\r\n'|putclip
fi
getclip

