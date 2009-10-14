#!/bin/bash
cygpath -au "$1"|tr -d '\n'|putclip
cygstart "$(getclip)"
findexec -p gimp-2.6
