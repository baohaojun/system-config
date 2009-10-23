#!/bin/bash
mkdir -p ~/images/
mv "$1" ~/images/
cygpath -au ~/images/"$(basename "$1")"|tr -d '\n'|putclip
cygstart "$(getclip)"
echo -n ../images/"$(basename "$1")"|putclip
findexec -p gimp-2.6
