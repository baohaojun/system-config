#!/bin/bash
mkdir -p ~/images/
mv "$1" ~/images/
sleep 5 && cygpath -au ~/images/"$(basename "$1")"|tr -d '\n'|putclip&
cygstart ~/images/"$(basename "$1")"
echo -n ../images/"$(basename "$1")"|putclip
findexec -p gimp-2.6
