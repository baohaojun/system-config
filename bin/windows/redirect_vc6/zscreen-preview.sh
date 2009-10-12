#!/bin/bash

cygpath -au "$1"|tr -d '\n'|putclip
/c/Program\ Files/GIMP-2.0/bin/gimp-2.6.exe "$1"&
findexec -p gimp-2.6
