#!/bin/bash

#put the file name into clipboard. Luckily, we have /dev/clipbaord for cygwin!

cygpath -au "$1" >/dev/clipboard


/c/Program\ Files/GIMP-2.0/bin/gimp-2.6.exe "$1"&
findexec -p gimp-2.6
