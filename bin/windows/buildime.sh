#!/bin/bash
set -e 
cd /q/gcode/scim-cs/scim
seq=$1
wpy2 "`cygpath -aml /q/bin/windows/terminateModule.py`" ywbhj"$seq" || true
setime.pl $seq > 1.reg
regedit /s 1.reg
rm -f 1.reg

shift
test "$#" = 0 && vccompile '' '' || "$@" 
cp ../Release/scim.dll /c/windows/system32/ywbhj$seq.dll -v


