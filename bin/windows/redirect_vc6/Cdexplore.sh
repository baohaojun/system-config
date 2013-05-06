#!/bin/bash
cp /c/windows/hh 'q:\system-config\bin\windows\redirect_vc6\dexplore.exe'
/c/windows/apppatch/apploc 'q:\system-config\bin\windows\redirect_vc6\dexplore.exe' "`cygpath -alw \"$1\"`" /L0409

