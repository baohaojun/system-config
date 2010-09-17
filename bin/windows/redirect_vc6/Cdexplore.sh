#!/bin/bash
cp /c/windows/hh 'q:\windows-config\bin\windows\redirect_vc6\dexplore.exe'
/c/windows/apppatch/apploc 'q:\windows-config\bin\windows\redirect_vc6\dexplore.exe' "`cygpath -alw \"$1\"`" /L0409

