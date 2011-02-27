#!/bin/bash
cp /c/windows/hh 'q:\bin\windows\redirect_vc6\dexplore.exe'
/c/windows/apppatch/apploc 'q:\bin\windows\redirect_vc6\dexplore.exe' "`cygpath -alw \"$1\"`" /L0409

