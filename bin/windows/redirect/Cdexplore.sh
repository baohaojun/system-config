#!/bin/bash

cp /c/windows/hh 'q:\bin\windows\redirect\dexplore.exe'
/c/windows/apppatch/apploc 'q:\bin\windows\redirect\dexplore.exe' "$@" /L0409
