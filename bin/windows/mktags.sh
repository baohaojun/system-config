#!/bin/bash

export PATH=/d/tools/emacswin/bin/:/bin:/usr/bin:"$PATH"

if [[ -z $1 ]]; 
    then (find . -type f -iname "*.[hc]pp"; find . -type f -iname "*.[sch]")|etags -;
    else (find . -type f -iname "$1")|etags -;
fi
