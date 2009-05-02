#!/bin/bash 
script=`echo -n "$1"|perl -npe 's!.*/!!; s!(\.exe$|$)|$!.sh!'`
shift
set -- "$script" "$@"
"$@"
