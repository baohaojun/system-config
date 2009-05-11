#!/bin/bash 
. ~/.bashrc-windows
script=`echo -n "$1"|perl -npe 's!\\\\!/!g; s!.*/!!; s!(\.exe$|$)|$!.sh!'`
shift
set -- "$script" "$@"
"$@"
