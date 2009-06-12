#!/bin/bash 
. ~/.bashrc-windows
script=`echo -n "$1"|/bin/perl -npe 's!.*/!!; s!(\.exe$|$)|$!.sh!'`
shift
set -- "$script" "$@"
"$@"
