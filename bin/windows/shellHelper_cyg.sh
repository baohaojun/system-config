#!/bin/bash 
. ~/system-config/.bashrc-windows
set -e
script=`echo -n "$1"|/bin/perl -npe 's!.*/!!; s!(\.exe$|$)|$!.sh!'`
shift
set -- "$script" "$@"
"$@"
