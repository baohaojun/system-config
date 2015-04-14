#!/bin/bash 
. ~/system-config/.bashrc-windows
set -e
(echo -n \>; for x in "$@"; do 
    echo -n \`"$x"\'\ 
done; echo \:) >> ~/shellHelper_vc6.log

script=`echo -n "$1"|perl -npe 's!\\\\!/!g; s!.*/!!; s!(\.exe$|$)|$!.sh!'`
shift
set -- "$script" "$@"
"$@"
