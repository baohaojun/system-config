#!/bin/bash 
. ~/.bashrc-windows
(echo -n \>; for x in "$@"; do 
    echo -n \`"$x"\'\ 
done; echo \:) >> ~/shellHelper_vc6.log

script=`echo -n "$1"|perl -npe 's!\\\\!/!g; s!.*/!!; s!(\.exe$|$)|$!.sh!'`
shift
set -- "$script" "$@"
"$@" >> ~/shellHelper_vc6.log 2>&1
