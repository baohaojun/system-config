#!/bin/bash 

argv=( "$@" )
for ((x=0; x<${#argv[@]}; x++)); do 
    char0=${argv[$x]:0:1}
    if test $char0 != - -a $char0 != +; then
        argv[$x]=$(cygpath -au "${argv[$x]}")
    fi
done

echo "${argv[@]}" >> ~/view-pic.sh
/bin/display "${argv[@]}"
