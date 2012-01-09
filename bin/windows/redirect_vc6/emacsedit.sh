#!/bin/bash 

findexec.exe -F 1 -t emacs@&
findexec.exe -F 1 -p puemacs&

argv=( "$@" )
for ((x=0; x<${#argv[@]}; x++)); do 
    char0=${argv[$x]:0:1}
    if test $char0 != - -a $char0 != +; then
        argv[$x]=$(cygpath -au "${argv[$x]}")
    fi
done

echo -- "${argv[@]}" >> ~/emacsedit.log
emacsclient "${argv[@]}"
