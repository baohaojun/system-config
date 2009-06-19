#!/bin/bash -x

findexec.exe -F 1 -c emacs-emacs

argv=( "$@" )
for ((x=0; x<${#argv[@]}; x++)); do 
    char0=${argv[$x]:0:1}
    if test $char0 != - -a $char0 != +; then
        argv[$x]="`cygpath -au \"${argv[$x]}\"`"
    fi
done

echo "${argv[@]}" >> ~/emacsedit.log
/bin/emacsclient "${argv[@]}"
