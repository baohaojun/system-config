#!/bin/bash 
FILE="`echo \"$1\"|sed -e 's!\\\\!/!g'`"
export PATH=/c/gnuserv/:"$PATH"

#if the first char of $FILE is /, then, it should be // --- it is a file on a remote shared folder. Local files all begin with d: or c:
if [[ ${FILE:0:1} == '/' ]]
then
    FILE=/"$FILE"
fi

if findexec.exe -F 1 -p emacs.exe
    then efindfile.sh "$FILE" 
    else ~/bin/emacs.bat "$FILE"
fi
