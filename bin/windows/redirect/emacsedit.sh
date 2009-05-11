#!/bin/bash 
FILE="$1"

(echo -n emacsedit4.sh\:\ ;  for x in "$@"; do 
    echo -n \""$x"\"\ 
done; echo) >> ~/emacsedit.log

#if the first char of $FILE is /, then, it should be // --- it is a file on a remote shared folder. Local files all begin with d: or c:

if findexec.exe -F 1 -p emacs.exe
    then efindfile.sh "$FILE" 
    else ~/bin/emacs.sh "$FILE"
fi
