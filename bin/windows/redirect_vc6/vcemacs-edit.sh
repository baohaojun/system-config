#!/bin/bash 

#exec 2>>~/emacsedit.log
set -x
findexec.exe -F 1 -t emacs@&
findexec.exe -F 1 -p puemacs&

echo -- "$@" >> ~/emacsedit.log
set -- "$1" "$2" "$(cygpath -au $3)"

cat "$3" | tr -d '\r' > "$3".bak-r
if ! diff "$3".bak-r "$3"; then
    mv "$3".bak-r "$3"
else
    rm "$3".bak-r
fi

echo --- "$@" >> ~/emacsedit.log
emacsclient "$@"
