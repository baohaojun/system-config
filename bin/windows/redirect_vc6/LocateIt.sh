#!/bin/bash

echo "$@" >~/runhere.log

FILE=$1
if echo "${FILE##*.}"|grep -i '^lnk$' -q; then #this is a .lnk file
    FILE=`readshortcut "$FILE"`
fi
FILE=`cygpath -alw "$FILE"`
echo "FILE is $FILE"
cygstart `which explorer.exe` /n,/select,\""$FILE"\"
