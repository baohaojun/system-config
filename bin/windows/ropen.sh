#!/bin/bash
#locate "$@" |perl -npe 's!^!d:!'

export TMPDIR=~/"Local Settings/Temp"
tmpfile="`mktemp -d`"
cd "$tmpfile"
echo "$1" |perl -npe 's"^/.*?:"pscp -P ";s"^pscp -P (.*?)#(.*?):(.*/)(.*)"pscp -P \2 \1:\3\4 \4; cygstart ."'|tr -d '\r'|sh 
#locate "$@" |perl -npe 's!^(.*)$!cygpath.exe -alw "$1"!'|sh|perl -npe 's!\\!/!g; s!^!c:/!'
