#!/bin/bash
#locate "$@" |perl -npe 's!^!d:!'
. /c/ssh-agent.log
findexec.exe -p emacs -F 1

while true; do 
    case "$1" in
        -w)
            shift
            TMPD=`mktemp -d`
            cd "$TMPD"
            LFILE="`echo \"$1\"|sed -e 's!.*/!!'`"
            RFILE="`echo \"$1\"|sed -e 's!/pscp:!!'`" 
            scp "$RFILE" "$LFILE"
            gnuclient "$LFILE"
            scp "$LFILE" "$RFILE"
            cd 
            rm "$TMPD"
            shift
            ;;
        ?*)
            gnudoit '(find-file '\""$1"\"')'
            shift
            ;;
        *)
            break
            ;;
    esac
done
#locate "$@" |perl -npe 's!^(.*)$!cygpath.exe -alw "$1"!'|sh|perl -npe 's!\\!/!g; s!^!c:/!'
