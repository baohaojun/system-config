#!/bin/bash

cd ~/Maildir;
result=$(pwd)/mail-check-result
test -e $result || { touch $result; sleep 1; touch */cur; }

need_recheck=false

for x in */new */cur */.nnmaildir/marks/read; do
    if test $x -nt $result; then
	need_recheck=true;
	break
    fi
done

function got-mail() {
    run_offlineimap=true
    if test "$1" != "$(cat $result)"; then
	echo $1 > $result
    else
	run_offlineimap=false
    fi
    if test $1 = true; then
	if test "$2" = sync; then
	    sync_nnmaildir -g
	    if test $run_offlineimap = true; then
		offlineimap
	    fi&
	    if test -z "$HAS_NEW_MAIL"; then
		export HAS_NEW_MAIL=true
		has-new-mail.sh
		exit $?
	    fi
	fi
	exit 0
    else
	exit 1
    fi
}

if test $need_recheck = false; then
    got-mail $(cat $result);
fi

for x in */new; do
    test $(ls $x|wc -l) == 0 || got-mail true
done

for x in */cur; do
    ls $x|perl -npe 's/.*!//'|grep -v S && got-mail true sync
done

got-mail false
