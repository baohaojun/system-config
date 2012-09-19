#!/bin/bash

cd ~/Maildir || exit 1
result=~/.logs/mail-check-result
test -e $result || { touch $result; sleep 1; touch */cur; }
hour=$(date +%_H)

function not_intesting_now() {
    if ((hour >= 8 && hour <= 18)); then
	[[ "$1" =~ - ]] #the only not interesting mail box is orgmode-inbox (which has a `-' in it)
	return $?
    else
	return 1 #everything is interesting out of working hour
    fi
}    

need_recheck=false

for x in */new */cur */.nnmaildir/marks/read; do
    if not_intesting_now "$x"; then
	continue
    fi
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
    if not_intesting_now "$x"; then
	continue
    fi
    test $(ls $x|wc -l) == 0 || got-mail true
done

maildir_sep=:
if uname | grep -i -q cygwin; then
    maildir_sep=!
fi

for x in */cur; do
    if not_intesting_now "$x"; then
	continue
    fi
    ls $x|perl -npe 's/.*'"$maildir_sep"'//'|grep -v S && got-mail true sync
done

got-mail false
