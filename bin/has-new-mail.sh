#!/bin/bash

# exit 0: has new mail
# exit 1: no new mail
# exit 2: don't know if we has mail, because offlineimap is running

if test "$DEBUG" = true; then
    set -x
fi

cd ~/Maildir || exit 1

function not_intesting_now() {
    if (cd "$1" && lookup-file -e .ignore-this); then
        return 0
    fi
    if ((hour >= 8 && hour <= 18)); then
        [[ "$1" =~ - ]] #the only not interesting mail box is orgmode-inbox (which has a `-' in it)
        return $?
    else
        return 1 #everything is interesting out of working hour
    fi
}

# this function will always exit
function got-mail() {
    echo $1 > $result
    if test $1 = true; then
        exit 0
    else
        exit 1
    fi
}

(

    result=~/.cache/system-config/logs/mail-check-result
    need_recheck=false


    if ! test -e $result; then
        need_recheck=true
    elif is-tty-io; then
        echo force recheck because we are on tty
        need_recheck=true
    else
        hour=$(date +%_H)
        for x in */new */cur */.nnmaildir/marks/read; do
            if not_intesting_now "$x"; then
                continue
            fi
            if test $x -nt $result; then
                echo "$x is newer than $result"

                if ! flock -n 9; then
                    echo "Can not lock the offlineimap lock"
                else
                    sync_nnmaildir -g
                fi
                need_recheck=true;
                break
            fi
        done
    fi

    if test $need_recheck = false; then
        if test "$(cat $result)" = true; then
            exit 0;
        else
            exit 1;
        fi
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
        echo checking $x
        ls $x|perl -npe 's/.*'"$maildir_sep"'//'|grep -v S &&
        {
            echo has got mail in $x
            got-mail true
        }
    done
    got-mail false
) 9> ~/.offlineimap/lock
