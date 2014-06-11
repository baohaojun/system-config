#!/bin/bash

function my-select()
{
    local y=1
    if test $# = 1; then
        echo 1
        return
    fi

    if test -z "$ORIGINAL_MATCHER"; then
        for x in "$@"; do
            echo $y\) "$x" >/dev/tty;
            ((y++))
        done
    fi

    if test "$ORIGINAL_MATCHER"; then
        NUM=$ORIGINAL_MATCHER
        export ORIGINAL_MATCHER=
    else
        read -e -p "#?" NUM
    fi

    if [[ x"$NUM" = x ]] || ( [[ "$NUM" =~ ^[1-9][0-9]*$ ]] &&  (( "$NUM" > $# )) ); then
        NUM=1
    elif [[ ! "$NUM" =~ ^[1-9][0-9]*$ ]]; then
        matches=()
        reverse=0
        if [[ "$NUM" =~ ^! ]]; then
            reverse=1
            NUM=${NUM:1}
        fi
        for arg in "$@"; do
            match=true
            for entry in $NUM; do
                if (test $reverse = 0 && ! echo "$arg" | grep -P -q -i -e "$entry") || (test $reverse = 1 && echo "$arg" |grep -P -i -q -e "$entry");
                then
                    match=false
                    break
                fi
            done
            if test $match = true; then
                matches=("${matches[@]}" "$arg")
            fi
        done
        ans=$(command my-select "${matches[@]}")
        if [[ "$ans" =~ ^[1-9][0-9]*$ ]] && (( ans <= "${#matches[@]}" )); then
            ans=${matches[ans-1]}

            num=1
            for x in "$@"; do
                if test "$x" = "$ans"; then
                    echo $num
                    exit 0;
                fi
                ((num++))
            done
        fi



    fi
    echo $NUM
}

my-select "$@"
