#!/bin/bash
src=$(/bin/readlink -f "$1")
if [[ -z "$src" ]]; then
    src=$(/bin/readlink "$1")
fi


function re_match()
{
    if echo "$1" | grep "$2" >/dev/null; then
        return 0;
    else
        return 1
    fi
}

function relink_func()
{
    if re_match "$src" "^/a/" || re_match "$src" "^/cygdrive/a/" || ! [[ -e "$src" ]]; then
        if re_match "$src" "^/./"; then
            for x in {c..e}; do
                new_src=/cygdrive/$x/"${src:3}"
                if [[ -e "$new_src" ]]; then
                    echo "$1" source found at "$new_src"
                    rm "$tgt"
                    ln -sf "$new_src" "$tgt"
                    return
                elif test -e "$(echo "$new_src" | perl -npe 's!/program files/!/program files (x86)/!i')"; then
                    new_src=$(echo "$new_src" | perl -npe 's!/program files/!/program files (x86)/!i')
                    echo "$1" source found at "$new_src"
                    rm "$tgt"
                    ln -sf "$new_src" "$tgt"
                    return
                fi
            done
            echo "$1" is not valid
        elif re_match "$src" "^/cygdrive/./"; then
            for x in {c..e}; do
                new_src=/cygdrive/$x/"${src:12}"
                if [[ -e "$new_src" ]]; then
                    echo "$1" source found at "$new_src"
                    rm "$tgt"
                    ln -sf "$new_src" "$tgt"
                    return
                fi
            done
            echo "$1" is not valid
        fi
    else
        echo "$1" source found at "$src"
        rm "$tgt"
        ln -sf "$src" "$tgt"
    fi
}

INPLACE=${INPLACE:-false}
if test $# -eq 0; then
    INPLACE=false
    find ~/bin/windows/ -path "*/lnks" -prune -o -type l -exec relink.sh '{}' \;
    exit
elif test $# -ne 1; then
    echo Error: can take at most 1 argument
elif test $INPLACE = false; then
    tgt=~/bin/windows/ext/"$(basename "$1")"
else
    tgt="$1"
fi

relink_func "$1"
