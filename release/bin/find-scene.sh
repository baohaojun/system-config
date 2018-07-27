#!/bin/bash

function picture-matches() {
    if test ! -e "$1" -o ! -e "$2"; then
        die "Usage: picture-matches FILE1.PNG FILE2.PNG"
    fi

    set -- "$(readlink -f "$1")" "$(readlink -f "$2")"
    default_tolerance=25

    export g_onmyoji_location=Wrench
    export onmyoji_parallel=0

    look_same_postfix=${ANDROID_SERIAL}.${g_onmyoji_location}${onmyoji_parallel}
    # ^^^^ this is because look-same.server is written like this ^^^^
    local sock_name=look-same.${look_same_postfix}

    local node_try
    for node_try in $(seq 1 2); do
        if nodejs_ret=$(
                nc.openbsd -U /tmp/$sock_name <<<"$1 $2 ${default_tolerance}"
                     ); then
            if test "$nodejs_ret" = yes; then
                return 0;
            elif test "${nodejs_ret}" -a "${nodejs_ret}" != no; then
                echo "nodejs_ret: $nodejs_ret" 1>&2
            fi
            break
        else
            (
                cd ~/tmp/
                set -x
                flock -n 9 || exit
                rm /tmp/$sock_name -f
                export NODE_PATH=/usr/local/lib/node_modules
                nohup nodejs ~/system-config/bin/look-same.server || (bhj-notify Onmyoji "You need to make sure nodejs works") 9>/dev/null
            ) 9> ~/tmp/onmyoji-$sock_name.lock >> ~/tmp/onmyoji-$sock_name.log 2>&1 &
            if test "${node_try}" = 2; then
                return 1
            fi
            sleep 1
        fi
    done

    local x
    ONMYOJI_RESDIR=$(dirname "$1")
    for x in $(seq 1 100); do
        x_png=${ONMYOJI_RESDIR}/${1%.png}.$x.png
        if test ! -e "${x_png}"; then
            if test -L "$1"; then
                if picture-matches "$(readlink -f "$1")" "$2"; then
                    return 0
                else
                    return 1
                fi
            fi
            return 1
        elif picture-matches ${x_png} $2; then
            return 0
        fi
    done
    return 1
}

function find-scene() {

}

picture-matches "$@"
