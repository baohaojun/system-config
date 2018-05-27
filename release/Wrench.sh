#!/bin/bash

memory=$(free | grep ^Mem: | pn 2)
ulimit -v $((memory / 2))

## start code-generator "^\\s *#\\s *"
# generate-getopt ttest kkill 1one-phone ddo-debug xexclusive
## end code-generator
## start generated code
TEMP=$( getopt -o dxk1th \
               --long do-debug,exclusive,kill,one-phone,test,help,no-do-debug,no-exclusive,no-kill,no-one-phone,no-test \
               -n $(basename -- $0) -- "$@")
do_debug=false
exclusive=false
kill=false
one_phone=false
test=false
eval set -- "$TEMP"
while true; do
    case "$1" in

        -d|--do-debug|--no-do-debug)
            if test "$1" = --no-do-debug; then
                do_debug=false
            else
                do_debug=true
            fi
            shift
            ;;
        -x|--exclusive|--no-exclusive)
            if test "$1" = --no-exclusive; then
                exclusive=false
            else
                exclusive=true
            fi
            shift
            ;;
        -k|--kill|--no-kill)
            if test "$1" = --no-kill; then
                kill=false
            else
                kill=true
            fi
            shift
            ;;
        -1|--one-phone|--no-one-phone)
            if test "$1" = --no-one-phone; then
                one_phone=false
            else
                one_phone=true
            fi
            shift
            ;;
        -t|--test|--no-test)
            if test "$1" = --no-test; then
                test=false
            else
                test=true
            fi
            shift
            ;;
        -h|--help)
            set +x
            echo -e
            echo
            echo Options and arguments:
            printf %06s '-d, '
            printf %-24s '--[no-]do-debug'
            echo
            printf %06s '-x, '
            printf %-24s '--[no-]exclusive'
            echo
            printf %06s '-k, '
            printf %-24s '--[no-]kill'
            echo
            printf %06s '-1, '
            printf %-24s '--[no-]one-phone'
            echo
            printf %06s '-t, '
            printf %-24s '--[no-]test'
            echo
            exit
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error"
            ;;
    esac
done


## end generated code

if test "$(lsb_release -cs)" = trusty -a -e ~/src/github/smartcm/etc/Wrench.config; then
    . ~/src/github/smartcm/etc/Wrench.config
fi

export EMACS=t

if test "$exclusive" = true; then
    if test -z "$ANDROID_SERIAL"; then
        export ANDROID_SERIAL=$(select-output-line -p "Select the adb device" my-adb devices?|pn 1)
    fi
    if test ! -e ~/.config/system-config/Wrench-adb.map; then
        echo -e "declare -A wrench_adb_map\nwrench_adb_map[max]=1" > ~/.config/system-config/Wrench-adb.map
    fi
    . ~/.config/system-config/Wrench-adb.map
    if test -z "${wrench_adb_map[$ANDROID_SERIAL]}"; then
        wrench_adb_map[$ANDROID_SERIAL]=${wrench_adb_map[max]}
        ((wrench_adb_map[max]++)) || true
        cat <<EOF > ~/.config/system-config/Wrench-adb.map.$$
declare -A wrench_adb_map
$(
    for x in ${!wrench_adb_map[@]}; do
        echo wrench_adb_map[$x]=${wrench_adb_map[$x]}
    done
)
EOF
        mv ~/.config/system-config/Wrench-adb.map.$$ ~/.config/system-config/Wrench-adb.map
    fi
    rsync ~/tmp/build-wrench/ ~/tmp/build-wrench.$ANDROID_SERIAL -a --chmod=D0755
    reset-env WRENCH_INSTANCE ${wrench_adb_map[$ANDROID_SERIAL]} PATH ${HOME}/system-config/bin/Linux:${HOME}/tmp/build-wrench.$ANDROID_SERIAL:"$PATH"
    . reget-env -k
    nohup Wrench.sh&
    exit
fi

if test "$kill" = true; then
    kill-env RUNNING_WRENCH true
    exit
fi

if test "$test" = true; then
    unset ANDROID_SERIAL
elif test "$ANDROID_SERIAL" -a "$one_phone" = true; then
    true
else
    export ANDROID_SERIAL=$(select-output-line -p "Select the adb device" my-adb devices?|pn 1)
fi

export LD_LIBRARY_PATH=/usr/local/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH


if test "${WRENCH_INSTANCE:-0}" != 0; then
    export RUNNING_WRENCH=true
fi

if test "$do_debug" = true; then
    cd ~/
    exec gdb --args Wrench
fi

if ! [[ $LANG =~ en_US ]]; then
    exec en_US Wrench.sh "$@"
fi

# adb forward --remove tcp:28888

if test "$#" != 0; then
    if test $# = 1 -a -e "$1" && [[ $1 =~ \.(twa|lua)$ ]]; then
        exec Wrench "$(readlink -f "$1")"
    fi
fi

if test "$#" = 1 -a ! -e "$1"; then
    what_to_do=$1
fi

if test "$what_to_do" -a "$what_to_do" != Wrench; then
    if [[ $what_to_do =~ \( ]]; then
        format=%s
    else
        format='wrench_call([==[%s]==])'
    fi
    cat <<EOF |tee ~/.cache/system-config/wrench-$$.twa
-- -*- mode: lua -*-
-- {%lua%}
$(printf "$format" "$what_to_do")
-- {%/lua%}
EOF
    Wrench ~/.cache/system-config/wrench-$$.twa
    mv ~/.cache/system-config/wrench-$$.twa ~/tmp/wrench.twa
else
    exec Wrench
fi
