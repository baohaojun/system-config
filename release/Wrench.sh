#!/bin/bash


## start code-generator "^\\s *#\\s *"
# generate-getopt ttest kkill 1one-phone
## end code-generator
## start generated code
TEMP=$(getopt -o k1th \
              --long kill,one-phone,test,help,no-kill,no-one-phone,no-test \
              -n $(basename -- $0) -- "$@")
kill=false
one_phone=false
test=false
eval set -- "$TEMP"
while true; do
    case "$1" in

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
            echo
            echo
            echo Options and arguments:
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


if test -e ~/system-config/src/github/smartcm/etc/Wrench.config; then
    . ~/system-config/src/github/smartcm/etc/Wrench.config
fi

export EMACS=t

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


export RUNNING_WRENCH=true

if ! [[ $LANG =~ en_US ]]; then
    exec en_US Wrench.sh "$@"
fi

# adb forward --remove tcp:28888

find-or-exec 'Wrench V%Wrench'

if test "$#" != 0; then
    if test $# = 1 -a -e "$1" && [[ $1 =~ \.(twa|lua)$ ]]; then
        exec Wrench "$(readlink -f "$1")"
    fi
fi

if test "$#" = 1 -a ! -e "$1"; then
    what_to_do=$1
elif sawfish-window-exists Wrench; then
    what_to_do=$(
        ask-for-input --history -a "Wrench" -p "你要小扳手的什么功能？（可以自己输入比如 baohaojun@@wx）"
              )
fi

if test "$what_to_do" = $'\003'; then
    exit
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
    rm ~/.cache/system-config/wrench-$$.twa
else
    exec Wrench
fi
