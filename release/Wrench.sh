#!/bin/bash

if ! [[ $LANG =~ en_US ]]; then
    exec en_US Wrench.sh "$@"
fi

memory=$(free | grep ^Mem: | pn 2)
ulimit -v $((memory / 2))

## start code-generator "^\\s *#\\s *"
# generate-getopt ttest \
    # kkill '?"干掉当前 adb device 对应的 Wrench 进程"' \
    # 1one-phone '?"此参数仅供内部使用"' \
    # ddo-debug '?"调试选项"' \
    # xexclusive '?"只给当前这一个 adb device 用，不要连接其他 adb device"' \
    # ssystem '?"设置 Sawfish 桌面系统和 Emacs 的 PATH 环境变量，全局使用这个 Wrench"' \
    # @silent '?"静默模式，不要显示窗口"'
## end code-generator
## start generated code
TEMP=$( getopt -o dxk1sth \
               --long do-debug,exclusive,kill,one-phone,silent,system,test,help,no-do-debug,no-exclusive,no-kill,no-one-phone,no-silent,no-system,no-test \
               -n $(basename -- $0) -- "$@")
declare do_debug=false
declare exclusive=false
declare kill=false
declare one_phone=false
declare silent=false
declare system=false
declare test=false
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
        --silent|--no-silent)
            if test "$1" = --no-silent; then
                silent=false
            else
                silent=true
            fi
            shift

            ;;
        -s|--system|--no-system)
            if test "$1" = --no-system; then
                system=false
            else
                system=true
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
            echo "调试选项"
            printf %06s '-x, '
            printf %-24s '--[no-]exclusive'
            echo "只给当前这一个 adb device 用，不要连接其他 adb device"
            printf %06s '-k, '
            printf %-24s '--[no-]kill'
            echo "干掉当前 adb device 对应的 Wrench 进程"
            printf %06s '-1, '
            printf %-24s '--[no-]one-phone'
            echo "此参数仅供内部使用"
            printf "%06s" " "
            printf %-24s '--[no-]silent'
            echo "静默模式，不要显示窗口"
            printf %06s '-s, '
            printf %-24s '--[no-]system'
            echo "设置 Sawfish 桌面系统和 Emacs 的 PATH 环境变量，全局使用这个 Wrench"
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
            die "internal error: $(. bt; echo; bt | indent-stdin)"
            ;;
    esac
done


## end generated code

if test "${silent}" = true; then
    set -- 'adb_quick_input{"input keyevent UNKNOWN"}'
fi

export EMACS=t

if test "$exclusive" = true; then
    if test -z "$ANDROID_SERIAL"; then
        export ANDROID_SERIAL=$(select-output-line -p "Select the adb device" my-adb devices?|pn 1)
    fi

    postfix=${ANDROID_SERIAL}${DISPLAY//:/.}
    if test ! -e ~/.config/system-config/Wrench-adb.map; then
        echo -e "declare -A wrench_adb_map\nwrench_adb_map[max]=1" > ~/.config/system-config/Wrench-adb.map
    fi
    . ~/.config/system-config/Wrench-adb.map
    if test -z "${wrench_adb_map[${postfix}]}"; then
        wrench_adb_map[${postfix}]=${wrench_adb_map[max]}
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
    rsync ~/tmp/build-wrench/ ~/tmp/build-wrench.$postfix -a --chmod=D0755
    path_args=(
        PATH ${HOME}/system-config/bin/Linux:${HOME}/tmp/build-wrench.$postfix:"$PATH"
    )
    if test "$(which Wrench)" = ~/tmp/build-wrench.$postfix/Wrench; then
        path_args=()
    fi
    . reset-env WRENCH_INSTANCE ${wrench_adb_map[$postfix]} "${path_args[@]}"

    if test "$system" = true; then
        (
            for x in $(ps.pl Wrench|pn 1); do
                if test "$(readlink -f /proc/$x/exe)" = ~/tmp/build-wrench/Wrench; then
                    kill $x
                fi
            done
        ) || true
        for client in emacsclient sawfish-client; do
            ${client} -e '
; {%sawfish-mode%}
(progn
  (setenv "WRENCH_INSTANCE" "'$WRENCH_INSTANCE'")
  (setenv "ANDROID_SERIAL" "'$ANDROID_SERIAL'")
  (setenv "PATH" "'"$PATH"'"))
; {%/sawfish-mode%}
'
        done
    fi
    set-about-me adb-serial $ANDROID_SERIAL || true
    nohup Wrench.sh -1 "$@"&
    exit
fi

if test "$kill" = true; then
    ps-killall -e ANDROID_SERIAL=${ANDROID_SERIAL} Wrench
    exit
fi

if test "$test" = true; then
    unset ANDROID_SERIAL
elif test "$ANDROID_SERIAL" && test "$one_phone" = true -o "$WRENCH_INSTANCE"; then
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
    if [[ $what_to_do =~ [\(\{] ]]; then
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
else
    exec Wrench
fi
