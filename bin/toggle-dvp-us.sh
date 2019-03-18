#!/bin/bash

set -x
exec > ~/tmp/am.log 2>&1
set -e
## start code-generator "^\\s *#\\s *"
# generate-getopt -P @udev
## end code-generator
## start generated code
TEMP=$(POSIXLY_CORRECT=true getopt -o h \
                      --long udev,help,no-udev \
                      -n $(basename -- $0) -- "$@")
declare udev=false
eval set -- "$TEMP"
while true; do
    case "$1" in

        --udev|--no-udev)
            if test "$1" = --no-udev; then
                udev=false
            else
                udev=true
            fi
            shift

            ;;
        -h|--help)
            set +x
            echo -e
            echo
            echo Options and arguments:
            printf "%06s" " "
            printf %-24s '--[no-]udev'
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

if test "$udev" = true; then
    at now <<<'myscr --bg bash -c "sleep 2; am dvp"'
    exit
fi

if [[ "$(tty)" =~ /dev/tty ]]; then
    if test "$(cat ~/.config/system-config/am-keyboard)" = en; then
        sudo setupcon dvp
        swap-control-alt
        echo dvp > ~/.config/system-config/am-keyboard
    else
        sudo setupcon en
        echo en > ~/.config/system-config/am-keyboard
    fi
    exit
fi

do-dvp() {
    (
        if test -e ~/.config/system-config/using-fcitx || ps.pl fcitx; then
            ps-killall fcitx
            touch ~/.config/system-config/using-fcitx
        fi
    ) >/dev/null 2>&1 || true
    echo am: dvp layout
    setxkbmap -layout us -variant dvp
    re-xmodmap 2>&1|tee
    do-natural-scrolling 9>/dev/null || true
}

(
    flock 9
    if test "$1" = am -o "$1" = dvp; then
        do-dvp
    elif test "$1" = ma || setxkbmap -query | grep 'variant:\s+dvp' -Pq; then
        echo am: normal layout
        setxkbmap -layout us
        xmodmap ~/system-config/etc/hardware-mach/.Xmodmap-undo
        do-unnatural-scrolling
        if test -e ~/.config/system-config/using-fcitx; then
            (
                exec 9>/dev/null
                cd ~/.cache/system-config/logs/
                setsid nohup fcitx
            )&
        fi >/dev/null 2>&1
    else
        do-dvp
    fi
) 9> ~/.cache/system-config/logs/$(basename $0).lock
