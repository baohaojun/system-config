#!/bin/bash

ps-killall gnome-screensaver
ps-killall firefox
firefox -new-tab http://admin:$(get-authinfo 192.168.33.8 admin)@192.168.33.8&

function send-key() {
    sawfish-browser-input "$1"
    sleep .1
}

function send-multi-key() {
    for x in "$@"; do
        while test "$x"; do
            y=${x:0:1}
            x=${x:1}
            if test "$y" = \\; then
                y=backslash
            fi
            send-key "$y"
        done
        send-key SPC
    done
}

function send-multi-args() {
    for x in "$@"; do
        send-key "$x"
    done
}

sleep 10
send-key RET RET RET RET
sleep 2
putclip "系统工具"
send-multi-args C-s C-y C-g RET
putclip 重启路由器
send-multi-args C-s C-y C-g RET

sleep 5

putclip 将使
send-multi-args C-s C-y C-g TAB RET RET
gnome-screensaver&
