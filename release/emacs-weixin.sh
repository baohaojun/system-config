#!/bin/bash

if ls -l /proc/$(ps.pl adb.fork-server|pn 1)/fd | grep '\.logs.*\.lock'; then
    ps-killall adb.fork-server
    adb devices
fi

export USE_BUFFER_NAME=send-to-$(basename $0).org
(
    if ! flock -n 9; then
        find-or-exec emacs "No such thing"
        emacsclient -e '(switch-to-buffer "'$USE_BUFFER_NAME'")'
        exit
    fi
    while true; do
        input=$(ask-for-input-with-emacs -p "What do you want say on $(basename $0)?" || true)
        if ! test "$input"; then
            exit
        fi
        if test -e ~/.t1-sig; then
            input=$(echo "$input"; echo; cat ~/.t1-sig)
        fi
        (
            flock 10
            putclip-android "$input"&

            case $(basename $0) in
                google+) # 发 Google Plus
                    adb-tap 560 500 # get rid of the pictures
                    adb-tap 560 1840
                    adb-long-press 99 383 # long press
                    adb-tap 497 281 # paste
                    adb-tap 985 935 # send
                    ;;
                t1-putclip)
                    # do nothing, I will post it myself
                    ;;
                weibo) # 发微博
                    adb-key SPACE
                    adb-long-press 440 281
                    adb-tap 545 191
                    adb-tap 991 166
                    ;;
                t1-sms) # 快速回短信
                    adb-tap 560 1840 # 点空格
                    adb-long-press 522 912 # 长按输入框
                    adb-tap 480 802
                    adb-tap 864 921
                    ;;
                cell-mail) # 回邮件
                    adb-swipe 586 878 586 268 500
                    adb-tap 560 1840 #
                    adb-tap-2 299 299
                    adb-tap 505 192
                    adb-tap 998 174
                    ;;
                weixin-new) # 朋友圈
                    adb-key SPACE
                    adb-tap 303 335
                    adb-tap 303 335
                    adb-tap 496 196
                    adb-tap 989 183
                    ;;
                *) # 最常见情形
                    adb-tap 560 1840 # 点一下底部输入框，弹出软键盘
                    sleep .1
                    adb-tap 560 1840 # 再点一下，可能在出键盘，需要输入一个空格
                    adb-tap-2 560 976 # 双击输入框
                    adb-tap 525 855 # 点一下粘贴钮
                    adb-tap 976 976 # 点一下发送钮
                    ;;
            esac
        ) 10> ~/.logs/$(basename $0).lock-send &
    done
) 9> ~/.logs/$(basename $0).lock
