#!/bin/bash

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
            (
                exec 9>/dev/null 10>&9
                putclip-android "$input"&

                function emacs-cell-phone() {
                    case "$1" in
                        google+) # send google plus
                            adb-tap 560 500 # get rid of the pictures
                            adb-tap 560 1840
                            adb-long-press 99 383 # long press
                            adb-tap 497 281 # paste
                            adb-tap 985 935 # send
                            ;;
                        t1-putclip)
                            # do nothing, I will post it myself
                            ;;
                        weibo) # send weibo
                            adb-key SPACE
                            adb-long-press 440 281
                            adb-tap 545 191
                            adb-tap 991 166
                            ;;
                        t1-sms) # quick reply sms
                            adb-tap 560 1840 # 点空格
                            adb-long-press 522 912 # 长按输入框
                            adb-tap 480 802
                            adb-tap 864 921
                            ;;
                        cell-mail) # reply mail
                            adb-swipe 586 878 586 268 500
                            adb-tap 560 1840 #
                            adb-tap-2 299 299
                            adb-tap 505 192
                            if test "$(gettask-android)" = com.google.android.gm; then
                                adb-tap 806 178
                            else
                                adb-tap 998 174
                            fi
                            ;;
                        both-weixin-and-weibo)
                            emacs-cell-phone weibo-brand-new
                            emacs-cell-phone weixin-brand-new
                            ;;
                        weibo-brand-new)
                            adb am start -n com.sina.weibo/.MainTabActivity
                            sleep .5
                            putclip-android "$input"&
                            adb-tap-mid-bot
                            adb-tap 193 924
                            emacs-cell-phone weixin-new
                            ;;
                        weixin-brand-new)
                            adb am start -n com.tencent.mm/.ui.LauncherUI
                            sleep .5
                            putclip-android "$input"&
                            adb-tap-2 141 178
                            adb-tap-2 141 178
                            adb-tap 510 290
                            adb-tap 347 487
                            adb-long-press 947 186
                            emacs-cell-phone weixin-new
                            ;;
                        weixin-new) # weixin friends
                            adb-key SPACE
                            adb-tap 303 335
                            adb-tap 303 335
                            adb-tap 496 196
                            adb-tap 989 183
                            ;;
                        *) # most cases
                            adb-tap 560 1840 # 点一下底部输入框，弹出软键盘
                            sleep .1
                            adb-tap 560 1840 # 再点一下，可能在出键盘，需要输入一个空格
                            adb-tap-2 560 976 # 双击输入框
                            adb-tap 525 855 # 点一下粘贴钮
                            adb-tap 976 976 # 点一下发送钮
                            ;;
                    esac
                }
                emacs-cell-phone $(basename $0)
            )
        ) 10> ~/.logs/$(basename $0).lock-send &
    done
) 9> ~/.logs/$(basename $0).lock
