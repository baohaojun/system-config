#!/bin/bash

if test -e ~/.config/bhj/weixin-adb-serial; then
    export ANDROID_SERIAL=$(cat ~/.config/bhj/weixin-adb-serial)
fi
export USE_BUFFER_NAME=send-to-$(basename $0).org
(
    if test $# != 0; then
        true
    elif ! flock -n 9; then
        find-or-exec emacs "No such thing"
        emacsclient -e '(switch-to-buffer "'$USE_BUFFER_NAME'")'
        exit
    fi
    if test $(basename $0) = notes-weixin; then
        export NO_WEIXIN_EMOJI=true
    fi
    while true; do
        if test $# != 0; then
            input=$*
            echo -n "$@" > /tmp/$USE_BUFFER_NAME.out
        else
            input=$(ask-for-input-with-emacs -p "What do you want say on $(basename $0)?" || true)
        fi
        if ! test "$input"; then
            exit
        fi
        if test -e ~/.t1-sig; then
            input=$(echo "$input"; echo; cat ~/.t1-sig)
            (echo; cat ~/.t1-sig) >> /tmp/$USE_BUFFER_NAME.out
        fi
        (
            flock 10
            (
                exec 9>/dev/null 10>&9
                PUTCLIP_ANDROID_FILE=/tmp/$USE_BUFFER_NAME.out t1wrench.lua putclip

                function emacs-cell-phone() {
                    case "$1" in
                        google+) # send google plus
                            adb-tap 467 650
                            adb-long-press 278 544
                            adb-tap 102 286
                            adb-tap 932 1818
                            ;;
                        t1-putclip)
                            # do nothing, I will post it myself
                            adb-key SCROLL_LOCK
                            ;;
                        both-weixin-and-weibo)
                            emacs-cell-phone weibo-brand-new
                            emacs-cell-phone weixin-brand-new
                            ;;
                        weibo-brand-new)
                            if tail -n 1 /tmp/$USE_BUFFER_NAME.out| grep -P '^\(图\)$'; then
                                t1wrench-get-pictures | xargs -d \\n t1wrench.lua picture_to_weibo_share
                            fi
                            t1wrench.lua t1_share_to_weibo
                            ;;
                        weixin-brand-new)
                            adb start-activity com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsCommentUI --ei sns_comment_type 1
                            emacs-cell-phone weixin-new
                            ;;
                        weixin-new) # weixin friends
                            adb-key SPACE
                            adb-tap 117 283 adb-tap 117 283 adb-tap 325 170 adb-tap 860 155 adb-tap 961 171
                            ;;
                        SmartisanNote)
                            adb-long-press 428 412
                            adb-tap 80 271
                            adb-tap 940 140
                            adb-tap 933 117
                            adb-tap 323 1272
                            adb-tap 919 123
                            ;;
                        weixin-note3)
                            adb-tap 560 1840 #
                            sleep .1
                            adb-tap 560 1840 #
                            adb-long-press 491 1000
                            if test "$(gettask-android)" = com.tencent.mobileqq; then
                                adb-tap 255 849
                            else
                                adb-tap 179 872
                            fi
                            adb-tap 1001 983
                            ;;
                        notes-weixin)
                            pic=$(adb-get-a-note|perl -npe 's/^~/$ENV{HOME}/')
                            activity=$(adb-top-activity)
                            if test "$activity" = com.tencent.mobileqq/com.tencent.mobileqq.activity.ChatActivity; then
                                adb-picture-to-qq-chat $pic
                            elif test "$activity" = com.tencent.mm/com.tencent.mm.ui.LauncherUI; then
                                adb-picture-to-weixin-chat $pic
                            elif test "$activity" = com.tencent.mm/com.tencent.mm.ui.chatting.ChattingUI; then
                                adb-picture-to-weixin-chat $pic
                            elif test "$activity" = com.tencent.mm/com.tencent.mm.plugin.sns.ui.SnsTimeLineUI; then
                                adb-picture-to-weixin-friends $pic
                            elif test "$activity" = com.sina.weibo/com.sina.weibo.MainTabActivity; then
                                adb-picture-to-weibo $pic
                            fi
                            ;;
                        *) # most cases
                            t1wrench.lua t1_post
                            ;;
                    esac
                }
                emacs-cell-phone $(basename $0)
            )
        ) 10> ~/.logs/$(basename $0).lock-send &
        if test $# != 0; then
            exit
        fi
    done
) 9> ~/.logs/$(basename $0).lock
