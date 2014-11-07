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
                PUTCLIP_ANDROID_FILE=/tmp/$USE_BUFFER_NAME.out putclip-android

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
                        weibo) # send weibo
                            if test "$(adb-top-activity)" = com.sina.weibo/com.sina.weibo.DetailWeiboActivity; then
                                repost=$(select-args -o repost comment)
                                if test $repost = repost; then
                                    adb-tap-bot-left
                                else
                                    adb-tap-mid-bot
                                fi
                                sleep .5
                            fi
                            adb-key SCROLL_LOCK;
                            adb-tap 991 166
                            ;;
                        t1-sms) # quick reply sms
                            adb-tap 182 1079
                            adb-long-press 522 921 # 长按输入框
                            adb-tap 149 786
                            adb-tap 864 921
                            ;;
                        cell-mail) # reply mail
                            activity=$(adb-top-activity)
                            if test "$activity" = com.android.email/com.android.email.activity.Welcome ||
                               test "$activity" = com.android.email/com.android.email2.ui.MailActivityEmail; then
                                adb-tap-mid-bot
                                sleep 2
                            fi
                            adb-key SCROLL_LOCK
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
                            adb start-activity com.sina.weibo/com.sina.weibo.EditActivity
                            PUTCLIP_ANDROID_FILE=/tmp/$USE_BUFFER_NAME.out putclip-android
                            emacs-cell-phone weibo
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
