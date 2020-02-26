#!/bin/bash

. aterr echo shit happened

set -ex

sleep 2

bhj-notify check-notification.sh.$$ "$(which Wrench.sh)"
touch ~/tmp/check-notification.sh.$ANDROID_SERIAL.lock
exec 9> ~/tmp/check-notification.sh.$ANDROID_SERIAL.lock
flock -n 9

(
    exec 9>/dev/null

    fuser -k ~/tmp/check-notification.sh.$ANDROID_SERIAL.log || true
    exec > ~/tmp/check-notification.sh.$ANDROID_SERIAL.log 2>&1

    set -x
    if test "$1"; then
        export ANDROID_SERIAL=$1
    else
        unset ANDROID_SERIAL
    fi

    if test "${ANDROID_SERIAL}" = "$(get-about-me adb-serial)"; then
        start-cuty& # do not watch weibo for too long.
        which_wrench=$(
            emacsclient -e '(shell-command-to-string "which Wrench.sh")' | em str2text
                    )
        if ! [[ $which_wrench =~ $ANDROID_SERIAL ]]; then
            (Wrench.sh -xs || true)&
        fi

        if ! [[ "$(which Wrench.sh)" =~ $ANDROID_SERIAL ]]; then
            (Wrench.sh -xs || true)&
            exit 0
        fi
    fi

    # this script checks if wrench notification is working
    notification_port=58888
    if test "$WRENCH_INSTANCE"; then
        ((notification_port += WRENCH_INSTANCE)) || true
    fi

    if expect -c "spawn timeout 2 telnet localhost $notification_port; exit [lindex [wait] 3]"; then
        bhj-notify check-notification.sh "this should not happen"
    else
        ret=$?

        if test "$ret" = 124; then
            exit 0
        fi

        if test "${ANDROID_SERIAL}" = "$(get-about-me adb-serial)"; then
            adb shell am start -a android.settings.ACTION_NOTIFICATION_LISTENER_SETTINGS
            onmyoji -o noshit restart-notification
        fi
    fi
)&
