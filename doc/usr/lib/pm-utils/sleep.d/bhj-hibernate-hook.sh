#!/bin/bash

case $1 in
    hibernate)
        ;;
    suspend)
        ;;
    thaw)
        ;;
    resume|post)
        (
            su - bhj -c "
                         set -x
                         sleep 1 && bash -x ~bhj/system-config/bin/switch-touchpad &
                         sleep 1 && re-xmodmap&
                         sleep 1 && sudo ps-killall pulseaudio&
                         . ~/.config/system-config/ssh-agent
                         notify-real-home&
            " >~bhj/.logs/bhj-hibernate-hook.sh.log 2>&1
            echo done "$@"
            echo
        ) > ~bhj/.logs/$(basename $0).log 2>&1
        ;;
    pre)
        (
            cmd=$(cat <<'EOF'
set -x
. ~/.renew-xauth
. ~/.config/system-config/.bashrc-path
xscreensaver-command -lock&
EOF
            )

            su - bhj -c "$cmd"
        ) > ~bhj/.logs/$(basename $0).log2 2>&1
        ;;
    *)  echo "ERROR: called incorrectly."
        ;;
esac
