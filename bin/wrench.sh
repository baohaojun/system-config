#!/bin/bash

command=$1
shift

## start code-generator "^\\s *#\\s *"
# generate-getopt c:chat t:text
## end code-generator
## start generated code
TEMP=$(getopt -o c:t:h \
              --long chat:,text:,help \
              -n $(basename -- $0) -- "$@")
chat=
text=
eval set -- "$TEMP"
while true; do
    case "$1" in

        -c|--chat)
            chat=$2
            shift 2
            ;;
        -t|--text)
            text=$2
            shift 2
            ;;
        -h|--help)
            set +x
            echo
            echo
            echo Options and arguments:
            printf %06s '-c, '
            printf %-24s '--chat=CHAT'
            echo
            printf %06s '-t, '
            printf %-24s '--text=TEXT'
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


function post-at-app() {
    cat <<EOF > ~/.cache/system-config/wrench-$$.twa
-- -*- mode: lua -*-
-- {%lua%}
t1_call("$chat@@$app")
if yes_or_no_p("in $app chat $chat?") then
   t1_post([[$text]])
end
if yes_or_no_p("Go back to home?") then
   adb_event"key HOME"
end
-- {%/lua%}
EOF

    Wrench ~/.cache/system-config/wrench-$$.twa
    # rm ~/.cache/system-config/wrench-$$.twa
}

function weixin() {
    app=wx
    post-at-app
}

function qq() {
    app=qq
    post-at-app
}

$command "$@"
