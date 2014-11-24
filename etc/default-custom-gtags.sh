#!/bin/bash
cd $(dirname $0)
if test -z "$DOING_CUSTOM_GTAGS_UPDATE"; then
    export DOING_CUSTOM_GTAGS_UPDATE=true
    . ~/etc/bash.d/cd_beagrep
    cd_beagrep
    mkgtags -i -v >/dev/null 2>&1
    exec grep-gtags -e "$1"
fi
