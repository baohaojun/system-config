#!/bin/bash
set -x

for x in $(seq 1 6); do 
    if test "$(su -c 'rm /data/test-su-ok; echo root > /data/test-su-ok; cat /data/test-su-ok')" = root; then
        echo "su is ok now";
        break
    fi
    echo su is not ready for $x, sleep...
    sleep 10
done
su -c 'logwrapper sh -x /sdcard/start-debian2.sh'
