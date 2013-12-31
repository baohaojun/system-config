#!/bin/bash

set -x

sleep .5
xdotool click 1
sleep .5
sawfish-send-window-event "Mozilla Firefox" RET

while test "$(sawfish-get-key)" = e; do
    exec $0
done
