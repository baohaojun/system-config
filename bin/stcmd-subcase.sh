#!/bin/bash
#adb bash -x stcmd-subcase.sh disable-swap-dump
my-adb forward tcp:54321 localreserved:scitd
echo srem-helper "$@" | nc localhost 54321
