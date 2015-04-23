#!/bin/bash
#adb bash -x stcmd-subcase.sh disable-swap-dump
my-adb forward tcp:54321 localreserved:scitd
echo "$@" | nc localhost 54321
