#!/bin/bash
my-adb start-activity com.baidu.netdisk/com.baidu.netdisk.ui.MainActivity

my-adb 'mkdir -p /sdcard/0/; rm /sdcard/0/*'
for x in "$@"; do
    adb push $x /sdcard/0/$x
done

sleep 5

adb-tap 544 346
adb-tap 489 1203
adb-tap 236 613
for i in $(seq 1 $#); do
    adb-tap 980 $((387 + (i - 1) * 200))
    sleep .2
done
adb-tap 755 1841
