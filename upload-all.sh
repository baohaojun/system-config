#!/bin/bash
shortVersion=$(perl -ne 'print $1 if m!T1聊天小扳手 (.*)</string>!' t1wrenchmainwindow.ui)
cd ~/tmp
my-adb start-activity com.baidu.netdisk/com.baidu.netdisk.ui.MainActivity

my-adb 'rm /sdcard/0/*'
for x in T1Wrench-linux.tgz T1Wrench-macos.zip T1Wrench-windows.zip; do
    adb push $x /sdcard/0/${x%.*}-$shortVersion.${x#*.}
done
adb-tap 544 346
adb-tap 489 1203
adb-tap 236 613
adb-tap 985 387
adb-tap 951 635
adb-tap 1001 805
adb-tap 755 1841
