#!/bin/bash
set -x
set -e
mm-ant

my-adb 'rm -f /sdcard/setclip-apk.txt; am startservice -n com.bhj.setclip/.PutClipService --ei getapk 1'
sleep 1
apk=$(my-adb 'cat /sdcard/setclip-apk.txt')
my-adb env CLASSPATH=$apk app_process /system/bin/ Input
