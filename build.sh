#!/bin/bash
set -x
set -e

# PATH=/usr/lib/jvm/java-7-openjdk-amd64/bin:$PATH
(
    for x in $(seq 1 60); do
        sleep 1;
        if test "$(adb-top-activity)" = 警告; then
            adb-tap 747 1186
            break
        fi
    done
) >/dev/null 2>&1 &
mm-ant

my-adb 'rm -f /sdcard/setclip-apk.txt; am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getapk 1'
sleep 1
my-adb kill app_process || true
my-adb sh /sdcard/setclip-apk.txt app_process /system/bin/ Input
