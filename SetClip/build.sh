#!/bin/bash
set -x
set -e

(
    cd jni
    ndk-build
)

# PATH=/usr/lib/jvm/java-7-openjdk-amd64/bin:$PATH

mm-ant "$@"

my-adb 'rm -f /sdcard/setclip-apk.txt; am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getapk 1'
sleep 1
my-adb kill app_process || true
my-adb sh /sdcard/setclip-apk.txt app_process /system/bin/ Input
