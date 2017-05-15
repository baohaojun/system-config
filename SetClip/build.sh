#!/bin/bash
set -x
set -e
set -e

me=$(readlink -f $0)
if test ! -e "$me"; then
    me=$(readlink -f "$(which $0)")
    if test ! -e "$me"; then
        die "Can't find out about me"
        exit 1
    fi
fi
b0=$(basename $0)

cd $(dirname $me)

(
    cd jni
    ndk-build
)

# PATH=/usr/lib/jvm/java-7-openjdk-amd64/bin:$PATH

mm-ant "$@"

my-adb 'rm -f /sdcard/setclip-apk.txt; am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getapk 1'
sleep 1
my-adb kill app_process || true
my-adb sh /sdcard/setclip-apk.txt app_process /system/bin/ com.Wrench.Input
