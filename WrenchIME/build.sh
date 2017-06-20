#!/bin/bash

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

cd jni

# Build
ndk-build -j8
cd -

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

