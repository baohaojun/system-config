#!/bin/bash

set -e
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

