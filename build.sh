#!/bin/bash
set -x
set -e

# PATH=/usr/lib/jvm/java-7-openjdk-amd64/bin:$PATH
(
    for x in $(seq 1 30); do
        sleep 1;
        if test "$(adb-top-activity)" = 警告; then
            adb-tap 747 1186
            break
        fi
    done
) >/dev/null 2>&1 &
mm-ant

