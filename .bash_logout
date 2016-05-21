#!/bin/bash
exec > ~/tmp/1.txt 2>&1
if test "$(tty)" = /dev/tty1; then
    command sudo chvt 7
fi
