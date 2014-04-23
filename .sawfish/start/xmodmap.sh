#!/bin/bash

if lsusb | grep 045e:00db -i; then
    re-xmodmap
else
    xmodmap ~/.Xmodmap
fi
