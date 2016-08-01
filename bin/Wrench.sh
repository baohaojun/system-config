#!/bin/bash

export EMACS=t
export ANDROID_SERIAL=$(select-output-line -p "Select the adb device" my-adb devices?|pn 1)
adb forward --remove tcp:28888
exec Wrench
