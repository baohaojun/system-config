#!/bin/bash

switch-touchpad on

sleep 30
if test "$(get-about-me mach)" = vostro; then
    sudo rmmod psmouse ; sudo modprobe psmouse
fi
switch-touchpad on
