#!/bin/bash

(
    sleep 5
    su -c 'screencap /sdcard/screen.png'
    log -t bhj "hello after screencap $?"
)&
