#!/bin/bash

(
    sleep 3
    su -c 'screencap /sdcard/screen.png'
    log -t bhj "hello after screencap $?"
)&
