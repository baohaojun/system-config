#!/bin/bash

for x in $(seq 1 100); do

    if test -e /sdcard/start-debian2.sh; then
	log -t debian will now start debian
	sh /sdcard/start-debian2.sh
	exit
    fi

    log -t debian debian not started for $x
    sleep 2

done
