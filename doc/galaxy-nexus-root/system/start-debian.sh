#!/bin/bash

(
    for x in $(seq 1 100); do
	
	if test -e /sdcard/start-debian2.sh; then
	    log -t bhj will now start debian
	    logwrapper sh -x /sdcard/start-debian2.sh
	    exit
	fi

	log -t bhj debian not started for $x
	sleep 2

    done
)&
