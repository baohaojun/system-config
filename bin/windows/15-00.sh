#!/bin/bash

bhj-notify "daily" "Check if you have any unfinished things to do before go off work"

if test $(dayofweek) = Fri; then
    do-weekly < /dev/null
fi
