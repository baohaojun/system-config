#!/bin/bash

if test "$1" = my-master; then
    echo my-master
else
    grep -v origin/master
fi
