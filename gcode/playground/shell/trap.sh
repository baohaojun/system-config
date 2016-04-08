#!/bin/bash

function when-killed() {
    echo "Error: $errno"
    exit 1
}

trap when-killed TERM

sleep 20&

function die() {
    errno="Error: $@"
    echo $errno
    kill $$
}

(die "What the fuck")
wait
