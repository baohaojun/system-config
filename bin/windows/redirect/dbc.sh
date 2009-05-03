#!/bin/bash

for x in "$@"; do 
    echo -n \""$x"\""|" 1>&2
done
if [[ $# == 0 ]]; then
    echo -n there\'s no args 1>&2
fi

echo 
