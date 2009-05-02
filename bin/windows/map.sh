#!/bin/bash
Command="$1"
shift
for x in "$@"
do 
    $Command "$x"
done
