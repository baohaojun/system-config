#!/bin/bash

js=$1
for x in $(find ~/.mozilla/ -name $js); do
    relative-link $js $x -f
done
