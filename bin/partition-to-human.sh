#!/bin/bash

set -e
for x in partition*; do
    [[ $x =~ \.human$ ]] && continue
    marvell-partition-to-human $x > $x.human;
done
