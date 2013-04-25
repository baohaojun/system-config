#!/bin/bash

for x in $(find . -maxdepth 1 -name '*.org'); do git log --pretty=format:"%ai $x%n" $x|tail -n 1; done > ~/.logs/$(basename $0).out
cat ~/.logs/$(basename $0).out | while read date time tz name; do
    year=${date:0:4}
    month=${date:5:2}
    day=${date:8:2}
    mkdir -p blog/$year/$month/$day
    git mv $name blog/$year/$month/$day/$(basename $name)
    relative-link blog/$year/$month/$day/$(basename $name) $name
    git add $name
done
