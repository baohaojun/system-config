#!/bin/bash


for x in $(find . -name '*.jar' -type f); do
    td=/tmp/$(basename $0)-$(basename $x).$$
    mkdir -p $td
    cd $td
    unzip $x >/dev/null 2>&1 
    echo "$x";
    for y in $(find . -name '*.class'); do
        echo "${y%.class}"
        javap "${y%.class}"
    done
    cd -
    rm $td -rf
done
    
