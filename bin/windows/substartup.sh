#!/bin/bash

cd ~/bin/windows/startup
rm *.stackdump */*.stackdump -f

if test $# = 0; then
    START="cygwin non-cygwin cygstart";
else
    START="$@"
fi

for x in $START; do 
    case $x in
        cygwin|non-cygwin) 
            for y in $x/*; do 
                "$y"&
            done
            ;;
        cygstart)
            for y in $x/*; do 
                cygstart "$y"
            done
            ;;
        *)
            echo 'Error: Unknown startup folder!'
            false
            ;;
    esac
done
