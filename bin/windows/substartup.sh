#!/bin/bash

cd ~/system-config/bin/windows/startup
rm *.stackdump */*.stackdump -f

function Usage()
{
    echo 'Usage: substartup.sh folders'
    echo 'Valid startup folders: cygwin non-cygwin cygstart ALL'
    exit -1

}

if test $# = 0; then
    Usage
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
                test -e "$y" && cygstart "$y"
            done
            ;;
        ALL)
            substartup.sh cygwin non-cygwin cygstart
            ;;
        *)
            Usage
            ;;
    esac
done
