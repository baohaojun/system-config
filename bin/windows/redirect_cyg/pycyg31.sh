#!/bin/bash

debug=false
if [[ "$1" == -x ]]
then
    debug=true
    shift
fi
pyfile="$1"
 if ! [[ ${pyfile:0:1} == / ]];
 then
     pyfile=`which "$pyfile"`
 fi
shift
pyfile=`cygpath -alw "$pyfile"`

wpy3 "$pyfile" "$@"
if [[ $debug == true ]]
then
    echo -n Press any key to exit ...
    read
fi
