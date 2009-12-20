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
echo python lib is: $PYHON_LIB
echo python include is: $PYHON_INCLUDE
python "$pyfile" "$@"
if [[ $debug == true ]]
then
    echo -n Press any key to exit ...
    read
fi
