#!/bin/bash

start_dir=`pwd`
while test ! -d "$start_dir"/.beagle -a "$start_dir" != /
do
    start_dir=`readlink -f "$start_dir"/..`
done

if test "$start_dir" = /
then
    echo 'Error: can not find .beagle anywhere'
    exit
fi

beagle_dir=$start_dir/.beagle

beagle-static-query \
    --add-static-backend "$beagle_dir" \
    --backend none --max-hits 100000 "$2"\
|sed -e 's!^file://!!'\
|xargs grep -H -n -I -e "$2"
