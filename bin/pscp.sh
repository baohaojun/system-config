#!/bin/bash

echo -n pscp\  >>~/pscp.log
for x in "$@"; do 
    echo -n \""$x"\"\  >> ~/pscp.log 
done
echo >>~/pscp.log
~/bin/.pscp-distr.exe "$@" 2>&1|tee ~/pscp.out
#notepad 
