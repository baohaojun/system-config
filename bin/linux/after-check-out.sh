#!/bin/bash

#下载一些软件，比如antlr
touch ~/.where ~/.where.lock
. ~/bin/linux/download-external.sh
download_external >/dev/null 2>&1 &

set -e
export PATH=~/bin/linux/config:$PATH

#update the system
upd_system

#编译一些软件
do_compile

echo 'OK'
