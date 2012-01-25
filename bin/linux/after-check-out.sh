#!/bin/bash

#下载一些软件，比如antlr
touch ~/.where ~/.where.lock

sudo perl -npe 's/^XKBVARIANT=.*/XKBVARIANT="dvp"/;' -i /etc/default/keyboard
sudo setupcon
sudo usermod -a -G fuse $USER

sudo perl -npe 's/ main$/ main contrib non-free/' -i /etc/apt/sources.list

. ~/bin/linux/download-external.sh
download_external >/dev/null 2>&1 &

set -e
export PATH=~/bin/linux/config:$PATH

#update the system
upd_system

#编译一些软件
do_compile

config-gfw

echo 'OK'
