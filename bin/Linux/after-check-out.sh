#!/bin/bash

. ~/.bashrc
touch ~/.where ~/.where.lock

if test $(whoami) = bhj; then
    sudo perl -npe 's/^XKBVARIANT=.*/XKBVARIANT="dvp"/;' -i /etc/default/keyboard
fi
sudo touch /etc/console-setup/* || true
sudo touch /etc/default/* || true # setupcon may fail when the timestamp of
                                  # these files are messed up by debian
                                  # installation (time zone or ntp not available
                                  # because we are not connected to the
                                  # Internet).
sudo setupcon
sudo usermod -a -G fuse $USER

sudo perl -npe 's/ main$/ main contrib non-free/' -i /etc/apt/sources.list

. ~/bin/Linux/download-external.sh
download_external >/dev/null 2>&1 &

set -e
export PATH=~/bin/Linux/config:$PATH

#update the system
upd_system

sudo usermod -a -G dialout $(whoami) || true
sudo perl -npe 's/^#user_allow_other/user_allow_other/' -i /etc/fuse.conf
mkdir -p ~/src/github
emacs-install-packages

sudo ln -s ~/doc/bash.info.gz /usr/local/share/info/ -f
sudo ginstall-info bash.info.gz /usr/local/share/info/dir

#编译一些软件
do_compile

echo 'OK'
