#!/bin/bash

. ~/system-config/.bashrc
touch ~/.cache/system-config/.where ~/.cache/system-config/.where.lock

. ~/system-config/bin/Linux/download-external.sh
cpan String::Approx
cpan String::ShellQuote

download_external >/dev/null 2>&1 &

set -e
export PATH=~/system-config/bin/Linux/config:$PATH

#update the system

mkdir -p ~/src/github
emacs-install-packages

if test ! -d /usr/local/share/info; then
    sudo mkdir -p /usr/local/share/info
fi

#编译一些软件
compile_beagrep
compile_gtags
compile_ctags

echo 'OK'
