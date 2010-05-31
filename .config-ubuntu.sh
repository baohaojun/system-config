#!/bin/bash

set -e
if test `whoami` = root; then
    echo 'You must not run this script as root!'
    exit -1
fi

sudo bash -c "cat <<EOF > /etc/apt/sources.list
deb http://192.168.0.46/ubuntu/ lucid main restricted
deb-src http://192.168.0.46/ubuntu/ lucid main restricted
deb http://192.168.0.46/ubuntu/ lucid-updates main restricted
deb-src http://192.168.0.46/ubuntu/ lucid-updates main restricted
deb http://192.168.0.46/ubuntu/ lucid universe
deb-src http://192.168.0.46/ubuntu/ lucid universe
deb http://192.168.0.46/ubuntu/ lucid-updates universe
deb-src http://192.168.0.46/ubuntu/ lucid-updates universe
deb http://192.168.0.46/ubuntu/ lucid multiverse
deb-src http://192.168.0.46/ubuntu/ lucid multiverse
deb http://192.168.0.46/ubuntu/ lucid-updates multiverse
deb-src http://192.168.0.46/ubuntu/ lucid-updates multiverse
deb http://security.ubuntu.com/ubuntu lucid-security main restricted
deb http://security.ubuntu.com/ubuntu lucid-security universe
deb http://security.ubuntu.com/ubuntu lucid-security multiverse
EOF"

sudo apt-get update
sudo apt-get install -y git-core ssh subversion vim


cd ~
git clone git://192.168.0.46/windows-config.git || (cd ~/windows-config && git pull)
cd ~/windows-config
git checkout -- . 
./bin/after-co-ln-s.sh
wget http://192.168.0.46/jdk-1_5_0_22-linux-amd64.bin
sudo sh ./jdk-1_5_0_22-linux-amd64.bin
sudo mv jdk1.5.0_22 /usr/local/jdk1.5
sudo apt-get install -y lftp
mkdir -p ~/bin/linux/ext/
cd ~/bin/linux/ext
lftp -c "mirror -c http://192.168.0.46/google"
tar zxfv ./google/android-sdk_r06-linux_86.tgz
mkdir -p ~/bin/linux/ext/android-sdk-linux_86/temp
cd ~/bin/linux/ext/android-sdk-linux_86/temp
ln ../../google/* . 
. ~/.bashrc
~/bin/linux/ext/android-sdk-linux_86/tools/android&
~/bin/linux/after-check-out.sh

echo 'OK, your configuration has completed!'
