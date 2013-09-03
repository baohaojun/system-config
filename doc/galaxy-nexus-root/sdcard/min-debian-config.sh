#!/bin/bash

apt-get install git sudo bash-completion locales python3
dpkg-reconfigure locales
echo 'bhj ALL=NOPASSWD: ALL' > /etc/sudoers.d/bhj
chmod 440 /etc/sudoers.d/bhj
