#!/bin/bash
mkdir /c/download
cd /c/download
wget -c http://download.sysinternals.com/Files/ProcessMonitor.zip
wget -c http://download.sysinternals.com/Files/ProcessExplorer.zip
wget -c http://download.sysinternals.com/Files/PsTools.zip
wget -c http://puttycyg.googlecode.com/files/puttycyg-20090219.zip
cd ~/bin/windows/lnks
for x in /c/download/*.zip; do 
    if [[ -f "$x" ]]; then
        /bin/unzip -o "$x";
    fi
done
mv ./*/* . 
chmod a+x ./*
