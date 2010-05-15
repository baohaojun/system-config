#!/bin/bash
set -e
cd ~/windows-config/
for x in * .*;
do
    if test $x = . -o $x = .. -o $x = .git; 
    then
        continue;
    fi
    if test -e ~/$x -a "$(readlink -f ~/$x)" != "$(readlink -f ~/windows-config/$x)"; 
    then
        echo "Error: ~/$x already exist and it's not softlink to ~/windows-config/$x"
        exit -1
    elif ! test -h ~/$x;
    then
        ln -s ~/windows-config/$x ~/
    fi
done
