#!/bin/bash

set -e

cd ~/src/github/Wrench-windows

ssh-exp vb-xp bash -c 'terminateModule.py Wrench'
psync vb-xp .
ssh-exp vb-xp bash -c 'cd ~/src/github/Wrench-windows; myscr ./Wrench.exe'
adb-over-tcp-for-ssh vb-xp
