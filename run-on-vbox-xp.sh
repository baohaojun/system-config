#!/bin/bash

set -e

cd ~/src/github/T1Wrench-windows

ssh-exp vb-xp bash -c 'terminateModule.py T1Wrench'
psync vb-xp .
ssh-exp vb-xp bash -c 'cd ~/src/github/T1Wrench-windows; myscr ./T1Wrench.exe'
adb-over-tcp-for-ssh vb-xp
