#!/bin/bash
run fetchmail&

cd ~/gcode/MboxTray/
cygstart --hide /cygdrive/c/python25/python mail.py
cygstart --hide ~/bin/windows/NetworkTest.exe&
cd ~/doc
regedit /s ime-noctrlshift-noaltshift.reg
net start 'vmware host agent'
