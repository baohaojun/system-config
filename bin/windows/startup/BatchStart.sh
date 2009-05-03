#!/bin/bash
run fetchmail&

cd ~/Projects/MboxTray/
cygstart --hide /c/python25/python mail.py
cygstart --hide ~/bin/windows/NetworkTest.exe&
cd ~/doc
regedit /s ime-noctrlshift-noaltshift.reg
net start 'vmware host agent'
