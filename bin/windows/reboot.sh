#!/bin/bash
if which shutdown | grep /windows/system -iq; then
    shutdown /r /t 000
else
    /bin/shutdown -f -r 0
fi
