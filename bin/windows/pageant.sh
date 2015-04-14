#!/bin/bash
if test -f ~/.ssh/isa_putty.ppk; then
    cygstart ~/system-config/bin/windows/pageant.exe "\"`cygpath -alwm ~/.ssh/isa_putty.ppk`\""
fi
