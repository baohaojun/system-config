#!/bin/bash
echo "$@" >> ~/rundll32.log
rundll32bak.exe "$@"
