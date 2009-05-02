#!/bin/bash
echo "$@" >/d/rundll32.log
rundll32bak.exe "$@"
