#!/bin/bash
echo "$@" >>~/dvips.log
echo "$@"
echo hello world
/h/bin/win32/dvips.exe "$@"
