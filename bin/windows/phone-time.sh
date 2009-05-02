#!/bin/bash

usbnet.exe
date +%m%d%H%M%Y.%S|xargs.exe adb.exe shell busybox date -s
