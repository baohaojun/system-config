#!/bin/bash

echo srem-helper "$@" | nc $(adb-get-wifi-ip) 54321
