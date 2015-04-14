#!/bin/bash

for x in "$@"; do 
    wlp "$x"
done | tee ~/.cache/system-config/logs/up.sh | putclip
