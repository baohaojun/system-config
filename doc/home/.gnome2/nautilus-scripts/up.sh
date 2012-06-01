#!/bin/bash

for x in "$@"; do 
    wlp "$x"
done | tee ~/.logs/up.sh | putclip
