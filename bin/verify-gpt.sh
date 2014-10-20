#!/bin/bash
adb "dd if=/dev/block/mmcblk0 of=/data/gpt bs=$(pretty 17K|pn 2) count=1"
adb pull /data/gpt ~/tmp
cd ~/tmp
cat gpt | gpt-part-parser.pl
