#!/bin/bash

cd ~/WorkWiKi/
FILE=~/WorkWiKi/WeeklyReport`date|awk '{print $6 $2 $3}'`
cp ~/WorkWiKi/WeeklyReport "$FILE"
edit.sh "$FILE"
