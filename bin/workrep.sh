#!/bin/bash
cd ~/WorkWiKi/
WRFILE=WeeklyReport`date|awk '{print $2$3$6}'`
cat ~/WorkWiKi/WeeklyReport >> $WRFILE
edit.sh $WRFILE
