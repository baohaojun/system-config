#!/bin/bash

if test $(basename $0) = test-freq-current-temp.sh; then
    adb shell su -c 'for freq in $(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies); do for x in $(seq 0 3); do echo 1 > /sys/devices/system/cpu/cpu$x/online; echo $freq > /sys/devices/system/cpu/cpu$x/cpufreq/scaling_max_freq; done; mkdir -p /sdcard/current/$freq; for n in $(seq 1 60); do for x in /sys/class/thermal/thermal_zone*; do cat $x/type; cat $x/temp; done | tee /sdcard/current/$freq/temp-$n.txt; cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq | tee /sdcard/current/$freq/freq-$n.txt; echo round $n; sleep 1; done; done'
else
    max_freq=$(adb cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies|perl -npe 's/\s*$//; s/.*\s+//')

    adb su -c '
    freq='$max_freq'
    for x in $(seq 0 3); do
        echo 1 > /sys/devices/system/cpu/cpu$x/online;
        echo $freq > /sys/devices/system/cpu/cpu$x/cpufreq/scaling_max_freq;
    done;
    mkdir -p /sdcard/current/$freq;
    for n in $(seq 1 60); do
        for x in /sys/class/thermal/thermal_zone*; do
            cat $x/type;
            cat $x/temp;
        done | tee /sdcard/current/$freq/temp-$n.txt;
        cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq | tee /sdcard/current/$freq/freq-$n.txt;
        echo round $n; sleep 1;
    done'
fi
