#!/bin/bash

if test $(basename $0) = test-freq-current-temp.sh; then
    adb remount
    adb mv /etc/thermal-engine-8974.conf /etc/thermal-engine-8974.conf.bak
    adb stop mpdecision
    adb restart thermal-engine
    adb shell su -c 'stop; for freq in $(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies); do for x in $(seq 0 3); do echo 1 > /sys/devices/system/cpu/cpu$x/online; echo $freq > /sys/devices/system/cpu/cpu$x/cpufreq/scaling_max_freq; done; mkdir -p /sdcard/current/$freq; for n in $(seq 1 60); do for x in /sys/class/thermal/thermal_zone*; do cat $x/type; cat $x/temp; done | tee /sdcard/current/$freq/temp-$n.txt; cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq | tee /sdcard/current/$freq/freq-$n.txt; echo round $n; sleep 1; done; done'
else
    if echo $0|grep -e -max-; then
        which=max
    else
        which=min
    fi
    max_freq=$(select-args $(adb cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies | tr ' ' '\n' | reverse))

    adb-tty su -c '
    freq='$max_freq'
    for x in $(seq 0 3); do
        echo 1 > /sys/devices/system/cpu/cpu$x/online;
        echo $freq > /sys/devices/system/cpu/cpu$x/cpufreq/scaling_'$which'_freq;
    done;
    mkdir -p /sdcard/current/$freq;
    n=0
    while true; do
        ((n++))
        for x in /sys/class/thermal/thermal_zone*; do
            cat $x/type;
            cat $x/temp;
        done | busybox tee /sdcard/current/$freq/temp-$n.txt;
        (for x in $(seq 0 3); do
            if test -e /sys/devices/system/cpu/cpu$x/cpufreq/scaling_cur_freq; then
                echo -n "$x ";
                cat /sys/devices/system/cpu/cpu$x/cpufreq/scaling_cur_freq
            fi
         done) | busybox tee /sdcard/current/$freq/freq-$n.txt;
        echo round $n; sleep 1;
    done'
fi
