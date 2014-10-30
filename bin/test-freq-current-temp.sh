#!/bin/bash

if test $(basename $0) = test-freq-current-temp.sh; then
    my-adb remount
    my-adb mv /etc/thermal-engine-8974.conf /etc/thermal-engine-8974.conf.bak
    my-adb stop mpdecision
    my-adb restart thermal-engine
    my-adb shell su -c 'stop;
                     for freq in $(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies); do
                         minfreq=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies|awk "{print \$1}")
                         for x in $(seq 0 3); do
                             echo 1 > /sys/devices/system/cpu/cpu$x/online;
                             echo $freq > /sys/devices/system/cpu/cpu$x/cpufreq/scaling_max_freq;
                             echo $minfreq > /sys/devices/system/cpu/cpu$x/cpufreq/scaling_min_freq;
                         done;
                         mkdir -p /sdcard/current/$freq;
                         start=$(busybox date +%s)
                         for n in $(seq 1 60); do
                             for x in /sys/class/thermal/thermal_zone*; do
                                 cat $x/type;
                                 cat $x/temp;
                             done | tee /sdcard/current/$freq/temp-$n.txt;
                             cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq |
                                 tee /sdcard/current/$freq/freq-$n.txt;
                             echo round $n;
                             sleep 2;
                             time=$(busybox date +%s)
                             if ((time - start > 120)); then
                                 break
                             fi
                         done;
                     done'
else
    if echo $0|grep -e -max-; then
        which=max
    else
        which=min
    fi
    max_freq=$(select-args $(my-adb cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies | tr ' ' '\n' | reverse; echo no-change))

    adb-tty su -c '
    freq='$max_freq'
    for x in $(seq 0 3); do
        echo 1 > /sys/devices/system/cpu/cpu$x/online;
        if test $freq != no-change; then
            echo $freq > /sys/devices/system/cpu/cpu$x/cpufreq/scaling_'$which'_freq;
        fi
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
