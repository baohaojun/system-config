#!/usr/bin/env bash

(
    set -x;
    reg add 'HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\services\w32time\Config' /v MaxPosPhaseCorrection /d 0x2a300 /t REG_DWORD /f
    reg add 'HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\services\w32time\Config' /v MaxNegPhaseCorrection /d 0x2a300 /t REG_DWORD /f

    net stop w32time;
    sleep 1;
    net start w32time;
    sleep 1;
    w32tm /config /update;
    w32tm /resync /rediscover
    echo $?
) 2>&1 |iconv -f gbk -t utf-8 | tee -a ~/time.txt
