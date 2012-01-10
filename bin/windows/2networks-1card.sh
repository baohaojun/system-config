#!/bin/bash
netsh interface ip set address name=eth0 source=dhcp
internal_ip=$(ifconfig  eth0|grep v4|perl -npe 's/.*: //')
external_ip=192.168.33.$(ifconfig /all |md5sum.exe |pn 1|perl -npe 's/^/((x=0x/; s/$/%255)); if test \$x -lt 0; then ((x=-x)); fi; echo \$x/'|bash)
netsh interface ip add address "eth0" $internal_ip 255.255.255.0
netsh interface ip add address "eth0" $external_ip 255.255.255.0 192.168.33.1
netsh interface ip set dns name=eth0 source=static addr=192.168.18.1 register=primary
