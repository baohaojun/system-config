#!/bin/bash
. /c/ssh-agent.log
ssh root@bhj3 perl -npe 's/^#up/up/' -i /etc/tinyproxy/tinyproxy.conf &&ssh root@bhj3 /etc/init.d/tinyproxy restart&

netsh interface ip set address name=eth0 source=static addr=192.168.1.3 mask=255.255.255.0 gateway=192.168.1.1 gwmetric=1
netsh interface ip set address name=eth0 source=dhcp
netsh interface ip set dns name=eth0 source=dhcp
ipconfig /renew
#cygstart ~/system-config/bin/ieProxyEnable.reg
cd ~/system-config/doc
regedit /s MsnBlogProxyEnable.reg
net stop 'vmware nat service'
net start 'vmware nat service'

