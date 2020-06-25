#!/usr/bin/env bash

if yes-or-no-p "update fstab"; then
cat <<EOF | sudo tee -a /etc/fstab

proc                    /proc           procfs  rw              0       0
# End auto-admin addition
linprocfs   /compat/linux/proc  linprocfs       rw      0       0
linsysfs    /compat/linux/sys   linsysfs        rw      0       0
tmpfs    /compat/linux/dev/shm  tmpfs   rw,mode=1777    0       0
EOF

fi

if yes-or-no-p "enable linux"; then
    sudo kldload linux
    sudo kldload linux64
    sudo sysrc 'linux_enable="YES"'
fi
