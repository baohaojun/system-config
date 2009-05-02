#!/bin/bash
if [[ x$1 == x ]]; then 
perl -npe 's"/usr/local/bin/daemonize(.*)0x00.*"\1 &"' -i ~/rootfs/etc/rc.d/rc3.d/daemonize_batch.sh
vi ~/rootfs/etc/rc.d/rc3.d/daemonize_batch.sh
#perl -npe 's".*(/etc/rc.d/rc3.d/daemonize_batch.sh).*"\1"' -i ~/rootfs/etc/init.d/rc
else 
perl -npe 's"/usr/local/bin/daemonize(.*)0x00.*"\1 &"' -i "$1"/rootfs/etc/rc.d/rc3.d/daemonize_batch.sh
vi "$1"/rootfs/etc/rc.d/rc3.d/daemonize_batch.sh
#perl -npe 's".*(/etc/rc.d/rc3.d/daemonize_batch.sh).*"\1"' -i "$1"/rootfs/etc/init.d/rc
fi
