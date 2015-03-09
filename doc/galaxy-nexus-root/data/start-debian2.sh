#!/bin/bash
set -x
set -e

# Make tmp
if [ ! -d /tmp ]; then
    mount -o rw,remount / && mkdir /tmp && chmod 1777 /tmp && mount -o ro,remount /
fi
if [ ! -f /tmp/.mounted ] ; then
    mount -t tmpfs none /tmp && touch /tmp/.mounted
fi

unset LD_PRELOAD

# setup posix shared memory
if [ ! -d /dev/shm ] ; then mkdir /dev/shm ; fi
chmod 1777 /dev/shm
if mount | grep /dev/shm; then
    true
else
    mount -t tmpfs none /dev/shm/
fi

# configure debian chroot
if [ -d /data/debian/proc ] && [ ! -d /data/debian/proc/1 ] ; then
   mount -o bind /proc /data/debian/proc
fi

if [ -d /data/debian/sys ] && [ ! -d /data/debian/sys/class ] ; then
    mount -o bind /sys /data/debian/sys
fi

if [ -d /data/debian/sys/kernel/debug ] && [ ! -d /data/debian/sys/kernel/debug/tracing ] ; then
    mount -o bind /sys/kernel/debug/ /data/debian/sys/kernel/debug
fi

if test ! -e /data/debian/dev/null || test -e /data/debian/dev/.dev-not-mounted -o -f /data/debian/dev/null; then
   mount -o bind /dev /data/debian/dev
   mount -o bind /dev/pts /data/debian/dev/pts
   mount -o bind /dev/shm /data/debian/dev/shm
   for x in system data cache factory d acct; do
       if test ! -e /$x; then
           continue
       fi
       mkdir -p /data/debian/android/$x
       mount -o bind /$x /data/debian/android/$x
   done
fi

# start debian ssh
export PATH=/bin:/usr/bin:/sbin:/usr/sbin:$PATH
# must ensure linux standard paths come before android paths, or else the wrong
# version will be used and cause segfault, because the /system/ etc. also are
# mounted (and symlinked) under the debian chroot jail.
echo 0 > /proc/sys/net/ipv4/icmp_echo_ignore_broadcasts

if test ! -e /data/debian/second-stage-done; then
    echo second-stage not done yet.
    exit 0
fi
chroot /data/debian /etc/init.d/ssh start

# mount sd in debian
if [ -d /mnt/sdcard/Android ] && [ -d /data/debian/mnt/sdcard/ ] && [ ! -d /data/debian/mnt/sdcard/Android ] ;
then
   mount -o bind /mnt/sdcard /data/debian/mnt/sdcard
fi
echo start debian done
