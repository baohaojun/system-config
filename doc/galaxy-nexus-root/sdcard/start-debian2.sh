set -x
if [ ! -d /system/debian/usr ]; then
    mount -w -o loop -t ext4 /sdcard/debian.img /system/debian
fi

# Make tmp
if [ ! -d /tmp ]; then 
    mount -o rw,remount / && mkdir /tmp && mount -o ro,remount /
fi
if [ ! -f /tmp/.mounted ] ; then 
    mount -t tmpfs none /tmp && touch /tmp/.mounted
fi

# setup posix shared memory
if [ ! -d /dev/shm ] ; then mkdir /dev/shm ; fi
chmod 1777 /dev/shm
mount -t tmpfs none /dev/shm/

# configure debian chroot
if [ -d /system/debian/proc ] && [ ! -d /system/debian/proc/1 ] ; then
   mount -o bind /proc /system/debian/proc
fi

if [ -d /system/debian/sys ] && [ ! -d /system/debian/sys/class ] ; then
    mount -o bind /sys /system/debian/sys
fi

if [ -d /system/debian/dev/.udev ] ; then
   mount -o bind /dev /system/debian/dev
   mount -o bind /dev/pts /system/debian/dev/pts
   mount -o bind /dev/shm /system/debian/dev/shm
   for x in system data cache factory d acct; do
       mkdir -p /system/debian/android/$x
       mount -o bind /$x /system/debian/android/$x
   done
fi

# start debian ssh
export PATH=/bin:/usr/bin:/sbin:/usr/sbin:$PATH 
# must ensure linux standard paths come before android paths, or else the wrong
# version will be used and cause segfault, because the /system/ etc. also are
# mounted (and symlinked) under the debian chroot jail.

chroot /system/debian /usr/sbin/service openbsd-inetd start

# mount sd in debian
if [ -d /mnt/sdcard/Android ] && [ -d /system/debian/mnt/sdcard/ ] && [ ! -d /system/debian/mnt/sdcard/Android ] ; 
then
   mount -o bind /mnt/sdcard /system/debian/mnt/sdcard
fi


