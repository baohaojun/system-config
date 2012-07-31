touch /data/shit
mount -o rw,remount -t ext4 /dev/block/platform/omap/omap_hsmmc.0/by-name/system /system
rm /system/bin/su
rm /system/xbin/su
mkdir /system/xbin
cat /superboot/su>/system/xbin/su
chmod 6755 /system/xbin/su
cat /superboot/Superuser.apk>/system/app/Superuser.apk
mount -o ro,remount -t ext4 /dev/block/platform/omap/omap_hsmmc.0/by-name/system /system
