#!/bin/bash

set -e
user=$1
utc=$2

exec 2>&1
PATH=$(
    for x in $(ls /home/$user/system-config/etc/path/Linux-x86_64 | sort -n); do
        readlink -f /home/$user/system-config/etc/path/Linux-x86_64/"$x"|grep . && echo : || true
    done | perl -npe 's/\n//';
    )$PATH
smb=$(get-vmlinux -u $utc|perl -npe 's/:\n/: /'|grep 'Flashing binary' | pn 3)
echo smb is $smb

mnt=$(echo "$smb" | perl -npe 's,smb://share.smartisan.cn,/mnt,; s,/mnt/SanFranciscoBuild,/mnt/sfodailybuild,')

if test -d "$mnt"; then
    echo $mnt

    root=$(echo $mnt | perl -npe 's,(/mnt/.*?)/.*?/,$1/adb-root/,')
    if test "$root" = "$mnt"; then
        die "error: adb-root.sh will write into the same dir $mnt"
    fi

    mkdir -p "$root"
    command rsync $mnt/ $root --exclude "system_*.img" --exclude "userdata_*.img" -av --progress
    cd "$root"

    HOME=/home/$user replace-bootimage -b "$(find . -name 'boot.img')" -- -d
    xml=rawprogram_unsparse.xml
    if test ! -e rawprogram_unsparse.xml; then
        xml=sparse_images/rawprogram_unsparse.xml
    fi

    perl -ne 'print unless m/userdata_/' -i $xml
    perl -ne 'print unless m/alterable.bin/' -i $xml
    perl -ne 'print unless m/system_/' -i $xml

    echo
    echo you will be able to root your device by flashing "$(echo $smb | perl -npe 's,(smb://.*?/.*?)/.*?/,$1/adb-root/,')"
    echo
    echo PLEASE MAKE SURE THAT YOU ARE FLASHING ON TO THE RIGHT PHONE
else
    die "error: $mnt is not found"
fi

# Local Variables: #
# after-save-hook: (lambda () (shell-command-to-string "psync jbuild1 adb-root.sh&")) #
# End: #
