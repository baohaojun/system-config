#!/bin/bash
set -ex


mkdir ~/external/bin/linux/ext/android-sdk-linux_86/google -p
cd ~/external/bin/linux/ext/android-sdk-linux_86/google

x=13
while true; do
    wget -N http://dl.google.com/android/android-sdk_r$x-linux_x86.tgz || break
    wget -N http://dl.google.com/android/installer_r$x-windows.exe || break
    wget -N http://dl.google.com/android/android-sdk_r$x-mac_x86.zip || break
    ((x++))
done&

(
    x=6
    while true; do
	wget -N http://dl.google.com/android/ndk/android-ndk-r$x-linux-x86.tar.bz2 || break
	wget -N http://dl.google.com/android/ndk/android-ndk-r$x-darwin-x86.tar.bz2
	wget -N http://dl.google.com/android/ndk/android-ndk-r$x-windows.zip
	((x++))
    done

    ((x--))
    for r in f e d c b; do
	wget -N http://dl.google.com/android/ndk/android-ndk-r$x$r-linux-x86.tar.bz2 || continue
	wget -N http://dl.google.com/android/ndk/android-ndk-r$x$r-darwin-x86.tar.bz2 || continue
	if wget -N http://dl.google.com/android/ndk/android-ndk-r$x$r-windows.zip; then break; fi
    done
)&

wget -N https://dl-ssl.google.com/android/repository/repository.xml

xmlstarlet sel -N\
     sdk="http://schemas.android.com/sdk/android/repository/3"\
     -B -t -m "//sdk:archive" -v "sdk:url" -o ':' -v "sdk:checksum"\
     -n repository.xml |\
perl -npe 's!(.*):(.*)!test `shasum </dev/null $1|awk "{print \\\\\$1}"`x = $2x && echo $1 already exist || (echo download $1;  lftp -c "pget -c -n 10 http://dl.google.com/android/repository/$1")!g'|bash -x

cd ../temp
ln ../google/* . -f
