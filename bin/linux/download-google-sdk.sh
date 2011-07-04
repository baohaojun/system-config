#!/bin/bash
set -ex


mkdir ~/external/bin/linux/ext/android-sdk-linux_86/google -p
cd ~/external/bin/linux/ext/android-sdk-linux_86/google
wget -N http://www.crystax.net/data/android-ndk-r4-linux-x86-crystax-4.tar.bz2

wget -N http://dl.google.com/android/android-sdk_r11-linux_x86.tgz
wget -N http://dl.google.com/android/installer_r11-windows.exe
wget -N http://dl.google.com/android/android-sdk_r11-mac_x86.zip

wget -N http://dl.google.com/android/ndk/android-ndk-r4b-linux-x86.zip
wget -N http://dl.google.com/android/ndk/android-ndk-r4b-darwin-x86.zip
wget -N http://dl.google.com/android/ndk/android-ndk-r4b-windows.zip

wget -N https://dl-ssl.google.com/android/repository/repository.xml

xmlstarlet sel -N\
     sdk="http://schemas.android.com/sdk/android/repository/3"\
     -B -t -m "//sdk:archive" -v "sdk:url" -o ':' -v "sdk:checksum"\
     -n repository.xml |\
perl -npe 's!(.*):(.*)!test `shasum </dev/null $1|awk "{print \\\\\$1}"`x = $2x && echo $1 already exist || (echo download $1;  lftp -c "pget -c -n 10 https://dl-ssl.google.com/android/repository/$1")!g'|bash -x
