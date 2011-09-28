#!/bin/bash
set -ex


mkdir ~/external/bin/linux/ext/android-sdk-linux_86/google -p
cd ~/external/bin/linux/ext/android-sdk-linux_86/google

x=11
while true; do
    wget -N http://dl.google.com/android/android-sdk_r$x-linux_x86.tgz || break
    wget -N http://dl.google.com/android/installer_r$x-windows.exe || break
    wget -N http://dl.google.com/android/android-sdk_r$x-mac_x86.zip || break
    ((x++))
done&

while true; do
    wget -N http://dl.google.com/android/ndk/android-ndk-r4-linux-x86.zip
    wget -N http://dl.google.com/android/ndk/android-ndk-r4-darwin-x86.zip
    wget -N http://dl.google.com/android/ndk/android-ndk-r4-windows.zip
    break
done&

wget -N https://dl-ssl.google.com/android/repository/repository.xml

xmlstarlet sel -N\
     sdk="http://schemas.android.com/sdk/android/repository/3"\
     -B -t -m "//sdk:archive" -v "sdk:url" -o ':' -v "sdk:checksum"\
     -n repository.xml |\
perl -npe 's!(.*):(.*)!test `shasum </dev/null $1|awk "{print \\\\\$1}"`x = $2x && echo $1 already exist || (echo download $1;  lftp -c "pget -c -n 10 http://dl.google.com/android/repository/$1")!g'|bash -x
