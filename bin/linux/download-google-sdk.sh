#!/bin/bash
set -ex

function wget() {
    command wget -4 -t 2 "$@"
}

mkdir ~/external/bin/linux/ext/android-sdk-linux_86/google -p
test -e ~/external/bin/linux/ext/android-sdk-linux_86/google/do.not.download && exit 0
cd ~/external/bin/linux/ext/android-sdk-linux_86/google

x=16
while true; do
    wget -N http://dl.google.com/android/android-sdk_r$x-linux.tgz || break
    wget -N http://dl.google.com/android/installer_r$x-windows.exe || break
    wget -N http://dl.google.com/android/android-sdk_r$x-macosx.zip || break
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


for x in http://android-sdk-addons.motodevupdate.com/addons.xml \
    http://developer.lgmobile.com/sdk/android/repository.xml \
    http://developer.sonyericsson.com/edk/android/repository.xml \
    http://dl.htcdev.com/sdk/addon.xml \
    http://innovator.samsungmobile.com/android/repository/repository.xml \
    https://dl-ssl.google.com/android/repository/addon.xml \
    https://dl-ssl.google.com/android/repository/repository-5.xml \
    http://software.intel.com/sites/landingpage/android/addon.xml \
    http://www.echobykyocera.com/download/echo_repository.xml; do
    wget -N -r $x || true
done


xmlstarlet sel -N\
     sdk="http://schemas.android.com/sdk/android/repository/3"\
     -B -t -m "//sdk:archive" -v "sdk:url" -o ':' -v "sdk:checksum"\
     -n dl-ssl.google.com/android/repository/repository.xml |\
perl -npe 's!(.*):(.*)!test `shasum </dev/null $1|awk "{print \\\\\$1}"`x = $2x && echo $1 already exist || (echo download $1; wget -N http://dl.google.com/android/repository/$1)!g'|bash -x

xmlstarlet sel -N\
     sdk="http://schemas.android.com/sdk/android/addon/3"\
     -B -t -m "//sdk:archive" -v "sdk:url" -o ':' -v "sdk:checksum"\
     -n dl-ssl.google.com/android/repository/addon.xml |\
perl -npe 's!(.*):(.*)!test `shasum </dev/null $1|awk "{print \\\\\$1}"`x = $2x && echo $1 already exist || (echo download $1; wget -N http://dl.google.com/android/repository/$1)!g'|bash -x

xmlstarlet sel -N\
     sdk="http://schemas.android.com/sdk/android/repository/5"\
     -B -t -m "//sdk:archive" -v "sdk:url" -o ':' -v "sdk:checksum"\
     -n dl-ssl.google.com/android/repository/repository-5.xml |\
perl -npe 's!(.*):(.*)!test `shasum </dev/null $1|awk "{print \\\\\$1}"`x = $2x && echo $1 already exist || (echo download $1; wget -N http://dl.google.com/android/repository/$1)!g'|bash -x

cd ../temp
ln ../google/* . -f


