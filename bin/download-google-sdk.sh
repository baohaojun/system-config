#!/bin/bash
set -ex

function wget() {
    command wget -4 -t 2 "$@"
}

mkdir ~/external/bin/Linux/ext/android-sdk-linux/google -p
test -e ~/external/bin/Linux/ext/android-sdk-linux/google/do.not.download && exit 0
cd ~/external/bin/Linux/ext/android-sdk-linux/google

x=$(if test -e sdk.ver; then cat sdk.ver; else echo 21; fi)
while true; do
    wget -N http://dl.google.com/android/android-sdk_r$x-linux.tgz || break
    if tty >/dev/null 2>&1; then
        wget -N http://dl.google.com/android/installer_r$x-windows.exe || break
        wget -N http://dl.google.com/android/android-sdk_r$x-macosx.zip || break
    fi
    ((x++))
done&

(
    x=$(if test -e ndk.ver; then cat ndk.ver; else echo 8; fi)
    ((x++))
    while true; do
	wget -N http://dl.google.com/android/ndk/android-ndk-r$x-linux-x86.tar.bz2 || break
	wget -N http://dl.google.com/android/ndk/android-ndk-r$x-darwin-x86.tar.bz2
	wget -N http://dl.google.com/android/ndk/android-ndk-r$x-windows.zip
	((x++))
    done

    ((x--))
    for r in f e d c b; do
	wget -N http://dl.google.com/android/ndk/android-ndk-r$x$r-linux-x86.tar.bz2 || continue
        if tty >/dev/null 2>&1; then
            break;
        fi
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
    http://software.intel.com/sites/landingpage/android/addon.xml \
    http://www.echobykyocera.com/download/echo_repository.xml; do
    wget -N -r $x || true
done

for x in $(seq 5 1000); do
    wget -N -r https://dl-ssl.google.com/android/repository/repository-$x.xml || break
done

((x--))

if tty >/dev/null 2>&1; then
    vpattern='windows\|macos'
else
    vpattern=shit
fi

for x in dl-ssl.google.com/android/repository/*.xml; do
    xmlstarlet sel -N \
        $(cat $x|grep -P -e 'sdk=".*?"' -o|perl -npe 's/"//g') \
        -B -t -m "//sdk:archive" -v "sdk:url" -o ':' -v "sdk:checksum" \
        -n $x    
done | sort -u | perl -npe 's!(.*):(.*)!test `shasum </dev/null \$(basename $1)|awk "{print \\\\\$1}"`x = $2x && echo $1 already exist || (echo download $1; if [[ $1 =~ :// ]]; then wget -N $1; else wget -N http://dl.google.com/android/repository/\$(basename $1); fi)!g'|grep -i -e "$vpattern" -v|bash -x

mkdir -p ../temp
cd ../temp
ln ../google/* . -f


