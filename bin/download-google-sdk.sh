#!/bin/bash
set -ex

function wget() {
    command wget -4 -t 2 "$@"
}

mkdir ~/external/bin/Linux/ext/android-sdk-linux/google -p
test -e ~/external/bin/Linux/ext/android-sdk-linux/google/do.not.download && exit 0
cd ~/external/bin/Linux/ext/android-sdk-linux/google

rm -rf sdk.html
wget -O sdk.html http://developer.android.com/sdk/index.html

for x in $(grep -P -o -e 'http://dl.google.com/android/.*?"' sdk.html | perl -npe 's/"//'); do
    if ! echo $x|grep adt-; then
        wget -N $x
    fi
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
    http://dl.google.com/android/repository/addon.xml \
    http://software.intel.com/sites/landingpage/android/addon.xml \
    http://www.echobykyocera.com/download/echo_repository.xml; do
    wget -N -r $x || true
done

re_ok=false
first_good_sdk=5
if test -e ~/.logs/first-good-sdk-xml.n; then
    first_good_sdk=$(cat ~/.logs/first-good-sdk-xml.n)
fi
for x in $(seq $first_good_sdk 1000); do
    if wget -N -r http://dl.google.com/android/repository/repository-$x.xml; then
        if test $re_ok = false; then
            echo $x > ~/.logs/first-good-sdk-xml.n
        fi
        re_ok=true;
    else
        if test $re_ok = true; then
            break
        fi
    fi
done

((x--))

if tty >/dev/null 2>&1; then
    vpattern='windows\|macos'
else
    vpattern=shit
fi

for x in dl.google.com/android/repository/*.xml; do
    xmlstarlet sel -N \
        $(cat $x|grep -P -e 'sdk=".*?"' -o|perl -npe 's/"//g') \
        -B -t -m "//sdk:archive" -v "sdk:url" -o ':' -v "sdk:checksum" \
        -n $x
done |perl -ne '
    BEGIN {
        sub debug {
            print STDERR "@_\n";
        }
    }
    chomp;
    ($file, $cs) = split /:/;
    $non_version = qx(extract-nonversion $file);
    $version = qx(extract-version $file);

    $cur_ver = $hash{$non_version}{version} || 0;

    if (qx(compare-version $cur_ver $version) eq "<") {
        $hash{$non_version} = {
            version => $version,
            file => $file,
            cs => $cs,
        }
    }

    END {
        for (keys %hash) {
            print "$hash{$_}{file}:$hash{$_}{cs}\n";
        }
    }
' | grep -v '^:$' | sort -u | perl -npe 's!(.*):(.*)!test `shasum </dev/null \$(basename $1)|awk "{print \\\\\$1}"`x = $2x && echo $1 already exist || (echo download $1; if [[ $1 =~ :// ]]; then wget -N $1; else wget -N http://dl.google.com/android/repository/\$(basename $1); fi)!g'|grep -i -e "$vpattern" -v|bash -x

mkdir -p ../temp
cd ../temp
ln ../google/* . -f || true
