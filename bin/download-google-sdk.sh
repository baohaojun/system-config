#!/bin/bash
set -ex

if ! is-tty-io && test -z "$http_proxy"; then
    bash -x start-http-proxy $0
    exit
fi

function wget() {
    command wget -4 -t 2 "$@"
}

export -f wget

if ! mkdir ~/external/bin/Linux/ext/android-sdk-linux/google -p; then
    mount-share-folders
    mkdir ~/external/bin/Linux/ext/android-sdk-linux/google -p
fi
if test -e ~/external/bin/Linux/ext/android-sdk-linux/google/download-all; then
    export DOWNLOAD_ALL=true
fi
test -e ~/external/bin/Linux/ext/android-sdk-linux/google/do.not.download && exit 0
cd ~/external/bin/Linux/ext/android-sdk-linux/google

rm -rf sdk.html
wget -O sdk.html http://developer.android.com/sdk/index.html

for x in $(grep -P -o -e 'https?://dl(-ssl)?.google.com/android/.*?"' sdk.html | perl -npe 's/"//'); do
    if ! echo $x|grep adt-; then
        wget -N $x
    fi
done&

wget -N -r http://dl-ssl.google.com/android/repository/addons_list-2.xml

for x in $(
              xmlstarlet sel -N sdk="http://schemas.android.com/sdk/android/addons-list/2" -t -v //sdk:url ./dl-ssl.google.com/android/repository/addons_list-2.xml |
                  while read path; do
                      if [[ $path =~ https?:// ]]; then
                          echo $path
                      else
                          echo http://dl-ssl.google.com/android/repository/$path
                      fi
                  done
          )
do
    wget -N -r $x || true
done

re_ok=false
first_good_sdk=8
if test -e ~/.cache/system-config/logs/first-good-sdk-xml.n; then
    first_good_sdk=$(cat ~/.cache/system-config/logs/first-good-sdk-xml.n)
fi
fail_times=0
for x in $(seq $first_good_sdk 1000); do
    if wget -N -r http://dl-ssl.google.com/android/repository/repository-$x.xml; then
        if test $re_ok = false; then
            echo $x > ~/.cache/system-config/logs/first-good-sdk-xml.n
        fi
        re_ok=true;
    else
        ((fail_times++)) || true
        if test $fail_times -lt 5; then
            continue
        fi
        if test $re_ok = true; then
            break
        fi
    fi
done

if tty >/dev/null 2>&1; then
    vpattern='windows\|macos'
else
    vpattern=shit
fi

function check-shasum-or-download() {
    if test ! -e $(basename $1).shasum; then
        if test $(shasum </dev/null $(basename $1)|pn 1)x = ${2}x; then
            echo $1 already downloaded
            touch $(basename $1).shasum
            return
        fi
    fi
    xhost=$3
    set -e
    (echo download $1; if [[ $1 =~ :// ]]; then wget -c $1; else wget -c http://$xhost/$(basename $1); fi)
    if test $(shasum </dev/null $(basename $1)|pn 1)x = ${2}x; then
        rm -f $(basename $1)
        if [[ $1 =~ :// ]]; then wget -c $1; else wget -c http://$xhost/$(basename $1); fi
    fi

    if test $(shasum </dev/null $(basename $1)|pn 1)x = ${2}x; then
        touch $(basename $1).shasum
    fi
}

export -f check-shasum-or-download

for x in $(find *.google.com -name '*.xml'); do
    xhost=$(dirname $x)
    xmlstarlet sel -N \
        $(cat $x|grep -P -e 'sdk=".*?"' -o|perl -npe 's/"//g') \
        -B -t -m "//sdk:archive" -v "sdk:url" -o ':' -v "sdk:checksum" \
        -n $x | perl -ne '
    BEGIN {
        sub debug {
            print STDERR "@_\n";
        }
    }
    chomp;
    debug "got a line: $_";
    s,https?://'$xhost'/,,;
    ($file, $cs) = split /:/;
    $non_version = qx(extract-nonversion $file);
    $version = qx(extract-version $file);

    $cur_ver = $hash{$non_version}{version} || 0;

    if ($ENV{DOWNLOAD_ALL} eq "true") {
        $hash{$file} = {
            version => $version,
            file => $file,
            cs => $cs,
        }
    } elsif (qx(compare-version $cur_ver $version) eq "<") {
        $hash{$non_version} = {
            version => $version,
            file => $file,
            cs => $cs,
        }
    } else {
        debug "$file version $version is old than $cur_ver, not selected"
    }

    END {
        for (keys %hash) {
            print "$hash{$_}{file}:$hash{$_}{cs}\n";
        }
    }
' | grep -v '^:$' | sort -u | perl -npe 's,(.*):(.*),check-shasum-or-download $1 $2 '$xhost',g'|grep -i -e "${vpattern:-shit}" -v || true
done |sort -R|xargs -d \\n -P 3 -n 1 bash -c 'for x in "$@"; do bash -x -c "$x"; done' true

mkdir -p ../temp

relative-link -f ./* ~/external/bin/Linux/ext/android-sdk-linux/temp
