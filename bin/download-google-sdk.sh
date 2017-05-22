#!/bin/bash
set -ex

. ensure-at-work


## start code-generator "^\\s *#\\s *"
# generate-getopt @update-xmls=true
## end code-generator
## start generated code
TEMP=$(getopt -o h \
              --long update-xmls,help,no-update-xmls \
              -n $(basename -- $0) -- "$@")
update_xmls=true
eval set -- "$TEMP"
while true; do
    case "$1" in

        --update-xmls|--no-update-xmls)
            if test "$1" = --no-update-xmls; then
                update_xmls=false
            else
                update_xmls=true
            fi
            shift
            ;;
        -h|--help)
            set +x
            echo
            echo
            echo Options and arguments:
            printf "%06s" " "
            printf %-24s '--[no-]update-xmls'
            echo
            exit
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error"
            ;;
    esac
done

## end generated code

if ! is-tty-io && test -z "$http_proxy"; then
    export http_proxy=http://localhost:19999
    export https_proxy=$http_proxy
fi

function wget() {
    command wget -4 -t 2 "$@"
}

export -f wget

if test -e ~/external/bin/Linux/ext/android-sdk-linux/google/do.not.download; then
    mkdir -p ~/external/bin/Linux/ext/android-sdk-linux/google
    . mount-share-folders
fi
if test -e ~/external/bin/Linux/ext/android-sdk-linux/google/download-all; then
    export DOWNLOAD_ALL=true
fi
test ! -e ~/external/bin/Linux/ext/android-sdk-linux/google/do.download && exit 0
cd ~/external/bin/Linux/ext/android-sdk-linux/google

if test "$update_xmls" = true; then
    rm -rf sdk.html
    wget -O sdk.html https://developer.android.com/studio/index.html

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
fi

function check-shasum-or-download() {
    if test -s $(basename $1).shasum; then
        touch $(basename $1).shasum
        return;
    fi

    ## start code-generator "^\\s *#\\s *"
    # generate-getopts -l a:algo=shasum
    ## end code-generator
    ## start generated code

    local algo=shasum
    local OPTIND=1
    while getopts "a:h" opt; do
        case "$opt" in

            a) algo=$OPTARG ;;
            h)
                echo
                echo
                printf %06s%s '-a ' 'algo'
                printf %-24s 'ALGO'
                echo ''
                shift
                exit 0
                ;;
            *)
                echo
                echo
                printf %06s%s '-a ' 'algo'
                printf %-24s 'ALGO'
                echo ''
                exit 2
                ;;
        esac
    done

    shift $((OPTIND - 1))

    ## end generated code

    if test $($algo </dev/null $(basename $1)|pn 1)x = ${2}x; then
        echo $1:$2 > $(basename $1).shasum
        echo $1 already downloaded
        return
    fi
    xhost=$3
    set -e
    (echo download $1; if [[ $1 =~ :// ]]; then wget -c $1; else wget -c http://$xhost/$(basename $1); fi)
    if test $($algo </dev/null $(basename $1)|pn 1)x != ${2}x; then
        rm -f $(basename $1)
        if [[ $1 =~ :// ]]; then wget -c $1; else wget -c http://$xhost/$(basename $1); fi
    fi

    if test $($algo </dev/null $(basename $1)|pn 1)x = ${2}x; then
        echo $1:${2} > $(basename $1).shasum
    fi
}

export -f check-shasum-or-download

(
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
    $hash{$file} = {
        version => $version,
        file => $file,
        cs => $cs,
    };

    END {
        for (keys %hash) {
            print "$hash{$_}{file}:$hash{$_}{cs}\n";
        }
    }
' | grep -v '^:$' | perl -npe 's,(.*):(.*),check-shasum-or-download $1 $2 '$xhost',g'|grep -i -e "${vpattern:-shit}" -v || true
    done
    for studio_url in $(grep -P -o -e 'https?://dl(-ssl)?.google.com/dl/android/studio/.*?"' sdk.html | perl -npe 's/"//'); do
        grep '^[0-9a-f]{64}\s' -P sdk.html | while read sha256 basename; do
            if test "$basename" = "$(basename "$studio_url")"; then
                echo check-shasum-or-download -a sha256sum -- "$studio_url" "$sha256"
                break
            fi
        done
    done
)|sort -u -R|xargs -d \\n -P 3 -n 1 bash -c 'for x in "$@"; do bash -x -c "$x"; done' true

mkdir -p ../temp

relative-link -f ./* ~/external/bin/Linux/ext/android-sdk-linux/temp
