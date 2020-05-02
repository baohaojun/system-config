#!/usr/bin/env bash
set -e

me=$(readlink -f $BASH_SOURCE)
if test ! -e "$me"; then
    me=$(readlink -f "$(which $BASH_SOURCE)")
    if test ! -e "$me"; then
        die "Can't find out about me"
        exit 1
    fi
fi

abs0=$BASH_SOURCE
if ! [[ $abs0 =~ ^/ ]]; then
    if [[ $abs0 =~ / ]] && test -e $PWD/$abs0; then
        abs0=$PWD/$abs0
    elif test -e "$(which $BASH_SOURCE)"; then
        abs0=$(which $BASH_SOURCE)
    else
        die "Can't find abs path for $BASH_SOURCE"
    fi
fi

b0=$(basename $BASH_SOURCE)

cd $(dirname $me)


## start code-generator "^\\s *#\\s *"
# generate-getopt t:build-type=debug
## end code-generator
## start generated code
TEMP=$( getopt -o t:h \
               --long build-type:,help \
               -n $(basename -- $0) -- "$@")
declare build_type=debug
eval set -- "$TEMP"
while true; do
    case "$1" in

        -t|--build-type)
            build_type=$2
            shift 2

            ;;
        -h|--help)
            set +x
            echo -e
            echo
            echo Options and arguments:
            printf %06s '-t, '
            printf %-24s '--build-type=BUILD_TYPE'
            echo
            exit
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error: $(. bt; echo; bt | indent-stdin)"
            ;;
    esac
done


## end generated code


if test "${build_type}" = debug; then
    ./gradlew assembleDebug
    apk=app/build/outputs/apk/debug/app-debug.apk
else
    ./gradlew aR
    apk=app/build/outputs/apk/release/app-release.apk


    if ! which zipalign; then
        if test ! -e ~/.local-config/bin/.ab; then
            die "Can't find android build tools, please add zipalign & zipsigner into PATH"
        else
            . .ab
        fi
    fi

    rm -f app/build/outputs/apk/release/app-release-aligned.apk "${apk}"

    zipalign -v 4 \
             app/build/outputs/apk/release/app-release-unsigned.apk \
             app/build/outputs/apk/release/app-release-aligned.apk

    for x in ${ANDROID_HOME}/build-tools/*; do
        if test -d $x; then
            PATH=$x:$PATH
        fi
    done

    apksigner sign --ks ~/.keystore/bhj.keystore --out \
              ${apk} \
              app/build/outputs/apk/release/app-release-aligned.apk
fi

adb uninstall com.bhj.setclip
adb install ${apk}

. .before-install-hook

my-adb 'rm -f /sdcard/setclip-apk.txt; am startservice --user 0 -n com.bhj.setclip/.PutClipService --ei getapk 1'
sleep 1
my-adb kill app_process || true
my-adb sh /sdcard/setclip-apk.txt app_process /system/bin/ com.Wrench.Input
