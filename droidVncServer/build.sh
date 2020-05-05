#!/usr/bin/env bash

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


set -e
## start code-generator "^\\s *#\\s *"
# generate-getopt -P d:build-dir=~/src/android-google
## end code-generator
## start generated code
TEMP=$(POSIXLY_CORRECT=true getopt -o d:h \
                      --long build-dir:,help \
                      -n $(basename -- $0) -- "$@")
declare build_dir=~/src/android-google
eval set -- "$TEMP"
while true; do
    case "$1" in

        -d|--build-dir)
            build_dir=$2
            shift 2

            ;;
        -h|--help)
            set +x
            echo -e
            echo
            echo Options and arguments:
            printf %6s '-d, '
            printf %-24s '--build-dir=BUILD_DIR'
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

cd "$build_dir"

# 必须在 vendor 下编，因为从 android pie 开始，vendor，device 两个目录 warning 不报错，其他全部报错。
if test -L ./vendor/droidVncServer; then
    rm -f ./vendor/droidVncServer
fi

rsync -a ~/src/github/Wrench/droidVncServer ./vendor

(
    if [[ $* =~ ' -q' ]]; then
        true
    else
        android-set-product aosp_arm64-user
    fi
    (
        cd vendor/droidVncServer
        printf -- "-*- mode: compilation -*-\nUsing short format, Entering directory \`%s'\n" ${abs0%/*}/..
        mma -j20 "$@" 2>&1
    ) | tee build.log
)
rsync -a ./vendor/droidVncServer $(dirname $abs0)/..
