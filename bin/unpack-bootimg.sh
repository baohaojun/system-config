#!/usr/bin/env bash

set -e

## start code-generator "^\\s *#\\s *"
# generate-getopt b:boot-img d:unpack-dir
## end code-generator
## start generated code
TEMP=$( getopt -o b:d:h \
               --long boot-img:,unpack-dir:,help \
               -n $(basename -- $0) -- "$@")
boot_img=
unpack_dir=
eval set -- "$TEMP"
while true; do
    case "$1" in

        -b|--boot-img)
            boot_img=$2
            shift 2
            ;;
        -d|--unpack-dir)
            unpack_dir=$2
            shift 2
            ;;
        -h|--help)
            set +x
            echo -e
            echo
            echo Options and arguments:
            printf %06s '-b, '
            printf %-24s '--boot-img=BOOT_IMG'
            echo
            printf %06s '-d, '
            printf %-24s '--unpack-dir=UNPACK_DIR'
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

mkdir -p $unpack_dir
boot_img=$(readlink -f $boot_img)

cd $unpack_dir
unbootimg "$boot_img"

mkdir -p ramdisk
cd ramdisk
cat ../ramdisk.img | gunzip |cpio -i
