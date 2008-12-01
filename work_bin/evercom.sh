#!/bin/bash

function Usage()
{
    Cry "$1"
    echo Usage: $0 [options]
    echo 
    echo Main options:
    echo -e -r ROOTFS \\t\\t the rootfs that is to be used, such as 10.193.53.23:/home/a22242/rootfs
    echo -e -z ZIMAGE \\t\\t the zimage that is to be used, such as zImage_iB20042
    echo -e -i IPADDR \\t\\t the ip address to assign to the board, such as 10.193.53.201
    echo -e -m MOREOP \\t\\t more options passed to the boot command line
    echo -e -h \\t\\t\\t print this help
    exit
}

function Cry()
{
    ! [[ -z $1 ]] && (echo Error: $0 "$1"; echo)
}

function Exit()
{
    Cry "$1"
    exit
}
	
function Debug()
{
    ! [[ -z $debug ]] && (echo Debug: "$@"; echo)
}

ROOTFS=10.193.53.25:/home/a22242/project/rootfs
IPADDR=10.193.53.201
MOREOP=`cat /users/a22242/.p5ops`
#unset ZIMAGE
ZIMAGE=a22242

TERMS=`getopt -o r:z:i:m:h -- "$@"`
eval set -- "$TERMS"

while true; do
    case "$1" in 
	-r)
	    ROOTFS="$2"
	    shift; shift
	    ;;
	-z)
	    ZIMAGE=`ls /tftpboot/*"$2"*`
	    Debug "$ZIMAGE"
	    ZIMAGE=`basename  "$ZIMAGE"`
	    ! [[ -f /tftpboot/"$ZIMAGE" ]] && Exit "ZIMAGE can't be found"
	    shift; shift
	    ;;
	-i) 
	    IPADDR="$2"
	    shift; shift
	    ;;
	-m)
	    MOREOP="$2"
	    shift; shift
	    ;;
	-h)
	    Usage;
	    exit
	    ;;
	--) 
	    shift 
	    break
	    ;;
	*)
	    echo "Internal error!"
	    exit 1;
	    ;;
    esac
done

! [[ -z $1 ]] && Usage "redundent arguments $@"

[[ -z $ZIMAGE ]] && Usage "ZIMAGE can't be empty!"

echo ~a22242/bin/.evercom.ep $ZIMAGE $ROOTFS $IPADDR $MOREOP

if [[ -f ~/bin/getsudo.ep ]]; then getsudo.ep; fi
sudo killall minicom -9 && sleep 3
#/home/a22242/killall minicom -9 && sleep 3

clear
expect -f ~a22242/bin/.evercom.ep $ZIMAGE $ROOTFS $IPADDR $MOREOP
