#!/bin/bash

CMD_UTILITY="/vob/ilj_release/devkit/pck/utility.sh"
CMD_CREATEIMAGESH="/vob/ilj_release/devkit/pck/createimage.sh"
PROG_NAME=$0
CFGFILE_DIR_PREFIX=/org/mirs/release/su/salsa_ap/
CFGFILE_DIR_POSTFIX=/lab/arm/kernel/image.cfg

TEMP_CFG_FILE=/tmp/image.cfg.`whoami`
TEMP_OPTARGS=/tmp/optargs.`whoami`;
ILJ_OPT_SCRIPT=/vob/ilj_release/devkit/pck/ilj_opt_script.sh

unset x
unset PROJROOT

function Exit()
{
    echo "$1"
    #mv $TMPDIR/*xml $PROJROOT 
    #rm $TMPDIR -rf
    exit
}

function Usage()
{
    if ! [[ -z $1 ]]; then 
        echo "$@"
    fi
    echo "Usage: $PROG_NAME -d DIR [[-s S19]...] -c CFG"
    echo
    echo Main Optons:
    echo "-help     print this message"
    echo "-d DIR    the rootfs you want to create image for"
    echo "-s S19    the image name you want to create .s19, for e.g. rootfs/sysconf/user"
    echo "          NOTE, you can specify multiple ones, like $PROG_NAME -s rootfs -s sysconf"
    echo "-c CFG    the image.cfg file you want to use"		
    Exit    
}

function MoreArgOrExit()
{
    if [[ -z $1 ]]; then
        Usage
    fi
}

while true; do 
    case "$1" in
        -help)
            Usage
            ;;
        -d)
            shift
            MoreArgOrExit $1
            PROJROOT=$1
            shift
            ;;
        -s)
            shift
            MoreArgOrExit $1
            S19_LIST[x]=$1
            ((x++))
            shift
            ;;
        -c)
            shift
            MoreArgOrExit $1
            CFG_FILE=$1
            shift
            ;;
        ?*)
            Usage
            ;;
        *)
            break
            ;;
    esac
done

if [[ -z $PROJROOT ]]; then 
    Usage "error: project root directory not specified"
fi

[[ -d $PROJROOT ]] || Usage "error: $PROJROOT is not a directory"

if ! [[ -f "$CFG_FILE" ]]; then
    CFG_FILE=`ls $CFGFILE_DIR_PREFIX$CFG_FILE$CFGFILE_DIR_POSTFIX 2>/dev/null` 
    [[ -f $CFG_FILE ]] ||\
        Usage "error: can't access config file $CFG_FILE"
    
fi

perl -npe 'chomp; s/^#.*//g; s/,\s*/,/g; s/^\s*(.*),.*/n,$1\n/g' $CFG_FILE > $TEMP_CFG_FILE

# the above line will compile $CFG_FILE into the following form:

#  ROOT_FILESYS,    0xB8200000,   0xB99FFFFF,   rootfs,        squashfs,  /,         Applications, libraries etc.
# #Unused,          0xB9A00000,   0xB9EC7FFF,   unused1,       ,          N/A,       
#  RESOURCES,       0xB9EC8000,   0xBA1C7FFF,   phx_data,      squashfs,  /phx/data, Embedded Java apps, midas resources, shared resources
#  CSD,             0xBA1C8000,   0xBA1CBFFF,   phx_csd,       squashfs,  /phx/csd,  CSD image

# ======>

# y,ROOT_FILESYS,0xB8200000,0xB99FFFFF,rootfs,squashfs,/,Applications
# n,RESOURCES,0xB9EC8000,0xBA1C7FFF,phx_data,squashfs,/phx/data,Embedded Java apps,midas resources
# n,CSD,0xBA1C8000,0xBA1CBFFF,phx_csd,squashfs,/phx/csd
# n,BP_IMAGE,0xBA1CC000,0xBA4CBFFF,bp,squashfs,/phx/bp

for x in "${S19_LIST[@]}"; do 
    perl -npe 'if (/'$x'/) {s/^n/y/;}' -i $TEMP_CFG_FILE;
done

#cat $TEMP_CFG_FILE

#echo 
#echo "project root is $PROJROOT"
#echo "    and image.cfg is $CFG_FILE"

mkdir -p $PROJROOT/image || exit
echo -- "-t $PROJROOT/rootfs -u `whoami`" > $TEMP_OPTARGS || exit

getsudo.ep

sudo /vob/ilj_release/devkit/pck/createimage.sh rootfs2image -g $ILJ_OPT_SCRIPT -o $TEMP_OPTARGS $TEMP_CFG_FILE $PROJROOT $PROJROOT/image

for x in "${S19_LIST[@]}"; do
    perl -npe 'chomp; s/^#.*//g; s/,\s*/,/g; s/^\s*(.*),.*/n,$1\n/g' $CFG_FILE > $TEMP_CFG_FILE
    perl -npe 'if (/'$x'/) {s/^n/y/;}' -i $TEMP_CFG_FILE;
    $CMD_UTILITY gensrecord -c ${TEMP_CFG_FILE} -i ${PROJROOT}/image -o ${PROJROOT}/image
done
