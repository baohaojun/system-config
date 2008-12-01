#!/bin/bash

unset REPOSVERSION

function Usage() 
{
    echo Usage: $0 [options]
    echo 
    echo Main options:
    echo -e -d DIR \\t\\tthe directory the rootfs will be installed, the \"project directory\" in pck, where the .mip file will be found
    echo -e -a ARC \\t\\tthe architecture of the rootfs, is it x86\? or is it arm\?
    echo -e -v VER \\t\\tthe version of the rootfs, such as 39\? 39A\?
    echo -e -r REPV \\tthe version of the repostory, such as 44 43, if xml is used, it is not important, but must be a valid one.
    echo -e -x XML \\t\\tthe xml you want to fetch from plhp037 and the use, such as BINA*FLASH \| BINA\*NFS \| DEVELOP
    echo -e -X XMLFILE \\tthe xml file you want use, on the local file system. a plain package list file is also acceptable
    echo -e -h \\t\\tdisplay this simple help
}

function Debug()
{
    if [[ $debug == debug ]]; then
	echo "$1"
	export PROJROOT="$PROJROOT"
	export PLATFORM="$PLATFORM"
	export TMPDIR="$TMPDIR"
	export DISTBASE=/vob/ilj_release/informal/salsa
	export IBVERSION="$IBVERSION"
	bash
    fi

}

function Exit()
{
    echo "$1"
    mv $TMPDIR/*xml $PROJROOT 2>/dev/null
    rm $TMPDIR -rf
    exit
}

function BestBaseName()
{
    echo "`basename \"$1\"`"
}

DISTBASE=/vob/ilj_release/informal/salsa
PROJROOT=`readlink -f ~/rootfs`/
PROJROOT=`readlink -f "$PROJROOT"/..`
ARCH=arm
for VERSION in "$DISTBASE"/"$ARCH"*/*IB*; do true; done
VERSION="`BestBaseName $VERSION|sed -e 's/RPMS.IB2.00.//g'`"

if [[ -f ~/.mkrootfs.rc ]]
    then
    . ~/.mkrootfs.rc
fi

TERMS=`getopt -o r:d:a:v:x:X:h -- "$@"`
eval set -- "$TERMS"

while true; do
    case "$1" in
	-d)
	    y=`echo $2|cut -b 1`
	    if [[ $y == / ]]
		then 
		PROJROOT="$2"
		else
		PROJROOT="`pwd`/$2"
	    fi
	    shift; shift;
	    ;;
	-a)
	    ARCH="$2"
	    shift; shift;
	    ;;
	-v)
	    VERSION="$2"
	    shift; shift;
	    ;;
	-r) 
	    REPOSVERSION="$2"
	    shift; shift;
	    ;;
	
	-x) 
	    XML="$2"
	    shift; shift;
	    ;;
	-X)
	    XMLFILE="$2"
	    shift; shift;
	    ;;
	-h)
	    Usage
	    exit
	    shift;
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

if ! [[ -z $1 ]]
    then 
    Usage
    exit
fi

export TMPDIR=/tmp/mkrootfs/`whoami`.$$
mkdir -p "$TMPDIR" || exit;
cd "$TMPDIR"
	
mkdir -p "$PROJROOT" || Exit "Can't mkdir project root directory"
    
PLATFORM=`BestBaseName "$DISTBASE"/"$ARCH"*`
ls -d "$DISTBASE"/"$PLATFORM" || Exit "Can't set a correct architecture"

if [[ -z $REPOSVERSION ]]; then REPOSVERSION="$VERSION"; fi
IBVERSION=`BestBaseName "$DISTBASE"/"$PLATFORM"/*"$REPOSVERSION"*`
if ls -d "$DISTBASE"/"$PLATFORM"/"$IBVERSION" ; 
    then
    IBVERSION="`echo \"$IBVERSION\"|sed -e 's/RPMS.//g'`"
    else 
    echo "I'll just guess the IBVERSION to be RPMS.IB2.00.$REPOSVERSION"
    IBVERSION=IB2.00."$REPOSVERSION" 
fi


if ! [[ -z "$XML" ]]
    then
    echo ftp plhp037 to fetch the xml...
    echo

    # you can customize here to avoid inputing passwd on plhp037 every time you use this feature.
    if [[ -f ~/.mkrootfs.lftp ]]
	then 
	cat ~/.mkrootfs.lftp >lftprc
	else
	echo "you dont have ~/.mkrootfs.lftp"
	echo "you should do the following to avoid this annoying msg:"
	echo "echo lftp ftp:`whoami`:PASSWD@//plhp036.comm.mot.com >~/.mkrootfs.lftp"
	echo "lftp ftp://plhp036.comm.mot.com; u `whoami`;" >lftprc
    fi

    echo "cd /org/mirs/release/su/informal/salsa_ap/iB200$VERSION/$ARCH||cd /org/mirs/release/su/informal/salsa_ap/iB200$VERSION/lab/$ARCH/cm ; mget *$XML*.xml" >>lftprc

    lftp -f lftprc
    
    XMLFILE=`BestBaseName *.xml`
    ls "$XMLFILE" || Exit "Can't get the XML from plhp037"
fi

if [[ -f "$XMLFILE" ]]
    then
    head "$XMLFILE" >/dev/null || Exit "Can't read the XMLFILE"
    if [[ `whoami` == a22242 ]]
	then 
	mkdir -p ~/xmls/"$VERSION"
	'cp' "$XMLFILE" ~/xmls/"$VERSION"/
    fi
	
fi

PROJROOT="`readlink -f \"$PROJROOT\"`"



if [[ -f "$XMLFILE" ]]
    then 
    cat "$XMLFILE"\
	|perl -e 'local $/; $_=<>; s/.*?URL="(.*?)".*?ARCH="(.*?)".*?Release>(.*?)<.*?(?=URL|$)/join " ", "rpm", $1, $2, $3, "\n"/eisg; print' \
	>"$PROJROOT"/apt/etc/sources.list.d/sources.list
    apt-get  --config-file="$PROJROOT"/apt/etc/apt.conf update 
    fi

Exit
