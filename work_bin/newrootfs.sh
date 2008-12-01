
#!/bin/bash

REPOSVERSION=lab

function Usage() 
{
    echo Usage: $0 [options]
    echo 
    echo Main options:
    echo -e -d DIR \\t\\tthe directory the rootfs will be installed, the \"project directory\" in pck, where the .mip file will be found
    echo -e -a ARC \\t\\tthe architecture of the rootfs, is it x86\? or is it arm\?
    echo -e -v VER \\t\\tthe version of the rootfs, such as 39\? 39A\?
    echo -e -r REPV \\tthe version of the repostory, such as 44 43, if xml is used, it is not important, but must be a valid one.
    echo -e -x XML \\t\\tthe xml you want to fetch from fl08lx152 and the use, such as BINA*FLASH \| BINA\*NFS \| DEVELOP
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
    mv $TMPDIR/*xml $PROJROOT 
    rm $TMPDIR -rf
    exit
}

function BestBaseName()
{
    echo "`basename \"$1\"`"
}

DISTBASE=/vob/ilj_release/informal/salsa
PROJROOT="`pwd`/default_rootfs"
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



if ! [[ -z "$XML" ]]
    then
    echo ftp fl08lx152 to fetch the xml...
    echo

    # you can customize here to avoid inputing passwd on fl08lx152 every time you use this feature.
    if [[ -f ~/.mkrootfs.lftp ]]
	then 
	cat ~/.mkrootfs.lftp >lftprc
	else
	echo "you dont have ~/.mkrootfs.lftp"
	echo "you should do the following to avoid this annoying msg:"
	echo "echo lftp ftp:`whoami`:PASSWD@//fl08lx152.comm.mot.com >~/.mkrootfs.lftp"
	echo "lftp ftp://fl08lx152.comm.mot.com; u `whoami`;" >lftprc
    fi

    echo "mget /org/mirs/release/su/informal/salsa_ap/$VERSION/$ARCH/*$XML*.xml;mget /org/mirs/release/su/informal/salsa_ap/$VERSION/$REPOSVERSION/$ARCH/cm/*$XML*.xml ; mget /org/mirs/release/su/salsa_ap/$VERSION/$REPOSVERSION/$ARCH/cm/*$XML*.xml" >>lftprc
    Debug "after write lftprc"
    lftp -f lftprc 
    
    XMLFILE=`BestBaseName *.xml`
    ls "$XMLFILE" || Exit "Can't get the XML from fl08lx152"
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

#/vob/ilj_release/devkit/pck/pck.sh rpmqa "$PROJROOT"|grep "Package:" |sed -e 's/Pa.*: //g' |sort -u |tee $TMPDIR/already.txt

#Debug "after rpmqa"

#if cat $TMPDIR/already.txt|wc -l|egrep '^0$'; then
#/vob/ilj_release/devkit/pck/pck.sh genenv "$PROJROOT" "$PLATFORM" "$DISTBASE" /vob/ilj_release/devkit/pck "$IBVERSION"
/vob/ilj_release/devkit/pck/pck.sh newproject "$PROJROOT" $PLATFORM /vob/ilj_release/devkit/pck
Debug "after genenv"
#fi

if [[ -f "$XMLFILE" ]]
    then 
    cat "$XMLFILE"\
	|perl -e 'local $/; $_=<>; s/.*?URL="(.*?)".*?ARCH="(.*?)".*?Release>(.*?)<.*?(?=URL|$)/join " ", "rpm", $1, $2, $3, "\n"/eisg; print' \
	>"$PROJROOT"/apt/etc/sources.list.d/sources.list
    fi


/vob/ilj_release/devkit/pck/pck.sh apt-get update "$PROJROOT"
Debug "after update"

/vob/ilj_release/devkit/pck/pck.sh apt-cache pkgnames "$PROJROOT"
Debug "after apt-cache pkgnames"

if [[ -f "$XMLFILE" ]]
    then
    cat "$XMLFILE" |perl -e 'local $/; $_=<>; s/.*?Name="(.*?)".*?Version>(.*?)<.*?(?=Name|$)/$1."=".$2."\n"/eisg; print'|sort\
	>"$PROJROOT"/package.list
fi



/vob/ilj_release/devkit/pck/pck.sh apt-cache dumpavail "$PROJROOT" | perl -npe 'chomp; if (m/^Package: (.*)/) {$_ = $1} elsif (m/^Version: (.*)/) {$_ = "=".$1."\n"} else {$_ = ""}' |sort -u > "$PROJROOT"/package-av.list; 

grep debuginfo "$PROJROOT"/package-av.list >> "$PROJROOT"/package.list

vim "$PROJROOT"/package.list

grep -v "#" "$PROJROOT"/package.list  > "$PROJROOT"/package.list.tmp
#cat $TMPDIR/already.txt|(subfile="$PROJROOT"/package.list ~a22242/bin/file_subtract.pl) > "$PROJROOT"/remove.list

Debug "after generating install/remove lists"

#you can also avoid inputing sudo passwd if you have sudo.ep; just think about it, don't ask me:-)
if [[ -x ~/bin/sudo.sh ]]; then ~/bin/sudo.sh; fi

 chown `whoami` "$PROJROOT" -R
 mkdir -p "$PROJROOT"/rootfs
 chmod u+w "$PROJROOT"/rootfs
 cd "$PROJROOT"/rootfs


#The following 2 lines if because createimage.sh has been upgraded, so, if one way don't work, go the other way.
apt-get --config-file="$PROJROOT"/apt/etc/apt.conf install `cat "$PROJROOT"/package.list.tmp`
Debug "after apt-get install"
apt-get --config-file="$PROJROOT"/apt/etc/apt.conf update
#sudo /vob/ilj_release/devkit/pck/createimage.sh apt-get install -p "$PROJROOT" "$PROJROOT"/package.list.tmp||\
#sudo /vob/ilj_release/devkit/pck/createimage.sh apt-get install "$PROJROOT" "$PROJROOT"/package.list.tmp

#sudo chown `whoami` $PROJROOT/rootfs/home -R
Exit

#sudo chown `whoami` "$PROJROOT"/rootfs -R
#sudo chmod +w "$PROJROOT"/rootfs
#/bin/rpm --rcfile="$PROJROOT"/apt/etc/.rpmrc -e --nodeps --rcfile="$PROJROOT"/apt/etc/.rpmrc --noorder ergo-devel ergo mme
#sudo rpm --rcfile=/home/a22242/project/rootfs25/apt/etc/.rpmrc -ev mme-devel mme mme-debuginfo --nodeps
#sudo rpm --rcfile=/home/a22242/project/rootfs31/apt/etc/.rpmrc -ev helix_libs-debuginfo helix_libs mme-debuginfo mme mme-devel mme_vris-debuginfo mme_vris mme_vris-devel ergo openwave_browser si_task si_task-devel ergo-devel libergohdr-debuginfo libergo-debuginfo libergo libergohdr-devel libergohdr ergo-debuginfo libergo-devel java

#PROJROOT=/home/a22242/project/rootfs35; 
#sudo rpm --rcfile=$PROJROOT/apt/etc/.rpmrc -ev `rpm --rcfile=$PROJROOT/apt/etc/.rpmrc -qa|grep -e 'alert\|mme\|ringtone\|audio\|volume\|helix' |sed -e 's/-[0-9].*//g'` --nodeps
#rpm --rcfile=$PROJROOT/apt/etc/.rpmrc -qa|grep -e 'alert\|mme\|ringtone\|audio\|volume\|helix'
#sudo rpm --rcfile=$PROJROOT/apt/etc/.rpmrc --prefix=$PROJROOT/rootfs --nodeps --force -Uvh
