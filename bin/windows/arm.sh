#!/bin/bash -x
if cleartool pwv|grep -e 'Set .*NONE'; 
then echo view not set;
else
set_view=`cleartool pwv|grep Set|awk '{print $3}'`
cd 
rm home/code
rm lrpm
mkdir home/work-code-$set_view 
ln -sf work-code-$set_view home/code
ln -sf rpm_$set_view lrpm
cd -
ROOTFS=`readlink -f ~/home/rootfs`
if [[ ! -z $1 ]] ; then ROOTFS=`readlink -f "$1"`; fi

if ! [[ -d $ROOTFS ]]; then echo $ROOTFS is not a valid rootfs! does it exist?; return ;fi
if [[ -d $ROOTFS/rootfs ]]; then ROOTFS=$ROOTFS/rootfs; fi
ROOTFS=`echo $ROOTFS|sed -e 's/\(.*\)rootfs.*/\1rootfs/g'`
echo $ROOTFS

cd /home/a22242/project;

if ! [[ $ROOTFS == `readlink -f ~/rootfs` ]]; then
rm rootfs;

TEMPROOTFS=`echo $ROOTFS |sed -e 's"^.*a22242/project/*""g'`
ln -sf $TEMPROOTFS rootfs
else
echo the rootfs is already linked!
fi


cd /vob/su_host_arm/AP_services/multimedia/mme
echo DEBUG env is set to $DEBUG
. /vob/ilj_release/devkit/scripts/iljenv.sh arm lab $DEBUG $ROOTFS

mkdir -p ~/rootfs/home/mme_test/bin
'cp' ~/mit_rcs ~/rootfs/home/mme_test -af&
'cp' ~/mit_bin/* ~/rootfs/home/mme_test/bin -af&
if /vob/ilj_release/devkit/scripts/rpmsetup.sh arm; then . /vob/ilj_release/devkit/scripts/rpmsetup.sh arm; fi
mkdir ../build ../buildfree ../buildvris ../buildhelix ../buildring -p
export PROJROOT=$ROOTFS/..
alias cco='cleartool co -nc'
alias cmerge='cleartool merge -nc'
alias cmb='cleartool mkbranch -nc'
alias cci='cleartool ci -nc'
echo set solib-absolute-prefix $TARGET_ROOT/>~/.gdbinit
echo set solib-search-path $TARGET_ROOT/phx/lib:$TARGET_ROOT/phx/lib/helixlibs >>~/.gdbinit
unalias cp 2>/dev/null
alias apt-get='apt-get  --config-file="$PROJROOT"/apt/etc/apt.conf'
fi
