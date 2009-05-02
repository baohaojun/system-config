if [[ -z $PROJROOT ]]; then 
    echo you need to set \$PROJROOT to the rootfs project directory, or else you need to set it every time
    read -e -p "Input your rootfs project directory:" PROJROOT
fi
if ! [[ -d "$PROJROOT"/rootfs ]]; then 
    echo "your PROJROOT has no rootfs in it? exit..."
    exit
fi

if [[ -z $1 ]]; then 
    echo what rpm you want to remove\?
    read RPMLIST
    set -- `echo "$RPMLIST"`
fi

unset RPMLIST

for x in "$@"; do RPMLIST="$RPMLIST  `rpm --rcfile=$PROJROOT/apt/etc/.rpmrc -qa|grep -e "$x"|sed -e 's/-[0-9].*//g'`"; done

echo 

echo "Are these all you want to remove?"
echo
echo $RPMLIST|tr ' ' '\n'
echo
echo 'Please CONFIRM by typeing in y (y/n)'
read confirm

if [[ $confirm != y ]] ;then exit; fi

 rpm --rcfile=$PROJROOT/apt/etc/.rpmrc -ev $RPMLIST --nodeps
