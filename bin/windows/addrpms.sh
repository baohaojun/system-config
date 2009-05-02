#!/bin/bash




function Usage()
{
    echo Usage: $0 [options] rpm\[s\]
    echo
    echo Main options:
    echo -e -h \\t print this Usage
    echo -e -d DIR\\t rootfs is indicated as DIR
    exit
}


declare RPM_LIST
((x=0))

while true; do 
    case "$1" in
        -h )
            Usage
            ;;

	-d)
            shift
            if [[ $# = 0 ]]
                then
                Usage
                break
            fi

            PROJROOT=`readlink -f "$1"`
            shift;
            ;;
        -*) 
            Usage
            ;;

	?*)
            RPM_LIST[$x]="$1"
            ((x++))
            shift
            ;;

	*)
	    break
	    ;;
    esac
done

if [[ -z $PROJROOT ]]; then 
    echo you need to set \$PROJROOT to the rootfs project directory, or else you need to set it every time
    read -e -p "Input your rootfs project directory: " PROJROOT
fi

if ! [[ -d ${PROJROOT}/rootfs ]]; then 
    if [[ -d ${PROJROOT}/../rootfs ]]; then
        PROJROOT=`readlink -f ${PROJROOT}/..`
    else
        echo "your PROJROOT has no rootfs in it? exit..."
        exit
    fi
fi

if [[ -z ${RPM_LIST[0]} ]]; then 
    read -e -p "what rpms you want to install?: " RPMLIST
    set -- `echo "$RPMLIST"`
else
    set -- `echo ${RPM_LIST[@]}`
fi


echo 

echo "You want to install the following rpms:"
echo
for x in "$@" ;do echo $x; done
echo
echo "in $PROJROOT; Please CONFIRM by typeing in y (y/n)"
read confirm

if [[ $confirm != y ]] ;then exit; fi

rpm --rcfile=$PROJROOT/apt/etc/.rpmrc --prefix=$PROJROOT/rootfs --nodeps --force -Uvh "$@"

