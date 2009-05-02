#!/bin/bash
declare -a args_array
declare -i x

#echo plink "$@" >> ~/plink.log &
while true; do 
    case "$1" in
        -p)
            args_array[$x]="-P"
            shift
            ((x++))
            ;;
        ?*)
            args_array[$x]="$1"
            shift
            ((x++))
            ;;
        *)
            break
            ;;
    esac
done

echo -n plink\  >> ~/plink.log

for y in "${args_array[@]}"
do
    echo -n \""$y"\"\  >>~/plink.log
done
echo >>~/plink.log
.plink-distr.exe "${args_array[@]}"
        
