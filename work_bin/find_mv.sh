#!/bin/bash

function Usage() {
    echo Usage: $0 '[-c category]' pattern
    exit
}

((x=0))

while ! [[ -z "$1" ]]
  do 
  case "$1" in
      -c) 
          if [[ -z "$2" ]]
              then 
              Usage;
          fi

          echo $0: set category to "$2"
          echo 
          CATE="$2"
          if ! [[ -d "$CATE" ]]; then
              CATE=~/Incoming/"$CATE"
          fi
          shift
          shift
          ;;
      *)
          patterns[$x]="$1"
          ((x++));
          shift
          ;;
  esac
done



for each_pattern in "${patterns[@]}"; do
    if [[ -z "$CATE" ]]; then
        if [[ -d "${patterns[0]}" ]]; then
            CATE="${patterns[0]}"
        else
            CATE=~/Incoming/"${patterns[0]}"
        fi
    fi
            
    mkdir "$CATE" -p
    
    
    find . -iname "*$each_pattern*" -exec mv -v -i '{}' "$CATE" \;

done

echo
echo -------------------------------
echo

find_diff.sh -c "$CATE" "${patterns[@]}"

