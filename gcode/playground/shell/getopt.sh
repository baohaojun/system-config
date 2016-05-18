#!/bin/bash

## start code-generator "^\\s *#\\s *"
# generate-getopt ggnu p:phone
## end code-generator
## start generated code
TEMP=$(getopt -o gp: --long "gnu,phone:"  -n $(basename -- $0) -- "$@")
gnu=false
phone=
eval set -- "$TEMP"
while true; do
    case "$1" in
        -g|--gnu)
            gnu=true
            shift
            ;;
        -p|--phone)
            phone=$2
            shift 2
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error"
            ;;
    esac
done

## end generated code

echo gnu is $gnu
