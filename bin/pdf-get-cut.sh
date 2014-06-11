#!/bin/bash

set -e

function die() {
    echo Error: "$@"
    exit -1
}

if test $# != 1; then
    die "Error: Usage $(basename $0) PDF"
fi

arg_pdf=$1
shift
function debug() {
    echo "$@" 1>&2
}

pages=$(pdf-get-pages "$arg_pdf")
nclip=20
((step = pages / nclip )) || true
width=$(pdf-get-width "$arg_pdf")
height=$(pdf-get-height "$arg_pdf")

function good-enough() {
    perl -e "if (abs($1 - $2) < 3) {exit 0;} else {exit 1;}"
}

## start code-generator "^\\s *#"
#     for x in left bot right top; do
#         echo min_$x=0; echo max_$x=200;
#     done
## end code-generator
## start generated code
min_left=0
max_left=200
min_bot=0
max_bot=200
min_right=0
max_right=200
min_top=0
max_top=200

## end generated code

TEMP=$(getopt -o l:r:b:t: --long "" -n $(basename $0) -- $CUT_SPEC)
eval set -- "$TEMP"
while true; do
    case "$1" in

        ## start code-generator "^\\s *#"
        # for x in left bot right top; do
        #     echo '' -${x:0:1}\)
        #     echo min_$x=\$\(subtract \$2 20\)
        #     echo max_$x=\$\(subtract \$2 -20\)
        #     echo shift 2
        #     echo \;\;
        # done
        ## end code-generator
        ## start generated code
        -l)
            min_left=$(subtract $2 20)
            max_left=$(subtract $2 -20)
            shift 2
            ;;
        -b)
            min_bot=$(subtract $2 20)
            max_bot=$(subtract $2 -20)
            shift 2
            ;;
        -r)
            min_right=$(subtract $2 20)
            max_right=$(subtract $2 -20)
            shift 2
            ;;
        -t)
            min_top=$(subtract $2 20)
            max_top=$(subtract $2 -20)
            shift 2
            ;;

        ## end generated code
        --)
            shift
            break
            ;;
        *)
            die "internal error"
            ;;
    esac
done


# order is left bot right top
for side in left bot right top; do
    if test $side = left -o $side = right; then
        nup=${nclip}x1
    else
        nup=1x${nclip}
    fi
    max=$(eval echo \$max_$side)
    min=$(eval echo \$min_$side)

    while true; do
        mid=$(perl -e "print (($max + $min)/2)")
        cut_top=0
        cut_bot=0
        cut_left=0
        cut_right=0

        if test $side = left; then
            cut_right=$(perl -e "print ($width-$mid)")
            papersize="{$(perl -e 'print '$mid'*'$nclip''),$height}"
        elif test $side = right; then
            cut_left=$(perl -e "print ($width-$mid)")
            papersize="{$(perl -e 'print '$mid'*'$nclip''),$height}"
        elif test $side = top; then
            cut_bot=$(perl -e "print ($height-$mid)")
            papersize="{$width,$(perl -e 'print '$mid'*'$nclip'')}"
        else
            cut_top=$(perl -e "print ($height-$mid)")
            papersize="{$width,$(perl -e 'print '$mid'*'$nclip'')}"
        fi
        pdfnup --no-landscape --rotateoversize false  --no-tidy --papersize $(echo "$papersize" | perl -npe 's/(,|\})/pt$1/g') \
            --nup $nup "$arg_pdf" $(echo $(seq 1 $step $pages) | perl -npe 's/ /,/g') \
            --outfile $side.pdf --clip true --trim "${cut_left}pt ${cut_bot}pt ${cut_right}pt ${cut_top}pt" >/dev/null 2>&1
        evince $side.pdf >/dev/null 2>&1 &
        debug "min is $min, mid is $mid, max is $max"
        ans=$(my-select good bad min max done)
        if good-enough $max $min; then
            echo "" -${side:0:1} $min
            break
        fi
        if test $ans = 1; then
            min=$mid
        elif test $ans = 2; then
            max=$mid
        elif test $ans = 3; then
            read -e -p "New min value: (currently $min) " min
        elif test $ans = 4; then
            read -e -p "New max value: (currently $max) " max
        else
            echo "" -${side:0:1} $mid
            break
        fi
    done
done
