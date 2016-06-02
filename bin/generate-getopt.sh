#!/bin/bash

# [[file:~/src/github/private-config/doc/generate-getopt.org::*%E6%9C%80%E7%BB%88%E8%84%9A%E6%9C%AC][the-ultimate-script]]

# All bash scripts should start with ~set -e~ to fail early and loudly.
set -e
declare -A short_opts_map
declare -A long_opts_map
declare -A help_map
declare -A default_val_map

for spec in "$@"; do
    if [[ $spec =~ ^@ ]]; then
        short=
    else
        short=${spec:0:1}
    fi
    spec=${spec:1}
    if [[ $spec =~ ^: ]]; then
        short=$short:
        spec=${spec:1}
    fi

    long=$(echo "$spec"|perl -npe 's/(\?|=).*//')
    help=$(echo "$spec" | perl -ne 'print if s/.*\?(.*?)(=.*|$)/$1/')
    default=$(echo "$spec" | perl -ne 'print if s/.*=(.*?)(\?.*|$)/$1/')

    short_opts_map[$long]=$short
    long_opts_map[$long]=$long
    if [[ $short =~ : ]]; then
        long_opts_map[$long]=$long:
    fi

    if test -z "$default" && ! [[ $short =~ : ]]; then
        default=false
    fi

    default_val_map[$long]=$default
    help_map[$long]=$help
done


cat <<EOF
TEMP=\$(getopt -o $(string-join '' "${short_opts_map[@]}") --long $(string-join , ${long_opts_map[@]})  -n \$(basename -- \$0) -- "\$@")
$(
for long in "${!long_opts_map[@]}"; do
    var=${long//-/_}
    echo ${var}=${default_val_map[$long]}
done
)
eval set -- "\$TEMP"
while true; do
  case "\$1" in

$(
for long in "${!long_opts_map[@]}"; do
    s=${short_opts_map[$long]}
    is_boolean=true
    if [[ $s =~ : ]]; then
        is_boolean=false
    fi
    s=${s/:/}
    var=${long//-/_}
    cat << EOF2
        $(
            if test "$s"; then
                echo -n "-$s|"
            fi
            echo -n "--$long";
            if test "$is_boolean" = true; then
                echo -n "|--no-$long"
            fi
            echo ")"
        )
        $(
            if test $is_boolean = false; then
                echo ${var}=\$2
                echo shift 2
            else
                cat << EOF3
                if test "\$1" = --no-$long; then
                    ${var}=false
                else
                    ${var}=true
                fi
                shift
EOF3
            fi
        )
EOF2
    echo ";;"
done

)
-h|--help)
    (
        for long in ${!long_opts_map[@]}; do
            echo --$long
        done
    )
    shift
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

EOF

# the-ultimate-script ends here
