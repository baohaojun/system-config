#!/usr/bin/env bash
# [[file:~/src/github/org-kungfu/org-kungfu.org::cql][cql]]
set -e
get-config() {
    git config -f ~/.config/org-kungfu/config.ini "$@"
}

user=$(get-config ok.username)
pass=$(get-config ok.password)
wikiurl=$(get-config ok.wikiurl)


## start code-generator "^\\s *#\\s *"
# generate-getopt q:cql-str e:extra-args
## end code-generator
## start generated code
TEMP=$( getopt -o q:e:h \
               --long cql-str:,extra-args:,help \
               -n $(basename -- $0) -- "$@")
declare cql_str=
declare extra_args=
eval set -- "$TEMP"
while true; do
    case "$1" in

        -q|--cql-str)
            cql_str=$2
            shift 2

            ;;
        -e|--extra-args)
            extra_args=$2
            shift 2

            ;;
        -h|--help)
            set +x
            echo -e
            echo
            echo Options and arguments:
            printf %6s '-q, '
            printf %-24s '--cql-str=CQL_STR'
            echo
            printf %6s '-e, '
            printf %-24s '--extra-args=EXTRA_ARGS'
            echo
            exit
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error: $(. bt; echo; bt | indent-stdin)"
            ;;
    esac
done


## end generated code

if test -z "${cql_str}"; then
    cql_str="$*"
fi

cql_str=$(
    urlify "$cql_str"
       )

curl -u "${user}:${pass}" "${wikiurl}rest/api/search?cql=$cql_str${extra_args}"
# cql ends here
