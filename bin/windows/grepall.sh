#!/bin/sh 


pwd
export PATH=~/bin:/bin:/usr/X11R6/bin:"$PATH"

function Usage()
{
    echo 1>&2 Usage: $0 [options] grep_word grep_where\[s\]
    echo 1>&2
    echo 1>&2 Main options:
    echo 1>&2 -e -help \\t\\t print this Usage
    echo 1>&2 -e -redo \\t\\t do not use cached grep result, redo the grep
    echo 1>&2 -e -\* \\t\\t other options are passed to grep 
    echo 1>&2 -e NOTE: \\t\\t the order of the options and other tokens can be changed freely
    exit
}

function Build_FindOrPrune_list()  #build the find or prune list, $1 is either "find" or "prune", $2 is the argument following -find/-prune
{
    eval list="$2"
    eval unset $1_keys
    for x in "${list[@]}"; do 
        case ${x} in
            :l*) #list of suffixes in ~/etc/grepall/${x:2}
                for y in $(cat ~/etc/grepall/${x:2}); do 
                    eval $1_options=\(\"\$\{$1_options\[\@\]\}\" -o -iname \"\*\".\"\$y\"\)
                    eval $1_keys='("${'$1_keys'[@]}"' '"*.$y")'
                done
                ;;
            :L*) #list of filenames in ~/etc/grepall/${x:2}
                for y in $(cat ~/etc/grepall/${x:2}); do 
                    eval $1_options=\(\"\$\{$1_options\[\@\]\}\" -o -iname \"\$y\"\)
                    eval $1_keys='("${'$1_keys'[@]}"' '"$y")'
                done
                ;;
            :f*) #this is a filename
                eval $1_options=\(\"\$\{$1_options\[\@\]\}\" -o -iname \"\$\{x\:2\}\"\)
                eval $1_keys='("${'$1_keys'[@]}" "${x:2}")'
                ;;
            :s*) #this is a suffix
                eval $1_options=\(\"\$\{$1_options\[\@\]\}\" -o -iname \"\*\".\"\$\{x\:2\}\"\)
                eval $1_keys='("${'$1_keys'[@]}" "*"."${x:2}")'
                ;;
            .*) #this is also a suffix
                eval $1_options=\(\"\$\{$1_options\[\@\]\}\" -o -iname \"\*\"\"\$\{x\}\"\)
                eval $1_keys='("${'$1_keys'[@]}" "*""${x}")'
                ;;
            *'*'*) #this should be a filename glob
                eval $1_options=\(\"\$\{$1_options\[\@\]\}\" -o -iname \"\$\{x\}\"\)
                eval $1_keys='("${'$1_keys'[@]}" "${x}")'
                ;;
        esac
    done

    if [[ $debug = true ]]
    then
        echo find_options is "${find_options[@]}"
        echo find_keys is "${find_keys[@]}"
        echo prune_options is "${prune_options[@]}"
        echo prune_keys is "${prune_keys[@]}"
    fi
    
}

unset grep_search_word
unset grep_search_where
unset grep_search_options
unset grep_redo
unset grep_nocache
unset grep_nonewfile
unset grep_regexp
declare -a find_options
declare -a prune_options
debug=true

find_pre_prunes=(
    -path "*/CVS" -o -path "*/.svn" 
    -o -path "*/autom4te.cache" 
    -o -path "*/{arch}" 
    -o -path "*/.hg" 
    -o -path "*/_darcs" 
    -o -path "*/.git" 
    -o -path "*/.bzr" 
    -o -path "*~*" 
    -o -path "*#" 
    -o -path "*/TAGS" 
    -o -path "*/semantic.cache" )

find_options=(-name "*")


grep_search_where=(/dev/null)


while true; do 
    case "$1" in
        
	-e)
	    grep_regexp="$1"
	    shift
	    grep_search_word="$1"
	    shift
	    ;;

        -prune)
            prune_options=()
            shift
            Build_FindOrPrune_list prune "$1"
            
            shift
            ;;

        :?*)
            edit_char=${1:1:1}
            if [[ "${1:1}" == ${1:1:1} ]]; then
                shift
                edit_arg="$1"
            else 
                edit_arg="${1:2}"
            fi
            case $edit_char in
                d|r)  #delete/remove
                    grep_search_word="`echo -n \"$grep_search_word\"|sed -e 's/'\"$edit_arg\"'//'`"
                    ;;
                s|p)  #start/prepend
                    grep_search_word="$edit_arg""$grep_search_word"
                    ;;
                e|a)  #end/append
                    grep_search_word="$grep_search_word""$edit_arg"
                    ;;     
            esac
            shift
            ;;
        
        -clear)
            unset grep_search_where
            shift
            ;;

        -debug)
            debug="$2"
            shift
            shift
            ;;

        -find)
            find_options=(-false)
            shift
            Build_FindOrPrune_list find "$1"
            shift
            ;;

        -xsh)
            set -x
            shift
            ;;


	-help) 
	    Usage
	    ;;

	-redo)
	    grep_redo=true
	    shift
	    ;;
        
        -nocache|-noc)
            grep_nocache=true
            shift
            ;;
        -nonewfile|-non)
            grep_nonewfile=true
            shift
            ;;

	--)
	    shift
	    if [[ $# = 0 ]]
	    then 
		break;
	    fi
	    
	    if [[ -z "$grep_search_word" ]]
	    then grep_search_word="$1"
		shift
	    fi

	    while ! [[ $# = 0 ]]; do 
		grep_search_where=("${grep_search_where[@]}" "$1")
		shift
	    done

	    ;;
	-*)
	    grep_search_options="$grep_search_options $1"
	    shift
	    ;;

	?*)
	    if [[ -z "$grep_search_word" ]]
	    then 
		grep_search_word="$1"
		shift
	    else
		grep_search_where=("${grep_search_where[@]}" "$1")
		shift
	    fi
	    ;;

	*)
	    break
	    ;;
    esac
done

if [[ $debug == true ]]; then
    echo prune_options="${prune_options[@]}" 1>&2
    echo find_options="${find_options[@]}" 1>&2
fi

#echo grep $grep_search_options $grep_search_word "${grep_search_where[@]}"

for each_where in "${grep_search_where[@]}"; do 
    ( [[ "$each_where" == /dev/null ]] || [[ "$each_where" == NUL ]] ) && continue;
    
    first_char_dir=`echo "$each_where"|cut -b 1`
    
    if [[ $first_char_dir == / ]]; 
    then
	dir=`readlink -f "$each_where"`
    else 
	dir=`readlink -f "$each_where"`;
    fi
    
    if [[ -z "$dir" ]]; 
    then dir="$each_where"
    fi
    
    
    cache=~/tmp/grepall/"$dir"
    mkdir -p "$cache"
    cache_find="$cache"/"`gcachesum P.\"${prune_keys[@]}\"F.\"${find_keys[@]}\" `".find
    cache="$cache"/"`gcachesum P.\"${prune_keys[@]}\"F.\"${find_keys[@]}\"G.$grep_search_options \"$grep_search_word\"`".grep

    if [[ $grep_redo = true ]]
    then 
	rm "$cache" "$cache_find" >/dev/null 2>&1
    fi
    
    if [[ "$cache_find" -nt "$cache" ]]; 
    then
        rm "$cache" >/dev/null 2>&1
    fi

    if [[ $grep_nocache = true ]]
    then
        rm "$cache" >/dev/null 2>&1
    fi
    
    if [[ $grep_nonewfile = true ]]
    then
        if [[ -f "$cache" ]]
        then
            non_tmp=`mktemp`
            cat "$cache" |perl -npe 's":\d+:.*""'|sort -u > $non_tmp
            cat $non_tmp|xargs -d \\n 2>/dev/null  grep -H -n $grep_search_options $grep_regexp "$grep_search_word" | sed -e '/^$\|^Binary/d' | tee "$cache"
            rm $non_tmp
            exit
        else
            echo "can't find" "$cache"
        fi
    fi


    if [[ -f "$cache" ]]; 
    then
        echo 1>&2 cache is "$cache" and it exist\! 
	cat "$cache"|perl -npe 's".*#.+#:\d+:.*""s'|sed -e '/^$/d';
    else
        echo 1>&2 cache is "$cache" and it is missing\!
        touch "$cache" || cache=/dev/null
        if [[ -f "$dir" ]]
        then
            [[ "$dir" == /dev/null ]] || \
                echo "$dir"|xargs -d \\n grep -H -n $grep_search_options $grep_regexp "$grep_search_word"|tee "$cache"
        else
            if ! [[ -f "$cache_find" ]]; then
                
                find "$dir" \( "${find_pre_prunes[@]}" "${prune_options[@]}" \) -prune -o -type f  \( "${find_options[@]}" \) -print 2>/dev/null >"$cache_find"
            fi
            
            cat "$cache_find" | xargs -d \\n 2>/dev/null grep -H -n $grep_search_options $grep_regexp "$grep_search_word" | sed -e '/^$\|^Binary/d' | tee "$cache"
        fi
    fi
done

echo
echo grep finished
