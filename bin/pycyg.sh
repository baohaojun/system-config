pyfile="$1"
shift
pyfile="`cygpath -alw \"$pyfile\"`"
/c/python25/python "$pyfile" "$@"
