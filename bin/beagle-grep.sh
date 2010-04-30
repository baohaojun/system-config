#!/bin/bash
echo -n beagle query argument \`'[01;31m'$(get_longest_token "$@")'[0m'\' 1>&2
echo 1>&2
my-beagle "$@"|xargs grep -H -n -I "$@" 
