#!/bin/bash

# Comments
# - Customize for your installation, for instance you might want to add default parameters like the following:
# java -jar release/confluence-cli-1.6.0.jar --server http://my-server --user automation --password automation "$@"

function pathname() {
    case $(uname) in
	CYGWIN*)
	    cygpath -alw "$1"
	    ;;
	*)
	    echo "$1"
	    ;;
    esac
}

java -jar $(pathname $(dirname $(readlink -f $0))/release/confluence-cli-1.6.0.jar) "$@"
