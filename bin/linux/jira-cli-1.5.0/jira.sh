#!/bin/bash

# Comments
# - Customize for your installation, for instance you might want to add default parameters like the following:
# java -jar release/jira-cli-1.5.0.jar --server http://my-server --user automation --password automation "$@"

java -jar $(dirname $(readlink -f $0))/release/jira-cli-1.5.0.jar "$@"
