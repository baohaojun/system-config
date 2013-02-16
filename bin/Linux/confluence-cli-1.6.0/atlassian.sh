#!/bin/bash
# This script makes it easier to manage cli version, user, and password settings for various CLIs
# Customize this for your installation. Be careful on upgrades or rename it to something else.
#
# Examples:
#     atlassian.sh confluence --action getServerInfo
#     atlassian.sh jira --action getServerInfo
#     atlassian myOtherConfluence --action getServerInfo
#

# - - - - - - - - - - - - - - - - - - - - START CUSTOMIZE FOR YOUR INSTALLATION !!!
user='automation'
password='automation'
# - - - - - - - - - - - - - - - - - - - - - END CUSTOMIZE FOR YOUR INSTALLATION !!!

application=$1

# - - - - - - - - - - - - - - - - - - - - START CUSTOMIZE FOR YOUR INSTALLATION !!!
if [ "$application" = "confluence" ]; then
    string="confluence-cli-2.3.0.jar --server http://... --user $user --password $password"
elif [ "$application" = "jira" ]; then
    string="jira-cli-2.3.0.jar --server http://... --user $user --password $password"
elif [ "$application" = "fisheye" ]; then
    string="fisheye-cli-2.3.0.jar --server http://... --user $user --password $password"
elif [ "$application" = "crucible" ]; then
    string="crucible-cli-2.3.0.jar --server http://... --user $user --password $password"
elif [ "$application" = "bamboo" ]; then
    string="bamboo-cli-2.3.0.jar --server http://... --user $user --password $password"
elif [ "$application" = "crowd" ]; then
    string="crowd-cli-2.3.0.jar --server http://... --user $user --password $password"
elif [ "$application" = "" ]; then
    echo "Missing application parameter. Specify an application like confluence, jira, or similar."
    echo "$0 <application name> <application specific parameters>"
    exit -99
else
    echo "Application $application not found in $0"
    exit -99
fi
# - - - - - - - - - - - - - - - - - - - - - END CUSTOMIZE FOR YOUR INSTALLATION !!!

java -jar `dirname $0`/lib/$string "${@:2}"
