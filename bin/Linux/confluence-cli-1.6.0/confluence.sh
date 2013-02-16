#!/bin/bash

# Comments
# - Customize for your installation, for instance you might want to add default parameters like the following:
# java -jar `dirname $0`/lib/confluence-cli-2.3.0.jar --server http://my-server --user automation --password automation "$@"

# See https://confluence.oceanobservatories.org/display/CIDev/Command+Line+Interface+for+Jira+and+Confluence

# in case the link is broken:
# 1. Import server cert to keystore
# 
# (Use firefox, click the icon left to the url, export the certificate.)
# 
# Use the attached "star_oceanobservatories.org.crt" certificate file (attached to ticket)
# 
# (from your terminal)
# keytool --import -alias ooicert -file star_oceanobservatories.org.crt
# keytool --import -alias ewiki -file ewiki.marvell.com 
# 
# enter password for keystore (if one doesn't exist, one will be created with password you enter)
# 
# The default location of your keystore will be ~/.keystore
# 

if test -e ~/.confkeys; then
    . ~/.confkeys
fi

if test "$KEYSTORE"; then
    add_args=$(echo -Djavax.net.ssl.trustStore=$KEYSTORE -Djavax.net.ssl.trustStorePassword=$KEYSTORE_PASS)
fi

java $add_args -jar $(wlp $(dirname $(readlink -f $0)))/lib/confluence-cli-2.3.0.jar "$@"
