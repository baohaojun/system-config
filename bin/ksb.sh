#!/bin/bash
. /c/ssh-agent.log
ssh bhj1 '~/bin/ksb.sh'
LOCALIP=`ssh bhj1 echo '$SSH_CLIENT'|sed -e 's/.*://; s/ .*//'`
findexec -F 1 -t smobee run ssh bhj1 bash -c "'export DISPLAY=${LOCALIP}:0; /usr/local/smobee/smobee/smobee'"

