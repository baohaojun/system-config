#!/bin/bash
set -e
fb_tmp=/c/firebolttmp

rm $fb_tmp -rf
unzip "$(cygpath -au "$1")" -d $fb_tmp 2>&1
echo OK
/q/firebolt/firebolt.exe "$(cygpath -alw $fb_tmp/*)"
