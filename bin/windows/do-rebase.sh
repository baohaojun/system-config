#!/bin/bash
set -x
chmod 777 "$TMP"
me=$(whoami)
meg=$(id|perl -npe 's/.*gid=(\d*).*/$1/')
test "$1" = chown && chown -R $me.$meg $(wlp /|perl -npe 's/^(.):/"\/" . lc $1/e')
function pause() {
    echo echo "$@"
    echo pause
}

(
    echo $(wlp /c/python25/python) $(wlp $(which terminateModule.py)) cygwin1
    pause will no do rebase
    echo $(wlp /bin/dash) /bin/rebaseall
    pause will now restart BatchStart.sh
    echo start $(wlp ~/bin/windows/startup/cygwin/BatchStart.sh)
    pause all done
) > /tmp/do-rebase.bat
chmod +x /tmp/do-rebase.bat
of /tmp/do-rebase.bat
