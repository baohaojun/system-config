#!/bin/bash --login
rm ~/.ido.last
. /c/ssh-agent.log >/dev/null
#export http_proxy=http://bhj3:8888
export PATH="/c/Python25:/c/Program Files/Microsoft DirectX SDK (March 2008)/Utilities/Bin/x86:/c/Program Files/RSA Security/RSA SecurID Software Token/:/c/Program Files/Visual Studio 2005 SDK/2007.02/VisualStudioIntegration/Tools/Sandcastle/ProductionTools/:/c/WINDOWS:/c/WINDOWS/System32/Wbem:/c/WINDOWS/system32:/c/gnuserv:/c/java/jdk1.6/bin:/c/ntutils:/c/scripts:/d/tools/emacswin/emacs-22.1/bin:/d/tools/mplayer/MPlayer-1.0rc2:"~/bin":/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/sbin"
unset INFOPATH
export VC_BUILD_CONFIG=release
texlivemake /d/tools/emacswin/bin/runemacs "$@" &
