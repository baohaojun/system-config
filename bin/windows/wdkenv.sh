#!/bin/bash

#call C:\WINDOWS\system32\cmd.exe /k C:\WinDDK\6001.18002\bin\setenv.bat C:\WinDDK\6001.18002\ fre x86 WXP
function wdkenv_func () {

    cat <<EOF >~/wdkenv.bat
call C:\WINDOWS\system32\cmd.exe /k C:\WinDDK\6001.18002\bin\setenv.bat C:\WinDDK\6001.18002\ fre x86 WXP
bash -c set
EOF

#bash: 386=1: command not found

#bash: UID: readonly variable
#bash: EUID: readonly variable
#bash: PPID: readonly variable
#bash: BASH_VERSINFO: readonly variable
#bash: SHELLOPTS: readonly variable


    cat ~/wdkenv.bat|u2d|cmd.exe|d2u|grep =|
    grep -v '^!'|grep -vi '^uid=\|^euid=\|^ppid=\|^bash_versinfo=\|^shellopts=\|^BASH\|^GOUPS='|sed -e 's/^/export /g' > ~/.wdk.set
    #. ~/.wdk.set #this will hang bash :-(
}

wdkenv_func
. ~/.wdk.set #must move it here
