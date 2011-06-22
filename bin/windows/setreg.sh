#!/bin/bash
. regfuncs.sh
q=`cygpath -alw ~`

cat >/c/vc.reg <<EOF
Windows Registry Editor Version 5.00

[HKEY_CURRENT_USER\Software\Microsoft\Devstudio\6.0\Build System\Components\Platforms\Win32 (x86)\Directories]
"Include Dirs"="`for x in ~/vc6/inc/*; do readlink -f "$x"; done|u2dpath|perl -npe 's/\\\\/\\\\\\\\/g'`"
"Library Dirs"="`for x in ~/vc6/lib/*; do readlink -f "$x"; done|u2dpath|perl -npe 's/\\\\/\\\\\\\\/g'`"
EOF

regedit /s 'c:\vc.reg'

regSetVal -s set '\HKEY_CLASSES_ROOT\.sh\' 'sh_auto_file'
regSetVal -s set '\HKEY_CLASSES_ROOT\sh_auto_file\shell\open\command\' "$(qq_cmdout cygpath -alw "$BASH"; echo -n ' --rcfile ~/.bashrc-windows -i "%1" %*')"
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\emacsedit\command\' "$q"'\windows-config\bin\windows\redirect_vc6\emacsedit.exe -n "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\bashHere\command\' "$q"'\windows-config\bin\windows\redirect_vc6\runHere bash "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\Locate It\command\' "$q"'\windows-config\bin\windows\redirect_vc6\LocateIt "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\WinPath\command\' "$q"'\windows-config\bin\windows\redirect_vc6\winpath "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\EmacsEdit\command\' "$q"'\windows-config\bin\windows\redirect_vc6\emacsedit.exe -n "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\bashHere\command\' "$q"'\windows-config\bin\windows\redirect_vc6\runHere bash "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\Locate It\command\' "$q"'\windows-config\bin\windows\redirect_vc6\LocateIt "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\WinPath\command\' "$q"'\windows-config\bin\windows\redirect_vc6\winpath "%1"'
regSetVal -i set '\HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\kernel\obcaseinsensitive' 0
regtool.exe unset '\HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\CYGWIN'

refresh-windows-env.py
