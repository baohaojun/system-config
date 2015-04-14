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
regSetVal -s set '\HKEY_CLASSES_ROOT\sh_auto_file\shell\open\command\' "$(qq_cmdout cygpath -alw "$BASH"; echo -n ' --rcfile ~/system-config/.bashrc-windows -i "%1" %*')"
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\emacsedit\command\' "$q"'\external\bin\CYGWIN\emacsedit.exe -n "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\bashHere\command\' "$q"'\external\bin\CYGWIN\runHere bash "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\Locate It\command\' "$q"'\external\bin\CYGWIN\LocateIt "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\WinPath\command\' "$q"'\external\bin\CYGWIN\winpath "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\EmacsEdit\command\' "$q"'\external\bin\CYGWIN\emacsedit.exe -n "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\bashHere\command\' "$q"'\external\bin\CYGWIN\runHere bash "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\Locate It\command\' "$q"'\external\bin\CYGWIN\LocateIt "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\WinPath\command\' "$q"'\external\bin\CYGWIN\winpath "%1"'
regSetVal -i set '\HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\kernel\obcaseinsensitive' 0
regtool.exe unset '\HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\CYGWIN'

refresh-windows-env.py
