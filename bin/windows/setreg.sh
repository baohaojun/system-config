#!/bin/bash
function qq_cmdout () #put " at begin and end of cmd output, which should be 1 line
{
    echo -n \"
    "$@"|tr -d '\n' #if "$@" produce more than 1 line, we are screwed!
    echo -n \"
}

function regCreateSubKeys () #create all the levels of subkeys
{
    echo "$1" | perl -ne '
    chomp;
    s!\\+!\\!g; #remove duplicated \\
    s!\\$!!; #remove trailing \\
    @F=split qr(\\);
    @_=@F[2..$#F]; 
    $KEY=join(q(\\), @F[0..1]);
    for (@_) {
        $KEY .= q(\\) . $_;
        system "regtool.exe", "-v", "add", $KEY
    }'
}

function regSetVal () 
{
    local key=$3
    local type=$1
    regCreateSubKeys "${key%\\*}"'\'
    shift 3
    regtool.exe $type set "$key" "$@"
}

function regAddPaths ()
{
    #add every folder/softlink-to-folder under $2 to the regentry in $1
    local key=$1
    local folder=$2
    regCreateSubKeys "${key%\\*}"'\'
    find "$(readlink -f "$folder")" -maxdepth 1 -ipath "*/.svn" -prune  -o \( -xtype d -print0 \)|xargs -0 regAddPaths "$key"
}

regAddPaths '\HKEY_CURRENT_USER\Software\Microsoft\DevStudio\6.0\Build System\Components\Platforms\Win32 (x86)\Directories\Include Dirs' ~/vc6/inc
regAddPaths '\HKEY_CURRENT_USER\Software\Microsoft\DevStudio\6.0\Build System\Components\Platforms\Win32 (x86)\Directories\Library Dirs' ~/vc6/lib

regSetVal -s set '\HKEY_CLASSES_ROOT\.sh\' 'sh_auto_file'
regSetVal -s set '\HKEY_CLASSES_ROOT\sh_auto_file\shell\open\command\' "$(qq_cmdout cygpath -alw "$BASH"; echo -n ' --rcfile ~/.bashrc-windows -i "%1" %*')"
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\emacsedit\command\' 'q:\bin\windows\redirect_vc6\emacsedit.exe -n "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\bashHere\command\' 'q:\bin\windows\redirect_vc6\runHere bash "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\Locate It\command\' 'q:\bin\windows\redirect_vc6\LocateIt "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\*\shell\WinPath\command\' 'q:\bin\windows\redirect_vc6\winpath "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\EmacsEdit\command\' 'q:\bin\windows\redirect_vc6\emacsedit.exe -n "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\bashHere\command\' 'q:\bin\windows\redirect_vc6\runHere bash "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\Locate It\command\' 'q:\bin\windows\redirect_vc6\LocateIt "%1"'
regSetVal -s set '\HKEY_CLASSES_ROOT\Directory\shell\WinPath\command\' 'q:\bin\windows\redirect_vc6\winpath "%1"'
regSetVal -i set '\HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\kernel\obcaseinsensitive' 0
