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
        system "regtool.exe", "add", $KEY
    }'
}

REGS=(
    '\HKEY_CLASSES_ROOT\.sh'
    '\HKEY_CLASSES_ROOT\sh_auto_file\shell\open\command\'
    '\HKEY_CLASSES_ROOT\*\shell\emacsedit\command\'
    '\HKEY_CLASSES_ROOT\*\shell\bashHere\command\'
    '\HKEY_CLASSES_ROOT\Directory\shell\EmacsEdit\command\'
    '\HKEY_CLASSES_ROOT\Directory\shell\bashHere\command\'
)

for x in "${REGS[@]}"; do regCreateSubKeys "$x"; done
regtool.exe -s set '\HKEY_CLASSES_ROOT\.sh' 'sh_auto_file'
regtool.exe -s set '\HKEY_CLASSES_ROOT\sh_auto_file\shell\open\command\' "$(qq_cmdout cygpath -alw "$BASH"; echo -n ' --rcfile ~/.bashrc-windows -i "%1" %*')"
regtool.exe -s set '\HKEY_CLASSES_ROOT\*\shell\emacsedit\command\' 'q:\bin\windows\redirect_vc6\emacsedit.exe -n "%1"'
regtool.exe -s set '\HKEY_CLASSES_ROOT\*\shell\bashHere\command\' 'q:\bin\windows\redirect_vc6\runHere bash "%1"'
regtool.exe -s set '\HKEY_CLASSES_ROOT\Directory\shell\EmacsEdit\command\' 'q:\bin\windows\redirect_vc6\emacsedit.exe -n "%1"'
regtool.exe -s set '\HKEY_CLASSES_ROOT\Directory\shell\bashHere\command\' 'q:\bin\windows\redirect_vc6\runHere bash "%1"'

