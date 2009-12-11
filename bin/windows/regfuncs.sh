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
