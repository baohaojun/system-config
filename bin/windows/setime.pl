#!/usr/bin/perl

$seq = $ARGV[0];
$kbd = $ARGV[1] || "kbddvp.dll";
$reg = <<EOF;
Windows Registry Editor Version 5.00

[HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Keyboard Layouts\\E0${seq}0804]
"Ime File"="ywbhj${seq}.dll"
"Layout File"="$kbd"
"Layout Text"="Chinese (Simplified) - YWB${seq}"

EOF

print $reg;
