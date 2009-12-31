#!/bin/perl

$seq = $ARGV[0];
$reg = <<EOF;
Windows Registry Editor Version 5.00

[HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Keyboard Layouts\\E0%s0804]
"Ime File"="ywbhj%s.dll"
"Layout File"="kbdus.dll"
"Layout Text"="Chinese (Simplified) - YWB%s"

EOF

printf $reg, $seq, $seq, $seq
