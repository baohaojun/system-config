#!/bin/perl

$Processes=`wmic.exe path win32_process get CommandLine,ProcessId,ParentProcessId </dev/null`;
@Processes = split /\r*\n/, $Processes;

$Processes[0] =~ m/^(CommandLine.*)(ParentProcessId.*)(ProcessId.*)$/ or #the order of pid/ppid seems to be random with wmic?
    die "can't match the first line";

$\="\n";
printf "%9s%9s  %s\n", PID, PPID, COMMAND;
foreach (@Processes[1..$#Processes]) {
    m/^(.*?)\s*([0-9]+)\s*([0-9]+)\s*$/ or die "can't match $_";
    printf "%9d%9d  %s\n", $3, $2, $1;
}

system('rm TempWmicBatchFile.bat')
