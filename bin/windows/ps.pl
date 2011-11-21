#!/bin/perl

use POSIX;

$Processes=`wmic.exe path win32_process get CommandLine,CreationDate,ProcessId,ParentProcessId </dev/null`;
@Processes = split /\r*\n/, $Processes;

$Processes[0] =~ m/^(CommandLine.*)(CreationDate.*)(ParentProcessId.*)(ProcessId.*)$/ or #the order of pid/ppid seems to be random with wmic?
    die "can't match the first line:\n" . $Processes[0];

$\="\n";
my ($cmd_n, $create_n, $ppid_n, $pid_n) = map(length, ($1, $2, $3, $4));

my $width = 12;
printf "%9s%9s  %$width" . "s  %s\n", "PID", "PPID", "CreationDate", "COMMAND";
sub my_mktime($) 
  {
    my $create= $_[0];
    my ($year, $mon, $mday, $hour, $min, $sec) = 
      (
       substr($create, 0, 4) - 1900,
       substr($create, 4, 2) - 1,
       substr($create, 6, 2),
       substr($create, 8, 2),
       substr($create, 10, 2),
       substr($create, 12, 2)
      );

    return mktime($sec, $min, $hour, $mday, $mon, $year);
  }
  
foreach (@Processes[1..$#Processes]) {
    chomp();
    my ($cmd, $create, $ppid, $pid) = (substr($_, 0, $cmd_n),
				       substr($_, $cmd_n, $create_n),
				       substr($_, $cmd_n + $create_n, $ppid_n),
				       substr($_, $cmd_n + $create_n + $ppid_n, $pid_n));
    if (@ARGV) {
	my $match = 1;
	for (@ARGV) {
	    unless ($cmd =~ m/$_/i) {
		$match = 0;
		last;
	    }
	}
	next unless $match;
    }

    
    $cmd =~ s/(\s*$|^\s*)//g;
    $create =~ s/(\s*$|^\s*)//g;
    $ppid =~ s/(\s*$|^\s*)//g;
    $pid =~ s/(\s*$|^\s*)//g;
    
    next if $ppid == $$;
    my $start_second = my_mktime($create);
    my $now = my_mktime(qx(date +'%Y%m%d%H%M%S'));

    printf "%9d%9d  %$width" . "s  %-s\n", $pid, $ppid, $now - $start_second, $cmd;
}

