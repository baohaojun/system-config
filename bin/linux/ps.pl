#!/bin/bash
export ARGS=$@
export MY_PID=$$
ps -eo pid,ppid,etime,cmd | perl -ne '
BEGIN{
    @args=split(/\s+/, $ENV{"ARGS"});
    $my_pid = $ENV{"MY_PID"};
}
$line = $_;
$match = 1;

($pid, $ppid) = split(/\s+/, $line);
next if ($pid == $my_pid or $ppid == $my_pid);
for (@args) {
    unless ($line =~ m/$_/i) {
        $match = 0;
        last;
    }

}

if ($match) {
    print $line;
}'
