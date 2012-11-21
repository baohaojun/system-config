#!/bin/bash
export ARGS=$@
export MY_PID=$$
echo "  PID  PPID     ELAPSED CMD" 1>&2

ps -eo pid,ppid,etime,cmd | perl -ne '
BEGIN{
    $found = 0;
    @args=split(/\s+/, $ENV{"ARGS"});
    $my_pid = $ENV{"MY_PID"};
}
$line = $_;
$match = 1;

($pid, $ppid) = split(/\s+/, $line);
next if ($pid == $my_pid or $ppid == $my_pid);
for (@args) {
    for (split(/\.|\\* /, $_)) {
        unless ($line =~ m/$_/i) {
            $match = 0;
            last;
        }
    }

}

if ($match) {
    print $line;
    $found = 1;
}

END{
    if (not $found) {
        exit 1;
    } else {
        exit 0;
    }
}
'
