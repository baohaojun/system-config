#!/bin/bash
export ARGS=$@
export MY_PID=$$
echo "  PID  PPID     ELAPSED CMD" 1>&2
if test $(basename $0) = ps.pl0; then
    export MATCH_PROG_ONLY=true
fi

ps -eo pid,ppid,etime,command | perl -ne '
BEGIN{
    $found = 0;
    @args=split(/\s+/, $ENV{"ARGS"});
    $my_pid = $ENV{"MY_PID"};
}
$to_match = $line = $_;
$match = 1;

($pid, $ppid, $elapsed, $prog) = split(/\s+/, $line);
if ($ENV{MATCH_PROG_ONLY} eq "true") {
    $to_match = $prog;
}
next if ($pid == $my_pid or $ppid == $my_pid);
for (@args) {
    for (split(/\.|\\* /, $_)) {
        unless ($to_match =~ m/$_/i) {
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
