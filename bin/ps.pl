#!/bin/bash
export MY_PID=$$
if test $(basename $0) = ps.pl0; then
    export MATCH_PROG_ONLY=true
fi

opts=
TEMP=$(getopt -o o: --long opts: -n $(basename $0) -- "$@")
eval set -- "$TEMP"
while true; do
    case "$1" in
        -o|--opts)
            opts=$2,
            shift 2
            ;;
        --)
            shift
            break
            ;;
        *)
            die "internal error"
            ;;
    esac
done

export ARGS=$@
MORE_OPTS_VARS=
if test "$opts"; then
    export MORE_OPTS_VARS=$(echo "$opts" | perl -npe 's/(^|,(?!$))/$1\$VAR_/g')
fi
ps -eo pid,ppid,etime,${opts}command | perl -ne '
BEGIN{
    $found = 0;
    @args=split(" ", $ENV{"ARGS"});
    $my_pid = $ENV{"MY_PID"};
}
$to_match = $line = $_;
$match = 1;

if ($. == 1) {
    print STDERR $_;
    next;
}

($pid, $ppid, $elapsed,'"$MORE_OPTS_VARS"'$prog) = split(" ", $line);
if ($ENV{MATCH_PROG_ONLY} eq "true")  {
    $to_match = $prog;
}
next if ($pid == $my_pid or $ppid == $my_pid);
for (@args) {
    for (split(/\.|\\* /, $_)) {
        if ($_ =~ s/^!//) {
            if ($to_match =~ m/$_/i) {
                $match = 0;
                last;
            } else {
                next;
            }
        }
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
