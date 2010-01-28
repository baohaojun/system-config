#!/usr/bin/env perl
use String::ShellQuote;
($myname, $myip, $remote_ip, $myfile) = (@ENV{WHOAMI, LOCALIP, REMOTEIP}, $ARGV[0]);

$ssh_cmd = "/scp:$myname\@$myip:$myfile";
$ssh_cmd = shell_quote($ssh_cmd);

if ($remote_ip eq 'localhost') {
    $ssh_cmd = <<EOC;
emacsclient -n $ssh_cmd
EOC

} else {
    $ssh_cmd = <<EOC;
psexec.exe -sid "\$(cygpath -alw "\$(which findexec)")" -t emacs@
emacsclient -n $ssh_cmd
EOC
}

$ssh_cmd =~ s/\n/;/g;
$ssh_cmd = shell_quote($ssh_cmd);

print "ssh $remote_ip $ssh_cmd\n";
system("ssh $remote_ip $ssh_cmd");

