#!/usr/bin/env perl
use Net::OpenSSH; 
($myname, $myip, $remote_ip, $myfile) = (@ENV{WHOAMI, LOCALIP, REMOTEIP}, $ARGV[0]);

$ssh_cmd = "/scp:$myname\@$myip:$myfile";
$ssh_cmd = Net::OpenSSH->shell_quote($ssh_cmd);

$ssh_cmd = <<EOC;
psexec.exe -sid "\$(cygpath -alw "\$(which findexec)")" -c emacs-
emacsclient -n $ssh_cmd
EOC

$ssh_cmd =~ s/\n/;/g;
$ssh_cmd = Net::OpenSSH->shell_quote($ssh_cmd);

print "ssh $remote_ip $ssh_cmd\n";
system("ssh $remote_ip $ssh_cmd");

