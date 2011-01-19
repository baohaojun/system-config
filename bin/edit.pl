#!/usr/bin/env perl
use String::ShellQuote;
use strict;
my ($myname, $myip, $remote_ip, $myfile) = (@ENV{"WHOAMI", "LOCALIP", "REMOTEIP"}, $ARGV[0]);

sub edit_1($)
{
  my $file=$_[0];
  $file =~ s,.*file://,,;
  if (-e $file) {
    $file = shell_quote($file);
    chomp($file = qx(readlink -f $file));
    return ('emacsclient', '-n', $file);
  } else {
    # external/opencore/android/mediascanner.cpp:622: error: invalid conversion from ‘const char*’ to ‘char*’  
    $file =~ m,^[a|b]/, and $file = substr($file, 2); # for git diff output
    $file =~ m,:$, and chop($file);

    my $line=1;
    $file =~ s,:([0-9]+)$,, and $line = $1;

    my $x=0;
    while (not -e $file and $x ++ < 10) {
      $file = "../$file";            
    }
    $file = shell_quote($file);
    chomp($file = qx(readlink -f $file));
    return ('emacsclient', '-n', "+$line", $file);
  }
}

if ($remote_ip) {
  my $ssh_cmd = "/scp:$myname\@$myip:$myfile";
  $ssh_cmd = shell_quote($ssh_cmd);

  $ssh_cmd = <<EOC;
if test `uname` != Linux; then
    psexec.exe -sid "\$(cygpath -alw "\$(which findexec)")" -t emacs@
fi
emacsclient -n $ssh_cmd
EOC
  $ssh_cmd = shell_quote($ssh_cmd);

  system("ssh $remote_ip $ssh_cmd");

} else {
  system(edit_1($myfile));
}
