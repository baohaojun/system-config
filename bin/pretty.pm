sub pretty($) {
    my $sign = "";
    (my $num) = (@_);
    if ($num < 0) {
	$num = -$num;
	$sign = "-";
    }
    $num = $num;
    $num =~ s/.*?((0x)?[a-f0-9]*).*/$1/i;
    $num = eval($num);

    my $kremain = $num % 1024;
    my $mremain = $num % (1024*1024);
    my $gremain = $num % (1024*1024*1024);


    my $g_num = ($num - $gremain) / (1024*1024*1024);
    my $m_num = ($gremain - $mremain) / (1024*1024);
    my $k_num = ($mremain - $kremain) / 1024;

    my $b_num = $kremain;

    my $pretty;
    $pretty = sprintf("%dG", $g_num) if $g_num;
    $pretty .= sprintf("%dM", $m_num) if $m_num;
    $pretty .= sprintf("%dK", $k_num) if $k_num;
    $pretty .= sprintf("%dB", $b_num) if $b_num;

    $pretty = "0B" if not $pretty;
    return $sign . $pretty;
}

sub debug(@) {
    #print STDERR "@_\n";
}

use POSIX;
sub un_pretty($) 
{
    my ($g_num, $m_num, $k_num, $b_num, $gremain, $mremain, $kremain);
    my ($un_pretty) = (@_);

    debug("un_pretty is $un_pretty");
    $un_pretty =~ s/\s*//g;

    if ($un_pretty =~ m/^(0x)?[a-f0-9]*$/) {
      return oct ($un_pretty) if $un_pretty =~ /^0/;
      return $un_pretty;
    }
    if ($un_pretty =~ s/^([^-+*\/]*?)G//i) {
      ($g_num) = strtol($1, 0);
    }

    if ($un_pretty =~ s/^([^-+*\/]*?)M//i) {
        ($m_num) = strtol($1, 0);
    }

    if ($un_pretty =~ s/^([^-+*\/]*?)K//i) {
	debug("matched K: $1");
        ($k_num) = strtol($1, 0);
    }

    if ($un_pretty =~ s/^([^-+*\/]*?)B?(?=\+|-|\*|\/|$)//i) {
	debug("matched B: $1");
        ($b_num) = strtol($1, 0);
    }

    my $ret = $g_num * 1024 * 1024 * 1024
	+ $m_num * 1024 * 1024
	+ $k_num * 1024
	+ $b_num;

    if ($un_pretty =~ s/^(\+|-)//) {
      my $factor = $1 eq "+" ? 1 : -1;
      $ret += $factor * un_pretty($un_pretty);
    } elsif ($un_pretty =~ s/^(\*|\/)//) {
	if ($1 eq '*') {
	    $ret *= un_pretty($un_pretty);
	} else {
	    $ret /= un_pretty($un_pretty);
	}
    }
    return $ret;
}

1;
