sub pretty($) {
    (my $num) = (@_);
    $num = $num;

    my $kremain = $num % 1024;
    my $mremain = $num % (1024*1024);
    
    my $m_num = ($num - $mremain) / (1024*1024);
    my $k_num = ($mremain - $kremain) / 1024;

    my $b_num = $kremain;

    my $pretty;
    $pretty = sprintf("%dM", $m_num) if $m_num;
    $pretty .= sprintf("%dK", $k_num) if $k_num;
    $pretty .= sprintf("%dB", $b_num) if $b_num;

    $pretty = "0B" if not $pretty;
    return $pretty;
}

sub un_pretty($) 
{
    my ($m_num, $k_num, $b_num, $mremain, $kremain);
    my ($un_pretty) = (@_);

    $un_pretty =~ s/\s*//g;

    if ($un_pretty =~ s/^(.*)M//i) {
        $m_num = $1;
    }
    
    if ($un_pretty =~ s/^(.*)K//i) {
        $k_num = $1;
    }

    if ($un_pretty =~ s/^(.*)B//i) {
        $b_num = $1;
    }
    return $m_num * 1024 * 1024 + $k_num * 1024 + $b_num;
}

1;
