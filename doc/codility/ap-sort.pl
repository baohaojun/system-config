#!/usr/bin/perl

use strict;

sub debug(@) {
    print STDERR "@_\n";
}

sub cmp_handles($$) {
    my ($a, $b) = @_;
    for (0..$#$a) {
        if ($$a[$_] <=> $$b[$_]) {
            return $$a[$_] <=> $$b[$_];
        }
    }
    return 0;
}

sub lcs(@) {
    my (@S) = @_;
    my $n = @S;
    push @S, @S;

    my @f = @S;
    for (@f) {
        $_ = -1;
    }


    # for j in range(1, 2*n):
    #     i = f[j-k-1]
    #     while i != -1 and S[j] != S[k+i+1]:
    #         if S[j] < S[k+i+1]:
    #             k = j-i-1
    #         i = f[i]
    #     if i == -1 and S[j] != S[k+i+1]:
    #         if S[j] < S[k+i+1]:
    #             k = j
    #         f[j-k] = -1
    #     else:
    #         f[j-k] = i+1
    # return k
    my $k = 0;
    for my $j (1 .. $#S) {
        my $i = $f[$j -$k -1];
        while ($i != -1 and $S[$j] != $S[$k + $i + 1]) {
            if ($S[$j] < $S[$k + $i + 1]) {
                $k = $j - $i - 1;
            }
            $i = $f[$i];
        }
        if ($i == -1 and $S[$j] != $S[$k + $i + 1]) {
            if ($S[$j] < $S[$k + $i + 1]) {
                $k = $j;
            }
            $f[$j - $k] = -1;
        } else {
            $f[$j - $k] = $i + 1;
        }
    }
    return @S[$k .. $k + @S / 2 - 1];
}


sub solution {
    my ($A, $P)=@_; my @A=@$A;
    # write your code here...

    for my $handles (@A) { # N
        @$handles = sort { $a <=> $b } @$handles; # M * Log(M)

        my $h0 = $$handles[0];
        for my $handle (@$handles) {
            $handle -= $h0;
        }

        my @diff;
        push @$handles, $P;
        for (1..$#$handles) {
            my $x = ($$handles[$_] - $$handles[$_ - 1]) % $P;
            push @diff, $x;
        }

        @$handles = lcs(@diff);
    }
    @A = sort {cmp_handles($a, $b)} @A;

    # debug "A is ";
    # for (@A) {
    #     debug "(@$_), ";
    # }
    # debug "\n";

    my $total = 0;
    my $current = 0;
    my $look_ahead = $current;

    while ($current < @A) {
        while ($look_ahead < @A) {
            if (cmp_handles($A[$current], $A[$look_ahead]) == 0) {
                $look_ahead++;
            } else {
                if ($look_ahead != $current) {
                    $total += ($look_ahead - $current) * ($look_ahead - $current - 1) / 2;
                }
                $current = $look_ahead;
                last;
            }
        }
        if ($look_ahead == @A) {
            $total += ($look_ahead - $current) * ($look_ahead - $current - 1) / 2;
            last;
        }
    }
    return $total;
}

