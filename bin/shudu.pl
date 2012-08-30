#!/usr/bin/perl

$l = 0;

while (<>) {
    chomp;
    last if $l == 9;
    $row[$l] = $_;
    $l++;
    die "invalid input: not 9 chars" if length($_) != 9;
    
    die "invalid input: non-digit, non-blank" if m/[^0-9 ]/;
    
    die "invalid input: repeated digit" if m/([0-9]).*\1/;
    
}

die "not enough rows" if $l != 9;

for $col (0..8) {
    for $row (0..8) {
        $col[$col] .= substr($row[$row], $col, 1);
        $m[$row][$col] = substr($row[$row], $col, 1);
    }
}

for (0..8) {
    die "invalid input: repeated digit" if $col[$_] =~ m/([0-9]).*\1/;
}

for $area (0..8) {
    $arow = int($area/3);
    $acol = int($area%3);
    my $area_str = "";
    for $i (0..8) {
        $irow = $arow*3 + int($i/3);
        $icol = $acol*3 + int($i%3);
        $area_str .= $m[$irow][$icol];
    }
    die "invalid input: repeated digit in area[$area]: $area_str" if $area_str =~ m/([0-9]).*\1/;
}

sub area_contains ($$$$)
{
    my ($m, $r, $c, $try) = @_;
    $r = int($r/3) * 3;
    $c = int($c/3) * 3;

    for $ar (0..2) {
        for $ac (0..2) {
            if ($m->[$r+$ar][$c+$ac] == $try) {
                return 1;
            }
        }
    }
    return 0;
}

sub fill_one_cell ($) {
    my ($m) = @_;
    my @dup;
    my ($found, $br, $bc);

    for $r (0..8) {
        for $c (0..8) {
            $dup[$r][$c] = $m->[$r][$c];
            if (not $found and $m->[$r][$c] eq " ") {
                $found = 1;
                $br = $r;
                $bc = $c;
                print "found space at [$r][$c]\n";
            }
        }
    }

    if (not $found) {
        return \@dup;
    }

    for my $try (1..9) {
        my $go = 1;
        for $x (0..8) {
            if ($m->[$x][$bc] == $try or $m->[$br][$x] == $try) {
                $go = 0;
                last;
            } else {
                
            }
        }

        if (area_contains($m, $br, $bc, $try)) {
            print "area at $br $bc contains $try\n";
            $go = 0;
        }

        if ($go == 0 and $try == 9) {
            return 0;
        }
        
        if ($go == 0) {
            next;
        }
        $dup[$br][$bc] = $try;
        print "try $try at [$br][$bc]\n";
        if ((my $res = fill_one_cell(\@dup)) != 0) {
            for $r (0..8) {
                for $c (0..8) {
                    print $res->[$r][$c];
                }
                print "\n";
            }
            exit(0);
        }
    }
                   }
                
            
           

fill_one_cell(\@m);
