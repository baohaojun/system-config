#!/usr/bin/perl
use Marpa::R2;
use strict;

my $grammar = Marpa::R2::Grammar->new(
    {   start   => 'Expression',
        actions => 'My_Actions',
        default_action => 'first_arg',
        rules   => [
            { lhs => 'Expression', rhs => [qw/Term/] },
            { lhs => 'Term', rhs => [qw/Factor/] },
            { lhs => 'Factor', rhs => [qw/Number/] },
            { lhs => 'Factor', rhs => [qw/LPAREN Expression RPAREN/], action => 'do_parens' },
            { lhs => 'Term', rhs => [qw/Term + Term/], action => 'do_add' },
            { lhs => 'Term', rhs => [qw/Term - Term/], action => 'do_subtract' },
            { lhs => 'Factor', rhs => [qw/Number Gmk Number/], action => 'do_gmk' },
            { lhs => 'Factor', rhs => [qw/Number Gmk/], action => 'do_gmk' },
            { lhs => 'Factor', rhs => [qw/Number Gmk Number Gmk Number/], action => 'do_gmk' },
            { lhs => 'Factor', rhs => [qw/Number Gmk Number Gmk/], action => 'do_gmk' },
            { lhs => 'Factor', rhs => [qw/Number Gmk Number Gmk Number Gmk Number/], action => 'do_gmk' },
            { lhs => 'Factor', rhs => [qw/Number Gmk Number Gmk Number Gmk/], action => 'do_gmk' },
            {   lhs    => 'Factor',
                rhs    => [qw/Factor * Factor/],
                action => 'do_multiply'
            },
            {   lhs    => 'Factor',
                rhs    => [qw/Factor \/ Factor/],
                action => 'do_divide'
            }
        ],
    }
);

$grammar->precompute();

my $recce = Marpa::R2::Recognizer->new( { grammar => $grammar } );

my $line = join(" ", @ARGV);

while ($line) {
    if ($line =~ s/^\s*\(//) {
        $recce->read( 'LPAREN', '(' );
    } elsif ($line =~ s/^\s*\)//) {
        $recce->read( 'RPAREN', ')' );
    } elsif ($line =~ s/^\s*([0-9]+)//) {
        $recce->read( 'Number', $1 );
    } elsif ($line =~ s/^\s*0x([0-9a-f]+)//i) {
        $recce->read( 'Number', hex $1 );
    } elsif ($line =~ s,^\s*([-+*/]),,i) {
        $recce->read( $1 );
    } elsif ($line =~ s,^\s*([gmk]),,i) {
        $recce->read( 'Gmk', lc $1 );
    } elsif ($line =~ s,^\s*$,,) {
        last;
    }
}


# $recce->read( 'Number', 42 );
# $recce->read('/');
# $recce->read( 'LPAREN', '(' );
# $recce->read( 'Number', 2 );
# $recce->read('-');
# $recce->read( 'Number', 7 );
# $recce->read( 'RPAREN', ')' );

sub My_Actions::do_add {
    my ( undef, $t1, undef, $t2 ) = @_;
    return $t1 + $t2;
}

sub My_Actions::do_gmk {
    my ( undef, $t1, $op1, $t2, $op2, $t3, $op3, $t4 ) = @_;
    if ($op1 eq "g") {
        $t1 *= 1024 ** 3;
    } elsif ($op1 eq "m") {
        $t1 *= 1024 ** 2;
    } elsif ($op1 eq "k") {
        $t1 *= 1024;
    } else {
        return $t1;
    }
    return $t1 + My_Actions::do_gmk(undef, $t2, $op2, $t3, $op3, $t4)
}

sub My_Actions::do_subtract {
    my ( undef, $t1, undef, $t2 ) = @_;
    return $t1 - $t2;
}

sub My_Actions::do_multiply {
    my ( undef, $t1, undef, $t2 ) = @_;
    return $t1 * $t2;
}

sub My_Actions::do_divide {
    my ( undef, $t1, undef, $t2 ) = @_;
    return $t1 / $t2;
}

sub My_Actions::do_parens {
    my ( undef, undef, $t, undef ) = @_;
    return $t;
}

sub My_Actions::first_arg { shift; return shift; }

my $value_ref = $recce->value;
my $value = $value_ref ? ${$value_ref} : 'No Parse';

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

printf "%s %d 0x%x", pretty $value, $value, $value;
