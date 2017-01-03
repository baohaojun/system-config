#!/usr/bin/perl
use strict;
my $state_normal = 0;
my $state_string = 1;
my $state_line_comment = 2;
my $state_block_comment = 3;
my $state_blank = 4;

my $state = $state_normal;
my $string_start;
my $indent;
unless (@ARGV) {
    @ARGV = ("/dev/stdin");
}

sub debug(@) {
    print STDERR "@_\n";
}

sub print_line(\@) {
    my ($line_ref) = @_;
    return unless @$line_ref;
    my $line = join('', @$line_ref, "\n");
    $line =~ s/ +([^0-9a-zA-Z_])/$1/g;
    $line =~ s/([^0-9a-zA-Z_]) +/$1/g;
    print ' 'x ($indent * 4) . $line;
    @$line_ref = ();
}

sub read1_no_cr($\$) {
    my ($file, $var) = @_;
    my $ret;
    {
        do {
            $ret = read $file, ${$var}, 1 or last;
        } while (${$var} eq "\r");
    }

    return $ret;
}

if (not @ARGV) {
    @ARGV = ("/dev/stdin");
}

for my $arg (@ARGV) {
    my $file;
    if ($file) {
        close $file;
    }
    if (not open($file, "<", $arg)) {
        warn "Error: can not open $arg\n";
        next;
    }

    $state = $state_normal;
    my $unget;
    my @line = ();

    $indent = 0;
  read_loop:
    while (1) {
        my $c;
        if (defined $unget) {
            $c = $unget;
            undef $unget;
        } else {
            read $file, $c, 1 or last;
            next if ($c eq "\r");
        }

        if ($state == $state_normal) {
            if ($c eq '"') {
                $state = $state_string;
                $string_start = '"';
            } elsif ($c eq "'") {
                $state = $state_string;
                $string_start = "'";
            } elsif ($c eq '/') {
                read $file, $unget, 1 or last;
                if ($unget eq '/') {
                    undef $unget;
                    do {
                        if ($c eq "\n") {
                            push @line, " " if @line;
                            next read_loop;
                        }
                    } while (read $file, $c, 1);
                } elsif ($unget eq '*') {
                    undef $unget;
                    $state = $state_block_comment;
                } else {
                    push @line, $c;
                }
            } elsif ($c eq ' ' or $c eq "\t" or $c eq "\f" or $c eq "\n") {
                if ($c eq "\n" and @line and ($line[0] eq '#' or $line[0] eq '@')) {
                    print_line @line;
                } else {
                    $state = $state_blank;
                }
            } elsif ($c eq "\\") {
                read $file, $unget, 1 or last read_loop;
                if ($unget eq "\n") {
                    undef $unget;
                    push @line, " " if @line;
                } else {
                    push @line, $c;
                }
            } else {
                if ($c eq '#') {
                    print_line @line;
                }
                push @line, $c;
                if ($line[0] ne "#" and ($c eq ";" or $c eq '{' or $c eq '}')) {
                    if ($c eq '}') {
                        $indent--;
                    }
                    print_line @line;
                    if ($c eq '{') {
                        $indent++;
                    }           }
            }
        } elsif ($state == $state_string) {
            do {
                if ($c eq $string_start) {
                    push @line, "$c$c";
                    $state = $state_normal;
                    next read_loop;
                } elsif ($c eq "\n") { # forgive run away string
                    push @line, "''";
                    $state = $state_normal;
                    next read_loop;
                }
            } while (read $file, $c, 1);
        } elsif ($state == $state_block_comment) {
            do {
                undef $unget;
                if ($c eq "*") {
                    read $file, $unget, 1 or last read_loop;
                    if ($unget eq '/') {
                        undef $unget;
                        push @line, " " if @line;
                        $state = $state_normal;
                        next read_loop;
                    }
                }
            } while (($c = $unget) or (read $file, $c, 1));
        } elsif ($state == $state_blank) {
            do {
                if ($c eq "\n" and @line and $line[0] eq '#') {
                    print_line @line;
                }
                unless ($c eq ' ' or $c eq "\t" or $c eq "\f" or $c eq "\n") {
                    push @line, " " if @line;
                    $unget = $c;
                    $state = $state_normal;
                    next read_loop;
                }
            } while (read $file, $c, 1);
        }
    }
}
