#!/usr/bin/env perl

use strict;
my @entry_arr;

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

sub verify(&$@)
{
    (my $func, my $flag, my @entry_arr) = @_;
    our $last_entry;
    for our $entry (@entry_arr) {
        if ($last_entry) {
            my $start = $entry->{"start"};
            my $size  = $entry->{"size"};
            my $last_start = $last_entry->{"start"};
            my $last_size = $last_entry->{"size"};

            if ($flag) {
                die sprintf("Error: last entry `%s' overlaps with current entry `%s' with %s", 
                            $last_entry->{entry_name}, 
                            $entry->{entry_name},
                            pretty($last_entry + $last_size - $start))
                    if $last_start + $last_size > $start;
                
                die sprintf("Error: last entry `%s' waste space by %s", 
                            $last_entry->{entry_name},
                            pretty($start - $last_start - $last_size))
                    if $last_start + $last_size < $start;
            }
        }
        &$func($entry);
        $last_entry = $entry;
     }
}

sub print_corrected_human
{
    our ($entry, $last_entry);

    if ($last_entry) {
        $entry->{start} = $last_entry->{start} + $last_entry->{size};
    }    
    printf ("%-17s = %-38s, start: %9s, size: %9s, end: %9s\n",
            $entry->{"entry_name"}, 
            $entry->{file_name}, 
            pretty($entry->{"start"}),
            pretty($entry->{"size"}),
            pretty($entry->{start} + $entry->{size}));
}

sub print_corrected_memmap
{
    our ($entry, $last_entry);
    
    if ($last_entry) {
        $entry->{start} = $last_entry->{start} + $last_entry->{size};
    }
    printf("%-17s = %-38s, 0x%08x,0x%08x\n",
           $entry->{entry_name},
           $entry->{file_name},
           $entry->{start},
           $entry->{size});
}

# verify(\&print_corrected_memmap, 0, @entry_arr);


sub do_read_memmap
{
    while(<>) {
        chomp();
        if (m/^\s*(.*?)\s*=\s*(.*?)\s*,\s*(.*?)\s*,\s*(.*?)\s*$/) {
            my %entry = 
                ( "entry_name"   => $1, 
                  "file_name"    => $2,
                  "start"        => eval($3),  # so that such as "0x1024" can be recognized
                  "size"         => eval($4),
                  "pretty_start" => pretty(eval($3)),
                  "pretty_size"  => pretty(eval($4)),
                );
            
            push @entry_arr, \%entry;
        }
    }
}

sub do_read_human
{
    while(<>) {
        chomp();
        if (m/^\s*(.*?)\s*=\s*(.*?)\s*, start: \s*(.*?), size: \s*(.*?), end: \s*(.*?)$/) {
            my %entry = 
                ( "entry_name"   => $1, 
                  "file_name"    => $2,
                  "start"        => un_pretty($3),  # so that such as "0x1024" can be recognized
                  "size"         => un_pretty($4),
                  "end"          => un_pretty($5),
                  "pretty_start" => $3,
                  "pretty_size"  => $4,
                );
            
            die sprintf("Error: entry `%s' start `%s' + size `%s' = `%s' != end `%s'!!!", 
                        $entry{entry_name},
                        $entry{pretty_start},
                        $entry{pretty_size},
                        pretty($entry{start} + $entry{size}),
                        pretty($entry{end}))
                if $entry{start} + $entry{size} != $entry{end};

            push @entry_arr, \%entry;
        }
    }
                  
}

if ($0 =~ m/map2human/) {
    do_read_memmap();
    @entry_arr = sort {$a->{"start"} <=> $b->{"start"}} @entry_arr;
    verify(\&print_corrected_human, 0, @entry_arr);
} else {
    do_read_human();
    @entry_arr = sort {$a->{"start"} <=> $b->{"start"}} @entry_arr;
    verify(\&print_corrected_memmap, 0, @entry_arr);
}
