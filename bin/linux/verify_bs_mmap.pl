#!/usr/bin/env perl

use strict;
use Getopt::Long;
use lib glob("~/bin");
use pretty;

my ($from, $to, $check, $max_addr) = ("", "", "gap", "512M");
my @entry_arr;


sub verify(&)
{
    (my $func) = @_;
    for our $entry (@entry_arr) {
        &$func($entry);
     }
}

sub print_human
{
    our $entry;
    printf ("%-17s = %-38s, start: %9s, size: %9s, end: %9s\n",
            $entry->{"name"}, 
            $entry->{file_name}, 
            pretty($entry->{"start"}),
            pretty($entry->{"size"}),
            pretty($entry->{start} + $entry->{size}));
}

sub print_memmap
{
    our $entry;
    printf("%-17s = %-38s, 0x%08x,0x%08x\n",
           $entry->{name},
           $entry->{file_name},
           $entry->{start},
           $entry->{size});
}

sub print_blob
{
    our $entry;
    our $print_header;
    BEGIN{
        if (not $print_header) {
            $print_header = 1;
        }
    }
    if ($print_header) {
        print 'char mtdparts[] ="mtdparts=onenand:\\' . "\n";
        $print_header = 0;
    }
        
    printf("0x%08x\@0x%08x(%s)\\\n", 
           $entry->{size},
           $entry->{start},
           $entry->{name});
    END{
        if (not $print_header) {
            print ';";'
        }
    }
}

sub print_kernel
{
    our $entry;
    our $seq = 0 unless defined $seq;
    
    our $print_header_ker;
    BEGIN{
        if (not $print_header_ker) {
            $print_header_ker = 1;
        }
    }
    if ($print_header_ker) {
        printf('static struct mtd_partition android_%s_v75_partitions[] = {' . "\n", pretty($max_addr));
        $print_header_ker = 0;
    }
        
    my $fmt = <<End;
	[%d] = {
		.name        = "%s",
		.offset      = 0x%08x,
		.size        = 0x%08x,
	},
End

    printf($fmt, 
           $seq++,
           $entry->{name},
           $entry->{start},
           $entry->{size}) unless $entry->{name} =~ m/^unknown/;

    END{
        if (not $print_header_ker) {
            print '};'
        }
    }
}

sub do_read_memmap
{
    while(<>) {
        chomp();
        if (m/^\s*(.*?)\s*=\s*(.*?)\s*,\s*(.*?)\s*,\s*(.*?)\s*$/) {
            my %entry = 
                ( "name"   => $1, 
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
        if (m/^\s*(.*?)\s*=\s*(.*?)\s*, \s*start: \s*(.*?), \s*size: \s*(.*?), \s*end: \s*(.*?)$/) {
            my %entry = 
                ( "name"   => $1, 
                  "file_name"    => $2,
                  "start"        => un_pretty($3),  # so that such as "0x1024" can be recognized
                  "size"         => un_pretty($4),
                  "end"          => un_pretty($5),
                  "pretty_start" => $3,
                  "pretty_size"  => $4,
                );

            if (0) {
                die sprintf("Error: entry `%s' start `%s' + size `%s' = `%s' != end `%s'!!!", 
                            $entry{name},
                            $entry{pretty_start},
                            $entry{pretty_size},
                            pretty($entry{start} + $entry{size}),
                            pretty($entry{end}))
                    if $entry{start} + $entry{size} != $entry{end};
            }

            push @entry_arr, \%entry;
        }
    }
}

sub do_read_blob
{
    while(<>) {
        chomp();
        while (m/(0x.*?)\@(0x.*?)\((.*?)\)/g) {
            my %entry = 
                ( "name"   => $3, 
                  "file_name"    => "unknown",
                  "start"        => eval($2),  # so that such as "0x1024" can be recognized
                  "size"         => eval($1),
                  "end"          => eval($2) + eval($1),
                  "pretty_start" => pretty(eval($2)),
                  "pretty_size"  => pretty(eval($1)),
                );

            push @entry_arr, \%entry;
        }
    }
}

sub do_read_kernel
{
    my ($name, $offset, $size);
    while(<>) {
        chomp();

        if (m/\.name.*?=.*?"(.*)"/) {
            $name = $1;
        } elsif (m/\.offset.*=\s*(.*?),/) {
            $offset = eval($1);
        } elsif (m/\.size.*=\s*(.*),/) {
            $size = eval($1);
        } else {
            if (defined $name and defined $offset and defined $size) {
                push @entry_arr, {
                    "size"       => $size,
                    "start"      => $offset,
                    "name" => $name,
                    "file_name"  => "unknown",
                };
                ($name, $offset, $size) = ();
            }
        }
    }
}

chomp($0 = `basename $0`);

sub Usage
{
    die "Error: Usage: $0 -f from_format -t to_format -c check_errors  -m max_address\n" . 
        "\n" . 
        "    Available formats: blob, map, human, kernel\n" . 
        "    max_address should be 128M/256M/512M\n";

    
}

GetOptions(
    "f=s" => \$from,
    "c=s" => \$check,
    "t=s" => \$to,
    "m=s" => \$max_addr,
    );

$max_addr = un_pretty($max_addr) if ($max_addr =~ m/[mk]/i);
$max_addr = eval($max_addr);

if ($from eq 'human') {
    do_read_human();
} elsif ($from eq 'map') {
    do_read_memmap();
} elsif ($from eq 'blob') {
    do_read_blob();
} elsif ($from eq "kernel") {
    do_read_kernel();
} else {
    Usage();
}

@entry_arr = sort {$a->{"start"} <=> $b->{"start"}} @entry_arr;
push @entry_arr, {
    "name" => "unknown_max",
    "file_name" => "unknown",
    "start" => eval($max_addr),
    "size" => 0};

if (1) {
    my @entry_arr_back;
    my $last_entry;
    for my $entry (@entry_arr) {
        my ($start, $size, $gap_entry, $last_start, $last_size) = 
            ($entry->{"start"}, $entry->{"size"});

        if ($last_entry) {
            $last_start = $last_entry->{"start"};
            $last_size = $last_entry->{"size"};

            if ($check =~ m/overlap/) {
                die sprintf("Error: last entry `%s' (%s-%s) overlaps with current entry `%s' (%s-%s)", 
                            $last_entry->{name}, 
                            pretty($last_start),
                            pretty($last_start + $last_size),
                            $entry->{name},
                            pretty($start),
                            pretty($start + $size))
                    # if the last entry is completely the same size/offset as the current entry, don't consider it an error.
                    if $last_start + $last_size > $start and not ($last_start == $start and $last_size == $size); 
            } elsif ($check =~ m/shrink/) {
                $entry->{start} = $start = $last_start + $last_size 
                    if $last_start + $last_size > $start;
            }
            
            if ($check =~ m/\bgap/) {
                push @entry_arr_back, $gap_entry = {
                    "name" => "unknown_gap",
                    "file_name" => "unknown_gap",
                    "start" => $last_start + $last_size, 
                    "size" => $start - ($last_start + $last_size),
                } if $last_start + $last_size < $start;
            } elsif ($check =~ m/enlarge/) {
                if ($last_start + $last_size < $start) {
                    $last_entry->{size} = $start - ($last_start + $last_size);
                    $last_entry->{file_name} .= "-enlaged";
                } else {
                    $entry->{start} = $last_start + $last_size;
                }
            }
        }

        push @entry_arr_back, $entry 
            unless $last_entry and $last_entry->{name} eq $entry->{name} and
            $last_size == $size and 
            $last_start == $start;

        $last_entry = $entry;
    }
    @entry_arr = @entry_arr_back;
    @entry_arr = sort {$a->{"start"} <=> $b->{"start"}} @entry_arr;
}

if ($to eq "human") {
  verify(\&print_human);
} elsif ($to eq "map") {
  verify(\&print_memmap);
} elsif ($to eq "blob") {
  verify(\&print_blob);
} elsif ($to eq "kernel") {
  verify(\&print_kernel);
} else {
  Usage();
}
