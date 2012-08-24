#!/usr/bin/perl

use strict;
chdir glob("~/external/ahd")
    or die 'Can not chdir "~/external/ahd"';

open(my $find_words_pipe, "-|", "find a/a/ -type f")
    or die 'Can not open find program to find all the words';

sub debug(@) {
    print STDERR "@_\n";
}

open(my $dictz_file, ">", "ahd.dz")
    or die "Can not open ahd.dz";

my %word_def_info;
my %word_def_written;
while (<$find_words_pipe>) {
    chomp;
    my $word = substr($_, 6);

    debug "handling $word";
    
    open(my $word_file, "<", $_)
	or die "Can not open $_";
    
    my @word_defs = map {chomp; $_} <$word_file>;
    close $word_file;

    if (@word_defs > 1) {
	my $command = join(' ', 'file-remove-dup', @word_defs);
	@word_defs = split(' ', qx($command));
    }

    $word_def_info{$word} = [] unless exists $word_def_info{$word};
    foreach my $def_filename (@word_defs) {
	unless (exists $word_def_written{$def_filename}) {
	    $word_def_written{$def_filename} = [tell $dictz_file];
	    open(my $def_file, "<", $def_filename)
		or die "Can not open $def_filename";
	    my @def = <$def_file>;
	    close $def_file;
	    print $dictz_file @def;
	    push @{$word_def_written{$def_filename}}, tell($dictz_file);
	}
	push $word_def_info{$word}, @{$word_def_written{$def_filename}};
    }
}

open(my $idx_file, ">", "ahd.idx")
    or die "Can not open ahd.idx";

foreach my $word (sort keys %word_def_info) {
    print $idx_file $word;
    print $idx_file pack("CC" . "I" x @{$word_def_info{$word}}, 0, @{$word_def_info{$word}}/2, @{$word_def_info{$word}});
}


