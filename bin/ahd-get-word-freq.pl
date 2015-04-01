#!/usr/bin/perl

use strict;
use Encode;
use String::ShellQuote;

open(my $ii_file, "<", "ahd.ii") or die "Can not open ii";
open(my $idx_file, "<", "ahd.idx") or die "Can not open idx";
open(my $dz_file, "<", "ahd.dz") or die "Can not open dz";

sub debug(@) {
    print STDERR "@_\n";
}

my $ascii_re = "^[\000-\177]+\$";
$ascii_re = qr($ascii_re);

sub get_normal_words($)
{
    my $word = $_[0];
    unless ($word =~ m/$ascii_re/) {
        $word = shell_quote($word);
        chomp($word = qx(AsciiDammit.py $word));
    }
    return $word;
}

my $ii;
my $last_entry_end = 0;
while (read($ii_file, $ii, 4) == 4) {
    my ($off_n_entries) = unpack("N", $ii);

    my $this_entry_name_end = $off_n_entries - 1;

    my $word;
    seek($idx_file, $last_entry_end, 0) or die "can not seek $idx_file";
    if (read($idx_file, $word, $this_entry_name_end - $last_entry_end) != $this_entry_name_end - $last_entry_end) {
        die "read idx file not ok";
    }
    debug "word is $word";

    my $n_entries;
    seek($idx_file, $off_n_entries, 0) or die "can not seek $idx_file";
    if (read($idx_file, $n_entries, 1) != 1) {
        die "read idx file not ok";
    }

    for my $i (1..ord($n_entries)) {
        my ($start, $end);
        if (read($idx_file, $start, 4) != 4 or read($idx_file, $end, 4) != 4) {
            die "read idx file not ok";
        }

        ($start) = unpack("N", "$start");
        ($end) = unpack("N", "$end");
        our %words;
        $words{$word} = 1;

        our %lc_words;
        $lc_words{lc $word} = 1;

        our %words_def;
        $words_def{$word} = "" unless exists $words_def{$word};

        my $def;
        seek($dz_file, $start, 0) or die "can not seek dz file";

        if ($end-$start < 0) {
            die "read dz file not ok: def: $def, start: $start, end: $end";
        }
        if (read($dz_file, $def, $end-$start) != $end - $start) {
            die "read dz file not ok: def: $def, start: $start, end: $end";
        }

        $def =~ s/<.*?>//g;
        $def =~ s,&#(.*?);,encode_utf8(chr($1)),eg;
        $def = lc $def;
        $words_def{$word} = $words_def{$word} . " $def";
    }
    $last_entry_end = tell($idx_file);
}

our %words;
our %lc_words;
our %words_def;
our %words_freq;
our %words_defined_by_me;
for my $word (sort keys %words) {
    print "doing $word\n";
    my $def = $words_def{$word};
    $def =~ s/<.*?>//;

    for (split(/(?:\s|\[|\]|\(|\)|;|:|,|“|”)+/, $def)) {
        next unless $_;
        s/·//g;
        if (not exists $lc_words{$_}) {
            our %fixed_word;
            if ($fixed_word{$_}) {
                $_ = $fixed_word{$_};
            } else {
                my $old_ = $_;
                if (substr($_, -2) eq "ed" or substr($_, -2) eq "es") {
                    $_ = substr($_, 0, -1);
                    if (exists $lc_words{$_}) {
                        $fixed_word{$old_} = $_;
                    } else {
                        $_ = substr($_, 0, -1);
                        if (exists $lc_words{$_}) {
                            $fixed_word{$old_} = $_;
                        } else {
                            $fixed_word{$old_} = $old_;
                        }
                    }
                } elsif (substr($_, -1) eq "s" or substr($_, -1) eq ".") {
                    $_ = substr($_, 0, -1);
                    if (exists $lc_words{$_}) {
                        $fixed_word{$old_} = $_;
                    } else {
                        $fixed_word{$old_} = $old_;
                    }
                } elsif (substr($_, -1) eq "ing") {
                    $_ = substr($_, 0, -3);
                    if (exists $lc_words{$_}) {
                        $fixed_word{$old_} = $_;
                    } else {
                        $_ .= "e";
                        if (exists $lc_words{$_}) {
                            $fixed_word{$old_} = $_;
                        } else {
                            $fixed_word{$old_} = $old_;
                        }
                    }
                } else {
                    $fixed_word{$old_} = $old_;
                }
            }
        }
        #next if m/ə|ā|ē|ō|â|ä|ô|û|ŏ|ĭ|ă|ě|à|á|æ|å/;

        $words_freq{$_}++;

        $words_defined_by_me{$_} = {} unless exists $words_defined_by_me{$_};
        $words_defined_by_me{$_}{$word} = 1;
    }
}

open(my $file_freq, ">", "ahd.freq") or die "can not open word freq file for writing";
for (sort { $words_freq{$a} <=> $words_freq{$b} } keys %words_freq) {
    printf $file_freq "%s %d\n", $_, $words_freq{$_};
}
close($file_freq);

open (my $dz_usage, ">", "usage.dz") or die "can not open word defines file for writing";
open (my $idx_usage, ">", "usage.idx") or die "can not open word usage idx for writing";
open (my $ii_usage, ">", "usage.ii") or die "can not open word usage idx for writing";

our %normal_map;
for my $key (sort keys %words_defined_by_me) {
    debug "computing normal for $key\n" if (keys %{$words_defined_by_me{$key}} > 5000);

    if (keys %{$words_defined_by_me{$key}} > 3000) {
        debug "should skip define key $key as it is too common\n";
        next;
    }

    if (keys %{$words_defined_by_me{$key}} <= 5) {
        debug "should skip key $key as it as it is useless\n";
        next;
    }

    $normal_map{$key} = lc get_normal_words($key) unless exists $normal_map{$key};

    for (keys %{$words_defined_by_me{$key}}) {
        $normal_map{$_} = lc get_normal_words($_) unless exists $normal_map{$_};
    }
}

for (sort { $normal_map{$a} cmp $normal_map{$b} } keys %words_defined_by_me) {
    my $key = $_;

    if (keys %{$words_defined_by_me{$key}} > 3000) {
        debug "should skip define key $key as it is too common\n";
        next;
    }

    if (keys %{$words_defined_by_me{$key}} <= 5) {
        debug "should skip $key as it as it is useless\n";
        next;
    }

    print $idx_usage $key;
    print $idx_usage pack("CC", 0, 1);
    print $ii_usage pack("N", tell($idx_usage) - 1);

    print $idx_usage pack("N", tell($dz_usage));
    print $dz_usage join(':', ($key, sort { $normal_map{$a} cmp $normal_map{$b} } (keys %{$words_defined_by_me{$key}})));
    print $idx_usage pack("N", tell($dz_usage));
    print $dz_usage "\n";
}
close($dz_usage);
close($idx_usage);
close($ii_usage);
