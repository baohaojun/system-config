#!/usr/bin/env perl
use strict;

my $num_re = qr(-?\d+(?:\.\d+)?|0x[a-fA-F0-9]+);
    
my $out_phone;
my $out_item;
my $part;

sub debug(@) {
    print STDERR "@_\n";
}

my $seq;
while (<>) {
    chomp;
    s/\r//g;
    s/\s+/ /g;
    my $src = $ARGV;
    $src =~ s!/gsm-result.txt$!!;
    
    if (m/start (?:of )?(.*) calibration/i) {
	debug "matched cali start";
	$part = $1;
	if ($1 !~ m/^(afc|apc|agc)$/i) {
	    die "Did not match this line: $_";
	}
	$seq = 0;
	if (lc $part eq "afc") {
	    debug "out set to afc";
	    close $out_phone if $out_phone;
	    open($out_phone, ">>", "$src-afc.csv");
	} elsif (lc $part eq "apc") {
	    debug "out set to apc";
	    close $out_phone if $out_phone;
	    open($out_phone, ">>", "$src-apc.csv");
	} else {
	    debug "out set to agc";
	    close $out_phone if $out_phone;
	    open($out_phone, ">>", "$src-agc.csv");
	}
    } elsif (m/Measured frequency offset ($num_re) for DAC word ($num_re)/) { 
        # Measured frequency offset 14 for DAC word -326
	die "Don't know how to handle '$_' in $part context" unless lc $part eq "afc";
	debug "matched afc log";
	print $out_phone "File, $src, Measured frequency offset, $1, DAC word, $2\n";
	open($out_item, ">>", "AFC-SEQ-$seq.csv");
	$seq++;
	print $out_item "File, $src, Measured frequency offset, $1, DAC word, $2\n";
	close $out_item;
    } elsif (m/DAC word ($num_re) matches frequency offset ($num_re), slope = ($num_re)/) {
	print $out_phone "File, $src, DAC word $1, frequency offset, $2, slope, $3\n";
	open($out_item, ">>", "AFC-SEQ-$seq.csv");
	$seq++;
	print $out_item "File, $src, DAC word $1, frequency offset, $2, slope, $3\n";
	close $out_item;
    } elsif (m/Band (\w+), Modulation (\w+), Channel ($num_re), Ramp Scale ($num_re), Measured Power ($num_re) \Q[dBm]\E, Min ($num_re)\Q[dBm]\E, Max ($num_re)\Q[dBm]\E/) {
	die "Don't know how to handle '$_' in $part context" unless lc $part eq "apc";
	debug "matched apc log";
	print $out_phone "File, $src, Band, $1, Modulation, $2, Channel, $3, Ramp Scale, $4, Measured Power, $5, Min, $6, Max, $7\n";
	open($out_item, ">>", "APC-Band~$1!Modulation~$2!Channel~$3!RampScale~$4.csv");
	print $out_item "File, $src, Band, $1, Modulation, $2, Channel, $3, Ramp Scale, $4, Measured Power, $5, Min, $6, Max, $7\n";
	close $out_item;
    } elsif (m!Band (\w+), Channel ($num_re), Gain Code ($num_re), Measured Gain ($num_re)\Q [1/16dB]\E, Min ($num_re)\Q[1/16dB]\E, Max ($num_re)\Q[1/16dB]\E!) {
        # Band EGSM, Channel 975, Gain Code 0x4a, Measured Gain  2175.00 [1/16dB], Min 1972.00[1/16dB], Max 2292.00[1/16dB],
	die "Don't know how to handle '$_' in $part context" unless lc $part eq "agc";
	debug "matched agc log";
	print $out_phone "File, $src, Band, $1, Channel, $2, Gain Code, $3, Measured Gain, $4, Min, $5, Max, $6\n";
	open($out_item, ">>", "AGC-Band~$1!!Channel~$2!GainCode~$3.csv");
	print $out_item "File, $src, Band, $1, Channel, $2, Gain Code, $3, Measured Gain, $4, Min, $5, Max, $6\n";
	close $out_item;
    }
}

