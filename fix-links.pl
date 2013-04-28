#!/usr/bin/perl

use strict;
use String::ShellQuote;

sub debug(@) {
    print STDERR "@_\n";
}

our $working_file;
sub fix_link($)
{
    my ($link) = @_;
    my $file;
    my $anchor;

    if ($link =~ m!http://baohaojun.github.(?:com|io)/(.*)!) {
        $file = $1;
    } elsif ($link =~ m/^file:/) {
        $link =~ s!^file:!!;
        $file = $link;
    } else {
        $file = $link;
    }

    $file =~ s!.*/!!;

    debug "file is $file";
    my $org_file = $file;
    if ($file =~ m/\.html$/) {
        $org_file =~ s/\.html$/.org/;
    }

    debug "org_file is $org_file";
    if ($org_file =~ m/\.org$/) {
        $org_file = shell_quote($org_file);
        chomp($org_file = qx(find blog -name $org_file));

        if ($org_file =~ m/\n/) {
            die "$link matched with multiple files";
        }
        $file = $org_file;
    }

    if (-e $file) {
        debug "file is $file";
        $file = shell_quote($file);
        chomp ($file = qx(relative-path $file $working_file));
        if ($file !~ m!/!) {
            $file = "./$file";
        }
        debug "file is $file";
        return $file;
    }

    return $link;
}

for my $filename (@ARGV) {
    open (my $fh, "<", $filename) or die "Can not open $filename";
    open (my $fh_new, ">", "$filename.$$") or die "Can not open $filename.$$";

    $working_file = shell_quote($filename);
    while (<$fh>) {
        s/\[\[(.*?)\]/"[[" . fix_link($1) . "]"/eg;
        print $fh_new $_;
    }
    close $fh;
    close $fh_new;
    rename "$filename.$$", $filename;
}
