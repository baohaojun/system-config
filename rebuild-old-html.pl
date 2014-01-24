#!/usr/bin/perl

use URI::Encode qw(uri_encode uri_decode);
use strict;

use String::ShellQuote;

sub debug(@) {
    print STDERR "@_\n";
}

sub fix_link($)
{
    my ($link) = @_;

    our $html_dir;
    our $base_html_sq;

    (my $org_file = "$html_dir/$link") =~ s/\.html$/.org/;
    if (-e "$html_dir/$link" or -e $org_file) {
        $link = shell_quote("$html_dir/$link");
        chomp($link = qx(relative-path $link $base_html_sq));
        return uri_encode($link);
    } else {
    }

    return $link;
}

unless (@ARGV) {
    @ARGV = split("\n", qx(find blog -name '*.org' -type f));
}

for our $org_file (@ARGV) {
    debug "org_file is $org_file";
    next unless -e $org_file;
    my $base_org = $org_file;
    $base_org =~ s!.*/!!;
    next if -e $base_org;

    my $html_file = $org_file;
    $html_file =~ s/\.org$/.html/;

    our $base_html = $html_file;
    $base_html =~ s!.*/!!;
    our $base_html_sq = shell_quote($base_html);

    open(my $fh_html, "<", $html_file) or die "cannot open $html_file";
    open (my $fh_base, ">", $base_html) or die "cannot open $base_html";

    our $html_dir = $html_file;
    $html_dir =~ s!(.*/).*!$1!;

    while (<$fh_html>) {
        s!\s(href\s*=\s*|src\s*=\s*)(.)(.*?)\2!" $1$2" . fix_link($3) . "$2"!eg;
        print $fh_base $_;
    }
    close $fh_base;
    close $fh_html;
}
