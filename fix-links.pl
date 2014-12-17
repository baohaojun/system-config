#!/usr/bin/perl

# do 2 things with links:
# 1. fix links to find the target file and use relative path
# 2. make images clickable
use strict;
use String::ShellQuote;

sub debug(@) {
    print STDERR "@_\n";
}

our $working_file_sq;
our $working_file;
our $working_file_dir;
sub fix_link($)
{
    my ($link) = @_;
    my $file;
    my $anchor;
    if ($link =~ m!^~/|^$ENV{HOME}/!) {
        $link =~ s!^~/!$ENV{HOME}/!;
        chomp(my $abs_path = qx(readlink -f $link));
        if ($abs_path !~ m!^$ENV{PWD}/!) {
            my $base;
            while (1) {
                my $abs_path_q = shell_quote("what to do for $abs_path? (type XXX.png! for a more meaningful name)");
                chomp(my $opt = qx(select-args -p $abs_path_q review images));
                if ($opt eq "review") {
                    system("of $link&");
                    system(qq!sawfish-client -e '(event-name (read-event "Press any key to continue..."))' !);
                    system("find-or-exec konsole; destroy-windows eog");
                } elsif ($opt eq "images") {
                    $base = $link;
                    $base =~ s!.*/!!;
                    last;
                } else {
                    $base = $opt;
                    if (-e "$ENV{PWD}/images/$base") {
                        print "$ENV{PWD}/images/$base already exist, input again\n";
                        next;
                    }
                    last;
                }
            }

            system("mv $link $ENV{PWD}/images/$base");
            $link = "$ENV{PWD}/images/$base";
        }
    }

    if ($link =~ m!http://baohaojun.github.(?:com|io)/(.*)!) {
        $file = $1;
    } elsif ($link =~ m/^file:/) {
        $link =~ s!^file:!!;
        $file = $link;
    } elsif ($link =~ m/:/) {
        return $link;
    } else {
        $file = $link;
    }

    $file =~ s!.*/!!;

    debug "file is $file";
    my $org_file = $file;
    if ($file =~ m/\.html$/) {
        $org_file =~ s/\.html$/.org/;
    }

    $org_file = shell_quote($org_file);
    chomp($org_file =
          (qx(find blog -name $org_file) or
           qx(find . -name $org_file)));

    if ($org_file =~ m/\n/) {
        $org_file =~ s/\n/ /g;
        chomp($org_file = qx(select-args $org_file));
    }

    if (not -e $org_file) {
        if (system("yes-or-no-p", "-y", "$link does not exist or matched with too many files, continue?") != 0) {
            die "$link matched with multiple files or no files at all";
        }
    } else {
        $file = $org_file;
    }

    if (-e $file) {
        debug "file is $file";
        $file = shell_quote($file);
        chomp ($file = qx(relative-path $file $working_file_sq));
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

    $working_file = $filename;
    ($working_file_dir = $filename) =~ s!(.*/).*!$1!;
    $working_file_sq = shell_quote($filename);
    my $changed = 0;
    while (<$fh>) {
        my $old = $_;
        s/\[\[(.*?)\]/"[[" . fix_link($1) . "]"/eg;
        s/\[\[([^]:]*\.(png|jpg))\]\]/[[$1][file:$1]]/gi; # make images clickable
        print $fh_new $_;
        if ($_ ne $old) {
            $changed = 1;
        }
    }
    close $fh;
    close $fh_new;


    if ($changed) {
        rename "$filename.$$", $filename;
    } else {
        debug "remove $filename.$$";
        unlink "$filename.$$";
    }
}
