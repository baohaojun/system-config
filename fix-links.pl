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
our %anchors;

sub fix_link($)
{
    my ($link) = @_;
    my $file;
    my $anchor;
    if ($link =~ m!^~/|^$ENV{HOME}/!) {
        $link =~ s!^~/!$ENV{HOME}/!;
        my $q_link = shell_quote($link);
        chomp(my $abs_path = qx(readlink -f $q_link));
        if ($abs_path !~ m!^$ENV{PWD}/!) {
            my $base;
            while (1) {
                my $abs_path_q = shell_quote("what to do for $abs_path? (type XXX.png! for a more meaningful name)");
                chomp(my $opt = qx(select-args -p $abs_path_q review images));
                if ($opt eq "review") {
                    system("of $q_link&");
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

            system("mkdir -p images; mv $q_link $ENV{PWD}/images/$base");
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
          (qx(if test -e blog; then find blog -name $org_file; fi) or
           qx(find . -name $org_file)));

    if ($org_file =~ m/\n/) {
        $org_file =~ s/\n/ /g;
        chomp($org_file = qx(select-args $org_file));
    }

    if (not -e $org_file and not $anchors{$link} and $link !~ m/^\s.*\s$/ and $link !~ m,^https?://,) {
        use Digest::MD5 qw(md5_hex);
        my $link_digest = md5_hex($link);
        my $md5_head = substr($link_digest, 0, 2);
        my $md5_tail = substr($link_digest, 2);
        if (not -e "$ENV{HOME}/.config/system-config/fix-links/$md5_head/$md5_tail") {
            if (system("yes-or-no-p", "-y", "'$link' does not exist or matched with too many files, continue?") != 0) {
                die "$link matched with multiple files or no files at all";
            }

            if (system("yes-or-no-p", "-y", "Save '$link' so that it won't bother you again?") == 0) {
                use File::Path qw(make_path);
                if (not -e "$ENV{HOME}/.config/system-config/fix-links/$md5_head") {
                    make_path "$ENV{HOME}/.config/system-config/fix-links/$md5_head"
                        or die "Can't make path $ENV{HOME}/.config/system-config/fix-links/$md5_head";
                }
                open(my $md5file, ">", "$ENV{HOME}/.config/system-config/fix-links/$md5_head/$md5_tail")
                    or die "Can't open $ENV{HOME}/.config/system-config/fix-links/$md5_head/$md5_tail";
                print $md5file "$link";
                close($md5file);
            }
        }
    } else {
        $file = $org_file;
    }

    if (-e $file) {
        debug "file is $file";
        if ($0 =~ /org2pdf/) {
            (my $ps_file = $file) =~ s/\.(png|jpg)$/.ps/;
            system("convert", "$file", "$ps_file");
            $file = $ps_file;
        }
        $file = shell_quote($file);
        chomp ($file = qx(relative-path $file $working_file_sq));
        if ($file !~ m!^/!) {
            $file = "./$file";
        }
        debug "file is $file";
        return $file;
    }

    return $link;
}

for my $filename (@ARGV) {
    open (my $fh, "<", $filename) or die "Can not open $filename";
    %anchors = ();
    while (<$fh>) {
        while (m/<<(.*?)>>/g) {
            $anchors{$1} = 1;
        }
    }
    close $fh;
    open (my $fh, "<", $filename) or die "Can not open $filename";
    open (my $fh_new, ">", "$filename.$$") or die "Can not open $filename.$$";

    $working_file = $filename;
    ($working_file_dir = $filename) =~ s!(.*/).*!$1!;
    $working_file_sq = shell_quote($filename);
    my $changed = 0;
    while (<$fh>) {
        my $old = $_;
        s/\[\[(.*?)\]/"[[" . fix_link($1) . "]"/eg;
        unless ($0 =~ /org2pdf/) {
            s/\[\[([^]:]*\.(png|jpg))\]\]/[[$1][file:$1]]/gi; # make images clickable
        }
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
