#!/bin/perl
use File::Basename;
use strict;
our $" = ", ";

my $file = $ARGV[0];
$file =~ s!/+$!!;

chomp(my $cwd = `pwd`);

if ($file =~ m#^//(?!/)#) {
    print $file;
    exit (0);
}

if ($cwd =~ m#^//(?!/)# and $file !~ m#^/#) {
    $file = "$cwd/$file";
    $file =~ s#/+#/#g;
    print "/$file";
    exit (0);
}

if ($file !~ m#^/#) {#file path is not absolute
    $file = "$cwd/$file";
}

$file =~ s#/+#/#g; #remove extra PATHSEP

sub canonic_files
{
    (my $base, my @dirs) = @_;
    my $dir;
    my @existing_dirs;
    my @good_base;

    foreach my $dir (@dirs) {
        if (not -d $dir) {
            @existing_dirs = (@existing_dirs, canonic_files(basename($dir), dirname($dir)));

        } else {
            @existing_dirs = (@existing_dirs, $dir);
        }
    }

    foreach my $dir (@existing_dirs) {
        opendir my $dh, $dir or next;
        my @matches = map {$_ = "$dir/$_"} grep {uc $base eq uc $_} readdir($dh);
        if (scalar @matches == 0) {
            @good_base = (@good_base, "$dir/$base");
        } else {
            @good_base = (@good_base, @matches);
        }
        closedir $dh;
        return @good_base;                      
    }
}

my @matches = canonic_files(basename($file), dirname($file));
if (scalar @matches == 0) {
    print "$file\n";
} else {
    foreach my $match (@matches) {
        print "$match\n";
    }
}

