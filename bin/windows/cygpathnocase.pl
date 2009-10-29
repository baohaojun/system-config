#!/bin/perl
use File::Basename;
use strict;
our $" = ", ";

my $file = $ARGV[0];

if ($file !~ m#^/#) {
    chomp($file = `readlink -m "$file"`);
}

print "\$file is $file\n";

sub canonic_files
{
    (my $base, my @dirs) = @_;
    my $dir;
    my @existing_dirs;
    my @good_base;

    foreach my $dir (@dirs) {
        if ($dir ne "/" and $dir ne "//") {
            print "basename is ", basename($dir), ", dirname is ", dirname($dir), "\n";
            @existing_dirs = (@existing_dirs, canonic_files(basename($dir), dirname($dir)));
            print " \@existing_dirs is ", @existing_dirs, "\n";
        } else {
            @existing_dirs = (@existing_dirs, $dir);
        }
    }

    foreach my $dir (@existing_dirs) {
        opendir my $dh, $dir or next;
        my @matches;
        if ($dir =~ m#/$#) {
            @matches = map {$_ = "$dir$_"} grep {uc $base eq uc $_} readdir($dh);
        } else {
            @matches = map {$_ = "$dir/$_"} grep {uc $base eq uc $_} readdir($dh);
        }
        if (scalar @matches == 0) {
            print "\$dir is $dir";
            if ($dir =~ m#/$#) {

                @good_base = (@good_base, $dir.$base); 
            } else {
                @good_base = (@good_base, "$dir/$base");
            }
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

