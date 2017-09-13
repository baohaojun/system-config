#!/usr/bin/perl

@ARGV = grep {-e $_} @ARGV;

while (<>) {
    if (m/rb_define_(.*)\(.*?"(.*?)"/) {
        my ($type, $tag) = ($1, $2);
        print "$tag $type $. $ARGV $_";
        if ($tag =~ m/\?$/) {
            $tag =~ s/\?$//;
            print "$tag $type $. $ARGV $_";
        }
    }
}
