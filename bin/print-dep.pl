#!/usr/bin/perl

$start = $ARGV[0];
@ARGV = ();

while (<>) {
    if (m/(\S+)\s+(\S+)/) {
        $map{$1} = [] unless $map{$1};
        push @{$map{$1}}, $2
    }
}


sub print_dep($) {
    $name = $_[0];
    push @pushed, $name;
    for (@{$map{$name}}) {
        for (@pushed) {
            print "$_ -> ";
        }
        print "$_\n";
        print_dep($_);
    }
    pop @pushed;
}

print_dep ($start or "device/qcom/icesky_msm8992/icesky_msm8992.mk") ;
