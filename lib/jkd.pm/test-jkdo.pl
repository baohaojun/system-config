#!/usr/bin/env perl
use strict;
use v5.10.1; # for say and switch
use autodie qw(:all);
use IPC::System::Simple qw(run runx capture capturex $EXITVAL EXIT_ANY);
binmode(STDOUT, ":utf8");
binmode(STDERR, ":utf8");
use Encode;
use utf8;
@ARGV = map {decode_utf8 $_} @ARGV;

BEGIN {
    push @INC, "$ENV{scm_common_libdir}/jkd.pm";
}
use Jkdo;
