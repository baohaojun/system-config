#!/usr/bin/perl -w
# (>>>FILE<<<) --- (>>>DESC<<<)
# Version: 0.01 (>>>VC_DATE<<<)
# Author: (>>>USER_NAME<<<) <(>>>AUTHOR<<<)>

use strict;
use warnings;
use CGI::Pretty;
use CGI::Carp qw(fatalsToBrowser);

my $cgi = new CGI;

print $cgi->header( -charset => "utf-8" );
print $cgi->start_html(
    -title => "(>>>TITLE<<<)",
    -encoding => 'utf-8',
);

(>>>POINT<<<)

print $cgi->end_html;
