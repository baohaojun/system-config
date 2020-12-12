#!/usr/bin/env perl
package gerrit;

use Config::GitLike;
use strict;
use v5.10.1; # for say and switch
use autodie qw(:all);
use IPC::System::Simple qw(run runx capture capturex $EXITVAL EXIT_ANY);
use Encode;
use utf8;

use JSON;

my $json = JSON->new->utf8->canonical->pretty;

use String::ShellQuote;

use strict;
use warnings;

use Carp;
use Exporter;
BEGIN { @gerrit::ISA = 'Exporter' }

@gerrit::EXPORT = qw(
                        gr_project_exists
                );

use feature 'signatures';
no warnings "experimental::signatures";

sub gr_project_exists($project) {
    use URI::Escape;
    my $escape = uri_escape("$project");
    capture(EXIT_ANY, "gr rest a/projects/$escape 2>/dev/null");
    return $EXITVAL == 0;
}
1;
__END__

=head1 NAME

gerrit - Perl extension for blah blah blah

=head1 SYNOPSIS

   use gerrit;
   blah blah blah

=head1 DESCRIPTION

Stub documentation for gerrit,

Blah blah blah.

=head2 EXPORT

None by default.

=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

Bao Haojun, E<lt>baohaojun@gmail.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2020 by Bao Haojun

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.

=cut
