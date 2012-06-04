#!/usr/bin/perl -w
#
# msgconvert.pl:
#
# Convert .MSG files (made by Outlook (Express)) to multipart MIME messages.
#

use Email::Outlook::Message;
use Email::LocalDelivery;
use Getopt::Long;
use Pod::Usage;
use File::Basename;
use vars qw($VERSION);
$VERSION = "0.903";

# Setup command line processing.
my $verbose = '';
my $mboxfile = '';
my $help = '';	    # Print help message and exit.
GetOptions(
  'mbox=s' => \$mboxfile,
  'verbose' => \$verbose,
  'help|?' => \$help) or pod2usage(2);
pod2usage(1) if $help;

# Check file names
defined $ARGV[0] or pod2usage(2);

foreach my $file (@ARGV) {
  my $mail = new Email::Outlook::Message($file, $verbose)->to_email_mime->as_string;
  if ($mboxfile ne '') {
    Email::LocalDelivery->deliver($mail, $mboxfile);
  } else {
    my $basename = basename($file, qr/\.msg/i);
    my $outfile = "$basename.mime";
    open OUT, ">:utf8", $outfile
      or die "Can't open $outfile for writing: $!";
    print OUT $mail;
    close OUT;
  }
}

#
# Usage info follows.
#
__END__

=head1 NAME

msgconvert.pl - Convert Outlook .msg files to mbox format

=head1 SYNOPSIS

msgconvert.pl [options] <file.msg>...

  Options:
    --mbox <file>   deliver messages to mbox file <file>
    --verbose	    be verbose
    --help	    help message

=head1 OPTIONS

=over 8

=item B<--mbox>

    Deliver to the given mbox file instead of creating individual .mime
    files.

=item B<--verbose>

    Print information about skipped parts of the .msg file.

=item B<--help>

    Print a brief help message.

=head1 DESCRIPTION

This program will convert the messages contained in the Microsoft Outlook
files <file.msg>...  to message/rfc822 files with extension .mime.
Alternatively, if the --mbox option is present, all messages will be put in
the given mbox file.  This program will complain about unrecognized OLE
parts in the input files on stderr.

=head1 BUGS

The program will not check whether output files already exist. Also, if you
feed it "foo.MSG" and "foo.msg", you'll end up with one "foo.mime",
containing one of the messages.

Not all data that's in the .MSG file is converted. There simply are some
parts whose meaning escapes me. One of these must contain the date the
message was sent, for example. Formatting of text messages will also be
lost. YMMV.

=head1 AUTHOR

Matijs van Zuijlen, C<matijs@matijs.net>

=head1 COPYRIGHT AND LICENSE

Copyright 2002, 2004, 2006, 2007 by Matijs van Zuijlen

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut
