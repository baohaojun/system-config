# Encoding.pm: Encodings definitions and aliases.
#
# Copyright 2010, 2011, 2012 Free Software Foundation, Inc.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License,
# or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
# Original author: Patrice Dumas <pertusus@free.fr>
# Parts (also from Patrice Dumas) come from texi2html.pl or texi2html.init.

package Texinfo::Encoding;

use strict;

use Encode;

require Exporter;
use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Texinfo::Covert::Text ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
  encoding_alias
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

# charset related definitions.

our %perl_charset_to_html = (
              'utf8'       => 'utf-8',
              'utf-8-strict'       => 'utf-8',
              'ascii'      => 'us-ascii',
              'shiftjis'      => 'shift_jis',
);

# encoding name normalization to html-compatible encoding names
our %encoding_aliases = (
              'latin1' => 'iso-8859-1',
);

foreach my $perl_charset (keys(%perl_charset_to_html)) {
   $encoding_aliases{$perl_charset} = $perl_charset_to_html{$perl_charset};
   $encoding_aliases{$perl_charset_to_html{$perl_charset}}
        = $perl_charset_to_html{$perl_charset};
}
our %eight_bit_encoding_aliases = (
  "iso-8859-1",  'iso8859_1',
  "iso-8859-2",  'iso8859_2',
  "iso-8859-15", 'iso8859_15',
  "koi8-r",      'koi8',
  "koi8-u",      'koi8',
);

foreach my $encoding (keys(%eight_bit_encoding_aliases)) {
  $encoding_aliases{$encoding} = $encoding;
  $encoding_aliases{$eight_bit_encoding_aliases{$encoding}} = $encoding;
}

our %canonical_texinfo_encodings;
# These are the encodings from the texinfo manual
foreach my $canonical_encoding('us-ascii', 'utf-8', 'iso-8859-1',
  'iso-8859-15','iso-8859-2','koi8-r', 'koi8-u') {
  $canonical_texinfo_encodings{$canonical_encoding} = 1;
}

sub encoding_alias ($)
{
  my $encoding = shift;
  my $canonical_texinfo_encoding;
  $canonical_texinfo_encoding
    = $encoding if ($canonical_texinfo_encodings{lc($encoding)});
  my $perl_encoding = Encode::resolve_alias($encoding);
  my $canonical_output_encoding;
  if ($perl_encoding) {
    $canonical_output_encoding = $encoding_aliases{$perl_encoding};
  }
  foreach my $possible_encoding ($encoding, $canonical_output_encoding, 
                                            $perl_encoding) {
    if (defined($possible_encoding)
        and $canonical_texinfo_encodings{lc($possible_encoding)}) {
      $canonical_texinfo_encoding = $possible_encoding;
    }
  }
  return ($canonical_texinfo_encoding, $perl_encoding, $canonical_output_encoding);
}

1;

__END__

=head1 NAME

Texinfo::Encoding - Encodings and encoding aliases

=head1 SYNOPSIS

  use Texinfo::Encoding qw(encoding_alias);

  my ($canonical_texinfo_encoding, $perl_encoding, 
      $canonical_output_encoding) = encoding_alias($encoding);

=head1 DESCRIPTION

Texinfo::Encoding takes care of encoding definition and aliasing.

=head1 METHODS

=over

=item ($canonical_texinfo_encoding, $perl_encoding, $canonical_output_encoding) = encoding_alias($encoding)

Taking an encoding name as argument, the function returns the 
corresponding canonical Texinfo encoding I<$canonical_texinfo_encoding> 
as described in the Texinfo manual (or undef), an encoding name suitable 
for perl I<$perl_encoding>, and an encoding name suitable for most 
output formaats, especially HTML, I<$canonical_output_encoding>. 

=back

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2010, 2011, 2012 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License,
or (at your option) any later version.

=cut
