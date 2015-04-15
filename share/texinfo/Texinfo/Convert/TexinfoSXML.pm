# TexinfoSXML.pm: output tree as Texinfo SXML.
#
# Copyright 2013 Free Software Foundation, Inc.
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
#
# This is a simple subclass of Texinfo::Convert::TexinfoXML that overrides
# format specific functions.

package Texinfo::Convert::TexinfoSXML;

use 5.00405;
use strict;

use Texinfo::Convert::TexinfoXML;
use Carp qw(cluck);

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter Texinfo::Convert::TexinfoXML);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Texinfo::Convert::TexinfoXML ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
  convert
  convert_tree
  output
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '5.1.90';

# SXML specific
my %defaults = (
  'ENABLE_ENCODING'      => 0,
  'SHOW_MENU'            => 1,
  'EXTENSION'            => 'sxml',
  #'output_perl_encoding' => 'utf8',
  'OUTPUT_ENCODING_NAME' => 'utf-8',
  'TEXINFO_DTD_VERSION'  => '5.0',
  'OUTFILE'              => undef,
  'SUBDIR'               => undef,
  'output_format'        => 'texinfosxml',
  'SPLIT'                => 0,
  'documentlanguage'     => 'en',
);

sub converter_defaults($$)
{
  return %defaults;
}

# format specific.  Used in few places where plain text is used outside
# of attributes.
sub protect_text($$)
{
  my $self = shift;
  my $string = shift;
  $string =~ s/\\/\\\\/g;
  $string =~ s/"/\\"/g;
  return $string;
}

sub sxml_attributes($$)
{
  my $self = shift;
  my $attributes = shift;
  if (ref($attributes) ne 'ARRAY') {
    cluck "attributes not an array($attributes).";
  }
  my $result = '(@';
  for (my $i = 0; $i < scalar(@$attributes); $i += 2) {
    $result .= " ($attributes->[$i] \"".$self->protect_text($attributes->[$i+1])."\")";
  }
  return $result . ')';
}

# format specific
sub element($$$)
{
  my $self = shift;
  my $element_name = shift;
  my $attributes = shift;
  my $result = '('.$element_name." ";
  $attributes = [] if (!defined($attributes));
  $result .= $self->sxml_attributes($attributes);
  $result .= ')';
  return $result;
}

# format specific
sub open_element($$$)
{
  my $self = shift;
  my $element_name = shift;
  my $attributes = shift;
  my $result = '('.$element_name." ";
  $attributes = [] if (!defined($attributes));
  $result .= $self->sxml_attributes($attributes);
  $result .= " ";
  return $result;
}

# format specific
sub close_element($$)
{
  my $self = shift;
  my $element_name = shift;
  my $result= ')';
  return $result;
}

my %commands_formatting = %Texinfo::Convert::TexinfoXML::commands_formatting;

# format specific
sub format_atom($$)
{
  my $self = shift;
  my $atom = shift;
  if ($commands_formatting{$atom} ne '') {
    return '('.$commands_formatting{$atom}.' (@))';
  } else {
    return '';
  }
}

# format specific
#FIXME
sub format_comment($$)
{
  my $self = shift;
  my $string = shift;

  return '';
}

# format specific
sub format_text($$)
{
  my $self = shift;
  my $root = shift;
  my $result = $self->protect_text($root->{'text'});
  if (! defined($root->{'type'}) or $root->{'type'} ne 'raw') {
    if (!$self->{'document_context'}->[-1]->{'monospace'}->[-1]) {
      $result =~ s/``/" (textldquo (@)) "/g;
      $result =~ s/\'\'/" (textrdquo (@)) "/g;
      $result =~ s/---/" (textmdash (@)) "/g;
      $result =~ s/--/" (textndash (@)) "/g;
      $result =~ s/'/" (textrsquo (@)) "/g;
      $result =~ s/`/" (textlsquo (@)) "/g;
    }
  }
  return '"'.$result.'" ';
}

# output format specific
sub format_header($)
{
  my $self = shift;
  my $header = '';
  my $encoding = '';
  if ($self->get_conf('OUTPUT_ENCODING_NAME')
      and $self->get_conf('OUTPUT_ENCODING_NAME') ne 'utf-8') {
    $encoding = $self->get_conf('OUTPUT_ENCODING_NAME');
  }
  if ($self->{'output_file'} ne '') {
    my $output_filename = $self->{'output_filename'};
  }
  return $header;
}
