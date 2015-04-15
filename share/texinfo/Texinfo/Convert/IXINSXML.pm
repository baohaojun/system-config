# IXIN.pm: output tree as IXIN with SXML converter.
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
# This module implements abstract functions that output the IXIN format
# using lower level formatting funtions, here adapted to lisp like 
# output.  For other output, the output specific functions should be
# redefined.  This module is not enough to output IXIN format, a module
# inheriting both from a converter module and this module is required.

package Texinfo::Convert::IXINSXML;

use 5.00405;
use strict;

use Texinfo::Convert::TexinfoSXML;
use Texinfo::Convert::IXIN;

use Carp qw(cluck);

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter Texinfo::Convert::TexinfoSXML Texinfo::Convert::IXIN);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Texinfo::Convert::IXIN ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
  output
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '5.1.90';

my %defaults = (
  'ENABLE_ENCODING'      => 0,
  'SHOW_MENU'            => 1,
  'EXTENSION'            => 'ixin',
  #'output_perl_encoding' => 'utf8',
  'OUTPUT_ENCODING_NAME' => 'utf-8',
  # useful?
  'TEXINFO_DTD_VERSION'  => '5.0',
  'OUTFILE'              => undef,
  'SUBDIR'               => undef,
  'output_format'        => 'ixinsxml',
  'SPLIT'                => 0,
  'documentlanguage'     => 'en',
  'USE_NODES'            => 1,
  'GLOBAL_COMMANDS'      => ['image'],
);

sub converter_defaults($$)
{
  return %defaults;
}

sub output($)
{
  my $self = shift;
  my $root = shift;

  return $self->output_ixin($root);
}
