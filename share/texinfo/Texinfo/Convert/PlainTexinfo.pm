# PlainTexinfo.pm: convert the Texinfo tree as Texinfo
#
# Copyright 2012 Free Software Foundation, Inc.
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

# This calls Texinfo::Convert::Texinfo::convert while inheriting standard
# functions from Texinfo::Convert::Converter.

package Texinfo::Convert::PlainTexinfo;

use 5.00405;
use strict;

use Texinfo::Convert::Converter;

use vars qw($VERSION @ISA);
@ISA = qw(Texinfo::Convert::Converter);

my %defaults = (
  'OUTFILE'              => '-',
  'EXTENSION'            => '.txi',
);

sub converter_defaults($$)
{
  return %defaults;
}

sub convert_tree($$)
{
  my $self = shift;
  my $root = shift;

  return $self->_convert($root);
}

sub convert($$)
{
  my $self = shift;
  my $root = shift;

  return $self->_convert($root);
}

sub _convert($$);

sub _convert($$)
{
  my $self = shift;
  my $root = shift;
  
  return Texinfo::Convert::Texinfo::convert($root, 
                                            $self->get_conf('FIX_TEXINFO'));
}

1;
