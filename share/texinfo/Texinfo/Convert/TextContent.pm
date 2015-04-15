# TextContent.pm: return the text contents stripped of commands
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

package Texinfo::Convert::TextContent;

use 5.00405;
use strict;

use Texinfo::Convert::Converter;
use Texinfo::Convert::Text;
use Texinfo::Common;

use vars qw($VERSION @ISA);
@ISA = qw(Texinfo::Convert::Converter);

my %ignored_brace_commands;
# Handle better @errormsg?
foreach my $ignored_brace_command ('hyphenation', 'errormsg') {
  $ignored_brace_commands{$ignored_brace_command} = 1;
}
my %ignored_block_commands;
foreach my $ignored_command (
  'html', 'tex', 'xml', 'docbook', 'ignore', 'macro', 'rmacro') {
  $ignored_block_commands{$ignored_command} = 1;
}

my %ignored_types;
foreach my $type ('empty_line_after_command', 'preamble',
            'empty_spaces_after_command',
            'empty_spaces_before_paragraph',
            'empty_spaces_after_close_brace',
            'empty_space_at_end_def_bracketed') {
  $ignored_types{$type} = 1;
}

my %defaults = (
  'SHOW_MENU'            => 1,
  'OUTFILE'              => '-',
);

sub converter_defaults($$)
{
  return %defaults;
}

sub converter_initialize($)
{
  my $self = shift;
  
  %{$self->{'formatting_misc_commands'}}
    = %Texinfo::Convert::Text::formatting_misc_commands;

  if ($self->get_conf('TEXTCONTENT_COMMENT')) {
    $self->{'formatting_misc_commands'}->{'c'} = 1;
    $self->{'formatting_misc_commands'}->{'comment'} = 1;
  }
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

  if (0) {
    print STDERR "root $root";
    print STDERR " cmd: \@$root->{'cmdname'}," if ($root->{'cmdname'});
    print STDERR " type: $root->{'type'}," if ($root->{'type'});
    my $text = $root->{'text'};
    if (defined($text)) {
      $text =~ s/\n/\\n/;
      print STDERR " text: `$text'";
    }
    print STDERR "\n";
    #print STDERR "  Special def_command: $root->{'extra'}->{'def_command'}\n"
    #  if (defined($root->{'extra'}) and $root->{'extra'}->{'def_command'});
  }

  return '' if (!($root->{'type'} and $root->{'type'} eq 'def_line')
     and (($root->{'type'} and $ignored_types{$root->{'type'}})
          or ($root->{'cmdname'}
             and ($ignored_brace_commands{$root->{'cmdname'}}
                 or ($ignored_block_commands{$root->{'cmdname'}}
                     and !(defined($self->{'expanded_formats_hash'})
                           and $self->{'expanded_formats_hash'}->{$root->{'cmdname'}}))
                 or ($Texinfo::Common::inline_format_commands{$root->{'cmdname'}}
                     and (!$root->{'extra'}->{'format'}
                          or !$self->{'expanded_formats_hash'}->{$root->{'extra'}->{'format'}}))
                 or ($root->{'cmdname'} eq 'menu' and !$self->get_conf('SHOW_MENU'))
             # here ignore most of the misc commands
                 or ($root->{'args'} and $root->{'args'}->[0]
                     and $root->{'args'}->[0]->{'type'}
                     and ($root->{'args'}->[0]->{'type'} eq 'misc_line_arg'
                         or $root->{'args'}->[0]->{'type'} eq 'misc_arg')
                     and !$self->{'formatting_misc_commands'}->{$root->{'cmdname'}})))));
  if (defined($root->{'text'})) {
    return $root->{'text'};
  }
  if (defined($root->{'cmdname'})) {
    if (exists($Texinfo::Common::no_brace_commands{$root->{'cmdname'}})) {
      return $Texinfo::Common::no_brace_commands{$root->{'cmdname'}};
    } elsif ($root->{'cmdname'} eq 'today') {
      my($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst)
        = localtime(time);
      $year += ($year < 70) ? 2000 : 1900;
      return "$Texinfo::Common::MONTH_NAMES[$mon] $mday, $year";
    } elsif (defined($Texinfo::Convert::Text::text_brace_no_arg_commands{$root->{'cmdname'}})) {
      return Texinfo::Convert::Text::brace_no_arg_command($root, undef);
    } elsif ($Texinfo::Common::accent_commands{$root->{'cmdname'}}) {
      my %options = Texinfo::Common::_convert_text_options($self);
      my $result = Texinfo::Convert::Text::text_accents ($root, 
                                        $options{'enabled_encoding'});
      return $result;
    }
  }
  my $result = '';
  if ($root->{'args'} 
      and (!$root->{'cmdname'} 
           or !$Texinfo::Common::block_item_commands{$root->{'cmdname'}})) {
    #if ($root->{'type'} and ($root->{'type'} eq 'def_line'
    #                         or $root->{'type'} eq 'menu_entry')) { 
    #}
    my $args;
    if ($root->{'cmdname'} 
      and $Texinfo::Common::inline_format_commands{$root->{'cmdname'}}) {
      my @args = @{$root->{'args'}};
      shift @args;
      $args = \@args;
    } else {
      $args = $root->{'args'};
    }
    foreach my $arg (@{$args}) {
      $result .= _convert ($self, $arg);
    }
  }
  if ($root->{'contents'}) {
    foreach my $content (@{$root->{'contents'}}) {
      $result .= _convert ($self, $content);
    }
  }
  $result = '{'.$result.'}'
     if ($root->{'type'} and $root->{'type'} eq 'bracketed'
         and (!$root->{'parent'}->{'type'} or
              ($root->{'parent'}->{'type'} ne 'block_line_arg'
               and $root->{'parent'}->{'type'} ne 'misc_line_arg')));

  return $result;
}

1;
