# Texinfo.pm: output a Texinfo tree as Texinfo.
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

package Texinfo::Convert::Texinfo;

use 5.00405;
use strict;

# commands definitions
use Texinfo::Common;

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration   use Texinfo::Convert::Texinfo ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
  convert
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '5.1.90';

my %misc_commands            = %Texinfo::Common::misc_commands;
my %brace_commands           = %Texinfo::Common::brace_commands;    
my %block_commands           = %Texinfo::Common::block_commands;    
my %def_commands             = %Texinfo::Common::def_commands;    

sub convert ($;$);
# Following subroutines deal with transforming a texinfo tree into texinfo
# text.  Should give the text that was used parsed, except for a few cases.
# Second argument is undocumented for now, as it may change, for instance
# become a hash if more has to be given.

# expand a tree to the corresponding texinfo.
sub convert ($;$)
{
  my $root = shift;
  my $fix = shift;
  die "convert: root undef\n" if (!defined($root));
  die "convert: bad root type (".ref($root).") $root\n" 
     if (ref($root) ne 'HASH');
  my $result = '';
  #print STDERR "$root ";
  #print STDERR "$root->{'type'}" if (defined($root->{'type'}));
  #print STDERR "\n";
  if (defined($root->{'text'})) {
    $result .= $root->{'text'};
  } else {
    if ($fix and $root->{'extra'} and $root->{'extra'}->{'invalid_nesting'}) {
      return '';
    }
    if ($root->{'cmdname'} 
       or ($root->{'type'} and ($root->{'type'} eq 'def_line'
                                or $root->{'type'} eq 'menu_entry'
                                or $root->{'type'} eq 'menu_comment'))) {
      #print STDERR "cmd: $root->{'cmdname'}\n";
      $result .= _expand_cmd_args_to_texi($root, $fix);
    }
    $result .= '{' if ($root->{'type'} and $root->{'type'} eq 'bracketed');
    #print STDERR "$root->{'contents'} @{$root->{'contents'}}\n" if (defined($root->{'contents'}));
    if (defined($root->{'contents'})) {
      die "bad contents type(" . ref($root->{'contents'})
          . ") $root->{'contents'}\n" if (ref($root->{'contents'}) ne 'ARRAY');
      foreach my $child (@{$root->{'contents'}}) {
        $result .= convert($child, $fix);
      }
    }
    $result .= '}' if ($root->{'type'} and $root->{'type'} eq 'bracketed');
    if ($root->{'cmdname'} and (defined($block_commands{$root->{'cmdname'}}))
        and ($block_commands{$root->{'cmdname'}} eq 'raw' 
          or ($fix and !($root->{'extra'} and $root->{'extra'}->{'end_command'})))) {
      $result .= '@end '.$root->{'cmdname'};
      $result .= "\n" if ($block_commands{$root->{'cmdname'}} ne 'raw');
    } 
  }
  #print STDERR "convert result: $result\n";
  return $result;
}


# expand a command argument as texinfo.
sub _expand_cmd_args_to_texi ($;$) {
  my $cmd = shift;
  my $fix = shift;
  my $cmdname = $cmd->{'cmdname'};
  $cmdname = '' if (!$cmd->{'cmdname'}); 
  my $result = '';
  $result = '@'.$cmdname if ($cmdname);
  #print STDERR "Expand $result\n";

  # this is done here otherwise for some constructs, there are
  # no 'args', and so the space is never readded.
  if ($cmd->{'extra'} and exists ($cmd->{'extra'}->{'spaces'})) {
    $result .= $cmd->{'extra'}->{'spaces'};
  }
  # must be before the next condition
  if ($block_commands{$cmdname}
         and ($def_commands{$cmdname}
              or $block_commands{$cmdname} eq 'multitable')
         and $cmd->{'args'}) {
     foreach my $arg (@{$cmd->{'args'}}) {
        $result .= convert($arg, $fix);
    }
  # for misc_commands with type special
  } elsif (($cmd->{'extra'} or $cmdname eq 'macro' or $cmdname eq 'rmacro') 
           and defined($cmd->{'extra'}->{'arg_line'})) {
    $result .= $cmd->{'extra'}->{'arg_line'};
  } elsif (($block_commands{$cmdname} or $cmdname eq 'node')
            and defined($cmd->{'args'})) {
    die "bad args type (".ref($cmd->{'args'}).") $cmd->{'args'}\n"
      if (ref($cmd->{'args'}) ne 'ARRAY');
    foreach my $arg (@{$cmd->{'args'}}) {
       $result .= convert($arg, $fix) . ',';
    }
    $result =~ s/,$//;
  } elsif ($fix and $misc_commands{$cmdname}
      and $misc_commands{$cmdname} eq 'skipline') {
    $result .="\n";
  } elsif (defined($cmd->{'args'})) {
    my $braces;
    $braces = 1 if ($cmd->{'args'}->[0]->{'type'} 
                    and ($cmd->{'args'}->[0]->{'type'} eq 'brace_command_arg'
                         or $cmd->{'args'}->[0]->{'type'} eq 'brace_command_context'));
    $result .= '{' if ($braces);
    if ($cmdname eq 'verb') {
      $result .= $cmd->{'type'};
    }
    #print STDERR "".Data::Dumper->Dump([$cmd]);
    my $arg_nr = 0;
    foreach my $arg (@{$cmd->{'args'}}) {
      if (exists($brace_commands{$cmdname}) or ($cmd->{'type'} 
                    and $cmd->{'type'} eq 'definfoenclose_command')) {
        $result .= ',' if ($arg_nr);
        $arg_nr++;
      }
      $result .= convert($arg, $fix);
    }
    if ($cmdname eq 'verb') {
      $result .= $cmd->{'type'};
    }
    #die "Shouldn't have args: $cmdname\n";
    $result .= '}' if ($braces);
  }
  $result .= '{'.$cmd->{'type'}.'}' if ($cmdname eq 'value');
  #print STDERR "Result: $result\n";
  return $result;
}

1;
__END__

=head1 NAME

Texinfo::Convert::Texinfo - Convert a Texinfo tree to Texinfo code

=head1 SYNOPSIS

  use Texinfo::Convert::Texinfo qw(convert);
  
  my $texinfo_text = convert($tree);

=head1 DESCRIPTION

Texinfo::Convert::Texinfo converts a Texinfo tree (described in 
L<Texinfo::Parser>) to Texinfo code.  If the Texinfo tree results from 
parsing some Texinfo document, The converted Texinfo code should be
exactly the same as the initial document, except that user defined @-macros 
and C<@value> are expanded, and some invalid code is discarded.

=head1 METHODS

=over

=item $texinfo_text = convert($tree)

Converts the Texinfo tree I<$tree> to Texinfo code.

=back

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2010, 2011, 2012 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at 
your option) any later version.

=cut
