# DebugTexinfo::DebugTree.pm: debug a Texinfo::Parser tree.
#
# Copyright 2011, 2012 Free Software Foundation, Inc.
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

# Example of calls
# with creation of elements corresponding to sections:
# ./texi2any.pl --set TEXINFO_OUTPUT_FORMAT=debugtree --set USE_NODES=0 file.texi
# with creation of elements corresponding to nodes:
# ./texi2any.pl --set TEXINFO_OUTPUT_FORMAT=debugtree --set USE_NODES=1 file.texi
# no elements
# ./texi2any.pl --set TEXINFO_OUTPUT_FORMAT=debugtree file.texi
# 
# Some unofficial info about the --debug command line option ... with
# --debug=1, the tree is not printed,
# --debug=10 (or more), the tree is printed at the end of the run,
# --debug=100 (or more), the tree is printed at each newline.


use Texinfo::Convert::Converter;

package DebugTexinfo::DebugTree;

@ISA = qw(Texinfo::Convert::Converter);

my %defaults = (
  'EXTENSION' => 'debugtree',
  'OUTFILE' => '-',
);

sub converter_defaults($$)
{
  return %defaults;
}

sub output($$)
{
  my $self = shift;
  my $root = shift;


  $self->_set_outfile();
  return undef unless $self->_create_destination_directory();

  my $fh;
  if (! $self->{'output_file'} eq '') {
    $fh = $self->Texinfo::Common::open_out ($self->{'output_file'});
    if (!$fh) {
      $self->document_error(sprintf($self->__("could not open %s for writing: %s"),
                                    $self->{'output_file'}, $!));
      return undef;
    }
  }

  my $elements;
  if ($self) {
    if ($self->get_conf('USE_NODES')) {
      $elements = Texinfo::Structuring::split_by_node($root);
    } elsif (defined($self->get_conf('USE_NODES'))) {
      #print STDERR "U sections\n";
      $elements = Texinfo::Structuring::split_by_section($root);
    }
    # Currently the information added is not used further.
    if ($elements and ($self->get_conf('SPLIT') 
                       or !$self->get_conf('MONOLITHIC'))) {
      #print STDERR "S ".$self->get_conf('SPLIT')."\n";
      Texinfo::Structuring::split_pages($elements,
                                        $self->get_conf('SPLIT'));
    }
  }
  if ($elements) {
    $root = {'type' => 'elements_root',
             'contents' => $elements };
  }
  return $self->_output_text (_print_tree($self, $root), $fh);
}

sub convert($$)
{
  my $self = shift;
  my $root = shift;

  return _print_tree($self, $root);
}

sub convert_tree($$)
{
  my $self = shift;
  my $root = shift;

  return _print_tree($self, $root);
}

sub _print_tree($$;$$);

sub _print_tree($$;$$)
{
  my $self = shift;
  my $root = shift;
  my $level = shift;
  my $argument = shift;
  $level = 0 if (!defined($level));

  my $result = ' ' x $level;
  if ($argument) {
    $result .= '%';
    $level++;
  }
  if ($root->{'cmdname'}) {
    $result .= "\@$root->{'cmdname'} ";
  }
  if (defined($root->{'type'})) {
    $result .= "$root->{'type'} ";
  }
  if (defined($root->{'text'})) {
    my $text = $root->{'text'};
    $text =~ s/\n/\\n/g;
    $text =~ s/\f/\\f/g;
    $text =~ s/\r/\\r/g;
    $result .= "|$text|";
  }
  $result .= "\n";
  if ($root->{'args'}) {
    foreach my $arg (@{$root->{'args'}}) {
      $result .= _print_tree ($self, $arg, $level +1, 1);
    }
  }
  if ($root->{'contents'}) {
    foreach my $content (@{$root->{'contents'}}) {
      $result .= _print_tree ($self, $content, $level+1);
    }
  }
  return $result;
}
