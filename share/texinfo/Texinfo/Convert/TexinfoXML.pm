# TexinfoXML.pm: output tree as Texinfo XML.
#
# Copyright 2011, 2012, 2013 Free Software Foundation, Inc.
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

package Texinfo::Convert::TexinfoXML;

use 5.00405;
use strict;

use Texinfo::Convert::Converter;
use Texinfo::Common;
use Texinfo::Convert::Unicode;
# for debugging and adding the original line for some commands
use Texinfo::Convert::Texinfo;
use Data::Dumper;
use Carp qw(cluck);

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter Texinfo::Convert::Converter);

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

# XML specific
my %defaults = (
  'ENABLE_ENCODING'      => 0,
  'SHOW_MENU'            => 1,
  'EXTENSION'            => 'xml',
  #'output_perl_encoding' => 'utf8',
  'OUTPUT_ENCODING_NAME' => 'utf-8',
  'TEXINFO_DTD_VERSION'  => '5.0',
  'OUTFILE'              => undef,
  'SUBDIR'               => undef,
  'output_format'        => 'xml',
  'SPLIT'                => 0,
  'documentlanguage'     => 'en',
);


# our because it is used in the xml to texi translator
our %commands_formatting = (
           '*' => 'linebreak',
           ' ' => ['spacecmd', 'type', 'spc'],
           "\t" => ['spacecmd', 'type', 'tab'],
           "\n" => ['spacecmd', 'type', 'nl'],
           '-' => 'hyphenbreak',  # hyphenation hint
           '|' => '',  # used in formatting commands @evenfooting and friends
           '/' => 'slashbreak',
           ':' => 'noeos',
           '!' => 'eosexcl',
           '?' => 'eosquest',
           '.' => 'eosperiod',
           '@' => 'arobase',
           '{' => 'lbrace',
           '}' => 'rbrace',
           '\\' => 'backslash',  # should only appear in math

           'TeX' => 'tex',
           'LaTeX' => 'latex',
           'bullet' => 'bullet',
           'copyright'    => 'copyright',
           'registeredsymbol'   => 'registered',
           'dots'    => 'dots',
           'enddots'    => 'enddots',
           'error'        => 'errorglyph',
           'expansion'     => 'expansion',
           'arrow'        => 'rarr',
           'click'        => ['click', 'command', 'arrow'],
           'minus'        => 'minus',
           'point'        => 'point',
           'print'        => 'printglyph',
           'result'       => 'result',
           'l'            => 'lslash',
           'L'            => 'Lslash',
           'today'        => ['today'],
           'comma'        => 'comma',
           'atchar'       => 'atchar',
           'lbracechar'   => 'lbracechar',
           'rbracechar'   => 'rbracechar',
           'backslashchar' => 'backslashchar',
           'hashchar'      => 'hashchar',
);

# use default XML formatting to complete the hash, removing XML
# specific formatting.  This avoids some code duplication.
my %default_xml_commands_formatting = 
    %{$Texinfo::Convert::Converter::default_xml_commands_formatting{'normal'}};

foreach my $command (keys(%default_xml_commands_formatting)) {
  if (!exists($commands_formatting{$command})) {
    if ($default_xml_commands_formatting{$command} ne '') {
      if ($default_xml_commands_formatting{$command} =~ /^&(.*);$/) {
        $commands_formatting{$command} = $1;
      } else {
        die "BUG: Strange xml_commands_formatting: $default_xml_commands_formatting{$command}\n";
      }
    } else {
      $commands_formatting{$command} = '';
    }
  }
}

# Following are XML specific formatting functions.

# format specific.  Used in few places where plain text is used outside
# of attributes.
sub protect_text($$)
{
  my $self = shift;
  my $string = shift;
  return $self->_protect_text($string);
}

sub _xml_attributes($$)
{
  my $self = shift;
  my $attributes = shift;
  if (ref($attributes) ne 'ARRAY') {
    cluck "attributes not an array($attributes).";
  }
  my $result = '';
  for (my $i = 0; $i < scalar(@$attributes); $i += 2) {
    # this cannot be used, because of formfeed, as in 
    # attribute < which is substituted from &formfeed; is not allowed
    #my $text = $self->_protect_text($attributes->[$i+1]);
    my $text = $self->xml_protect_text($attributes->[$i+1]);
    # in fact form feed is not allowed at all in XML, even protected
    # and even in xml 1.1 in contrast to what is said on internet.
    # maybe this is a limitation of libxml?
    #$text =~ s/\f/&#12;/g;
    if ($attributes->[$i] ne 'spaces' 
        and $attributes->[$i] ne 'trailingspaces') {
      $text =~ s/\f/&attrformfeed;/g;
      # &attrformfeed; resolves to \f so \ are doubled
      $text =~ s/\\/\\\\/g;
    }
    $result .= " $attributes->[$i]=\"".$text."\"";
  }
  return $result;
}

# format specific
sub element($$$)
{
  my $self = shift;
  my $element_name = shift;
  my $attributes = shift;
  my $result= '<'.$element_name;
  $result .= $self->_xml_attributes($attributes) if ($attributes);
  $result .= '/>';
  return $result;
}

# format specific
sub open_element($$$)
{
  my $self = shift;
  my $element_name = shift;
  my $attributes = shift;
  my $result= '<'."$element_name";
  $result .= $self->_xml_attributes($attributes) if ($attributes);
  $result .= '>';
  return $result;
}

# format specific
sub close_element($$)
{
  my $self = shift;
  my $element_name = shift;
  my $result= "</$element_name>";
  return $result;
}

# format specific
sub format_atom($$)
{
  my $self = shift;
  my $atom = shift;
  if ($commands_formatting{$atom} ne '') {
    return '&'.$commands_formatting{$atom}.';';
  } else {
    return '';
  }
}

# format specific
sub format_comment($$)
{
  my $self = shift;
  my $string = shift;

  return $self->xml_comment($string);
}

# form feed is not accepted in xml, replace it.
sub _protect_text($$)
{
  my $self = shift;
  my $text = shift;
  my $result = $self->xml_protect_text($text);
  $result =~ s/\f/&formfeed;/g;
  return $result;
}

# format specific
sub format_text($$)
{
  my $self = shift;
  my $root = shift;
  my $result = $self->_protect_text($root->{'text'});
  if (! defined($root->{'type'}) or $root->{'type'} ne 'raw') {
    if (!$self->{'document_context'}->[-1]->{'monospace'}->[-1]) {
      $result =~ s/``/&textldquo;/g;
      $result =~ s/\'\'/&textrdquo;/g;
      $result =~ s/---/&textmdash;/g;
      $result =~ s/--/&textndash;/g;
      $result =~ s/'/&textrsquo;/g;
      $result =~ s/`/&textlsquo;/g;
    }
  }
  return $result;
}

# output format specific
sub format_header($)
{
  my $self = shift;
  my $encoding = '';
  if ($self->get_conf('OUTPUT_ENCODING_NAME')
      and $self->get_conf('OUTPUT_ENCODING_NAME') ne 'utf-8') {
    $encoding = " encoding=\"".$self->get_conf('OUTPUT_ENCODING_NAME')."\" ";
  }
  my $texinfo_dtd_version = $self->get_conf('TEXINFO_DTD_VERSION');
  if (!defined($texinfo_dtd_version)) {
    $texinfo_dtd_version = '1.00';
  }

  my $header =  "<?xml version=\"1.0\"${encoding}?>".'
<!DOCTYPE texinfo PUBLIC "-//GNU//DTD TexinfoML V'.$texinfo_dtd_version.'//EN" "http://www.gnu.org/software/texinfo/dtd/'.$texinfo_dtd_version.'/texinfo.dtd">
'. $self->open_element('texinfo', ['xml:lang', $self->get_conf('documentlanguage')])."\n";
  if ($self->{'output_file'} ne '') {
    my $output_filename = $self->{'output_filename'};
    $header .= $self->open_element('filename',['file', $output_filename])
             .$self->close_element('filename')."\n";
  }
  return $header;
}

# following is not format specific.  Some infos are taken from generic XML, but 
# XML specific formatting is stripped.

my %accents = (
 '=' => 'macr',
# following are not entities
 'H' => 'doubleacute',
 'u' => 'breve',
 'v' => 'caron',
);

# our because it is used in the xml to texi translator
our %accent_types = (%Texinfo::Convert::Converter::xml_accent_entities, %accents);

# no entity
my @other_accents = ('dotaccent', 'tieaccent', 'ubaraccent', 'udotaccent');
foreach my $accent (@other_accents) {
  $accent_types{$accent} = $accent;
}

my %misc_command_line_attributes = (
  'setfilename' => 'file',
  'documentencoding' => 'encoding',
  'verbatiminclude' => 'file',
  'documentlanguage' => 'xml:lang',
);

my %misc_command_numbered_arguments_attributes = (
  'definfoenclose' => [ 'command', 'open', 'close' ],
  'alias' => [ 'new', 'existing' ],
  'syncodeindex' => [ 'from', 'to' ],
  'synindex' => [ 'from', 'to' ],
);

my %misc_commands = %Texinfo::Common::misc_commands;

foreach my $command ('item', 'headitem', 'itemx', 'tab', 
                      keys %Texinfo::Common::def_commands) {
  delete $misc_commands{$command};
}

my %default_args_code_style
  = %Texinfo::Convert::Converter::default_args_code_style;
my %regular_font_style_commands = %Texinfo::Common::regular_font_style_commands;

# our because it is used in the xml to texi translator
our %commands_args_elements = (
  'email' => ['emailaddress', 'emailname'],
  'uref' => ['urefurl', 'urefdesc', 'urefreplacement'],
  'url' => ['urefurl', 'urefdesc', 'urefreplacement'],
  'inforef' => ['inforefnodename', 'inforefrefname', 'inforefinfoname'],
  'image' => ['imagefile', 'imagewidth', 'imageheight', 
              'alttext', 'imageextension'],
  'quotation' => ['quotationtype'],
  'float' => ['floattype', 'floatname'],
  'itemize' => ['itemprepend'],
  'enumerate' => ['enumeratefirst'],
);

foreach my $ref_cmd ('pxref', 'xref', 'ref') {
  $commands_args_elements{$ref_cmd} 
    = ['xrefnodename', 'xrefinfoname', 'xrefprinteddesc', 'xrefinfofile', 
       'xrefprintedname'];
}

foreach my $explained_command (keys(%Texinfo::Common::explained_commands)) {
  $commands_args_elements{$explained_command} = ["${explained_command}word",
                                                 "${explained_command}desc"];
}

foreach my $inline_command (keys(%Texinfo::Common::inline_commands)) {
  $commands_args_elements{$inline_command} = ["${inline_command}format",
                                              "${inline_command}content"];
}

my $inline_command = 'inlinefmtifelse';
$commands_args_elements{$inline_command} = ["${inline_command}format",
             "${inline_command}contentif", "${inline_command}contentelse"];

my %commands_elements;
foreach my $command (keys(%Texinfo::Common::brace_commands)) {
  $commands_elements{$command} = [$command];
  if ($commands_args_elements{$command}) {
    push @{$commands_elements{$command}}, @{$commands_args_elements{$command}};
  }
}

my %defcommand_name_type = (
 'deffn'     => 'function',
 'defvr'     => 'variable',
 'deftypefn' => 'function',
 'deftypeop' => 'operation',
 'deftypevr' => 'variable',
 'defcv'     => 'classvar',
 'deftypecv' => 'classvar',
 'defop'     => 'operation',
 'deftp'     => 'datatype',
);

my %ignored_types;
foreach my $type (
            # those are put as spaces in the corresponding @-command
            'empty_spaces_after_command',
            'empty_spaces_before_argument',
  ) {
  $ignored_types{$type} = 1;
}

# this is used in IXIN, to ignore everything before first node.
sub _set_ignored_type($$)
{
  my $self = shift;
  my $type = shift;

  $ignored_types{$type} = 1;
}

my %type_elements = (
  'paragraph' => 'para',
  'preformatted' => 'pre',
  'menu_entry' => 'menuentry',
  'menu_entry_node' => 'menunode',
  'menu_comment' => 'menucomment',
  'menu_entry_description' => 'menudescription',
  'menu_entry_name' => 'menutitle',
  'preamble' => 'preamble',
  'table_item' => 'tableitem',
  'table_entry' => 'tableentry',
  'table_term' => 'tableterm',
  'row' => 'row',
  'multitable_head' => 'thead',
  'multitable_body' => 'tbody',
  'def_item' => 'definitionitem',
  'before_item' => 'beforefirstitem',
);

my %default_context_block_commands = (
  'float' => 1,
);

sub converter_defaults($$)
{
  return %defaults;
}

sub converter_initialize($)
{
  my $self = shift;

  $self->{'document_context'} = [{'monospace' => [0]}];
  $self->{'context_block_commands'} = {%default_context_block_commands};
  foreach my $raw (keys (%Texinfo::Common::format_raw_commands)) {
    $self->{'context_block_commands'}->{$raw} = 1
         if $self->{'expanded_formats_hash'}->{$raw};
  } 
  if ($self->{'parser'}) {
    my ($index_names, $merged_indices)
       = $self->{'parser'}->indices_information();
    $self->{'index_names'} = $index_names;
  }
}

sub output($$)
{
  my $self = shift;
  my $root = shift;

  $self->_set_outfile();
  return undef unless $self->_create_destination_directory();

  my $fh;
  if (! $self->{'output_file'} eq '') {
    $fh = $self->Texinfo::Common::open_out($self->{'output_file'});
    if (!$fh) {
      $self->document_error(sprintf($self->__("could not open %s for writing: %s"),
                                    $self->{'output_file'}, $!));
      return undef;
    }
  }

  $self->_set_global_multiple_commands(-1);

  my $result = '';
  $result .= $self->_output_text($self->format_header(), $fh);
  if ($self->get_conf('USE_NODES')) {
    $result .= $self->convert_document_nodes($root, $fh);
  } else {
    $result .= $self->convert_document_sections($root, $fh);
  }
  $result .= $self->_output_text($self->close_element('texinfo')."\n", $fh);
  if ($fh and $self->{'output_file'} ne '-') {
    $self->register_close_file($self->{'output_file'});
    if (!close ($fh)) {
      $self->document_error(sprintf($self->__("error on closing %s: %s"),
                                    $self->{'output_file'}, $!));
    }
  }

  return $result;
}

sub _format_command($$)
{
  my $self = shift;
  my $command = shift;

  if (! ref($commands_formatting{$command})) {
    return $self->format_atom($command);
  } else {
    my @spec = @{$commands_formatting{$command}};
    my $element_name = shift @spec;
    return $self->element($element_name, \@spec);
  }
}

sub _index_entry($$)
{
  my $self = shift;
  my $root = shift;
  if ($root->{'extra'} and $root->{'extra'}->{'index_entry'}) {
    my $index_entry = $root->{'extra'}->{'index_entry'};
    my $attribute = ['index', $index_entry->{'index_name'}];
    push @$attribute, ('number', $index_entry->{'number'})
        if (defined($index_entry->{'number'}));
    # in case the index is not a default index, or the style of the
    # entry (in code or not) is not the default for this index
    if ($self->{'index_names'}) {
      my $in_code = $self->{'index_names'}->{$index_entry->{'index_name'}}->{'in_code'};
      if (!$Texinfo::Common::index_names{$index_entry->{'index_name'}}
          or $in_code != $Texinfo::Common::index_names{$index_entry->{'index_name'}}->{'in_code'}) {
        push @$attribute, ('incode', $in_code);
      }
      if ($self->{'index_names'}->{$index_entry->{'index_name'}}->{'merged_in'}) {
        push @$attribute, ('mergedindex', 
         $self->{'index_names'}->{$index_entry->{'index_name'}}->{'merged_in'});
      }
    }
    my $result = $self->open_element('indexterm', $attribute);
    push @{$self->{'document_context'}}, {'monospace' => [0]};
    $self->{'document_context'}->[-1]->{'monospace'}->[-1] = 1
      if ($index_entry->{'in_code'});
    $result .= $self->_convert({'contents' => $index_entry->{'content'}});
    pop @{$self->{'document_context'}};
    $result .= $self->close_element('indexterm');
    return $result;
  }
  return '';
}

sub _infoenclose_attribute($$) {
  my $self = shift;
  my $root = shift;
  my @attribute = ();
  return @attribute if (!$root->{'extra'});
  push @attribute, ('begin', $root->{'extra'}->{'begin'})
    if (defined($root->{'extra'}->{'begin'}));
  push @attribute, ('end', $root->{'extra'}->{'end'})
    if (defined($root->{'extra'}->{'end'}));
  return @attribute;
}

sub _accent($$;$$$)
{
  my $self = shift;
  my $text = shift;
  my $root = shift;
  my $in_upper_case = shift;
  my $attributes = shift;
  $attributes = [] if (!defined($attributes));

  unshift @$attributes, ('type', $accent_types{$root->{'cmdname'}});
  my $result = $self->open_element('accent', $attributes);
  $result .= $text;
  $result .= $self->close_element('accent');
  return $result;
}

sub convert($$;$)
{
  my $self = shift;
  my $root = shift;
  my $fh = shift;
  
  return $self->convert_document_sections($root, $fh);
}

sub convert_tree($$)
{
  my $self = shift;
  my $root = shift;

  return $self->_convert($root);
}

sub _protect_in_spaces($)
{
  my $text = shift;
  $text =~ s/\n/\\n/g;
  $text =~ s/\f/\\f/g;
  return $text;
}

sub _leading_spaces($)
{
  my $root = shift;
  if ($root->{'extra'} and $root->{'extra'}->{'spaces_after_command'}
      and $root->{'extra'}->{'spaces_after_command'}->{'type'} eq 'empty_spaces_after_command') {
    return ('spaces', _protect_in_spaces(
         $root->{'extra'}->{'spaces_after_command'}->{'text'}));
  } else {
    return ();
  }
}

sub _leading_spaces_before_argument($)
{
  my $root = shift;
  if ($root->{'extra'} and $root->{'extra'}->{'spaces_before_argument'}
      and $root->{'extra'}->{'spaces_before_argument'}->{'type'} eq 'empty_spaces_before_argument'
      and $root->{'extra'}->{'spaces_before_argument'}->{'text'} ne '') {
    return ('spaces', _protect_in_spaces(
                 $root->{'extra'}->{'spaces_before_argument'}->{'text'}));
  } else {
    return ();
  }
}

sub _end_line_spaces($$)
{
  my $root = shift;
  my $type = shift;

  my $end_spaces = undef;
  if ($root->{'args'}->[-1]->{'contents'}) {
    my $index = -1;
    if ($root->{'args'}->[-1]->{'contents'}->[-1]->{'cmdname'}
        and ($root->{'args'}->[-1]->{'contents'}->[-1]->{'cmdname'} eq 'c' 
             or $root->{'args'}->[-1]->{'contents'}->[-1]->{'cmdname'} eq 'comment')) {
      $index = -2;
    }
    if ($root->{'args'}->[-1]->{'contents'}->[$index]
        and $root->{'args'}->[-1]->{'contents'}->[$index]->{'type'}
        and $root->{'args'}->[-1]->{'contents'}->[$index]->{'type'} eq $type
        and defined($root->{'args'}->[-1]->{'contents'}->[$index]->{'text'})
        and $root->{'args'}->[-1]->{'contents'}->[$index]->{'text'} !~ /\S/) {
      $end_spaces = $root->{'args'}->[-1]->{'contents'}->[$index]->{'text'};
      chomp $end_spaces;
    }
  }
  return $end_spaces;
}

sub _arg_line($)
{
  my $self = shift;
  my $root = shift;
  if ($root->{'extra'} and defined($root->{'extra'}->{'arg_line'})) {
    my $line = $root->{'extra'}->{'arg_line'};
    chomp($line);
    if ($line ne '') {
      return ('line', $line);
    }
  } 
  return ();
}

sub _trailing_spaces_arg($$)
{
  my $self = shift;
  my $root = shift;
  
  my @spaces = $self->_collect_leading_trailing_spaces_arg($root);
  if (defined($spaces[1])) {
    chomp($spaces[1]);
    if ($spaces[1] ne '') {
      return ('trailingspaces', _protect_in_spaces($spaces[1]));
    }
  }
  return ();
}

sub _leading_spaces_arg($$)
{
  my $self = shift;
  my $root = shift;

  my @result = ();
  my @spaces = $self->_collect_leading_trailing_spaces_arg($root);
  if (defined($spaces[0]) and $spaces[0] ne '') {
    @result = ('spaces', _protect_in_spaces($spaces[0]));
  }
  return @result;
}

sub _leading_trailing_spaces_arg($$)
{
  my $self = shift;
  my $root = shift;

  my @result;
  my @spaces = $self->_collect_leading_trailing_spaces_arg($root);
  if (defined($spaces[0]) and $spaces[0] ne '') {
    push @result, ('spaces', _protect_in_spaces($spaces[0]));
  }
  if (defined($spaces[1])) {
    chomp($spaces[1]);
    if ($spaces[1] ne '') {
      push @result, ('trailingspaces', _protect_in_spaces($spaces[1]));
    }
  }
  return @result;
}

sub _texinfo_line($$)
{
  my $self = shift;
  my $root = shift;

  my ($comment, $tree) = Texinfo::Convert::Converter::_tree_without_comment(
                                                                        $root);
  my $line = Texinfo::Convert::Texinfo::convert($tree);
  chomp($line);
  if ($line ne '') {
    return ('line', $line);
  } else {
    return ();
  }
}

my @node_directions = ('Next', 'Prev', 'Up');

# not used here, but it is consistent with other %commands_args_elements
# entries and may be used by XML to Texinfo converters
$commands_args_elements{'node'} = ['nodename'];
foreach my $direction (@node_directions) {
  push @{$commands_args_elements{'node'}}, 'node'.lc($direction);
}

sub _convert($$;$);

sub _convert($$;$)
{
  my $self = shift;
  my $root = shift;

  if (0) {
  #if (1) { #}
    print STDERR "root\n";
    print STDERR "  Command: $root->{'cmdname'}\n" if ($root->{'cmdname'});
    print STDERR "  Type: $root->{'type'}\n" if ($root->{'type'});
    print STDERR "  Text: $root->{'text'}\n" if (defined($root->{'text'}));
    #print STDERR "  Special def_command: $root->{'extra'}->{'def_command'}\n"
    #  if (defined($root->{'extra'}) and $root->{'extra'}->{'def_command'});
  }

  return '' if ($root->{'type'} and $ignored_types{$root->{'type'}});
  my $result = '';
  if (defined($root->{'text'})) {
    if ($self->{'document_context'}->[-1]->{'raw'}) {
      # ignore the newline at the end of the @xml line, and the last in xml
      if ($root->{'type'} and ($root->{'type'} eq 'empty_line_after_command'
                               or $root->{'type'} eq 'last_raw_newline')) {
        return '';
      } else {
        return $root->{'text'};
      }
    } elsif ($root->{'type'} 
             and $root->{'type'} eq 'empty_line_after_command'
             and $root->{'extra'}->{'command'}) {
      my $command_name = $root->{'extra'}->{'command'}->{'cmdname'};
      
      if ($Texinfo::Common::format_raw_commands{$command_name} and 
          $self->{'expanded_formats_hash'}->{$command_name}) {
        return '';
      }
    }
    $result = $self->format_text($root);
    return $result;
  }
  my @close_elements;
  if ($root->{'cmdname'}) {
    if (defined($commands_formatting{$root->{'cmdname'}})) {
      if ($root->{'cmdname'} eq 'click' 
          and $root->{'extra'} 
          and defined($root->{'extra'}->{'clickstyle'})) {
        return $self->element('click', ['command', $root->{'extra'}->{'clickstyle'}]);;
      }
      if ($self->{'itemize_line'} and $root->{'type'} 
          and $root->{'type'} eq 'command_as_argument'
          and !$root->{'args'}) {
        return $self->element('formattingcommand', ['command', $root->{'cmdname'}]);
      }
      return $self->_format_command($root->{'cmdname'});
    } elsif ($accent_types{$root->{'cmdname'}}) {
      if ($self->get_conf('ENABLE_ENCODING')) {
        return $self->convert_accents($root, \&_accent);
      } else {
        my $attributes = [];
        if (!$root->{'args'}) {
          $result = '';
        } else {
          $result = $self->_convert($root->{'args'}->[0]);
          if ($root->{'extra'} and $root->{'extra'}->{'spaces'}) {
            push @$attributes,  ('spaces', $root->{'extra'}->{'spaces'});
          }
          if ($root->{'args'}->[0]->{'type'} eq 'following_arg') {
             push @$attributes, ('bracketed', 'off');
          }
        }
        return $self->_accent($result, $root,  undef, $attributes);
      }
    } elsif ($root->{'cmdname'} eq 'item' or $root->{'cmdname'} eq 'itemx'
             or $root->{'cmdname'} eq 'headitem' or $root->{'cmdname'} eq 'tab') {
      if ($root->{'cmdname'} eq 'item'
          and $root->{'parent'}->{'cmdname'}
          and ($root->{'parent'}->{'cmdname'} eq 'itemize'
               or $root->{'parent'}->{'cmdname'} eq 'enumerate')) {
        $result .= $self->open_element('listitem', [_leading_spaces($root)]);
        if ($root->{'parent'}->{'cmdname'} eq 'itemize'
            and $root->{'parent'}->{'extra'} 
            and $root->{'parent'}->{'extra'}->{'block_command_line_contents'}
            and $root->{'parent'}->{'extra'}->{'block_command_line_contents'}->[0]) {
          $result .= $self->open_element('prepend')
            .$self->_convert({'contents' 
        => $root->{'parent'}->{'extra'}->{'block_command_line_contents'}->[0]})
            .$self->close_element('prepend');
        }
        unshift @close_elements, 'listitem';
      } elsif (($root->{'cmdname'} eq 'item' or $root->{'cmdname'} eq 'itemx')
               and $root->{'parent'}->{'type'} 
               and $root->{'parent'}->{'type'} eq 'table_term') {
        my $table_command = $root->{'parent'}->{'parent'}->{'parent'};
        my $format_item_command;
        my $attribute = [];
        if ($table_command->{'extra'} 
            and $table_command->{'extra'}->{'command_as_argument'}) {
          $format_item_command 
            = $table_command->{'extra'}->{'command_as_argument'}->{'cmdname'};
          $attribute 
           = [$self->_infoenclose_attribute($table_command->{'extra'}->{'command_as_argument'})];
        }
        $result .= $self->open_element($root->{'cmdname'}, [_leading_spaces($root)]);
        if ($format_item_command) {
          $result .=  $self->open_element('itemformat', ['command', $format_item_command, @$attribute]);
        }
        $result .= $self->_index_entry($root);
        my $in_code;
        $in_code = 1
          if ($format_item_command 
              and defined($default_args_code_style{$format_item_command})
              and $default_args_code_style{$format_item_command}->[0]);
        my $in_monospace_not_normal;
        if ($format_item_command) {
          if (defined($default_args_code_style{$format_item_command})
              and $default_args_code_style{$format_item_command}->[0]) {
            $in_monospace_not_normal = 1;
          } elsif ($regular_font_style_commands{$format_item_command}) {
            $in_monospace_not_normal = 0;
          }
        }
        push @{$self->{'document_context'}->[-1]->{'monospace'}}, 
          $in_monospace_not_normal
            if (defined($in_monospace_not_normal));

        $result .= $self->_convert($root->{'args'}->[0]);
        pop @{$self->{'document_context'}->[-1]->{'monospace'}} 
          if (defined($in_monospace_not_normal));
        chomp ($result);
        if ($format_item_command) {
          $result .= $self->close_element('itemformat');
        }
        $result .= $self->close_element($root->{'cmdname'})."\n";
      } else {
        unless (($root->{'cmdname'} eq 'item' 
                     or $root->{'cmdname'} eq 'headitem'
                     or $root->{'cmdname'} eq 'tab')
                    and $root->{'parent'}->{'type'}
                    and $root->{'parent'}->{'type'} eq 'row') {
          print STDERR "BUG: multitable cell command not in a row "
            .Texinfo::Parser::_print_current($root);
        }
        
        $result .= $self->open_element('entry', ['command', 
               $root->{'cmdname'}, _leading_spaces($root)]);
        unshift @close_elements, 'entry';
      }
    } elsif ($root->{'type'} and $root->{'type'} eq 'index_entry_command') {
      my $element;
      my $attribute = [];
      if (exists $Texinfo::Common::misc_commands{$root->{'cmdname'}}) {
        $element = $root->{'cmdname'};
      } else {
        $element = 'indexcommand';
        $attribute = ['command', $root->{'cmdname'}];
      }
      push @$attribute, ('index', $root->{'extra'}->{'index_entry'}->{'index_name'});
      push @$attribute, _leading_spaces($root);
      my $end_line;
      if ($root->{'args'}->[0]) {
        $end_line = $self->_end_line_or_comment($root->{'args'}->[0]->{'contents'});
      } else {
        # May that happen?
        $end_line = '';
      }
      return $self->open_element($element, ${attribute}).
        $self->_index_entry($root).$self->close_element($element).${end_line};
    } elsif (exists($misc_commands{$root->{'cmdname'}})) {
      my $command = $root->{'cmdname'};
      my $type = $misc_commands{$root->{'cmdname'}};
      if ($type eq 'text') {
        return '' if ($root->{'cmdname'} eq 'end');
        my $attribute;
        if ($misc_command_line_attributes{$root->{'cmdname'}}) {
          if ($root->{'extra'} and defined($root->{'extra'}->{'text_arg'})) {
            push @$attribute, ($misc_command_line_attributes{$root->{'cmdname'}},
                  $root->{'extra'}->{'text_arg'});
          }
        }
        my ($arg, $end_line)
            = $self->_convert_argument_and_end_line($root->{'args'}->[0]);
        push @$attribute, _leading_spaces($root);
        return $self->open_element($command, $attribute).$arg
                .$self->close_element($command).${end_line};
      } elsif ($type eq 'line') {
        if ($root->{'cmdname'} eq 'node') {
          my $nodename;
          if (defined($root->{'extra'}->{'normalized'})) {
            $nodename = $root->{'extra'}->{'normalized'};
          } else {
            $nodename = '';
          }
          # FIXME avoid protection, here?
          $result .= $self->open_element('node', ['name', $nodename, _leading_spaces($root)]);
          push @{$self->{'document_context'}->[-1]->{'monospace'}}, 1;
          $result .= $self->open_element('nodename', 
            [$self->_trailing_spaces_arg($root->{'args'}->[0])])
             .$self->_convert({'contents' => $root->{'extra'}->{'node_content'}})
             .$self->close_element('nodename');
          # first arg is the node name.
          my $direction_index = 1;
          my $pending_empty_directions = '';
          foreach my $direction(@node_directions) {
            my $element = 'node'.lc($direction);
            if ($root->{'node_'.lc($direction)}) {
              my $node_direction = $root->{'node_'.lc($direction)};
              my $node_name = '';
              my $attribute = [];
              if (! defined($root->{'extra'}->{'nodes_manuals'}->[$direction_index])) {
                push @$attribute, ('automatic', 'on');
              }
              if ($root->{'args'}->[$direction_index]) {
                push @$attribute, $self->_leading_trailing_spaces_arg(
                                 $root->{'args'}->[$direction_index]);
              }
              if ($node_direction->{'extra'}->{'manual_content'}) {
                $node_name .= $self->_convert({
                             'contents' => [{'text' => '('},
                             @{$node_direction->{'extra'}->{'manual_content'}},
                                          {'text' => ')'}]});
              }
              if ($node_direction->{'extra'}->{'node_content'}) {
                $node_name .= Texinfo::Common::normalize_top_node_name($self->_convert({
                  'contents' => $node_direction->{'extra'}->{'node_content'}}));
              }
              $result .= "$pending_empty_directions".
                $self->open_element($element, ${attribute}).$node_name.
                $self->close_element($element);
              $pending_empty_directions = '';
            } else {
              if ($root->{'args'}->[$direction_index]) {
                my $spaces_attribute = $self->_leading_trailing_spaces_arg(
                                 $root->{'args'}->[$direction_index]);
                $pending_empty_directions .= $self->open_element($element,
                    [$self->_leading_trailing_spaces_arg(
                                 $root->{'args'}->[$direction_index])])
                            .$self->close_element($element);
              }
            }
            $direction_index++;
          }
          my $end_line;
          if ($root->{'args'}->[0]) {
            $end_line 
              = $self->_end_line_or_comment($root->{'args'}->[-1]->{'contents'});
          } else {
            $end_line = "\n";
          }
          if (! $self->get_conf('USE_NODES')) {
            $result .= $self->close_element('node');
          }
          $result .= ${end_line};
          pop @{$self->{'document_context'}->[-1]->{'monospace'}};
        } elsif ($Texinfo::Common::root_commands{$root->{'cmdname'}}) {
          my $attribute = [_leading_spaces($root)];
          $command = $self->_level_corrected_section($root);
          if ($command ne $root->{'cmdname'}) {
            unshift @$attribute, ('originalcommand', $root->{'cmdname'});
          }
          $result .= $self->open_element($command, $attribute);
          my $closed_section_element;
          if ($self->get_conf('USE_NODES')) {
            $closed_section_element = $self->close_element($command);
          } else {
            $closed_section_element = '';
          }

          if ($root->{'args'} and $root->{'args'}->[0]) {
            my ($arg, $end_line)
              = $self->_convert_argument_and_end_line($root->{'args'}->[0]);
            $result .= $self->open_element('sectiontitle').$arg
                      .$self->close_element('sectiontitle')
                      .$closed_section_element.$end_line;
          } else {
            $result .= $closed_section_element;
          }
        } else {
          my $attribute = [_leading_spaces($root)];
          if ($root->{'cmdname'} eq 'listoffloats' and $root->{'extra'} 
              and $root->{'extra'}->{'type'} 
              and defined($root->{'extra'}->{'type'}->{'normalized'})) {
            unshift @$attribute, ('type', $root->{'extra'}->{'type'}->{'normalized'});
          }
          my ($arg, $end_line)
            = $self->_convert_argument_and_end_line($root->{'args'}->[0]);
          return $self->open_element($command, ${attribute}).$arg
               .$self->close_element($command).$end_line;
        }
      } elsif ($type eq 'skipline') {
        # the command associated with an element is closed at the end of the
        # element. @bye is withing the element, but we want it to appear after
        # the comand closing.  So we delay the output of @bye, and store it.
        if ($root->{'cmdname'} eq 'bye' and $root->{'parent'}
            and $root->{'parent'}->{'type'}
            and $root->{'parent'}->{'type'} eq 'element'
            and !($root->{'parent'}->{'extra'}
                  and ($root->{'parent'}->{'extra'}->{'no_section'}
                       or $root->{'parent'}->{'extra'}->{'no_node'}))) {
          #print STDERR "$root->{'parent'} $root->{'parent'}->{'type'}\n";
          $self->{'pending_bye'} = $self->open_element($command)
                    .$self->close_element($command)."\n";
          return '';
        }
        my $attribute = [];
        if ($root->{'args'} and $root->{'args'}->[0] 
            and defined($root->{'args'}->[0]->{'text'})) {
          my $line = $root->{'args'}->[0]->{'text'};
          chomp($line);
          $attribute = ['line', $line]
             if ($line ne '');
        }
        return $self->open_element($command, $attribute)
                 .$self->close_element($command)."\n";
      } elsif ($type eq 'noarg' or $type eq 'skipspace') {
        my $spaces = '';
        $spaces = $root->{'extra'}->{'spaces_after_command'}->{'text'}
          if ($root->{'extra'} and $root->{'extra'}->{'spaces_after_command'}
              and $root->{'extra'}->{'spaces_after_command'}->{'type'} eq 'empty_spaces_after_command');
        return $self->open_element($command)
                .$self->close_element($command).$spaces;
      } elsif ($type eq 'special') {
        if ($root->{'cmdname'} eq 'clear' or $root->{'cmdname'} eq 'set') {
          my $attribute = [];
          if ($root->{'args'} and $root->{'args'}->[0]
              and defined($root->{'args'}->[0]->{'text'})) {
            push @$attribute, ('name', $root->{'args'}->[0]->{'text'});
          }
          my $value = '';
          if ($root->{'cmdname'} eq 'set' and $root->{'args'} and $root->{'args'}->[1]
              and defined($root->{'args'}->[1]->{'text'})) {
            $value = $self->protect_text($root->{'args'}->[1]->{'text'});
          }
          push @$attribute, $self->_arg_line($root);
          return $self->open_element($command, $attribute)
                         .$value.$self->close_element($command)."\n";
        } elsif ($root->{'cmdname'} eq 'clickstyle') {
          my $attribute = [$self->_arg_line($root)];
          my $value = '';
          if ($root->{'args'} and $root->{'args'}->[0]
              and defined($root->{'args'}->[0]->{'text'})) {
            my $click_command = $root->{'args'}->[0]->{'text'};
            $click_command =~ s/^\@//;
            unshift @$attribute, ('command', $click_command);
            $value = $self->protect_text($root->{'args'}->[0]->{'text'});
          };
          return $self->open_element($command, $attribute)
                         .$value.$self->close_element($command)."\n";
        } else {
          # should only be unmacro
          my $attribute = [$self->_arg_line($root)];
          if ($root->{'args'} and $root->{'args'}->[0]
              and defined($root->{'args'}->[0]->{'text'})) {
            unshift @$attribute, ('name', $root->{'args'}->[0]->{'text'});
          }
          return $self->open_element($command, $attribute)
                    .$self->close_element($command)."\n";
        }
      } elsif ($type eq 'lineraw') {
        if ($root->{'cmdname'} eq 'c' or $root->{'cmdname'} eq 'comment') {
          return $self->format_comment(" $root->{'cmdname'}".$root->{'args'}->[0]->{'text'})
        } else {
          my $value = '';
          if ($root->{'args'} and $root->{'args'}->[0]
              and defined($root->{'args'}->[0]->{'text'})) {
            $value = $self->protect_text($root->{'args'}->[0]->{'text'});
          }
          chomp ($value);
          return $self->open_element($command).$value
                    .$self->close_element($command)."\n";
        }
      } else {
        print STDERR "BUG: unknown misc_command style $type\n" if ($type !~ /^\d$/);
        my $args_attributes;
        if ($misc_command_numbered_arguments_attributes{$root->{'cmdname'}}) {
          $args_attributes = $misc_command_numbered_arguments_attributes{$root->{'cmdname'}};
        } else {
          $args_attributes = ['value'];
        }
        my $attribute = [];
        my $arg_index = 0;
        if (defined($root->{'extra'}) 
            and defined($root->{'extra'}->{'misc_args'})) {
          foreach my $arg_attribute (@{$args_attributes}) {
            if (defined ($root->{'extra'}->{'misc_args'}->[$arg_index])) {
              push @$attribute, ( $arg_attribute, 
                        $root->{'extra'}->{'misc_args'}->[$arg_index]);
            }
            $arg_index++;
          }
        }
        my $end_line;
        if ($root->{'args'}->[0]) {
          $end_line = $self->_end_line_or_comment(
                                         $root->{'args'}->[0]->{'contents'});
          push @$attribute, $self->_texinfo_line($root->{'args'}->[0]);
        } else {
          $end_line = "\n";
        }
        return $self->open_element($command, $attribute)
                    .$self->close_element($command).$end_line;
      }
    } elsif ($root->{'type'}
             and $root->{'type'} eq 'definfoenclose_command') {
      my $in_monospace_not_normal;
      if (defined($default_args_code_style{$root->{'cmdname'}})
          and $default_args_code_style{$root->{'cmdname'}}->[0]) {
        $in_monospace_not_normal = 1;
      } elsif ($regular_font_style_commands{$root->{'cmdname'}}) {
        $in_monospace_not_normal = 0;
      }
      push @{$self->{'document_context'}->[-1]->{'monospace'}}, 
        $in_monospace_not_normal
          if (defined($in_monospace_not_normal));
      my $arg = $self->_convert($root->{'args'}->[0]);
      $result .= $self->open_element('infoenclose', ['command', $root->{'cmdname'},
                                        $self->_infoenclose_attribute($root)])
                 .$arg.$self->close_element('infoenclose');
      pop @{$self->{'document_context'}->[-1]->{'monospace'}}
        if (defined($in_monospace_not_normal));
    } elsif ($root->{'args'}
             and exists($Texinfo::Common::brace_commands{$root->{'cmdname'}})) {
      if ($Texinfo::Common::context_brace_commands{$root->{'cmdname'}}) {
        push @{$self->{'document_context'}}, {'monospace' => [0]};
      }
      if ($Texinfo::Common::inline_format_commands{$root->{'cmdname'}}
          and $root->{'extra'} and $root->{'extra'}->{'format'}
          and $self->{'expanded_formats_hash'}->{$root->{'extra'}->{'format'}}) {
        if ($root->{'cmdname'} eq 'inlineraw') {
          push @{$self->{'document_context'}}, {'monospace' => [0]};
          $self->{'document_context'}->[-1]->{'raw'} = 1;
        }
        if (scalar (@{$root->{'extra'}->{'brace_command_contents'}}) == 2
            and defined($root->{'extra'}->{'brace_command_contents'}->[-1])) {
          $result .= $self->_convert({'contents' 
                        => $root->{'extra'}->{'brace_command_contents'}->[-1]});
        }
        if ($root->{'cmdname'} eq 'inlineraw') {
          pop @{$self->{'document_context'}};
        }
        return $result;
      }
      my @elements = @{$commands_elements{$root->{'cmdname'}}};
      my $command;
      if (scalar(@elements) > 1) {
        $command = shift @elements;
      }
      # this is used for commands without args, or associated to the
      # first argument
      my $attribute = [];
      if ($root->{'cmdname'} eq 'verb') {
        push @$attribute, ('delimiter', $root->{'type'});
      } elsif ($root->{'cmdname'} eq 'anchor') {
        my $anchor_name;
        if (defined($root->{'extra'}->{'normalized'})) {
          $anchor_name = $root->{'extra'}->{'normalized'};
        } else {
          $anchor_name = '';
        }
        push @$attribute, ('name', $anchor_name);
      }
      my $arg_index = 0;
      foreach my $element (@elements) {
        if (defined($root->{'args'}->[$arg_index])) {
          my $in_monospace_not_normal;
          if (defined($default_args_code_style{$root->{'cmdname'}})
              and $default_args_code_style{$root->{'cmdname'}}->[$arg_index]) {
            $in_monospace_not_normal = 1;
          } elsif ($regular_font_style_commands{$root->{'cmdname'}}) {
            $in_monospace_not_normal = 0;
          }
          push @{$self->{'document_context'}->[-1]->{'monospace'}},
            $in_monospace_not_normal
              if (defined($in_monospace_not_normal));
          my $arg = $self->_convert($root->{'args'}->[$arg_index]);
          if ($arg_index > 0) {
            push @$attribute, 
              $self->_leading_spaces_arg($root->{'args'}->[$arg_index]);
          }
          if (!defined($command) or $arg ne '' or scalar(@$attribute) > 0) {
            # ${attribute} is only set for @verb
            push @$attribute, _leading_spaces_before_argument($root)
               if (!defined($command));
            $result .= $self->open_element($element, $attribute).$arg
                      .$self->close_element($element);
          }
          $attribute = [];
          pop @{$self->{'document_context'}->[-1]->{'monospace'}}
            if (defined($in_monospace_not_normal));
        } else {
          last;
        }
        $arg_index++;
      }
      # This is for the main command
      $attribute = [];
      if ($root->{'cmdname'} eq 'image') {
        if ($self->_is_inline($root)) {
          push @$attribute, ('where', 'inline');
        }
      } elsif ($Texinfo::Common::ref_commands{$root->{'cmdname'}}) {
        if ($root->{'extra'}->{'brace_command_contents'}) {
          if ($root->{'extra'}->{'node_argument'}
              and $root->{'extra'}->{'node_argument'}->{'node_content'}
              and defined($root->{'extra'}->{'node_argument'}->{'normalized'})) {
            push @$attribute, ('label', 
                 $root->{'extra'}->{'node_argument'}->{'normalized'});
          }
          my $manual;
          my $manual_arg_index = 3;
          if ($root->{'cmdname'} eq 'inforef') {
            $manual_arg_index = 2;
          }
          if ($root->{'extra'}->{'brace_command_contents'}->[$manual_arg_index]) {
            $manual = Texinfo::Convert::Text::convert({'contents'
             => $root->{'extra'}->{'brace_command_contents'}->[$manual_arg_index]}, 
                      {'code' => 1,
                       Texinfo::Common::_convert_text_options($self)});
          }
          if (!defined($manual) and $root->{'extra'}->{'node_argument'}
              and $root->{'extra'}->{'node_argument'}->{'manual_content'}) {
            $manual = Texinfo::Convert::Text::convert({'contents' 
                 => $root->{'extra'}->{'node_argument'}->{'manual_content'}},
              {'code' => 1, Texinfo::Common::_convert_text_options($self)});
          }
          if (defined($manual)) {
            my $manual_base = $manual;
            $manual_base =~ s/\.[^\.]*$//;
            $manual_base =~ s/^.*\///;
            
            push @$attribute, ('manual', $manual_base)
                  if ($manual_base ne '');
          }
        }
      }
      if (defined($command)) {
        push @$attribute, _leading_spaces_before_argument($root);
        $result = $self->open_element($command, $attribute).$result
                  .$self->close_element($command);
      }
      if ($Texinfo::Common::context_brace_commands{$root->{'cmdname'}}) {
        pop @{$self->{'document_context'}};
      }
    } elsif (exists($Texinfo::Common::block_commands{$root->{'cmdname'}})) {
      if ($self->{'context_block_commands'}->{$root->{'cmdname'}}) {
        push @{$self->{'document_context'}}, {'monospace' => [0]};
      }
      my $prepended_elements = '';
      my $attribute = [];
      $self->{'itemize_line'} = 1 if ($root->{'cmdname'} eq 'itemize');
      if ($root->{'extra'} and $root->{'extra'}->{'command_as_argument'}) {
        my $command_as_arg = $root->{'extra'}->{'command_as_argument'};
        push @$attribute,
         ('commandarg', $command_as_arg->{'cmdname'},
             $self->_infoenclose_attribute($command_as_arg));
      } elsif ($root->{'extra'}
               and $root->{'extra'}->{'enumerate_specification'}) {
        push @$attribute,('first', $root->{'extra'}->{'enumerate_specification'});
      } elsif ($root->{'cmdname'} eq 'float' and $root->{'extra'}) {
        if (defined($root->{'extra'}->{'normalized'})) {
          push @$attribute, ('name', $root->{'extra'}->{'normalized'});
        }
        if ($root->{'extra'}->{'type'} and 
            defined($root->{'extra'}->{'type'}->{'normalized'})) {
          push @$attribute, ('type', $root->{'extra'}->{'type'}->{'normalized'});
        }
        if (defined($root->{'number'})) {
          push @$attribute, ('number', $root->{'number'});
        }
      } elsif ($root->{'cmdname'} eq 'verbatim') {
        push @$attribute, ('xml:space', 'preserve');
      } elsif ($root->{'cmdname'} eq 'macro' 
               or $root->{'cmdname'} eq 'rmacro') {
        if (defined($root->{'args'})) {
          my @args = @{$root->{'args'}};
          my $name_arg = shift @args;
          if (defined($name_arg) and defined($name_arg->{'text'})) {
            push @$attribute, ('name', $name_arg->{'text'});
          }
          
          while (@args) {
            my $formal_arg = shift @args;
            $prepended_elements .= $self->open_element('formalarg')
                .$self->protect_text($formal_arg->{'text'})
                .$self->close_element('formalarg');
          }
        }
        push @$attribute, $self->_arg_line($root);
      }
      if ($self->{'expanded_formats_hash'}->{$root->{'cmdname'}}) {
        $self->{'document_context'}->[-1]->{'raw'} = 1;
      } else {
        my $end_command = $root->{'extra'}->{'end_command'};
        my $end_command_space = [_leading_spaces($end_command)];
        if (scalar(@$end_command_space)) {
          $end_command_space->[0] = 'endspaces';
        }
        $result .= $self->open_element($root->{'cmdname'}, [@$attribute, 
                              _leading_spaces($root), @$end_command_space])
                      .${prepended_elements};
        my $end_line = '';
        if ($root->{'args'}) {
          if ($commands_args_elements{$root->{'cmdname'}}) {
            my $arg_index = 0;
            foreach my $element (@{$commands_args_elements{$root->{'cmdname'}}}) {
              if (defined($root->{'args'}->[$arg_index])) {
                my $in_code;
                 $in_code = 1
                  if (defined($default_args_code_style{$root->{'cmdname'}})
                    and $default_args_code_style{$root->{'cmdname'}}->[$arg_index]);
                push @{$self->{'document_context'}->[-1]->{'monospace'}}, 1 
                  if ($in_code);
                my $arg;
                if ($arg_index+1 eq scalar(@{$root->{'args'}})) {
                  # last argument
                  ($arg, $end_line) 
                    = $self->_convert_argument_and_end_line($root->{'args'}->[$arg_index]);
                } else {
                  $arg = $self->_convert($root->{'args'}->[$arg_index]);
                }
                my $spaces = [];
                if ($arg_index != 0) {
                  push @$spaces, $self->_leading_spaces_arg(
                                              $root->{'args'}->[$arg_index]);
                }
                if ($arg ne '' or scalar(@$spaces)) {
                  $result .= $self->open_element($element, $spaces).$arg
                           .$self->close_element($element);
                }
                pop @{$self->{'document_context'}->[-1]->{'monospace'}} 
                  if ($in_code);
              } else {
                last;
              }
              $arg_index++;
            }
          } else {
            my $contents_possible_comment;
            # in that case the end of line is in the columnfractions line
            # or in the columnprototypes.  
            if ($root->{'cmdname'} eq 'multitable' and $root->{'extra'}) {
              if ($root->{'extra'}->{'prototypes_line'}) {
                $result .= $self->open_element('columnprototypes');
                my $first_proto = 1;
                foreach my $prototype (@{$root->{'extra'}->{'prototypes_line'}}) {
                  if ($prototype->{'text'} and $prototype->{'text'} !~ /\S/) {
                    if (!$first_proto) {
                      my $spaces = $prototype->{'text'};
                      chomp($spaces);
                      $result .= $spaces;
                    }
                  } else {
                    my $attribute = [];
                    if ($prototype->{'type'} 
                        and $prototype->{'type'} eq 'bracketed') {
                      push @$attribute, ('bracketed', 'on');
                      push @$attribute, _leading_spaces_before_argument($prototype);
                    }
                    $result .= $self->open_element('columnprototype', $attribute)
                           .$self->_convert($prototype)
                           .$self->close_element('columnprototype');
                  }
                  $first_proto = 0;
                }
                $result .= $self->close_element('columnprototypes');
                $contents_possible_comment 
                  = $root->{'args'}->[-1]->{'contents'};
              } elsif ($root->{'extra'}->{'columnfractions'}) {
                my $cmd;
                foreach my $content (@{$root->{'args'}->[0]->{'contents'}}) {
                  if ($content->{'cmdname'}
                      and $content->{'cmdname'} eq 'columnfractions') {
                    $cmd = $content;
                    last;
                  }
                }
                my $attribute = [$self->_texinfo_line($cmd->{'args'}->[0])];
                $result .= $self->open_element('columnfractions', $attribute);
                foreach my $fraction (@{$root->{'extra'}->{'columnfractions'}}) {
                  $result .= $self->open_element('columnfraction', 
                                                ['value', $fraction])
                             .$self->close_element('columnfraction');
                }
                $result .= $self->close_element('columnfractions');
                $contents_possible_comment 
                  = $root->{'args'}->[-1]->{'contents'}->[-1]->{'args'}->[-1]->{'contents'}
                    if ($root->{'args'}->[-1]->{'contents'}
                        and $root->{'args'}->[-1]->{'contents'}->[-1]->{'args'}
                        and $root->{'args'}->[-1]->{'contents'}->[-1]->{'args'}->[-1]->{'contents'});
              } else { # bogus multitable
                $result .= "\n";
              }
            } else {
              # get end of lines from @*table.
              my $end_spaces = _end_line_spaces($root, 
                                           'space_at_end_block_command');
              if (defined($end_spaces)) {
                $end_line .= $end_spaces 
                # This also catches block @-commands with no argument that
                # have a bogus argument, such as text on @example line
                #print STDERR "NOT xtable: $root->{'cmdname'}\n" 
                #  if (!$Texinfo::Common::item_line_commands{$root->{'cmdname'}});
              }
              $contents_possible_comment = $root->{'args'}->[-1]->{'contents'}
                if ($root->{'args'}->[-1]->{'contents'});
            }
            $end_line .= $self->_end_line_or_comment($contents_possible_comment);
          }
        }
        $result .= $end_line;
        unshift @close_elements, $root->{'cmdname'};
      }
      delete $self->{'itemize_line'} if ($self->{'itemize_line'});
    }
  }
  if ($root->{'type'}) {
    if (defined($type_elements{$root->{'type'}})) {
      my $attribute = [];
      if ($root->{'type'} eq 'preformatted') {
        push @$attribute, ('xml:space', 'preserve');
      } elsif ($root->{'type'} eq 'menu_entry') {
        push @$attribute, ('leadingtext', $self->_convert($root->{'args'}->[0]));
      } elsif (($root->{'type'} eq 'menu_entry_node' 
                or $root->{'type'} eq 'menu_entry_name')
               and $self->{'pending_menu_entry_separator'}) {
        push @$attribute, ('separator',
               $self->_convert($self->{'pending_menu_entry_separator'}));
        delete $self->{'pending_menu_entry_separator'};
      }
      $result .= $self->open_element($type_elements{$root->{'type'}}, $attribute);
    }
    if ($root->{'type'} eq 'def_line') {
      if ($root->{'cmdname'}) {
        $result .= $self->open_element($root->{'cmdname'}, [_leading_spaces($root)]);
      }
      $result .= $self->open_element('definitionterm');
      $result .= $self->_index_entry($root);
      push @{$self->{'document_context'}->[-1]->{'monospace'}}, 1;
      if ($root->{'extra'} and $root->{'extra'}->{'def_args'}) {
        my $main_command;
        my $alias;
        if ($Texinfo::Common::def_aliases{$root->{'extra'}->{'def_command'}}) {
          $main_command = $Texinfo::Common::def_aliases{$root->{'extra'}->{'def_command'}};
          $alias = 1;
        } else {
          $main_command = $root->{'extra'}->{'def_command'};
          $alias = 0;
        }
        foreach my $arg (@{$root->{'extra'}->{'def_args'}}) {
          my $type = $arg->[0];
          my $content = $self->_convert($arg->[1]);
          if ($type eq 'spaces') {
            $result .= $content;
          } else {
            my $attribute = [];
            if ($type eq 'category' and $alias) {
              push @$attribute, ('automatic', 'on');
            }
            my $element;
            if ($type eq 'name') {
              $element = $defcommand_name_type{$main_command};
            } elsif ($type eq 'arg') {
              $element = 'param';
            } elsif ($type eq 'typearg') {
              $element = 'paramtype';
            } else {
              $element = $type;
            }
            if ($arg->[1]->{'type'}
                and $arg->[1]->{'type'} eq 'bracketed_def_content') {
              push @$attribute, ('bracketed', 'on');
              push @$attribute, _leading_spaces_before_argument($arg->[1]);
            }
            $result .= $self->open_element("def$element", $attribute).$content
                      .$self->close_element("def$element");
          }
        }
      }
      pop @{$self->{'document_context'}->[-1]->{'monospace'}};
      $result .= $self->close_element('definitionterm');
      if ($root->{'cmdname'}) {
        $result .= $self->close_element($root->{'cmdname'});
      }
      chomp ($result);
      $result .= "\n";
    }
  }
  if ($root->{'contents'}) {
    my $in_code;
    if ($root->{'cmdname'} 
        and $Texinfo::Common::preformatted_code_commands{$root->{'cmdname'}}) {
      $in_code = 1;
    }
    push @{$self->{'document_context'}->[-1]->{'monospace'}}, 1 
      if ($in_code);
    if (ref($root->{'contents'}) ne 'ARRAY') {
      cluck "contents not an array($root->{'contents'}).";
    }
    foreach my $content (@{$root->{'contents'}}) {
      $result .= $self->_convert($content);
    }
    pop @{$self->{'document_context'}->[-1]->{'monospace'}}
      if ($in_code);
  }
  my $arg_nr = -1;
  if ($root->{'type'} and $root->{'type'} eq 'menu_entry') {
    foreach my $arg (@{$root->{'args'}}) {
      $arg_nr++;
      # menu_entry_leading_text is added as attribute leadingtext of menu_entry
      # menu_entry_separator is recorded here and then added ass attribute
      # separator
      next if ($arg->{'type'} eq 'menu_entry_leading_text'
               or $arg->{'type'} eq 'menu_entry_separator');
      if ($root->{'args'}->[$arg_nr +1]
          and $root->{'args'}->[$arg_nr +1]->{'type'}
          and $root->{'args'}->[$arg_nr +1]->{'type'} eq 'menu_entry_separator') {
        $self->{'pending_menu_entry_separator'} = $root->{'args'}->[$arg_nr +1];
      }
      my $in_code;
      if ($arg->{'type'} eq 'menu_entry_node') {
        $in_code = 1;
      }
      push @{$self->{'document_context'}->[-1]->{'monospace'}}, 1 
        if ($in_code);
      $result .= $self->_convert($arg);
      pop @{$self->{'document_context'}->[-1]->{'monospace'}}
        if ($in_code);
    }
  }
  if ($root->{'type'}) {
    if (defined($type_elements{$root->{'type'}})) {
      $result .= $self->close_element($type_elements{$root->{'type'}});
    }
  }
  $result = '{'.$result.'}' 
     if ($root->{'type'} and $root->{'type'} eq 'bracketed'
         and (!$root->{'parent'}->{'type'} or
              ($root->{'parent'}->{'type'} ne 'block_line_arg'
               and $root->{'parent'}->{'type'} ne 'misc_line_arg')));
  foreach my $element (@close_elements) {
    $result .= $self->close_element($element);
  }
  if ($root->{'cmdname'} 
      and exists($Texinfo::Common::block_commands{$root->{'cmdname'}})) {
    my $end_command = $root->{'extra'}->{'end_command'}; 
    if ($self->{'expanded_formats_hash'}->{$root->{'cmdname'}}) {
    } else {
      my $end_line = '';
      if ($end_command) {
        my $end_spaces = _end_line_spaces($end_command, 'spaces_at_end');
        $end_line .= $end_spaces if (defined($end_spaces));
        $end_line 
         .= $self->_end_line_or_comment($end_command->{'args'}->[0]->{'contents'})
           if ($end_command->{'args'}->[0]
               and $end_command->{'args'}->[0]->{'contents'});
      } else {
        #$end_line = "\n";
      }
      $result .= $end_line;
    }
    if ($self->{'context_block_commands'}->{$root->{'cmdname'}}) {
      pop @{$self->{'document_context'}};
    }
  # The command is closed either when the corresponding tree element
  # is done, and the command is not associated to an element, or when
  # the element is closed.
  } elsif ((($root->{'type'} and $root->{'type'} eq 'element'
             and $root->{'extra'} and $root->{'extra'}->{'element_command'}
             and !($root->{'extra'}->{'element_command'}->{'cmdname'}
                   and $root->{'extra'}->{'element_command'}->{'cmdname'} eq 'node'))
            or ($root->{'cmdname'} 
                and $Texinfo::Common::root_commands{$root->{'cmdname'}}
                and $root->{'cmdname'} ne 'node'
                and !($root->{'parent'} and $root->{'parent'}->{'type'}
                     and $root->{'parent'}->{'type'} eq 'element'
                     and $root->{'parent'}->{'extra'} 
                     and $root->{'parent'}->{'extra'}->{'element_command'}
                     and $root->{'parent'}->{'extra'}->{'element_command'} eq $root)))
           and !$self->get_conf('USE_NODES')) {
    if ($root->{'type'} and $root->{'type'} eq 'element') {
      $root = $root->{'extra'}->{'element_command'};
    }
    my $command = $self->_level_corrected_section($root);
    if (!($root->{'section_childs'} and scalar(@{$root->{'section_childs'}}))
        or $command eq 'top') {
      $result .= $self->close_element($command)."\n";
      my $current = $root;
      while ($current->{'section_up'}
             # the most up element is a virtual sectioning root element, this
             # condition avoids getting into it
             and $current->{'section_up'}->{'cmdname'}
             and !$current->{'section_next'}
             and $self->_level_corrected_section($current->{'section_up'}) ne 'top') {
        $current = $current->{'section_up'};
        $result .= $self->close_element($self->_level_corrected_section($current)) ."\n";
      }
    }
    if ($self->{'pending_bye'}) {
      $result .= $self->{'pending_bye'};
      delete $self->{'pending_bye'};
    }
  } elsif ((($root->{'type'} and $root->{'type'} eq 'element'
             and $root->{'extra'} and $root->{'extra'}->{'element_command'}
             and $root->{'extra'}->{'element_command'}->{'cmdname'}
             and $root->{'extra'}->{'element_command'}->{'cmdname'} eq 'node')
            or ($root->{'cmdname'} 
                and $root->{'cmdname'} eq 'node'
                and !($root->{'parent'} and $root->{'parent'}->{'type'}
                     and $root->{'parent'}->{'type'} eq 'element'
                     and $root->{'parent'}->{'extra'} 
                     and $root->{'parent'}->{'extra'}->{'element_command'}
                     and $root->{'parent'}->{'extra'}->{'element_command'} eq $root)))
           and $self->get_conf('USE_NODES')) {
    #if ($root->{'type'} and $root->{'type'} eq 'element') {
    #  $root = $root->{'extra'}->{'element_command'};
    #}
    $result .= $self->close_element('node');
    
    if ($self->{'pending_bye'}) {
      $result .= $self->{'pending_bye'};
      delete $self->{'pending_bye'};
    }
  }
  return $result;
}

1;

__END__
# Automatically generated from maintain/template.pod

=head1 NAME

Texinfo::Convert::TexinfoXML - Convert Texinfo tree to TexinfoXML

=head1 SYNOPSIS

  my $converter 
    = Texinfo::Convert::TexinfoXML->converter({'parser' => $parser});

  $converter->output($tree);

=head1 DESCRIPTION

Texinfo::Convert::TexinfoXML converts a Texinfo tree to TexinfoXML.

=head1 METHODS

=over

=item $converter = Texinfo::Convert::TexinfoXML->converter($options)

Initialize an TexinfoXML converter.  

The I<$options> hash reference holds options for the converter.  In
this option hash reference a parser object may be associated with the 
I<parser> key.  The other options should be configuration options
described in the Texinfo manual.  Those options, when appropriate,
override the document content.

See L<Texinfo::Convert::Converter> for more informations.

=item $converter->output($tree)

Convert a Texinfo tree I<$tree> and output the result in files as
described in the Texinfo manual.

=item $result = $converter->convert($tree)

Convert a Texinfo tree I<$tree> or tree portion and return 
the resulting output.

=item $result = $converter->convert_tree($tree)

Convert a Texinfo tree portion I<$tree> and return the resulting 
output.  This function do not try to output a full document but only
portions of document.  For a full document use C<convert>.

=item $result = $converter->output_internal_links()

Returns text representing the links in the document.  At present the format 
should follow the C<--internal-links> option of texi2any/makeinfo specification
and this is only relevant for HTML.

=back

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2012 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at 
your option) any later version.

=cut
