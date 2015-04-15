# DocBook.pm: output tree as DocBook.
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

package Texinfo::Convert::DocBook;

use 5.00405;
use strict;

use Texinfo::Convert::Converter;
use Texinfo::Common;
use Texinfo::Convert::Unicode;
use Texinfo::Convert::Text;
# for debugging
use Texinfo::Convert::Texinfo;
use Data::Dumper;
use Carp qw(cluck);

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter Texinfo::Convert::Converter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Texinfo::Convert::DocBook ':all';
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

my $mdash = '&#'.hex('2014').';';
my $ndash = '&#'.hex('2013').';';
my $ldquo = '&#'.hex('201C').';';
my $rdquo = '&#'.hex('201D').';';
my $rsquo = '&#'.hex('2019').';';
my $lsquo = '&#'.hex('2018').';';
my $nbsp = '&#'.hex('00A0').';';

my %defaults = (
  #'ENABLE_ENCODING'      => 0,
  'SHOW_MENU'            => 0,
  'EXTENSION'            => 'xml', # dbk?
  'OUTPUT_ENCODING_NAME' => 'utf-8',
  'OUTFILE'              => undef,
  'SUBDIR'               => undef,
  'output_format'        => 'docbook',
  'SPLIT'                => 0,
  'documentlanguage'     => 'en',
  'OPEN_QUOTE_SYMBOL'    => $lsquo,
  'CLOSE_QUOTE_SYMBOL'   => $rsquo,
);

my @docbook_image_extensions
  = ('eps', 'gif', 'jpg', 'jpeg', 'pdf', 'png', 'svg');

my %docbook_special_quotations;
foreach my $special_quotation ('note', 'caution', 'important', 'tip', 'warning') {
  $docbook_special_quotations{$special_quotation} = 1;
}

# For '*', there is no line break in DocBook, except in cmdsynopsis (in this
# case it is <sbr>.  But currently we don't use cmdsynopsis, and it is unlikely
# that cmdsynopsis is ever used.
my %docbook_specific_formatting = (
  'TeX' => '&tex;',
  'LaTeX' => '&latex;',
  "\t" => $nbsp,
  "\n" => $nbsp,
  " " => $nbsp,
  'tie' => $nbsp,
);
my %docbook_commands_formatting
  = %{$Texinfo::Convert::Converter::default_xml_commands_formatting{'normal'}};

foreach my $command (keys(%Texinfo::Convert::Unicode::unicode_entities)) {
  $docbook_commands_formatting{$command}
   = $Texinfo::Convert::Unicode::unicode_entities{$command};
}

foreach my $command (keys(%docbook_specific_formatting)) {
  $docbook_commands_formatting{$command} 
    = $docbook_specific_formatting{$command};
}

my %quoted_style_commands = (
  'samp' => 1,
);

my @inline_elements = ('emphasis', 'abbrev', 'acronym', 'link', 
  'inlinemediaobject', 'firstterm', 'footnote', 'replaceable', 'wordasword');
my %inline_elements;
foreach my $inline_element (@inline_elements) {
  $inline_elements{$inline_element} = 1;
};

my %style_attribute_commands;
%style_attribute_commands = (
      'b'           => 'emphasis role="bold"',
      'cite'        => 'citetitle',
      'code'        => 'literal',
      'command'     => 'command',
      'dfn'         => 'firstterm',
      'emph'        => 'emphasis',
      'env'         => 'envar',
      'file'        => 'filename',
      'headitemfont' => 'emphasis role="bold"', # not really that, in fact it is 
                             # in <th> rather than <td>
      'i'           => 'emphasis',
      'indicateurl' => 'literal',
      'sansserif'   => '',
      'kbd'         => 'userinput',
      'key'         => 'keycap',
      'option'      => 'option',
      'r'           => 'lineannotation',
      'samp'        => 'literal',
      'strong'      => 'emphasis role="bold"',
      't'           => 'literal',
      'var'         => 'replaceable',
      'verb'        => 'literal',
      'footnote'    => 'footnote',
      'math'        => 'mathphrase',
);

# this weird construct does like uniq, it avoids duplicates.
# it may be required since math is not in the %style_commands as it is 
# in context command.
my @all_style_commands = keys %{{ map { $_ => 1 }
    (keys(%Texinfo::Common::style_commands), keys(%style_attribute_commands),
     'w', 'dmn', 'titlefont') }};
# 'w' is special
my $w_command_mark = '<!-- /@w -->';

my %style_commands_formatting;
foreach my $command(@all_style_commands) {
  $style_commands_formatting{$command} = {};
  if ($style_attribute_commands{$command}) {
    $style_commands_formatting{$command}->{'attribute'} 
      = $style_attribute_commands{$command};
  }
  if ($quoted_style_commands{$command}) {
    $style_commands_formatting{$command}->{'quote'} = 1;
  }
}

my %docbook_misc_elements_with_arg_map = (
  'settitle' => 'title',
  'exdent' => 'simpara',
  'center' => '',
);

my %docbook_misc_commands = %Texinfo::Common::misc_commands;

foreach my $command ('item', 'headitem', 'itemx', 'tab', 
                      keys %Texinfo::Common::def_commands) {
  delete $docbook_misc_commands{$command};
}

my %docbook_global_commands = (
  'documentlanguage' => 1,
  'documentencoding' => 1,
);

sub converter_global_commands($)
{
  return keys(%docbook_global_commands);
}

my %default_args_code_style 
  = %Texinfo::Convert::Converter::default_args_code_style;
my %regular_font_style_commands = %Texinfo::Common::regular_font_style_commands;

my %defcommand_name_type = (
 'deffn'     => 'function',
 'defvr'     => 'varname',
 'deftypefn' => 'function',
 'deftypeop' => 'methodname',
 'deftypevr' => 'varname',
 'defcv'     => 'property',
 'deftypecv' => 'property',
 'defop'     => 'methodname',
 'deftp'     => 'structname',
);

my %def_argument_types_docbook = (
  'type' => ['returnvalue'],
  'class' => ['ooclass', 'classname'],
  'arg' => ['replaceable'],
  'typearg' => ['type'],
);


my %ignored_types;
foreach my $type ('empty_line_after_command',
            'preamble',
            'preamble_before_setfilename',
            'empty_spaces_after_command', 
            'spaces_at_end',
            'empty_spaces_before_argument', 'empty_spaces_before_paragraph',
            'empty_spaces_after_close_brace', 
            'empty_space_at_end_def_bracketed',
            'menu_entry_separator',
            'menu_entry_leading_text',
  ) {
  $ignored_types{$type} = 1;
}

my %type_elements = (
  'paragraph' => 'para',
  'table_item' => 'listitem',
  'table_entry' => 'varlistentry',
  'row' => 'row',
  'multitable_head' => 'thead',
  'multitable_body' => 'tbody',
  # Unfortunatly there does not seem to be anything better in DocBook.
  'def_item' => 'blockquote',
);

my %default_context_block_commands = (
  'float' => 1,
);

my %docbook_preformatted_formats = (
# command
   'example' => 'screen',
   'smallexample' => 'screen',
   'display' => 'literallayout',
   'smalldisplay' => 'literallayout',
   'lisp' => 'programlisting',
   'smalllisp' => 'programlisting',
   'format' => 'abstract',
   'smallformat' => 'screen',
# type
   'menu_comment' => 'literallayout',
   'menu_description' => 'literallayout',
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
}

sub convert($$;$)
{
  my $self = shift;
  my $root = shift;
  my $fh = shift;

  $self->_set_global_multiple_commands(-1);
  return $self->convert_document_sections($root, $fh);
}

sub convert_tree($$)
{
  my $self = shift;
  my $root = shift;

  return $self->_convert($root);
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

  $self->_set_global_multiple_commands(-1);

  my $encoding = '';
  if ($self->get_conf('OUTPUT_ENCODING_NAME') 
      and $self->get_conf('OUTPUT_ENCODING_NAME') ne 'utf-8') {
    $encoding = " encoding=\"".$self->get_conf('OUTPUT_ENCODING_NAME')."\" ";
  }

  my $id;
  if ($self->{'output_file'} ne '') {
    my $output_filename = $self->{'output_filename'};
    $id = " id=\"".$self->xml_protect_text($output_filename)."\"";
  } else {
    $id = '';
  }

  my $header =  "<?xml version=\"1.0\"${encoding}?>".'
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.2//EN" "http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd" [
  <!ENTITY tex "TeX">
  <!ENTITY latex "LaTeX">
]>
'. "<book${id} lang=\"".$self->get_conf('documentlanguage') ."\">\n";

  my $result = '';
  $result .= $self->_output_text($header, $fh);
  $result .= $self->convert_document_sections($root, $fh);
  $result .= $self->_output_text("</book>\n", $fh);
  if ($fh and $self->{'output_file'} ne '-') {
    $self->register_close_file($self->{'output_file'});
    if (!close ($fh)) {
      $self->document_error(sprintf($self->__("error on closing %s: %s"),
                                    $self->{'output_file'}, $!));
    }
  }
  return $result;
}

my %docbook_sections = (
  'top'  => 'chapter',
  'part' => 'part',
  'chapter'  => 'chapter',
  'unnumbered'  => 'chapter',
  'centerchap'  => 'chapter',
  'appendix' => 'appendix',
  'majorheading' => 'other',
  'chapheading' => 'other',
  'heading' => 'sect1',
  'subheading' => 'sect2',
  'subsubheading' => 'sect3',
  2 => 'sect1',
  3 => 'sect2',
  4 => 'sect3'
);

my %docbook_special_unnumbered;
foreach my $special_unnumbered ('acknowledgements', 'colophon', 
                                'dedication', 'preface') {
  $docbook_special_unnumbered{$special_unnumbered} = 1;
}

sub _docbook_section_element($$)
{
  my $self = shift;
  my $root = shift;
  my $heading_level = $root->{'level'};
  if (exists $docbook_sections{$heading_level}) {
    return $docbook_sections{$heading_level};
  }
  my $command = $self->_level_corrected_section($root);
  if ($command eq 'unnumbered'
      and $root->{'extra'}->{'associated_node'} 
      and $root->{'extra'}->{'associated_node'}->{'extra'}->{'normalized'}
      and $docbook_special_unnumbered{lc($root->{'extra'}->{'associated_node'}->{'extra'}->{'normalized'})}) {
    return lc($root->{'extra'}->{'associated_node'}->{'extra'}->{'normalized'});
  }

  return $docbook_sections{$command};
}

sub _index_entry($$)
{
  my $self = shift;
  my $root = shift;
  if ($root->{'extra'} and $root->{'extra'}->{'index_entry'}) {
    my $index_entry = $root->{'extra'}->{'index_entry'};
    # FIXME DocBook 5 role->type
    my $result = "<indexterm role=\"$index_entry->{'index_name'}\"><primary>";
    push @{$self->{'document_context'}}, {'monospace' => [0]};
    $self->{'document_context'}->[-1]->{'monospace'}->[-1] = 1
      if ($index_entry->{'in_code'});
    $result .= $self->_convert({'contents' => $index_entry->{'content'}});
    pop @{$self->{'document_context'}};
    return $result ."</primary></indexterm>"
  }
  return '';
}

sub docbook_accent($$$;$)
{
  my $self = shift;
  my $text = shift;
  my $command = shift;
  my $in_upper_case = shift;
  my $accent = $command->{'cmdname'};

  if ($in_upper_case and $text =~ /^\w$/) {
    $text = uc ($text);
  }
  if (exists($Texinfo::Convert::Unicode::unicode_accented_letters{$accent})
      and exists($Texinfo::Convert::Unicode::unicode_accented_letters{$accent}->{$text})) {
    return '&#' .
      hex($Texinfo::Convert::Unicode::unicode_accented_letters{$accent}->{$text}). ';';
  }
  # FIXME it is not possible to call xml_protect_text since what is in $text
  # may already be xml.  But this means that each time ascii_accent changes
  # it should be changed here too.
  return $text . '&lt;' if ($accent eq 'v');
  return Texinfo::Convert::Text::ascii_accent($text, $command);
}

sub _parse_attribute($)
{
  my $element = shift;
  return ('', '') if (!defined($element));
  my $attributes = '';
  if ($element =~ /^(\w+)(\s+.*)/)
  {
    $element = $1;
    $attributes = $2;
  }
  return ($element, $attributes);
}

sub _protect_text($$)
{
  my $self = shift;
  my $text = shift;
  my $result = $self->xml_protect_text($text);
  # form feed not allowed in XML
  $result =~ s/\f/ /g;
  return $result;
}

sub _convert($$;$);

sub _convert($$;$)
{
  my $self = shift;
  my $root = shift;

  if (0) {
  #if (1) {
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
    if (defined($root->{'type'}) and $root->{'type'} eq '_converted') {
      return $root->{'text'};
    } elsif ($self->{'document_context'}->[-1]->{'raw'}) {
      return $root->{'text'};
    }
    $result = $self->_protect_text($root->{'text'});
    if (! defined($root->{'type'}) or $root->{'type'} ne 'raw') {
      if (!$self->{'document_context'}->[-1]->{'monospace'}->[-1]) {
        $result =~ s/``/$ldquo/g;
        $result =~ s/\'\'/$rdquo/g;
        $result =~ s/`/$lsquo/g;
        $result =~ s/\'/$rsquo/g;
        $result =~ s/---/$mdash/g;
        $result =~ s/--/$ndash/g;
      }
    }
    return $result;
  }
  my @close_elements;
  if ($root->{'cmdname'}) {
    if (defined($docbook_commands_formatting{$root->{'cmdname'}})) {
      my $command;
      if ($root->{'cmdname'} eq 'click' 
          and $root->{'extra'} 
          and defined($root->{'extra'}->{'clickstyle'})) {
        $command = $root->{'extra'}->{'clickstyle'};
      } else {
        $command = $root->{'cmdname'};
      }
      if ($self->{'translated_commands'}->{$command}) {
        return $self->_convert(Texinfo::Common::translated_command_tree($self,
                                                                   $command));
      } else {
        return $docbook_commands_formatting{$command};
      }
    } elsif ($root->{'cmdname'} eq 'today') {
      return $self->_convert(Texinfo::Common::expand_today($self));
    } elsif ($Texinfo::Common::accent_commands{$root->{'cmdname'}}) {
      return $self->convert_accents($root, \&docbook_accent);
    } elsif ($root->{'cmdname'} eq 'item' or $root->{'cmdname'} eq 'itemx'
             or $root->{'cmdname'} eq 'headitem' or $root->{'cmdname'} eq 'tab') {
      if ($root->{'cmdname'} eq 'item'
          and $root->{'parent'}->{'cmdname'}
          and ($root->{'parent'}->{'cmdname'} eq 'itemize'
               or $root->{'parent'}->{'cmdname'} eq 'enumerate')) {
        $result .= "<listitem>";
        if ($root->{'parent'}->{'cmdname'} eq 'itemize'
            and $root->{'parent'}->{'extra'}
            and !($root->{'parent'}->{'extra'}->{'command_as_argument'}
                  and $root->{'parent'}->{'extra'}->{'command_as_argument'}->{'cmdname'} eq 'bullet')
            and $root->{'parent'}->{'extra'}->{'block_command_line_contents'}
            and $root->{'parent'}->{'extra'}->{'block_command_line_contents'}->[0]) {
       #   $result .= $self->_convert({'contents'
       # => $root->{'parent'}->{'extra'}->{'block_command_line_contents'}->[0]})
       #     ." ";
          $self->{'pending_prepend'} = $self->_convert({'contents'
       => $root->{'parent'}->{'extra'}->{'block_command_line_contents'}->[0]}) ." ";
        }
        push @close_elements, 'listitem';
      } elsif (($root->{'cmdname'} eq 'item' or $root->{'cmdname'} eq 'itemx')
               and $root->{'parent'}->{'type'} 
               and $root->{'parent'}->{'type'} eq 'table_term') {

        my $converted_tree = $self->_table_item_content_tree($root,
                                         $root->{'extra'}->{'misc_content'});

        $result .= "<term>";
        $result .= $self->_index_entry($root);
        $result .= $self->_convert($converted_tree);
        chomp ($result);
        $result .= "\n";
        $result .= "</term>";
      } else {
        unless (($root->{'cmdname'} eq 'item' 
                     or $root->{'cmdname'} eq 'headitem'
                     or $root->{'cmdname'} eq 'tab')
                    and $root->{'parent'}->{'type'}
                    and $root->{'parent'}->{'type'} eq 'row') {
          print STDERR "BUG: multitable cell command not in a row "
            .Texinfo::Parser::_print_current($root);
        }
        
        $result .= "<entry>";
        push @close_elements, 'entry';
      }
    } elsif ($root->{'type'} and $root->{'type'} eq 'index_entry_command') {
      my $end_line;
      if ($root->{'args'}->[0]) {
        $end_line = $self->_end_line_or_comment($root->{'args'}->[0]->{'contents'});
        if ($self->{'document_context'}->[-1]->{'in_preformatted'}) {
          chomp($end_line);
        }
      } else {
        # May that happen?
        $end_line = '';
      }
      return $self->_index_entry($root).${end_line};
    } elsif (exists($docbook_misc_commands{$root->{'cmdname'}})) {
      if ($docbook_global_commands{$root->{'cmdname'}}) {
        $self->_informative_command($root);
        return '';
      }
      my $command;
      if (exists ($docbook_misc_elements_with_arg_map{$root->{'cmdname'}})) {
        $command = $docbook_misc_elements_with_arg_map{$root->{'cmdname'}};
      }
      my $type = $docbook_misc_commands{$root->{'cmdname'}};
      if ($type eq 'text') {
        if ($root->{'cmdname'} eq 'verbatiminclude') {
          my $verbatim_include_verbatim
            = Texinfo::Common::expand_verbatiminclude($self, $root);
          if (defined($verbatim_include_verbatim)) {
            $result .= $self->_convert($verbatim_include_verbatim);
          } else {
            return '';
          }
        } else {
          return '';
        }
      } elsif ($type eq 'line') {
        if ($root->{'cmdname'} eq 'node') {
          if ($root->{'extra'} and !$root->{'extra'}->{'associated_section'}
              and defined($root->{'extra'}->{'normalized'})) {
            $result .= "<anchor id=\"$root->{'extra'}->{'normalized'}\"/>\n";
          }
        } elsif ($Texinfo::Common::root_commands{$root->{'cmdname'}}) {
          my $attribute = '';
          # FIXME it is not clear that a label should be set for
          # @appendix* or @chapter/@*section as the formatter should be
          # able to figure it out.  For @unnumbered or if ! NUMBER_SECTIONS
          # having a label (empty) is important.
          my $label = '';
          if (defined($root->{'number'})
            and ($self->get_conf('NUMBER_SECTIONS')
                 or !defined($self->get_conf('NUMBER_SECTIONS')))) {
            # Looking at docbook2html output, Appendix is appended in the 
            # section title, so only the letter is used.
            $label = $root->{'number'};
          }
          $command = $self->_docbook_section_element($root);
          if (! $docbook_special_unnumbered{$command}) {
            $attribute = " label=\"$label\"";
          }
          if ($root->{'extra'} and $root->{'extra'}->{'associated_node'}) {
            $attribute .= " id=\"$root->{'extra'}->{'associated_node'}->{'extra'}->{'normalized'}\"";
          }
          $result .= "<$command${attribute}>\n";
          if ($root->{'args'} and $root->{'args'}->[0]) {
            my ($arg, $end_line)
              = $self->_convert_argument_and_end_line($root->{'args'}->[0]);
            $result .= "<title>$arg</title>$end_line";
            chomp ($result);
            $result .= "\n";
          }
        } elsif ($Texinfo::Common::sectioning_commands{$root->{'cmdname'}}) {
          if ($root->{'args'} and $root->{'args'}->[0]) {
            my ($arg, $end_line)
              = $self->_convert_argument_and_end_line($root->{'args'}->[0]);
            $result .= 
              "<bridgehead renderas=\"$docbook_sections{$root->{'cmdname'}}\">$arg</bridgehead>$end_line";
            chomp ($result);
            $result .= "\n";
            return $result;
          }
          return '';
        } else {
          my $attribute = '';
          if (defined($command)) {
            my ($arg, $end_line)
              = $self->_convert_argument_and_end_line($root->{'args'}->[0]);
            if ($command eq '') {
              $result .= "$arg$end_line";
            } else {
              $result .= "<$command${attribute}>$arg</$command>$end_line";
            }
            chomp ($result);
            $result .= "\n";
            return $result;
          }
          return '';
        }
      } elsif ($type eq 'skipline' or $type eq 'noarg') {
        if ($root->{'cmdname'} eq 'insertcopying') {
          if ($self->{'extra'} and $self->{'extra'}->{'copying'}) {
            return $self->_convert({'contents'
               => $self->{'extra'}->{'copying'}->{'contents'}});
          } else {
            return '';
          }
        } else {
          return '';
        }
      } elsif ($type eq 'special' or $type eq 'skipspace') {
        return '';
      } elsif ($type eq 'lineraw') {
        if ($root->{'cmdname'} eq 'c' or $root->{'cmdname'} eq 'comment') {
          return $self->xml_comment($root->{'args'}->[0]->{'text'})
        } else {
          return "";
        }
      } else {
        $self->_bug_message("unknown misc_command style $type", $root)
          if ($type !~ /^\d$/);
        if ($root->{'cmdname'} eq 'printindex') {
          if (defined($root->{'extra'})
              and defined($root->{'extra'}->{'misc_args'})) {
            # FIXME DocBook 5
            #return "<index type=\"$root->{'extra'}->{'misc_args'}->[0]\"></index>\n";
            return "<index role=\"$root->{'extra'}->{'misc_args'}->[0]\"></index>\n";
          } else {
            return "<index></index>\n";
          }
        } else {
          return '';
        }
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
      $result .= $self->xml_protect_text($root->{'extra'}->{'begin'}).$arg
                .$self->xml_protect_text($root->{'extra'}->{'end'});
      pop @{$self->{'document_context'}->[-1]->{'monospace'}}
        if (defined($in_monospace_not_normal));
    } elsif ($root->{'args'}
             and exists($Texinfo::Common::brace_commands{$root->{'cmdname'}})) {
      if ($style_commands_formatting{$root->{'cmdname'}}) {
        if ($Texinfo::Common::context_brace_commands{$root->{'cmdname'}}) {
          push @{$self->{'document_context'}}, {'monospace' => [0]};
        }
        my $formatting = $style_commands_formatting{$root->{'cmdname'}};

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

        my ($style, $attribute_text) = _parse_attribute($formatting->{'attribute'});
        my $result = $self->_convert($root->{'args'}->[0]);
        if ($style ne '' and (!$self->{'document_context'}->[-1]->{'inline'}
                               or $inline_elements{$style})) {
          $result = "<$style${attribute_text}>$result</$style>";
          if ($root->{'cmdname'} eq 'math') {
            $result = "<inlineequation>$result</inlineequation>";
          }
        }
        if (defined($formatting->{'quote'})) {
          $result = $self->get_conf('OPEN_QUOTE_SYMBOL') . $result
                   . $self->get_conf('CLOSE_QUOTE_SYMBOL');
        }
        pop @{$self->{'document_context'}->[-1]->{'monospace'}}
          if (defined($in_monospace_not_normal));
        if ($Texinfo::Common::context_brace_commands{$root->{'cmdname'}}) {
          pop @{$self->{'document_context'}};
        }
        if ($root->{'cmdname'} eq 'w') {
          $result .= $w_command_mark;
        }
        return $result;
      } elsif ($root->{'cmdname'} eq 'anchor') {
        if ($root->{'extra'} and defined($root->{'extra'}->{'normalized'})) {
          return "<anchor id=\"$root->{'extra'}->{'normalized'}\"/>";
        } else {
          return '';
        }
      } elsif ($Texinfo::Common::ref_commands{$root->{'cmdname'}}) {
        if ($root->{'extra'} and $root->{'extra'}->{'brace_command_contents'}) {
          if ($root->{'cmdname'} eq 'inforef') {
            my $filename;
            if (scalar (@{$root->{'extra'}->{'brace_command_contents'}}) == 3
                and defined($root->{'extra'}->{'brace_command_contents'}->[-1])) {
              $filename 
                = $self->xml_protect_text(Texinfo::Convert::Text::convert(
              {'contents' => $root->{'extra'}->{'brace_command_contents'}->[-1]},
              {'code' => 1, Texinfo::Common::_convert_text_options($self)}));
            }
            my $node;
            if (defined($root->{'extra'}->{'brace_command_contents'}->[0])) {
              $node = {'contents' 
                        => $root->{'extra'}->{'brace_command_contents'}->[0]};
            }
            if ($node and defined($filename)) {
              return $self->_convert($self->gdt(
                   "See Info file \@file{{myfile}}, node \@samp{{mynode}}",
                   { 'myfile' => {'type' => '_converted', 'text' => $filename},
                     'mynode' => $node}));
            } elsif ($node) {
              return $self->_convert($self->gdt(
                   "See node \@samp{{mynode}}",
                   {'mynode' => $node}));
            } elsif (defined($filename)) {
              return $self->_convert($self->gdt(
                   "See Info file \@file{{myfile}}",
                   { 'myfile' => {'type' => '_converted', 'text' => $filename}}));
            }
            #my $name;
            #if (scalar (@{$root->{'extra'}->{'brace_command_contents'}}) >= 2
            #    and defined($root->{'extra'}->{'brace_command_contents'}->[1])) {
            #  $name = $self->_convert({'contents' 
            #       => $root->{'extra'}->{'brace_command_contents'}->[0]});
            #}
          } else {
            my $book_contents;
            if (scalar (@{$root->{'extra'}->{'brace_command_contents'}}) == 5
                and defined($root->{'extra'}->{'brace_command_contents'}->[-1])) {
              $book_contents = $root->{'extra'}->{'brace_command_contents'}->[-1];
            }
            my $manual_file_contents;
            if (scalar (@{$root->{'extra'}->{'brace_command_contents'}}) >= 4
                and defined($root->{'extra'}->{'brace_command_contents'}->[3])) {
              $manual_file_contents = $root->{'extra'}->{'brace_command_contents'}->[3];
            }
            my $section_name_contents;
            if (defined($root->{'extra'}->{'brace_command_contents'}->[2])) {
              $section_name_contents 
                = $root->{'extra'}->{'brace_command_contents'}->[2];
            } elsif (defined($root->{'extra'}->{'brace_command_contents'}->[1])) {
              $section_name_contents
                = $root->{'extra'}->{'brace_command_contents'}->[1];
            } elsif (defined($root->{'extra'}->{'brace_command_contents'}->[0])
                     and (!$book_contents 
                          or $root->{'extra'}->{'node_argument'}->{'manual_content'}
                          or $root->{'extra'}->{'node_argument'}->{'normalized'} ne 'Top')) {
              $section_name_contents
                = $root->{'extra'}->{'brace_command_contents'}->[0];
            }
            # external ref
            if ($book_contents or $manual_file_contents) {
              return '' if (!$book_contents);
              if ($section_name_contents) {
                if ($root->{'cmdname'} eq 'ref') {
                  return $self->_convert(
                    $self->gdt('section ``{section_name}\'\' in @cite{{book}}',
                      { 'section_name' => {'contents' => $section_name_contents},
                        'book' => $book_contents }));
                } elsif ($root->{'cmdname'} eq 'xref') {
                  return $self->_convert(
                    $self->gdt('See section ``{section_name}\'\' in @cite{{book}}',
                      { 'section_name' => {'contents' => $section_name_contents},
                        'book' => $book_contents }));
                } elsif ($root->{'cmdname'} eq 'pxref') {
                  return $self->_convert(
                    $self->gdt('see section ``{section_name}\'\' in @cite{{book}}',
                      { 'section_name' => {'contents' => $section_name_contents},
                        'book' => $book_contents }));
                }
              } else {
                if ($root->{'cmdname'} eq 'ref') {
                  return $self->_convert(
                    $self->gdt('@cite{{book}}',
                      {'book' => $book_contents }));
                } elsif ($root->{'cmdname'} eq 'xref') {
                  return $self->_convert(
                    $self->gdt('See @cite{{book}}',
                      {'book' => $book_contents }));
                } elsif ($root->{'cmdname'} eq 'pxref') {
                  return $self->_convert(
                    $self->gdt('see @cite{{book}}',
                      {'book' => $book_contents }));
                }
              }
            } else {
              my $linkend = '';
              if ($root->{'extra'}->{'node_argument'}
                  and defined($root->{'extra'}->{'node_argument'}->{'normalized'})
                  and !$root->{'extra'}->{'node_argument'}->{'manual_content'}) {
                $linkend = " linkend=\"$root->{'extra'}->{'node_argument'}->{'normalized'}\"";
              }
              my $argument = "<link${linkend}>".$self->_convert({'contents' => 
                        $section_name_contents}) ."</link>";
              if ($root->{'cmdname'} eq 'ref') {
                return $self->_convert(
                        $self->gdt('{title_ref}', {'title_ref' => 
                             {'type' => '_converted',
                              'text' => $argument}}));
              } elsif ($root->{'cmdname'} eq 'xref') {
                return $self->_convert(
                        $self->gdt('See {title_ref}', {'title_ref' =>
                             {'type' => '_converted',
                              'text' => $argument}}));
              } elsif ($root->{'cmdname'} eq 'pxref') {
                return $self->_convert(
                        $self->gdt('see {title_ref}', {'title_ref' =>
                             {'type' => '_converted',
                              'text' => $argument}}));
              }
            }
          }
        } else {
          return '';
        }
      } elsif ($root->{'cmdname'} eq 'image') {
        if (defined($root->{'extra'}->{'brace_command_contents'}->[0])) {
          my $basefile = Texinfo::Convert::Text::convert(
           {'contents' => $root->{'extra'}->{'brace_command_contents'}->[0]},
           {'code' => 1, Texinfo::Common::_convert_text_options($self)});
          my $element;
          my $is_inline = $self->_is_inline($root);
          if ($is_inline) {
            $result .= "<inlinemediaobject>";
          } else {
            $result .= "<informalfigure><mediaobject>";
          }
          my @files;
          foreach my $extension (@docbook_image_extensions) {
            if ($self->Texinfo::Common::locate_include_file ("$basefile.$extension")) {
              push @files, ["$basefile.$extension", uc($extension)];
            }
          }
          my $image_file_found = scalar(@files);;
          if (!$image_file_found) {
            push @files, ["$basefile.jpg", 'JPG'];
          }
          foreach my $file (@files) {
            $result .= "<imageobject><imagedata fileref=\""
               .$self->xml_protect_text($file->[0])
               ."\" format=\"$file->[1]\"></imagedata></imageobject>";
          }
          my ($image_text, $image_width)
            = $self->Texinfo::Convert::Plaintext::_image_text($root, $basefile);
          if (defined($image_text)) {
            $result .= "<textobject><literallayout>"
               .$self->_protect_text($image_text)
               .'</literallayout></textobject>';
          }
          if (!defined($image_text) and !$image_file_found) {
            $self->line_warn(sprintf(
                     $self->__("\@image file `%s' not found, using `%s'"), 
                       $basefile, "$basefile.jpg"), $root->{'line_nr'});
          }

          if ($is_inline) {
            $result .= "</inlinemediaobject>";
          } else {
            $result .= "</mediaobject></informalfigure>";
          }
        }
      } elsif ($root->{'cmdname'} eq 'email') {
        if ($root->{'extra'} and $root->{'extra'}->{'brace_command_contents'}) {
          my $name;
          my $email;
          my $email_text;
          if (scalar (@{$root->{'extra'}->{'brace_command_contents'}}) == 2
              and defined($root->{'extra'}->{'brace_command_contents'}->[-1])) {
            $name = $root->{'extra'}->{'brace_command_contents'}->[1];
          }
          if (defined($root->{'extra'}->{'brace_command_contents'}->[0])) {
            $email = $root->{'extra'}->{'brace_command_contents'}->[0];
            $email_text 
              = $self->_protect_text(Texinfo::Convert::Text::convert(
                                         {'contents' => $email},
                                         {'code' => 1,
                                  Texinfo::Common::_convert_text_options($self)}));
          }
          if ($name and $email) {
            return "<ulink url=\"mailto:$email_text\">"
              .$self->_convert({'contents' => $name}).'</ulink>';
          } elsif ($email) {
            return "<email>$email_text</email>";
          } elsif ($name) {
            return $self->_convert({'contents' => $name});
          }
        } else {
          return '';
        }
      } elsif ($root->{'cmdname'} eq 'uref' or $root->{'cmdname'} eq 'url') {
        if ($root->{'extra'} and $root->{'extra'}->{'brace_command_contents'}) {
          my ($url_text, $url_content);
          if (defined($root->{'extra'}->{'brace_command_contents'}->[0])) {
            $url_content = $root->{'extra'}->{'brace_command_contents'}->[0];
            $url_text = $self->_protect_text(Texinfo::Convert::Text::convert(
                                         {'contents' => $url_content},
                                         {'code' => 1,
                                  Texinfo::Common::_convert_text_options($self)}));
          } else {
            $url_text = '';
          }
          my $replacement;
          if (scalar(@{$root->{'extra'}->{'brace_command_contents'}}) >= 2 
              and defined($root->{'extra'}->{'brace_command_contents'}->[1])) {
            $replacement = $self->_convert({'contents' 
                      => $root->{'extra'}->{'brace_command_contents'}->[1]});
          }
          if (!defined($replacement) or $replacement eq '') {
            if (scalar(@{$root->{'extra'}->{'brace_command_contents'}}) == 3
                and defined($root->{'extra'}->{'brace_command_contents'}->[2])) {
              $replacement = $self->_convert({'contents' 
                      => $root->{'extra'}->{'brace_command_contents'}->[2]});
            }
          }
          if (!defined($replacement) or $replacement eq '') {
            $replacement = $url_text;
          }
          return "<ulink url=\"$url_text\">$replacement</ulink>";
          # DocBook 5
          # return "<link xl:href=\"$url_text\">$replacement</link>";
        }
      } elsif ($root->{'cmdname'} eq 'abbr' or $root->{'cmdname'} eq 'acronym') {
        my $argument;
        if (scalar (@{$root->{'extra'}->{'brace_command_contents'}}) >= 1
            and defined($root->{'extra'}->{'brace_command_contents'}->[0])) {
          my $arg = $self->_convert({'contents' 
                      => $root->{'extra'}->{'brace_command_contents'}->[0]});
          if ($arg ne '') {
            my $element;
            if ($root->{'cmdname'} eq 'abbr') {
              $element = 'abbrev';
            } else {
              $element = $root->{'cmdname'};
            }
            $argument = "<$element>$arg</$element>";
          }
        }
        
        if (scalar (@{$root->{'extra'}->{'brace_command_contents'}}) == 2
           and defined($root->{'extra'}->{'brace_command_contents'}->[-1])) {
          if (defined($argument)) {
            my $tree = $self->gdt('{abbr_or_acronym} ({explanation})',
                           {'abbr_or_acronym' => {'type' => '_converted',
                                                  'text' => $argument},
                            'explanation' =>
                             $root->{'extra'}->{'brace_command_contents'}->[-1]});
            return $self->_convert($tree);
          } else {
            return $self->_convert({'contents' 
                    => $root->{'extra'}->{'brace_command_contents'}->[-1]});
          }
        } elsif (defined($argument)) {
          return $argument;
        } else {
          return '';
        }
      } elsif ($Texinfo::Common::inline_commands{$root->{'cmdname'}}) {
        my $expand = 0;
        if ($Texinfo::Common::inline_format_commands{$root->{'cmdname'}}) {
          if ($root->{'cmdname'} eq 'inlinefmtifelse'
              or ($root->{'extra'} and $root->{'extra'}->{'format'}
                  and $self->{'expanded_formats_hash'}->{$root->{'extra'}->{'format'}})) {
            $expand = 1;
          }
        } elsif (defined($root->{'extra'}->{'expand_index'})) {
          $expand = 1;
        }
        return '' if (! $expand);
        my $arg_index = 1;
        if ($root->{'cmdname'} eq 'inlineraw') {
          push @{$self->{'document_context'}}, {'monospace' => [0]};
          $self->{'document_context'}->[-1]->{'raw'} = 1;
        } elsif ($root->{'cmdname'} eq 'inlinefmtifelse' 
                 and ! $self->{'expanded_formats_hash'}->{$root->{'extra'}->{'format'}}) {
          $arg_index = 2;
        }
        if (scalar (@{$root->{'extra'}->{'brace_command_contents'}}) > $arg_index
            and defined($root->{'extra'}->{'brace_command_contents'}->[$arg_index])) {
          $result .= $self->_convert({'contents'
                        => $root->{'extra'}->{'brace_command_contents'}->[$arg_index]});
        }
        if ($root->{'cmdname'} eq 'inlineraw') {
          pop @{$self->{'document_context'}};
        }
        return $result;
      } else {
        # ignored brace command
        return '';
      }
    # special case to ensure that @w leads to something even if empty
    } elsif ($root->{'cmdname'} eq 'w') {
      return $w_command_mark;
    } elsif (exists($Texinfo::Common::block_commands{$root->{'cmdname'}})) {
      if ($self->{'context_block_commands'}->{$root->{'cmdname'}}) {
        push @{$self->{'document_context'}}, {'monospace' => [0]};
      }
      my @attributes;
      my $appended = '';
      my @elements;
      if (exists($docbook_preformatted_formats{$root->{'cmdname'}})) {
        push @{$self->{'document_context'}->[-1]->{'preformatted_stack'}}, 
           $docbook_preformatted_formats{$root->{'cmdname'}};
      } elsif ($root->{'cmdname'} eq 'enumerate') {
        push @elements, 'orderedlist'; 
        my $numeration;
        if ($root->{'extra'}
               and $root->{'extra'}->{'enumerate_specification'}) {
          if ($root->{'extra'}->{'enumerate_specification'} =~ /^[A-Z]/) {
            $numeration = 'upperalpha';
          } elsif ($root->{'extra'}->{'enumerate_specification'} =~ /^[a-z]/) {
            $numeration = 'loweralpha';
          } else {
            $numeration = 'arabic';
          }
        } else {
          $numeration = 'arabic';
        }
        push @attributes, " numeration=\"$numeration\"";
      } elsif ($Texinfo::Common::item_line_commands{$root->{'cmdname'}}) {
        push @elements, 'variablelist';
      } elsif ($root->{'cmdname'} eq 'itemize') {
        push @elements, 'itemizedlist';
        #push @attributes, " mark=\"\"";
      } elsif ($root->{'cmdname'} eq 'multitable') {
        push @elements, "informaltable";
        push @attributes, '';
        my $columns_count;
        if ($root->{'extra'} and defined($root->{'extra'}->{'max_columns'})) {
          $columns_count = $root->{'extra'}->{'max_columns'};
        } else {
          $columns_count = 0;
        }
        push @elements, 'tgroup';
        push @attributes, " cols=\"$columns_count\"";
        if ($root->{'extra'}) {
          my @fractions;
          my $multiply;
          if ($root->{'extra'}->{'prototypes'}) {
            $multiply = 1;
            foreach my $prototype (@{$root->{'extra'}->{'prototypes'}}) {
              my $prototype_text
                = Texinfo::Convert::Text::convert($prototype,
                               {Texinfo::Common::_convert_text_options($self)});
              push @fractions, 
                Texinfo::Convert::Unicode::string_width($prototype_text);
            }
          } elsif ($root->{'extra'}->{'columnfractions'}) {
            @fractions = @{$root->{'extra'}->{'columnfractions'}};
            $multiply = 100;
          }
          foreach my $fraction (@fractions) {
            $appended .= '<colspec colwidth="'.($fraction*$multiply)
                         .'*"></colspec>';
          }
        }
      } elsif ($root->{'cmdname'} eq 'float') {
        if ($root->{'extra'} and defined($root->{'extra'}->{'normalized'})) {
          $result .= "<anchor id=\"$root->{'extra'}->{'normalized'}\"/>\n";
        }
      } elsif ($root->{'cmdname'} eq 'verbatim') {
        push @elements, 'screen';
      } elsif ($root->{'cmdname'} eq 'quotation' 
               or $root->{'cmdname'} eq 'smallquotation') {
        my $element;
        if ($root->{'extra'}) {
          if ($root->{'extra'}->{'authors'}) {
            foreach my $author (@{$root->{'extra'}->{'authors'}}) {
              if ($author->{'extra'} and $author->{'extra'}->{'misc_content'}) {
                $appended .= '<attribution>'.$self->_convert(
                  {'contents' => $author->{'extra'}->{'misc_content'}})
                           ."</attribution>\n";
              }
            }
          }
          if ($root->{'extra'}->{'block_command_line_contents'}
              and defined($root->{'extra'}->{'block_command_line_contents'}->[0])) {
            my $quotation_arg_text = Texinfo::Convert::Text::convert(
                     {'contents' => $root->{'extra'}->{'block_command_line_contents'}->[0]},
                     {Texinfo::Common::_convert_text_options($self)});
            if ($docbook_special_quotations{lc($quotation_arg_text)}) {
              $element = lc($quotation_arg_text);
            } else {
              $self->{'pending_prepend'} 
                = $self->_convert($self->gdt('@b{{quotation_arg}:} ',
                              {'quotation_arg' =>
                    $root->{'extra'}->{'block_command_line_contents'}->[0]}));
            }
          }
        }
        $element = 'blockquote' if (!defined($element));
        push @elements, $element;
      } elsif ($root->{'cmdname'} eq 'copying') {
        push @elements, ('bookinfo', 'legalnotice');
      } elsif ($Texinfo::Common::format_raw_commands{$root->{'cmdname'}}) {
        return '' if (!$self->{'expanded_formats_hash'}->{$root->{'cmdname'}});
        # the context is here only for the command, so this is forgotten
        # once all the raw internal text has been formatted
        $self->{'document_context'}->[-1]->{'raw'} = 1;
      } elsif ($Texinfo::Common::block_commands{$root->{'cmdname'}} eq 'raw') {
        return '';
      } elsif ($Texinfo::Common::menu_commands{$root->{'cmdname'}}) {
        return '';
      }
      foreach my $element (@elements) {
        my $attribute = shift @attributes;
        $attribute = '' if (!defined($attribute));
        $result .= "<$element${attribute}>";
        unshift @close_elements, $element;
      }
      $result .= $appended if (defined($appended));
    }
  }
  if ($root->{'type'}) {
    if (exists($docbook_preformatted_formats{$root->{'type'}})) {
      push @{$self->{'document_context'}->[-1]->{'preformatted_stack'}}, 
         $docbook_preformatted_formats{$root->{'type'}};
    }
    if (defined($type_elements{$root->{'type'}})) {
      $result .= "<$type_elements{$root->{'type'}}>";
    } elsif ($root->{'type'} eq 'preformatted') {
      $result .= "<$self->{'document_context'}->[-1]->{'preformatted_stack'}->[-1]>";
      $self->{'document_context'}->[-1]->{'in_preformatted'} = 1;
    } elsif ($root->{'type'} eq 'def_line') {
      $result .= "<synopsis>";
      $result .= $self->_index_entry($root);
      push @{$self->{'document_context'}}, {'monospace' => [1]};
      $self->{'document_context'}->[-1]->{'inline'}++;
      if ($root->{'extra'} and $root->{'extra'}->{'def_args'}) {
        my $main_command;
        if ($Texinfo::Common::def_aliases{$root->{'extra'}->{'def_command'}}) {
          $main_command = $Texinfo::Common::def_aliases{$root->{'extra'}->{'def_command'}};
        } else {
          $main_command = $root->{'extra'}->{'def_command'};
        }
        foreach my $arg (@{$root->{'extra'}->{'def_args'}}) {
          my $type = $arg->[0];
          my $content = $self->_convert($arg->[1]);
          if ($type eq 'spaces' or $type eq 'delimiter') {
            $result .= $content;
          } elsif ($type eq 'category') {
            $result .= "<phrase role=\"category\"><emphasis role=\"bold\">$content</emphasis>:</phrase>";
          } elsif ($type eq 'name') {
            $result .= "<$defcommand_name_type{$main_command}>$content</$defcommand_name_type{$main_command}>";
          } else {
            if (!defined($def_argument_types_docbook{$type})) {
              print STDERR "BUG: no def_argument_types_docbook for $type\n";
              next;
            }
            foreach my $element (reverse (
                                   @{$def_argument_types_docbook{$type}})) {
              $content = "<$element>$content</$element>";
            }
            $result .= $content;
          }
        }
      }
      pop @{$self->{'document_context'}};
      $result .= "</synopsis>";
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
    if (defined($self->{'pending_prepend'}) and $self->_in_inline($root)) {
      $result .= $self->{'pending_prepend'};
      delete $self->{'pending_prepend'};
    }
    foreach my $content (@{$root->{'contents'}}) {
      $result .= $self->_convert($content);
    }
    pop @{$self->{'document_context'}->[-1]->{'monospace'}}
      if ($in_code);
  }
  if ($root->{'type'}) {
    if (defined($type_elements{$root->{'type'}})) {
      $result .= "</$type_elements{$root->{'type'}}>";
    } elsif ($root->{'type'} eq 'preformatted') {
      $result .= "</$self->{'document_context'}->[-1]->{'preformatted_stack'}->[-1]>";
      delete $self->{'document_context'}->[-1]->{'in_preformatted'};
    }
  }
  $result = '{'.$result.'}' 
     if ($root->{'type'} and $root->{'type'} eq 'bracketed'
         and (!$root->{'parent'}->{'type'} or
              ($root->{'parent'}->{'type'} ne 'block_line_arg'
               and $root->{'parent'}->{'type'} ne 'misc_line_arg')));
  foreach my $element (@close_elements) {
    $result .= "</$element>";
  }
  if ($root->{'cmdname'} 
      and exists($Texinfo::Common::block_commands{$root->{'cmdname'}})) {
    # a pending_prepend still there may happen if a quotation is empty.
    delete $self->{'pending_prepend'};
    #$result .= "</$root->{'cmdname'}>\n";
    if ($self->{'document_context'}->[-1]->{'raw'}) {
      chomp ($result);
      chomp ($result);
    } else {
      if (exists($docbook_preformatted_formats{$root->{'cmdname'}})) {
        my $format = pop @{$self->{'document_context'}->[-1]->{'preformatted_stack'}};
        die "BUG $format ne $docbook_preformatted_formats{$root->{'cmdname'}}"
         if ($format ne $docbook_preformatted_formats{$root->{'cmdname'}});
      }
    }
    if ($self->{'context_block_commands'}->{$root->{'cmdname'}}) {
      pop @{$self->{'document_context'}};
    }
  } elsif ($root->{'type'} and exists($docbook_preformatted_formats{$root->{'type'}})) {
    my $format = pop @{$self->{'document_context'}->[-1]->{'preformatted_stack'}};
    die "BUG $format ne $docbook_preformatted_formats{$root->{'type'}}"
      if ($format ne $docbook_preformatted_formats{$root->{'type'}});
  # The command is closed either when the corresponding tree element
  # is done, and the command is not associated to an element, or when
  # the element is closed.
  } elsif (($root->{'type'} and $root->{'type'} eq 'element'
            and $root->{'extra'} and $root->{'extra'}->{'element_command'})
           or ($root->{'cmdname'} 
               and $Texinfo::Common::root_commands{$root->{'cmdname'}}
               and $root->{'cmdname'} ne 'node'
               and !($root->{'parent'} and $root->{'parent'}->{'type'}
                     and $root->{'parent'}->{'type'} eq 'element'
                     and $root->{'parent'}->{'extra'} 
                     and $root->{'parent'}->{'extra'}->{'element_command'} eq $root))) {
    if ($root->{'type'} and $root->{'type'} eq 'element') {
      $root = $root->{'extra'}->{'element_command'};
    }
    my $command = $self->_docbook_section_element($root);
    my $command_texi = $self->_level_corrected_section($root);
    if (!($root->{'section_childs'} and scalar(@{$root->{'section_childs'}}))
        or $command_texi eq 'top') {
      $result .= "</$command>\n";
      my $current = $root;
      while ($current->{'section_up'}
             # the most up element is a virtual sectioning root element, this
             # condition avoids getting into it
             and $current->{'section_up'}->{'cmdname'}
             and !$current->{'section_next'}
             and $self->_level_corrected_section($current->{'section_up'}) ne 'top') {
        $current = $current->{'section_up'};
        $result .= '</'.$self->_docbook_section_element($current) .">\n";
      }
    }
  }
  return $result;
}

# figure: mandatory title->use it with shortcaption?. Has a caption. 

1;

__END__
# Automatically generated from maintain/template.pod

=head1 NAME

Texinfo::Convert::DocBook - Convert Texinfo tree to DocBook

=head1 SYNOPSIS

  my $converter 
    = Texinfo::Convert::DocBook->converter({'parser' => $parser});

  $converter->output($tree);

=head1 DESCRIPTION

Texinfo::Convert::DocBook converts a Texinfo tree to DocBook.

=head1 METHODS

=over

=item $converter = Texinfo::Convert::DocBook->converter($options)

Initialize an DocBook converter.  

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
