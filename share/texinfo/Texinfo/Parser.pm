# Parser.pm: parse texinfo code into a tree.
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

# The organization of the file is the following:
#  module definitions.
#  default parser state.  With explanation of the internal structures.
#  initializations, determination of command types.
#  user visible subroutines.
#  internal subroutines, doing the parsing.

package Texinfo::Parser;

# We need the unicode stuff.
use 5.006;
use strict;

# debug
use Carp qw(cluck);

use Data::Dumper;

# to detect if an encoding may be used to open the files
use Encode;

# for fileparse
use File::Basename;

#use POSIX qw(setlocale LC_ALL LC_CTYPE LC_MESSAGES);

# commands definitions
use Texinfo::Common;
# Error reporting and counting, translation of strings.
use Texinfo::Report;
# encoding_alias
use Texinfo::Encoding;

# to expand file names in @include and similar @-commands
use Texinfo::Convert::Text;
# to normalize node name, anchor, float arg, listoffloats and first *ref argument.
use Texinfo::Convert::NodeNameNormalization;
# in error messages, and for macro body expansion
use Texinfo::Convert::Texinfo;

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter Texinfo::Report);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use Texinfo::Parser ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
  parser
  parse_texi_text
  parse_texi_line
  parse_texi_file
  indices_information
  floats_information
  internal_references_information
  labels_information
  global_commands_information
  global_informations
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '5.1.90';

sub N__($)
{
  return $_[0];
}

#sub __($$)
#{
#  my $self = shift;
#  return &{$self->{'gettext'}}(@_);
#}

# Customization variables obeyed by the Parser, and the default values.
our %default_customization_values = (
  'TEST' => 0,
  'DEBUG' => 0,     # if >= 10, tree is printed in texi2any.pl after parsing.
                    # If >= 100 tree is printed every line.
  'SHOW_MENU' => 1,             # if false no menu error related.
  'INLINE_INSERTCOPYING' => 0,
  'IGNORE_BEFORE_SETFILENAME' => 1,
  'MACRO_BODY_IGNORES_LEADING_SPACE' => 0,
  'IGNORE_SPACE_AFTER_BRACED_COMMAND_NAME' => 1,
  'INPUT_PERL_ENCODING' => undef, # input perl encoding name, set from 
                              # @documentencoding in the default case
  'INPUT_ENCODING_NAME' => undef, # encoding name normalized as preferred
                              # IANA, set from @documentencoding in the default
                              # case
  'CPP_LINE_DIRECTIVES' => 1, # handle cpp like synchronization lines
  'MAX_MACRO_CALL_NESTING' => 100000, # max number of nested macro calls
  'GLOBAL_COMMANDS' => [],    # list of commands registered 
  # This is not used directly, but passed to Convert::Text through 
  # Texinfo::Common::_convert_text_options
  'ENABLE_ENCODING' => 1,     # output accented and special characters
                              # based on @documentencoding
  # following are used in Texinfo::Structuring
  'TOP_NODE_UP' => '(dir)',   # up node of Top node
  'SIMPLE_MENU' => 0,         # not used in the parser but in structuring
  'USE_UP_NODE_FOR_ELEMENT_UP' => 0, # Use node up for Up if there is no 
                                     # section up.
);

my %parser_default_configuration = (%Texinfo::Common::default_parser_state_configuration,
                                    %default_customization_values);

# the other possible keys for the parser state are:
#
# expanded_formats_hash   each key comes from expanded_formats value is 1
# index_names             a structure holding the link between index 
#                         names, prefixes, merged indices,
#                         initial value is %index_names in Texinfo::Common.
# context_stack           stack of the contexts, more recent on top.
#                         'line' is added when on a line or 
#                         block @-command line,
#                         'def' is added instead if on a definition line.
#                         'preformatted' is added in block commands 
#                         where there is no paragraphs and spaces are kept 
#                         (format, example, display...)
#                         'rawpreformatted' is added in raw block commands
#                         (html, xml, docbook...)
#                         'menu' is added in menu commands
#                         'math', 'footnote', 'caption', 'shortcaption', 
#                         'inlineraw' are also added when in those commands
# conditionals_stack      a stack of conditional commands that are expanded.
# raw_formats_stack       a stack of 1 or 0 for raw formats (@html... or 
#                         @inlineraw), is 0 if within a raw format that is
#                         not expanded.
# macro_stack             stack of macros being expanded (more recent first)
# definfoenclose          an hash, key is the command name, value is an array
#                         reference with 2 values, beginning and ending.
# input                   a stack, with last at bottom.  Holds the opened files
#                         or text.  Pending macro expansion or text expansion
#                         is also in that structure.
# misc_commands           the same than %misc_commands in Texinfo::Common, 
#                         but with index entry commands dynamically added
# close_paragraph_commands      same than %close_paragraph_commands, but with
#                               insertcopying removed if INLINE_INSERTCOPYING
# close_preformatted_commands   same than %close_preformatted_commands, but with
#                               insertcopying removed if INLINE_INSERTCOPYING
# no_paragraph_commands   the same than %default_no_paragraph_commands
#                         below, with index
#                         entry commands dynamically added
# simple_text_commands    the same than %simple_text_commands below, but 
#                         with index entry commands dynamically added
# current_node            last seen node.
# current_section         last seen section.
# nodes                   list of nodes.
# command_index_prefix    associate a command name to an index prefix.
# prefix_to_index_name    associate an index prefix to the index name.
# floats                  key is the normalized float type, value is an array
#                         reference holding all the floats.
# internal_references     an array holding all the internal references.

# set                     points to the value set when initializing, for
#                         configuration items that are not to be overriden
#                         by @-commands.  For example documentlanguage.

# A line information is an hash reference with the keys:
# line_nr        the line number
# file_name      the file name
# macro          if in a macro expansion, the name of the macro
#
# A text fragment information is a 2 element array reference, the first is the
# text fragment, the second is the line information.

# The input structure is an array, the first is the most recently included
# file.  The last element may be a file if the parsing is done on a file, 
# with parse_texi_file, or simply pending text, if called as parse_texi_text.
# each element of the array is a hash reference.  The key are:
# pending    an array reference containing pending text fragments, either the
#            text given as parse_texi_text or macro expansion text.
# name       file name
# line_nr    current line number in the file
# fh         filehandle for the file

# content is not copied but reference is copied when duplicating a parser.
my %tree_informations;
foreach my $tree_information ('values', 'macros', 'explained_commands', 'labels') {
  $tree_informations{$tree_information} = 1;
}

# The commands in initialization_overrides are not set in the document if
# set at the parser initialization.
my %initialization_overrides = (
  'INPUT_ENCODING_NAME' => 1,
  'documentlanguage' => 1,
);

my %no_brace_commands         = %Texinfo::Common::no_brace_commands;
my %misc_commands             = %Texinfo::Common::misc_commands;
my %brace_commands            = %Texinfo::Common::brace_commands;    
my %accent_commands           = %Texinfo::Common::accent_commands;
my %context_brace_commands    = %Texinfo::Common::context_brace_commands;
my %block_commands            = %Texinfo::Common::block_commands;
my %block_item_commands       = %Texinfo::Common::block_item_commands;
my %close_paragraph_commands  = %Texinfo::Common::close_paragraph_commands;
my %def_map                   = %Texinfo::Common::def_map;
my %def_commands              = %Texinfo::Common::def_commands;
my %def_aliases               = %Texinfo::Common::def_aliases;
my %menu_commands             = %Texinfo::Common::menu_commands;
my %preformatted_commands     = %Texinfo::Common::preformatted_commands;
my %format_raw_commands = %Texinfo::Common::format_raw_commands;
my %item_container_commands   = %Texinfo::Common::item_container_commands;
my %item_line_commands        = %Texinfo::Common::item_line_commands;
my %deprecated_commands       = %Texinfo::Common::deprecated_commands;
my %root_commands             = %Texinfo::Common::root_commands;
my %sectioning_commands       = %Texinfo::Common::sectioning_commands;
my %command_index_prefix      = %Texinfo::Common::command_index_prefix;
my %command_structuring_level = %Texinfo::Common::command_structuring_level;
my %ref_commands              = %Texinfo::Common::ref_commands;
my %region_commands           = %Texinfo::Common::region_commands;
my %code_style_commands       = %Texinfo::Common::code_style_commands;
my %in_heading_commands       = %Texinfo::Common::in_heading_commands;
my %explained_commands        = %Texinfo::Common::explained_commands;
my %inline_format_commands    = %Texinfo::Common::inline_format_commands;
my %inline_commands           = %Texinfo::Common::inline_commands;
my %inline_conditional_commands = %Texinfo::Common::inline_conditional_commands;
my %all_commands              = %Texinfo::Common::all_commands;

# equivalence between a @set flag and an @@-command
my %set_flag_command_equivalent = (
  'txicodequoteundirected' => 'codequoteundirected',
  'txicodequotebacktick'   => 'codequotebacktick',
#  'txideftypefnnl'         => 'deftypefnnewline',
);


# keep line information for those commands.
my %keep_line_nr_brace_commands = %context_brace_commands;
foreach my $keep_line_nr_brace_command ('titlefont', 'anchor') {
  $keep_line_nr_brace_commands{$keep_line_nr_brace_command} = 1;
}
foreach my $brace_command (keys (%brace_commands)) {
  $keep_line_nr_brace_commands{$brace_command} = 1
    if ($brace_commands{$brace_command} > 1);
}

my %type_with_paragraph;
foreach my $type ('before_item', 'text_root', 'document_root',
                  'brace_command_context') {
  $type_with_paragraph{$type} = 1;
}

my %command_ignore_space_after;
foreach my $command ('anchor', 'hyphenation', 'caption', 'shortcaption') {
  $command_ignore_space_after{$command} = 1;
}

my %global_multiple_commands;
foreach my $global_multiple_command (
  'author', 'footnote', 'hyphenation', 'insertcopying', 'printindex',
  'subtitle','titlefont', 'listoffloats', 'detailmenu',
  keys(%Texinfo::Common::document_settable_at_commands), ) {
  $global_multiple_commands{$global_multiple_command} = 1;
}

my %global_unique_commands;
foreach my $global_unique_command (
  'copying', 'settitle', 
  'shorttitlepage', 'title', 'titlepage', 'top', 
  keys(%Texinfo::Common::document_settable_unique_at_commands), ) {
  $global_unique_commands{$global_unique_command} = 1;
}

my %index_names = %Texinfo::Common::index_names;

# index names that cannot be set by the user.
my %forbidden_index_name = ();

foreach my $name(keys(%index_names)) {
  foreach my $prefix (@{$index_names{$name}->{'prefix'}}) {
    $forbidden_index_name{$prefix} = 1;
  }
}

foreach my $other_forbidden_index_name ('info','ps','pdf','htm',
   'html', 'log','aux','dvi','texi','txi','texinfo','tex','bib') {
  $forbidden_index_name{$other_forbidden_index_name} = 1;
}

# @-commands that do not start a paragraph
my %default_no_paragraph_commands;
# @-commands that should be at a line beginning
my %begin_line_commands;

foreach my $command ('node', 'end') {
  $begin_line_commands{$command} = $command;
}

foreach my $no_paragraph_command ('titlefont', 'caption', 'shortcaption', 
          'image', '*', 'hyphenation', 'anchor', 'errormsg') {
  $default_no_paragraph_commands{$no_paragraph_command} = 1;
}

foreach my $no_paragraph_command (keys(%misc_commands)) {
  $default_no_paragraph_commands{$no_paragraph_command} = 1;
  $begin_line_commands{$no_paragraph_command} = 1;
}

# verbatiminclude is not said to begin at the beginning of the line
# in the manual
foreach my $misc_not_begin_line ('comment', 'c', 'sp', 'refill', 
                                'noindent', 'indent', 'columnfractions',
                                'tab', 'item', 'headitem', 'verbatiminclude',
                                'set', 'clear',
                                'vskip', keys(%in_heading_commands)) {
  delete $begin_line_commands{$misc_not_begin_line};
}

my %block_arg_commands;
foreach my $block_command (keys(%block_commands)) {
  $begin_line_commands{$block_command} = 1;
  $default_no_paragraph_commands{$block_command} = 1;
  $block_arg_commands{$block_command} = 1 
    if ($block_commands{$block_command} ne 'raw');
#        and ! $format_raw_commands{$block_command});
}

my %close_preformatted_commands = %close_paragraph_commands;
foreach my $no_close_preformatted('sp') {
  delete $close_preformatted_commands{$no_close_preformatted};
}
# FIXME to close preformated or not to close?
#foreach my $format_raw_command(keys(%format_raw_commands)) {
#  $close_preformatted_commands{$format_raw_command} = 1;
#}

# commands that may appear in accents
my %in_accent_commands = %accent_commands;
foreach my $brace_command(keys(%brace_commands)) {
  $in_accent_commands{$brace_command} = 1 if (!$brace_commands{$brace_command});
}
foreach my $no_brace_command (keys(%no_brace_commands)) {
  $in_accent_commands{$no_brace_command} = 1;
}
$in_accent_commands{'c'} = 1;
$in_accent_commands{'comment'} = 1;

# commands that may appear in texts arguments
my %in_full_text_commands;
foreach my $command (keys(%brace_commands), keys(%no_brace_commands)) {
  $in_full_text_commands{$command} = 1;
}
foreach my $misc_command_in_full_text('c', 'comment', 'refill', 'noindent',
                         'indent', 'columnfractions', 'set', 'clear', 'end') {
  $in_full_text_commands{$misc_command_in_full_text} = 1;
}

foreach my $out_format (keys(%format_raw_commands)) {
  $in_full_text_commands{$out_format} = 1;
}
delete $in_full_text_commands{'caption'};
delete $in_full_text_commands{'shortcaption'};
foreach my $block_command (keys(%block_commands)) {
  $in_full_text_commands{$block_command} = 1 
    if ($block_commands{$block_command} eq 'conditional');
}

# commands that may happen on lines where everything is
# permitted
my %in_full_line_commands = %in_full_text_commands;
foreach my $not_in_full_line_commands('noindent', 'indent') {
  delete $in_full_line_commands{$not_in_full_line_commands};
}

# commands that may happen on sectioning commands 
my %in_full_line_commands_no_refs = %in_full_line_commands;
foreach my $not_in_full_line_commands_no_refs ('titlefont', 
                                   'anchor', 'footnote', 'verb') {
  delete $in_full_line_commands_no_refs{$not_in_full_line_commands_no_refs};
}

# commands that may happen in simple text arguments
my %in_simple_text_commands = %in_full_line_commands_no_refs;
foreach my $not_in_simple_text_command('xref', 'ref', 'pxref', 'inforef') {
  delete $in_simple_text_commands{$not_in_simple_text_command};
}

# commands that only accept simple text as argument in any context.
my %simple_text_commands;
foreach my $misc_command(keys(%misc_commands)) {
  if ($misc_commands{$misc_command} =~ /^\d+$/ 
      or ($misc_commands{$misc_command} eq 'line' 
          and !($sectioning_commands{$misc_command}
                or $def_commands{$misc_command}))
      or $misc_commands{$misc_command} eq 'text') {
    $simple_text_commands{$misc_command} = 1;
  }
}

my %full_line_commands_no_refs = (%sectioning_commands,
                                  %def_commands);

delete $simple_text_commands{'center'};
delete $simple_text_commands{'exdent'};
foreach my $command ('titlefont', 'anchor', 'xref','ref','pxref', 
                     'inforef', 'shortcaption', 'math', 'indicateurl',
                     'email', 'uref', 'url', 'image', 'abbr', 'acronym', 
                     'dmn', 'ctrl', 'errormsg') {
  $simple_text_commands{$command} = 1;
}

# commands that accept full text, but no block or top-level commands
my %full_text_commands;
foreach my $brace_command (keys (%brace_commands)) {  
  if ($brace_commands{$brace_command} == 1 
      and !$simple_text_commands{$brace_command} 
      and !$context_brace_commands{$brace_command}
      and !$accent_commands{$brace_command}) {
    $full_text_commands{$brace_command} = 1;
  }
}

# commands that accept almost the same than in full text, except
# what do not make sense on a line.
my %full_line_commands;
$full_line_commands{'center'} = 1;
$full_line_commands{'exdent'} = 1;
$full_line_commands{'item'} = 1;
$full_line_commands{'itemx'} = 1;

# Fill the valid nestings hash.  All commands not in that hash 
# are considered to accept anything within.  There are additional
# context tests, to make sure, for instance that we are testing
# @-commands on the block, misc or node @-command line and not
# in the content.
# index entry commands are dynamically set as in_simple_text_commands
my %default_valid_nestings;

foreach my $command (keys(%accent_commands)) {
  $default_valid_nestings{$command} = \%in_accent_commands;
}
foreach my $command (keys(%full_text_commands)) {
  $default_valid_nestings{$command} = \%in_full_text_commands;
}
foreach my $command (keys(%simple_text_commands)) {
  $default_valid_nestings{$command} = \%in_simple_text_commands;
}
foreach my $command (keys(%full_line_commands)) {
  $default_valid_nestings{$command} = \%in_full_line_commands;
}
foreach my $command (keys(%full_line_commands_no_refs)) {
  $default_valid_nestings{$command} = \%in_full_line_commands_no_refs;
}
# Only for block commands with line arguments
foreach my $command (keys(%block_commands)) {
  if ($block_commands{$command} and $block_commands{$command} ne 'raw'
      and $block_commands{$command} ne 'conditional'
      and !$def_commands{$command}) {
    $default_valid_nestings{$command} = \%in_simple_text_commands;
  }
}


my @preformatted_contexts = ('preformatted', 'rawpreformatted');
my %preformatted_contexts;
foreach my $preformatted_context (@preformatted_contexts) {
  $preformatted_contexts{$preformatted_context} = 1;
}

# contexts on the context_stack stack where empty line don't trigger
# paragraph
my %no_paragraph_contexts;
foreach my $no_paragraph_context ('math', 'menu', @preformatted_contexts, 
                                  'def', 'inlineraw') {
  $no_paragraph_contexts{$no_paragraph_context} = 1;
};



# Format a bug message
sub _bug_message($$;$$)
{
  my $self = shift;
  my $message = shift;
  my $line_number = shift;
  my $current = shift;

  my $line_message = '';
  if ($line_number) {
    my $file = $line_number->{'file_name'};
    $line_message 
      = "last location: $line_number->{'file_name'}:$line_number->{'line_nr'}";
    if ($line_number->{'macro'} ne '') {
      $line_message .= " (possibly involving $line_number->{'macro'})";
    }
    $line_message .= "\n";
  }
  my $message_context_stack = "context_stack: (@{$self->{'context_stack'}})\n";
  my $current_element_message = '';
  if ($current) {
    $current_element_message = "current: ". _print_current($current);
  }
  warn "You found a bug: $message\n\n".
       "Additional informations:\n".
       $line_message.$message_context_stack.$current_element_message;
  
}

# simple deep copy of a structure
sub _deep_copy($)
{
  my $struct = shift;
  my $string = Data::Dumper->Dump([$struct], ['struct']);
  eval $string;
  return $struct;
}

# return true if effect of global commands should be ignored.
sub _ignore_global_commands($)
{
  my $self = shift;
  return !$self->{'expanded_formats_stack'}->[-1];
}

# enter all the commands associated with an index name using the prefix
# list
sub _register_index_commands($$)
{
  my $self = shift;
  my $index_name = shift;
  if (!$self->{'index_names'}->{$index_name}->{'prefix'}) {
    $self->{'index_names'}->{$index_name}->{'prefix'} = [$index_name];
  }
  if (!exists($self->{'index_names'}->{$index_name}->{'name'})) {
    $self->{'index_names'}->{$index_name}->{'name'} = $index_name;
  }
  if (!exists($self->{'index_names'}->{$index_name}->{'contained_indices'})) {
    $self->{'index_names'}->{$index_name}->{'contained_indices'}->{$index_name} = 1;
  }
  foreach my $prefix (@{$self->{'index_names'}->{$index_name}->{'prefix'}}) {
    $self->{'misc_commands'}->{$prefix.'index'} = 'line';
    $self->{'no_paragraph_commands'}->{$prefix.'index'} = 1;
    $self->{'valid_nestings'}->{$prefix.'index'} = \%in_simple_text_commands;
    $self->{'command_index_prefix'}->{$prefix.'index'} = $prefix;
    $self->{'prefix_to_index_name'}->{$prefix} = $index_name;
  }
}

# initialization entry point.  Set up a parser.
# The last argument, optional, is a hash provided by the user to change
# the default values for what is present in %parser_default_configuration.
# The exact arguments of the function depend on how it was called,
# in a object oriented way or not.
sub parser(;$$)
{
  my $class = shift;
  my $conf;

  my $parser = _deep_copy(\%parser_default_configuration);
  # _deep_copy doesn't handle subs
  $parser->{'gettext'} = $parser_default_configuration{'gettext'};
  $parser->{'pgettext'} = $parser_default_configuration{'pgettext'};

  # called not object-oriented
  if (ref($class) eq 'HASH') {
    #print STDERR "Not oo\n"
    $conf = $class;
    bless $parser;

  } elsif (ref($class)) { 
    # called on an existing parser, interpreted as a duplication
    my $old_parser = $class;
    $class = ref($class);
    foreach my $key (keys(%parser_default_configuration)) {
      if ($tree_informations{$key}) {
        if (defined($old_parser->{$key})) {
          foreach my $info_key (keys(%{$old_parser->{$key}})) {
            $parser->{$key}->{$info_key}
              = $old_parser->{$key}->{$info_key};
          }
        }
      } else {
        $parser->{$key} = _deep_copy($old_parser->{$key});
      }
    }
    #$parser = _deep_copy($old_parser);
    $parser->{'gettext'} = $old_parser->{'gettext'};
    $parser->{'pgettext'} = $old_parser->{'pgettext'};
    bless $parser, $class;
    $conf = shift;

  } elsif (defined($class)) {
    bless $parser, $class;
    $conf = shift;
  } else {
    bless $parser;
    $conf = shift;
  }

  if (defined($conf)) {
    foreach my $key (keys(%$conf)) {
      if (exists($parser_default_configuration{$key})) {
        if (ref($conf->{$key}) ne 'CODE' and $key ne 'values') {
          $parser->{$key} = _deep_copy($conf->{$key});
        } else {
          $parser->{$key} = $conf->{$key};
        }
        if ($initialization_overrides{$key}) {
          $parser->{'set'}->{$key} = $parser->{$key};
        }
      } else {
        warn "$key not a possible customization in Texinfo::Parser::parser\n";
      }
    }
  }
  #foreach my $value (keys %{$parser->{'values'}}) {
  #  print STDERR "   ->  $value $parser->{'values'}->{$value}\n";
  #}
  # Now initialize command hash that are dynamically modified, notably
  # those for index commands, and lists, based on defaults and user provided.
  $parser->{'misc_commands'} = _deep_copy (\%misc_commands);
  $parser->{'valid_nestings'} = _deep_copy (\%default_valid_nestings);
  $parser->{'no_paragraph_commands'} = { %default_no_paragraph_commands };
  $parser->{'index_names'} = _deep_copy (\%index_names);
  $parser->{'command_index_prefix'} = {%command_index_prefix};
  $parser->{'close_paragraph_commands'} = {%close_paragraph_commands};
  $parser->{'close_preformatted_commands'} = {%close_preformatted_commands};
  if ($parser->{'INLINE_INSERTCOPYING'}) {
    delete $parser->{'close_paragraph_commands'}->{'insercopying'};
    delete $parser->{'close_preformatted_commands'}->{'insercopying'};
  }
  # a hash is simply concatenated.  It should be like %index_names.
  if (ref($parser->{'indices'}) eq 'HASH') {
    %{$parser->{'index_names'}} = (%{$parser->{'index_names'}}, 
                                   %{$parser->{'indices'}});
  } else { # an array holds index names defined with @defindex
    foreach my $name (@{$parser->{'indices'}}) {
      $parser->{'index_names'}->{$name} = {'in_code' => 0};
    }
  }
  foreach my $index (keys (%{$parser->{'index_names'}})) {
    $parser->_register_index_commands($index);
  }
  if ($parser->{'merged_indices'}) {
    foreach my $index_from (keys (%{$parser->{'merged_indices'}})) {
      my $index_to = $parser->{'merged_indices'}->{$index_from};
      if (defined($parser->{'index_names'}->{$index_from})
          and defined($parser->{'index_names'}->{$index_to})) {
        $parser->{'index_names'}->{$index_from}->{'merged_in'} = $index_to;
        $parser->{'index_names'}->{$index_to}->{'contained_indices'}->{$index_from} = 1;
      }
    }
  }
  foreach my $explained_command(keys(%explained_commands)) {
    if  (!defined($parser->{'explained_commands'}->{$explained_command})) {
      $parser->{'explained_commands'}->{$explained_command} = {};
    }
  }
  $parser->{'context_stack'} = [ $parser->{'context'} ];
  $parser->{'regions_stack'} = [];
  $parser->{'macro_stack'} = [];
  $parser->{'conditionals_stack'} = [];
  $parser->{'expanded_formats_stack'} = [1];

  # turn the array to a hash for speed.  Not sure it really matters for such
  # a small array.
  foreach my $expanded_format(@{$parser->{'expanded_formats'}}) {
    $parser->{'expanded_formats_hash'}->{$expanded_format} = 1;
  }

  %{$parser->{'global_commands'}} = %global_multiple_commands;

  foreach my $global_command (@{$parser->{'GLOBAL_COMMANDS'}}) {
    $parser->{'global_commands'}->{$global_command} = 1;
  }

  $parser->Texinfo::Report::new;

  return $parser;
}

sub get_conf($$)
{
  my $self = shift;
  my $var = shift;
  return $self->{$var};
}

# split a scalar text in an array lines.
sub _text_to_lines($)
{
  my $text = shift;
  die if (!defined($text));
  my $had_final_end_line = chomp($text);
  my $lines = [ map {$_."\n"} split (/\n/, $text, -1) ];
  $lines = [''] if (!@$lines);
  chomp($lines->[-1]) unless ($had_final_end_line);
  return $lines;
}

# construct a text fragments array matching a lines array, based on information
# supplied.
# If $fixed_line_number is set the line number is not increased, otherwise
# it is increased, beginning at $first_line.
sub _complete_line_nr($$;$$$)
{
  my $lines = shift;
  my $first_line = shift;
  my $file = shift;
  my $macro = shift;
  my $fixed_line_number = shift;

  $macro = '' if (!defined($macro));
  $file = '' if (!defined($file));
  my $new_lines = [];

  if (defined($first_line)) {
    my $line_index = $first_line;
    foreach my $index(0..scalar(@$lines)-1) {
      $line_index = $index+$first_line if (!$fixed_line_number);
      $new_lines->[$index] = [ $lines->[$index],  
                             { 'line_nr' => $line_index,
                               'file_name' => $file, 'macro' => $macro } ];
    }
  } else {
    foreach my $line (@$lines) {
      push @$new_lines, [ $line ];
    }
  }
  return $new_lines;
}

# entry point for text fragments.
# Used in tests.
# Note that it has no associated root type a opposed to parse_texi_line
# and parse_texi_file.
sub parse_texi_text($$;$$$$)
{
  my $self = shift;
  my $text = shift;
  my $lines_nr = shift;
  my $file = shift;
  my $macro = shift;
  my $fixed_line_number = shift;

  return undef if (!defined($text));

  my $lines_array = [];
  if (!ref($text)) {
    $text = _text_to_lines($text);
  }
  $lines_nr = [] if (!defined($lines_nr));
  if (!ref($lines_nr)) {
    #$file =~ s/^.*\/// if (defined($file) and $self->{'TEST'});
    $lines_array = _complete_line_nr($text, $lines_nr, $file, 
                                     $macro, $fixed_line_number);
  } else {
    while (@$text) {
      my $line_nr = shift @$lines_nr;
      my $line = shift @$text;
      push @$lines_array, [$line, $line_nr];
    }
  }

  $self = parser() if (!defined($self));
  $self->{'input'} = [{'pending' => $lines_array}];
  my $tree = $self->_parse_texi();
  return $tree;
}

# Not used for now, as a @contents after the first sectioning command
# is correct if not using TeX.
sub _check_contents_location($$)
{
  my $self = shift;
  my $tree = shift;

  my $commands = $self->global_commands_information();
  return unless ($commands);
  # Find the last sectioning command
  my $index = -1;
  my %ending_root_commands;
  my $found = 0;
  while ($tree->{'contents'}->[$index]) {
    if (defined($tree->{'contents'}->[$index]->{'cmdname'})) {
      $ending_root_commands{$tree->{'contents'}->[$index]} = 1;
      if ($sectioning_commands{$tree->{'contents'}->[$index]->{'cmdname'}}) {
        $found = 1;
        last;
      }
    }
    $index--;
  }
  return if (!$found);

  #print STDERR "ending_root_commands ".join('|',keys(%ending_root_commands))."\n";
  #print STDERR "tree contents: ".join('|', @{$tree->{'contents'}})."\n";
  foreach my $command ('contents', 'shortcontents', 'summarycontents') {
    if ($commands->{$command}) {
      foreach my $current (@{$commands->{$command}}) {
        my $root_command = $self->Texinfo::Common::find_parent_root_command($current);
        #print STDERR "root_command for $current->{'cmdname'}: $root_command\n";
        if (defined($root_command) 
            and !$ending_root_commands{$root_command}) {
          $self->line_warn(sprintf($self->__(
                  "\@%s should only appear at beginning or end of document"),
                            $current->{'cmdname'}), $current->{'line_nr'});                       
        }
      }
    }
  }
}

# parse a texi file
sub parse_texi_file($$)
{
  my $self = shift;
  my $file_name = shift;

  my $filehandle = do { local *FH };
  if (! open($filehandle, $file_name)) { 
    $self->document_error(sprintf($self->__("could not open %s: %s"), 
                                  $file_name, $!));
    return undef;
  }
  my $line_nr = 0;
  my $line;
  my @first_lines;

  my $pending_first_texi_line;
  # the first line not empty and not with \input is kept in 
  # $pending_first_texi_line and put in the pending lines just below
  while ($line = <$filehandle>) {
    $line_nr++;
    if ($line =~ /^ *\\input/ or $line =~ /^\s*$/) {
      $line =~ s/\x{7F}.*\s*//;
      push @first_lines, $line;
    } else {
      $pending_first_texi_line = $line;
      last;
    }
  }
  my $root = { 'contents' => [], 'type' => 'text_root' };
  if (@first_lines) {
    push @{$root->{'contents'}}, { 'type' => 'preamble', 'contents' => [] };
    foreach my $line (@first_lines) {
      push @{$root->{'contents'}->[-1]->{'contents'}}, 
                                   { 'text' => $line,
                                     'type' => 'preamble_text' };
    }
  }
  my ($directories, $suffix);
  ($file_name, $directories, $suffix) = fileparse($file_name)
            if ($self->{'TEST'});
  $self = parser() if (!defined($self));
  $self->{'input'} = [{
       'pending' => [[$pending_first_texi_line, {'line_nr' => $line_nr,
                                'macro' => '', 'file_name' => $file_name}]],
       'name' => $file_name,
       'line_nr' => $line_nr,
       'fh' => $filehandle
        }];
  $self->{'info'}->{'input_file_name'} = $file_name;
  my $tree = $self->_parse_texi($root);

  # Find 'text_root', which contains everything before first node/section.
  # if there are elements, 'text_root' is the first content, otherwise it
  # is the root.
  my $text_root;
  if ($tree->{'type'} eq 'text_root') {
    $text_root = $tree;
  } elsif ($tree->{'contents'} and $tree->{'contents'}->[0]
           and $tree->{'contents'}->[0]->{'type'} eq 'text_root') {
    $text_root = $tree->{'contents'}->[0];
  }

  # Put everything before @setfilename in a special type.  This allows to
  # ignore everything before @setfilename.
  if ($self->{'IGNORE_BEFORE_SETFILENAME'} and $text_root and 
      $self->{'extra'} and $self->{'extra'}->{'setfilename'}
      and $self->{'extra'}->{'setfilename'}->{'parent'} eq $text_root) {
    my $before_setfilename = {'type' => 'preamble_before_setfilename',
                              'parent' => $text_root,
                              'contents' => []};
    while ($text_root->{'contents'}->[0] ne $self->{'extra'}->{'setfilename'}) {
      my $content = shift @{$text_root->{'contents'}};
      $content->{'parent'} = $before_setfilename;
      push @{$before_setfilename->{'contents'}}, $content;
    }
    unshift (@{$text_root->{'contents'}}, $before_setfilename)
      if (@{$before_setfilename->{'contents'}});
  }
  #$self->_check_contents_location($tree);

  return $tree;
}

sub parse_texi_line($$;$$$$)
{
  my $self = shift;
  my $text = shift;
  my $lines_nr = shift;
  my $file = shift;
  my $macro = shift;
  my $fixed_line_number = shift;

  return undef if (!defined($text));

  if (!ref($text)) {
    $text = _text_to_lines($text);
  }
  #$file =~ s/^.*\/// if (defined($file) and $self->{'TEST'});
  my $lines_array = _complete_line_nr($text, $lines_nr, $file, 
                                     $macro, $fixed_line_number);

  $self = parser() if (!defined($self));
  $self->{'input'} = [{'pending' => $lines_array}];
  my $tree = $self->_parse_texi({'contents' => [], 'type' => 'root_line'});
  return $tree;
}

# return indices informations
sub indices_information($)
{
  my $self = shift;
  return ($self->{'index_names'}, $self->{'merged_indices'});
  #return ($self->{'index_names'}, $self->{'merged_indices'}, $self->{'index_entries'});
}

sub floats_information($)
{
  my $self = shift;
  return $self->{'floats'};
}

sub internal_references_information($)
{
  my $self = shift;
  return $self->{'internal_references'};
}

sub global_commands_information($)
{
  my $self = shift;
  return $self->{'extra'};
}

# @ dircategory_direntry
# @ unassociated_menus
# perl_encoding
# input_encoding_name
# input_file_name
sub global_informations($)
{
  my $self = shift;
  return $self->{'info'};
}

sub labels_information($)
{
  my $self = shift;
  return $self->{'labels'};
}

# Following are the internal subroutines.  The most important are
# _parse_texi:  the main parser loop.
# _end_line:    called at an end of line.  Handling of @include lines is 
#               done here.
# _next_text:   present the next text fragment, from pending text or line,
#               as described above.

# for debugging
sub _print_current($)
{
  my $current = shift;
  return Texinfo::Common::_print_current($current);
}

# for debugging
sub _print_command_args_texi($)
{
  my $current = shift;
  return '' if (!$current->{'cmdname'});
  my $args = '';
  my $with_brace;
  if ($current->{'args'} and @{$current->{'args'}}) {
    $with_brace 
        = ($current->{'args'}->[0]->{'type'} eq 'brace_command_arg'
           or $current->{'args'}->[0]->{'type'} eq 'brace_command_context');
    $args .= '{' if ($with_brace);
    foreach my $arg (@{$current->{'args'}}) {
      $args .= Texinfo::Convert::Texinfo::convert($arg).', ';
    }
    $args =~ s/, $//;
  }
  chomp($args);
  if ($with_brace) {
    $args .= '}';
  }
  return '@'.$current->{'cmdname'} .$args."\n";
}

sub _print_current_keys($)
{
  my $current = shift;
  my $string = _print_current($current);
  foreach my $key (keys (%$current)) {
    $string .= "   $key: $current->{$key}\n";
  }
  if ($current->{'extra'}) {
    $string .= "    EXTRA\n";
    foreach my $key (keys (%{$current->{'extra'}})) {
      $string .= "    $key: $current->{'extra'}->{$key}\n";
    }
  }
  return $string;
}

# For debugging
my @kept_keys = ('contents', 'cmdname', 'type', 'text', 'args');
my %kept_keys;
foreach my $key (@kept_keys) {
  $kept_keys{$key} = 1;
}
sub _filter_print_keys { [grep {$kept_keys{$_}} ( sort keys %{$_[0]} )] };
sub _print_tree($)
{
  my $tree = shift;
  local $Data::Dumper::Sortkeys = \&_filter_print_keys;
  local $Data::Dumper::Purity = 1;
  local $Data::Dumper::Indent = 1;

  return Data::Dumper->Dump([$tree]);
}

sub _register_global_command($$$$)
{
  my $self = shift;
  my $command = shift;
  my $current = shift;
  my $line_nr = shift;
  if ($command eq 'summarycontents' and !$self->{'global_commands'}->{$command}) {
    $command = 'shortcontents';
  }
  if ($self->{'global_commands'}->{$command} and $command ne 'author') {
    push @{$self->{'extra'}->{$command}}, $current
      unless (_ignore_global_commands($self));
    $current->{'line_nr'} = $line_nr if (!$current->{'line_nr'});
    return 1;
  } elsif ($global_unique_commands{$command}) {
    # setfilename ignored in an included file
    $current->{'line_nr'} = $line_nr if (!$current->{'line_nr'});
    if ($command eq 'setfilename'
        and scalar(@{$self->{'input'}}) > 1) {
    } elsif (exists ($self->{'extra'}->{$current->{'cmdname'}})) {
      $self->line_warn(sprintf($self->__('multiple @%s'), 
                               $current->{'cmdname'}), $line_nr); 
    } else {
      $self->{'extra'}->{$current->{'cmdname'}} = $current
                     unless (_ignore_global_commands($self));
    }
    return 1;
  }
  return 0;
}

# parse a @macro line
sub _parse_macro_command_line($$$$$;$)
{
  my $self = shift;
  my $command = shift;
  my $line = shift;
  my $parent = shift;
  my $line_nr = shift;
  my $macro = { 'cmdname' => $command, 'parent' => $parent, 'contents' => [],
               'extra' => {'arg_line' => $line}, 'line_nr' => $line_nr };
  # REMACRO
  if ($line =~ /^\s+([[:alnum:]][[:alnum:]-]*)\s*(.*)/) {
    my $macro_name = $1;
    my $args_def = $2;
    my @args;

    if ($args_def =~ s/^\s*{\s*(.*?)\s*}\s*//) {
      @args = split(/\s*,\s*/, $1);
    }
 
    # accept an @-command after the arguments in case there is a @c or
    # @comment
    if ($args_def =~ /^\s*[^\@]/) {
      $self->line_error(sprintf($self->__("bad syntax for \@%s argument: %s"), 
                                 $command, $args_def),
                        $line_nr);
      $macro->{'extra'}->{'invalid_syntax'} = 1;
    }
    print STDERR "MACRO \@$command $macro_name\n" if ($self->{'DEBUG'});

    $macro->{'args'} = [ 
      { 'type' => 'macro_name', 'text' => $macro_name, 
          'parent' => $macro } ];
    my $index = 0;
    foreach my $formal_arg (@args) {
      push @{$macro->{'args'}}, 
        { 'type' => 'macro_arg', 'text' => $formal_arg, 
          'parent' => $macro};
      if ($formal_arg !~ /^[\w\-]+$/) {
        $self->line_error(sprintf($self->__("bad or empty \@%s formal argument: %s"),
                                           $command, $formal_arg), $line_nr);
        $macro->{'extra'}->{'invalid_syntax'} = 1;
      }
      $macro->{'extra'}->{'args_index'}->{$formal_arg} = $index;
      $index++;
    }
  } elsif ($line !~ /\S/) {
    $self->line_error(sprintf($self->
                    __("%c%s requires a name"), ord('@'), $command), $line_nr);
    $macro->{'extra'}->{'invalid_syntax'} = 1;
  } else {
    $self->line_error(sprintf($self->
                    __("bad name for \@%s"), $command), $line_nr);
    $macro->{'extra'}->{'invalid_syntax'} = 1;
  }
  return $macro;
}

# start a paragraph if in a context where paragraphs are to be started.
sub _begin_paragraph($$;$)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;

  # !$current->{'type'} is true for @-commands.  In fact it is unclear
  # that there may be cases of !$current->{'type'} and !$current->{'cmdname'}
  if ((!$current->{'type'} or $type_with_paragraph{$current->{'type'}})
      and !$no_paragraph_contexts{$self->{'context_stack'}->[-1]}) {
    if (!defined($current->{'contents'})) {
      $self->_bug_message("contents undef", $line_nr, $current);
      die;
    }

    # find whether an @indent precedes the paragraph
    my $indent;
    if (scalar(@{$current->{'contents'}})) {
      my $index = scalar(@{$current->{'contents'}}) -1;
      while ($index >= 0
            and !($current->{'contents'}->[$index]->{'type'} 
              and ($current->{'contents'}->[$index]->{'type'} eq 'empty_line'
                   or $current->{'contents'}->[$index]->{'type'} eq 'paragraph'))
            and !($current->{'contents'}->[$index]->{'cmdname'}
                  and $self->{'close_paragraph_commands'}->{$current->{'contents'}->[$index]->{'cmdname'}})) {
        if ($current->{'contents'}->[$index]->{'cmdname'}
          and ($current->{'contents'}->[$index]->{'cmdname'} eq 'indent'
              or $current->{'contents'}->[$index]->{'cmdname'} eq 'noindent')) {
          $indent = $current->{'contents'}->[$index]->{'cmdname'};
          last;
        }
        $index--;
      }
    }
    push @{$current->{'contents'}}, 
            { 'type' => 'paragraph', 'parent' => $current, 'contents' => [] };
    $current->{'contents'}->[-1]->{'extra'}->{$indent} = 1 if ($indent);
    $current = $current->{'contents'}->[-1];
    print STDERR "PARAGRAPH\n" if ($self->{'DEBUG'});
    return $current;
  }
  return 0;
}

sub _begin_preformatted($)
{
  my $self = shift;
  my $current = shift;
  if ($preformatted_contexts{$self->{'context_stack'}->[-1]}) {
    push @{$current->{'contents'}}, 
          { 'type' => $self->{'context_stack'}->[-1], 
            'parent' => $current, 'contents' => [] };
    $current = $current->{'contents'}->[-1];
    print STDERR "PREFORMATTED $self->{'context_stack'}->[-1]\n" if ($self->{'DEBUG'});
  }
  return $current;
}

# wrapper around line_warn.  Set line_nr to be the line_nr of the command,
# corresponding to the opening of the command.  Call line_warn with
# sprintf if needed.
sub _command_warn($$$$;@)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;
  my $message = shift;

  if ($current->{'line_nr'}) {
    $line_nr = $current->{'line_nr'};
  }
  if (@_) {
    $self->line_warn(sprintf($message, @_), $line_nr);
  } else {
    $self->line_warn($message, $line_nr);
  }
}

sub _command_error($$$$;@)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;
  my $message = shift;

  # use the beginning of the @-command for the error message
  # line number if available. 
  # FIXME line_nr currently not registered for regular brace commands
  if ($current->{'line_nr'}) {
    $line_nr = $current->{'line_nr'};
  }
  if (@_) {
    $self->line_error(sprintf($message, @_), $line_nr);
  } else {
    $self->line_error($message, $line_nr);
  }
}

# currently doesn't do much more than
# return $_[1]->{'parent'}
sub _close_brace_command($$$;$$)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;
  my $closed_command = shift;
  my $interrupting_command = shift;

  if ($current->{'cmdname'} ne 'verb' or $current->{'type'} eq '') {
    if (defined($closed_command)) {
      $self->_command_error($current, $line_nr,
        $self->__("\@end %s seen before \@%s closing brace"), 
                  $closed_command, $current->{'cmdname'});
    } elsif (defined($interrupting_command)) {
      $self->_command_error($current, $line_nr,
        $self->__("\@%s seen before \@%s closing brace"), 
                  $interrupting_command, $current->{'cmdname'});
                                                       
    } else {
      $self->_command_error($current, $line_nr, 
        $self->__("%c%s missing close brace"), ord('@'), $current->{'cmdname'});
    }
  } else {
    $self->_command_error($current, $line_nr,
       $self->__("\@%s missing closing delimiter sequence: %s}"),
       $current->{'cmdname'}, $current->{'type'});
  }
  $current = $current->{'parent'};
  return $current;
}

sub _in_code($$)
{
  my $self = shift;
  my $current = shift;

  while ($current->{'parent'} and $current->{'parent'}->{'cmdname'}
          and exists $brace_commands{$current->{'parent'}->{'cmdname'}}
          and !exists $context_brace_commands{$current->{'parent'}->{'cmdname'}}) {
    return 1 if ($code_style_commands{$current->{'parent'}->{'cmdname'}});
    $current = $current->{'parent'}->{'parent'};
  }
  return 0;
}

# close brace commands, that don't set a new context (ie @caption, @footnote)
sub _close_all_style_commands($$$;$$)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;
  my $closed_command = shift;
  my $interrupting_command = shift;

  while ($current->{'parent'} and $current->{'parent'}->{'cmdname'}
          and exists $brace_commands{$current->{'parent'}->{'cmdname'}}
          and !exists $context_brace_commands{$current->{'parent'}->{'cmdname'}}) {
    $current = _close_brace_command($self, $current->{'parent'}, $line_nr,
                                    $closed_command, $interrupting_command);
  }
  return $current;
}

# close brace commands except for @caption, @footnote then the paragraph
sub _end_paragraph($$$;$$)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;
  my $closed_command = shift;
  my $interrupting_command = shift;

  $current = _close_all_style_commands($self, $current, $line_nr, 
                                       $closed_command, $interrupting_command);
  if ($current->{'type'} and $current->{'type'} eq 'paragraph') {
    print STDERR "CLOSE PARA\n" if ($self->{'DEBUG'});
    $current = $current->{'parent'};
  } 
  return $current;
}

# close brace commands except for @caption, @footnote then the preformatted
sub _end_preformatted($$$;$$)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;
  my $closed_command = shift;
  my $interrupting_command = shift;

  $current = _close_all_style_commands($self, $current, $line_nr,
                                       $closed_command, $interrupting_command);
  if ($current->{'type'} and $preformatted_contexts{$current->{'type'}}) {
    print STDERR "CLOSE PREFORMATTED $current->{'type'}\n" if ($self->{'DEBUG'});
    # completly remove void preformatted contexts
    if (!@{$current->{'contents'}}) {
      my $removed = pop @{$current->{'parent'}->{'contents'}};
      print STDERR "popping $removed->{'type'}\n" if ($self->{'DEBUG'});
    }
    $current = $current->{'parent'};
  }
  return $current;
}

# check that there are no text holding environment (currently
# checking only paragraphs and preformatted) in contents
sub _check_no_text($)
{
  my $current = shift;
  my $after_paragraph = 0;
  foreach my $content (@{$current->{'contents'}}) {
    if ($content->{'type'} and $content->{'type'} eq 'paragraph') {
      $after_paragraph = 1;
      last;
    } elsif ($content->{'type'} and $preformatted_contexts{$content->{'type'}}) {
      foreach my $preformatted_content (@{$content->{'contents'}}) {
        if ((defined($preformatted_content->{'text'}) 
             and $preformatted_content->{'text'} =~ /\S/)
            or ($preformatted_content->{'cmdname'} 
                and ($preformatted_content->{'cmdname'} ne 'c'
                     and $preformatted_content->{'cmdname'} ne 'comment')
                and !($preformatted_content->{'type'} 
                      and $preformatted_content->{'type'} eq 'index_entry_command'))) {
          $after_paragraph = 1;
          last;
        }
      }
      last if ($after_paragraph);
    }
  }
  return $after_paragraph;
}

# put everything after the last @item/@itemx in an item_table type container
# and distinguish table_term and table_entry.
sub _gather_previous_item($$;$$)
{
  my $self = shift;
  my $current = shift;
  my $next_command = shift;
  my $line_nr = shift;

  # nothing to do in that case.
  if ($current->{'contents'}->[-1]->{'type'}
      and $current->{'contents'}->[-1]->{'type'} eq 'before_item') {
    if ($next_command and $next_command eq 'itemx') {
      $self->line_warn(sprintf($self->__("\@itemx should not begin \@%s"), 
                                $current->{'cmdname'}), $line_nr);
    }
    return;
  }
  #print STDERR "GATHER "._print_current($current)."\n";
  my $type;
  # if before an itemx, the type is different since there should not be 
  # real content, so it may be treated differently
  if ($next_command and $next_command eq 'itemx') {
    $type = 'inter_item';
  } else {
    $type = 'table_item';
  }
  my $table_gathered = {'type' => $type,
                       'contents' => []};
  # remove everything that is not an @item/@items or before_item to 
  # put it in the table_item, starting from the end.
  my $contents_count = scalar(@{$current->{'contents'}});
  for (my $i = 0; $i < $contents_count; $i++) {
    #print STDERR "_gather_previous_item $i on $contents_count: "._print_current($current->{'contents'}->[-1])."\n";
    if ($current->{'contents'}->[-1]->{'cmdname'} 
        and ($current->{'contents'}->[-1]->{'cmdname'} eq 'item' 
             or ($current->{'contents'}->[-1]->{'cmdname'} eq 'itemx'))) {
      last;
    } else {
      my $item_content = pop @{$current->{'contents'}};
      $item_content->{'parent'} = $table_gathered;
      unshift @{$table_gathered->{'contents'}}, $item_content;
    }
  }
  if ($type eq 'table_item') {
    my $table_entry = {'type' => 'table_entry',
                    'parent' => $current,
                    'contents' => []};
    my $table_term = {'type' => 'table_term',
                    'parent' => $table_entry,
                    'contents' => []};
    push @{$table_entry->{'contents'}}, $table_term;
    my $contents_count = scalar(@{$current->{'contents'}});
    for (my $i = 0; $i < $contents_count; $i++) {
      if ($current->{'contents'}->[-1]->{'type'} 
           and ($current->{'contents'}->[-1]->{'type'} eq 'before_item'
                or $current->{'contents'}->[-1]->{'type'} eq 'table_entry')) {
        last;
      } else {
        my $item_content = pop @{$current->{'contents'}};
        $item_content->{'parent'} = $table_term;
        unshift @{$table_term->{'contents'}}, $item_content;
        # debug
        if (! (($item_content->{'cmdname'} 
                and ($item_content->{'cmdname'} eq 'itemx'
                    or $item_content->{'cmdname'} eq 'item'))
               or ($item_content->{'type'} 
                   and $item_content->{'type'} eq 'inter_item'))) {
          $self->_bug_message("wrong element in table term", $line_nr, 
                              $item_content);
        }
      }
    }
    push @{$current->{'contents'}}, $table_entry;
    if (scalar(@{$table_gathered->{'contents'}})) {
      push @{$table_entry->{'contents'}}, $table_gathered;
      $table_gathered->{'parent'} = $table_entry;
    }
  } else {
    my $after_paragraph = _check_no_text($table_gathered);
    if ($after_paragraph) {
      $self->line_error($self->__("\@itemx must follow \@item"), $line_nr);
    }
    if (scalar(@{$table_gathered->{'contents'}})) {
      push @{$current->{'contents'}}, $table_gathered;
      $table_gathered->{'parent'} = $current;
    }
  }
}

# Starting from the end, gather everything util the def_line to put in 
# a def_item
sub _gather_def_item($;$)
{
  my $current = shift;
  my $next_command = shift;
  my $type;
  # means that we are between a @def*x and a @def
  if ($next_command) {
    $type = 'inter_def_item';
  } else {
    $type = 'def_item';
  }

  # This may happen for a construct like
  # @deffnx a b @section
  # but otherwise the end of line will lead to the command closing
  return if (!$current->{'cmdname'} or $current->{'cmdname'} =~ /x$/);
  #print STDERR "_gather_def_item($type) in "._print_current($current)."\n";
  my $def_item = {'type' => $type,
                  'parent' => $current,
                  'contents' => []};
  # remove everything that is not a def_line to put it in the def_item,
  # starting from the end.
  my $contents_count = scalar(@{$current->{'contents'}});
  for (my $i = 0; $i < $contents_count; $i++) {
    #print STDERR "_gather_def_item $type ($i on $contents_count) "._print_current($current->{'contents'}->[-1])."\n";
    if ($current->{'contents'}->[-1]->{'type'} 
        and $current->{'contents'}->[-1]->{'type'} eq 'def_line') {
     #   and !$current->{'contents'}->[-1]->{'extra'}->{'not_after_command'}) {
      last;
    } else {
      my $item_content = pop @{$current->{'contents'}};
      $item_content->{'parent'} = $def_item;
      unshift @{$def_item->{'contents'}}, $item_content;
    }
  }
  if (scalar(@{$def_item->{'contents'}})) {
    push @{$current->{'contents'}}, $def_item;
  }
}

# close formats
sub _close_command_cleanup($$$) {
  my $self = shift;
  my $current = shift;

  return unless ($current->{'cmdname'});
  # remove the dynamic counters in multitable, they are not of use in the final
  # tree.  Also determine the multitable_body and multitable_head with 
  # @item or @headitem rows.
  if ($current->{'cmdname'} eq 'multitable') {
    my $in_head_or_rows;
    my @contents = @{$current->{'contents'}};
    $current->{'contents'} = [];
    foreach my $row (@contents) {
      if ($row->{'type'} and $row->{'type'} eq 'row') {
        delete $row->{'cells_count'};
        if ($row->{'contents'}->[0]->{'cmdname'} eq 'headitem') {
          if (!$in_head_or_rows) {
            push @{$current->{'contents'}}, {'type' => 'multitable_head',
                                             'parent' => $current};
            $in_head_or_rows = 1;
          }
        } elsif ($row->{'contents'}->[0]->{'cmdname'} eq 'item') {
          if (!defined($in_head_or_rows) or $in_head_or_rows) {
            push @{$current->{'contents'}}, {'type' => 'multitable_body',
                                             'parent' => $current};
            $in_head_or_rows = 0;
          }
        }
        push @{$current->{'contents'}->[-1]->{'contents'}}, $row;
        $row->{'parent'} = $current->{'contents'}->[-1];
      } else {
        push @{$current->{'contents'}}, $row;
        $in_head_or_rows = undef;
      }
    }
    delete $current->{'rows_count'};
  } elsif ($item_container_commands{$current->{'cmdname'}}) {
    delete $current->{'items_count'};
  }

  # put everything after the last @def*x command in a def_item type container.
  if ($def_commands{$current->{'cmdname'}}) {
    # At this point the end command hasn't been added to the command contents.
    # so checks cannot be done at this point.
    _gather_def_item($current);
  }

  if ($item_line_commands{$current->{'cmdname'}}) {
    # At this point the end command hasn't been added to the command contents.
    # so checks cannot be done at this point.
    if (@{$current->{'contents'}}) {
      $self->_gather_previous_item($current);
    }
  }
  
  # put end out of before_item, and replace it at the end of the parent.
  # remove empty before_item.
  # warn if not empty before_item, but format is empty
  if ($block_item_commands{$current->{'cmdname'}}) {
    if (@{$current->{'contents'}}) {
      my $leading_spaces = 0;
      my $before_item;
      if ($current->{'contents'}->[0]->{'type'}
          and $current->{'contents'}->[0]->{'type'} eq 'empty_line_after_command'
          and $current->{'contents'}->[1]
          and $current->{'contents'}->[1]->{'type'}
          and $current->{'contents'}->[1]->{'type'} eq 'before_item') {
        $leading_spaces = 1;
        $before_item = $current->{'contents'}->[1];
      } elsif ($current->{'contents'}->[0]->{'type'} 
              and $current->{'contents'}->[0]->{'type'} eq 'before_item') {
        $before_item = $current->{'contents'}->[0];
      }
      if ($before_item) {
        if ($current->{'extra'}->{'end_command'}
            and @{$before_item->{'contents'}} 
            and $before_item->{'contents'}->[-1] eq $current->{'extra'}->{'end_command'}) {
          my $end = pop @{$before_item->{'contents'}};
          $end->{'parent'} = $current;
          push @{$current->{'contents'}}, $end;
        }
        # remove empty before_items
        if (!@{$before_item->{'contents'}}) {
          if ($leading_spaces) {
            my $space = shift @{$current->{'contents'}};
            shift @{$current->{'contents'}};
            unshift @{$current->{'contents'}}, $space;
          } else {
            shift @{$current->{'contents'}};
          }
        } else {
          # warn if not empty before_item, but format is empty
          my $empty_before_item = 1;
          foreach my $before_item_content (@{$before_item->{'contents'}}) {
            if (!$before_item_content->{'cmdname'} or 
                  ($before_item_content->{'cmdname'} ne 'c' 
                   and $before_item_content->{'cmdname'} ne 'comment')) {
              $empty_before_item = 0;
              last;
            }
          }
          if (!$empty_before_item) {
            my $empty_format = 1;
            foreach my $format_content (@{$current->{'contents'}}) {
              next if ($format_content eq $before_item);
              if (($format_content->{'cmdname'} and 
                   ($format_content->{'cmdname'} ne 'c'
                    and $format_content->{'cmdname'} ne 'comment'
                    and $format_content->{'cmdname'} ne 'end'))
                  or ($format_content->{'type'} and
                    ($format_content->{'type'} ne 'empty_line_after_command'))) {
                $empty_format = 0;
                last;
              }
            }
            if ($empty_format) {
              $self->line_warn(sprintf($self->__("\@%s has text but no \@item"),
                                        $current->{'cmdname'}), $current->{'line_nr'});                       
            }
          }
        }
      }
    }
  }
}

# close the current command, with error messages and give the parent.
# If the last argument is given it is the command being closed if
# hadn't there be an error, currently only block command, used for a
# better error message.
sub _close_current($$$;$$)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;
  my $closed_command = shift;
  my $interrupting_command = shift;

  if ($current->{'cmdname'}) {
    print STDERR "CLOSING(_close_current) \@$current->{'cmdname'}\n" if ($self->{'DEBUG'});
    if (exists($brace_commands{$current->{'cmdname'}})) {
      pop @{$self->{'context_stack'}}
         if (exists $context_brace_commands{$current->{'cmdname'}});
      $current = _close_brace_command($self, $current, $line_nr, 
                                      $closed_command, $interrupting_command);
    } elsif (exists($block_commands{$current->{'cmdname'}})) {
      if (defined($closed_command)) {
        $self->line_error(sprintf($self->__("`\@end' expected `%s', but saw `%s'"),
                                   $current->{'cmdname'}, $closed_command), $line_nr);
      } elsif ($interrupting_command) {
        $self->line_error(sprintf($self->__("\@%s seen before \@end %s"),
                                  $interrupting_command, $current->{'cmdname'}),
                          $line_nr);
      } else {
        $self->line_error(sprintf($self->__("no matching `%cend %s'"),
                                   ord('@'), $current->{'cmdname'}), $line_nr);
        if ($block_commands{$current->{'cmdname'}} eq 'conditional') {
          # in this case we are within an ignored conditional
          my $conditional = pop @{$current->{'parent'}->{'contents'}};
        }
      }
      if ($preformatted_commands{$current->{'cmdname'}}
          or $menu_commands{$current->{'cmdname'}}
          or $format_raw_commands{$current->{'cmdname'}}) {
        my $context = pop @{$self->{'context_stack'}};
        pop @{$self->{'expanded_formats_stack'}} 
          if ($format_raw_commands{$current->{'cmdname'}});
      }
      pop @{$self->{'regions_stack'}} 
         if ($region_commands{$current->{'cmdname'}});
      $current = $current->{'parent'};
    } else {
      # There @item and @tab commands are closed, and also line commands
      # with invalid content
      $current = $current->{'parent'};
    }
  } elsif ($current->{'type'}) {
    print STDERR "CLOSING type $current->{'type'}\n" if ($self->{'DEBUG'});
    if ($current->{'type'} eq 'bracketed') {
      $self->_command_error($current, $line_nr, 
                            $self->__("misplaced %c"), ord('{'));
    } elsif ($current->{'type'} eq 'menu_comment' 
          or $current->{'type'} eq 'menu_entry_description') {
      my $context = pop @{$self->{'context_stack'}};
      if ($context ne 'preformatted') {
        $self->_bug_message("context $context instead of preformatted", 
                            $line_nr, $current);
      }
      # close empty menu_comment
      if (!@{$current->{'contents'}}) {
        pop @{$current->{'parent'}->{'contents'}};
      }
    } elsif ($current->{'type'} eq 'misc_line_arg'
             or $current->{'type'} eq 'block_line_arg') {
      my $context = pop @{$self->{'context_stack'}};
      if ($context ne 'line' and $context ne 'def') {
        $self->_bug_message("context $context instead of line or def", 
                            $line_nr, $current);
        die;
      }
    }
    $current = $current->{'parent'};
  } else { # Should never go here.
    $current = $current->{'parent'} if ($current->{'parent'});
    $self->_bug_message("No type nor cmdname when closing", 
                        $line_nr, $current);
  }
  return $current;
}

# a closed_command arg means closing until that command is found.
# no command arg means closing until the root or a root_command
# is found.
sub _close_commands($$$;$$)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;
  my $closed_command = shift;
  my $interrupting_command = shift;;

  $current = _end_paragraph($self, $current, $line_nr, $closed_command, 
                            $interrupting_command);
  $current = _end_preformatted($self, $current, $line_nr, $closed_command,
                               $interrupting_command);

        # stop if the command is found
  while (!($closed_command and $current->{'cmdname'}
           and $current->{'cmdname'} eq $closed_command) 
         # stop if at the root
         and $current->{'parent'}
     # stop if in a root command 
     # or in a context_brace_commands and searching for a specific 
     # end block command (with $closed_command set).  
     # This second condition means that a footnote is not closed when 
     # looking for the end of a block command, but is closed when 
     # completly closing the stack.
         and !($current->{'cmdname'}
               and ($root_commands{$current->{'cmdname'}}
                    or ($closed_command and $current->{'parent'}->{'cmdname'}
                       and $context_brace_commands{$current->{'parent'}->{'cmdname'}})))){
    $self->_close_command_cleanup($current);
    $current = $self->_close_current($current, $line_nr, $closed_command, 
                                     $interrupting_command);
  }

  my $closed_element;
  if ($closed_command and $current->{'cmdname'} 
      and $current->{'cmdname'} eq $closed_command) {
    if ($preformatted_commands{$current->{'cmdname'}}) {
      my $context = pop @{$self->{'context_stack'}};
      if ($context ne 'preformatted') {
        $self->_bug_message("context $context instead of preformatted for $closed_command", 
                            $line_nr, $current);
      }
    } elsif ($format_raw_commands{$current->{'cmdname'}}) {
      my $context = pop @{$self->{'context_stack'}};
      if ($context ne 'rawpreformatted') {
        $self->_bug_message("context $context instead of rawpreformatted for $closed_command", 
                            $line_nr, $current);
      }
      pop @{$self->{'expanded_formats_stack'}};
    } elsif ($menu_commands{$current->{'cmdname'}}) {
      my $context = pop @{$self->{'context_stack'}};
      # may be in menu, but context is preformatted if in a preformatted too.
      if ($context ne 'menu' and $context ne 'preformatted') {
        $self->_bug_message("context $context instead of preformatted or menu for $closed_command", 
                            $line_nr, $current);
      }
    }
    #print STDERR "close context $context for $current->{'cmdname'}\n"
    #  if ($self->{'DEBUG'});
    pop @{$self->{'regions_stack'}} 
       if ($region_commands{$current->{'cmdname'}});
    $closed_element = $current;
    #$self->_close_command_cleanup($current);
    $current = $current->{'parent'};
  } elsif ($closed_command) {
    $self->line_error(sprintf($self->__("unmatched `%c%s'"), 
                       ord('@'), "end $closed_command"), $line_nr);
  }
  return ($closed_element, $current);
}

# begin paragraph if needed.  If not try to merge with the previous
# content if it is also some text.
sub _merge_text($$$)
{
  my $self = shift;
  my $current = shift;
  my $text = shift;

  my $paragraph;

  my $no_merge_with_following_text = 0;
  if ($text =~ /\S/) {
    my $leading_spaces;
    if ($text =~ /^(\s+)/) {
      $leading_spaces = $1;
    }
    if ($current->{'contents'} and @{$current->{'contents'}}
      and $current->{'contents'}->[-1]->{'type'}
      and ($current->{'contents'}->[-1]->{'type'} eq 'empty_line_after_command'
         or $current->{'contents'}->[-1]->{'type'} eq 'empty_spaces_before_argument'
         or $current->{'contents'}->[-1]->{'type'} eq 'empty_spaces_after_close_brace')) {
      $no_merge_with_following_text = 1;
    }
    if (_abort_empty_line($self, $current, $leading_spaces)) {
      $text =~ s/^(\s+)//;
    } 

    $paragraph = _begin_paragraph($self, $current);
    $current = $paragraph if ($paragraph);
  }

  if (!defined($current->{'contents'})) {
    $self->_bug_message("No contents in _merge_text", 
                            undef, $current);
    die;
  }

  if (@{$current->{'contents'}} 
      and exists($current->{'contents'}->[-1]->{'text'}) 
      and $current->{'contents'}->[-1]->{'text'} !~ /\n/
      and !$no_merge_with_following_text) {
    $current->{'contents'}->[-1]->{'text'} .= $text;
    print STDERR "MERGED TEXT: $text|||\n" if ($self->{'DEBUG'});
  } else {
    push @{$current->{'contents'}}, { 'text' => $text, 'parent' => $current };
    print STDERR "NEW TEXT: $text|||\n" if ($self->{'DEBUG'});
  }
  return $current;
}

# return the parent if in a item_container command, itemize or enumerate
sub _item_container_parent($)
{
  my $current = shift;
  if ((($current->{'cmdname'} and $current->{'cmdname'} eq 'item')
       or ($current->{'type'} and $current->{'type'} eq 'before_item'))
      and ($current->{'parent'} and $current->{'parent'}->{'cmdname'}
        and $item_container_commands{$current->{'parent'}->{'cmdname'}})) {
    return ($current->{'parent'});
  }
  return undef;
}

# return the parent if in a item_line command, @*table
sub _item_line_parent($)
{
  my $current = shift;
  if ($current->{'cmdname'} and ($current->{'cmdname'} eq 'item'
       or $current->{'cmdname'} eq 'itemx')) {
      $current = $current->{'parent'}->{'parent'};
  } elsif ($current->{'type'} and $current->{'type'} eq 'before_item'
            and $current->{'parent'}) {
    $current = $current->{'parent'};
  }
  return $current if ($current->{'cmdname'} 
                       and $item_line_commands{$current->{'cmdname'}});
  return undef;
}

# return the parent if in a multitable
sub _item_multitable_parent($)
{
  my $current = shift;
  if (($current->{'cmdname'} and ($current->{'cmdname'} eq 'headitem'
       or $current->{'cmdname'} eq 'item' or $current->{'cmdname'} eq 'tab'))
      and $current->{'parent'} and $current->{'parent'}->{'parent'}) {
    $current = $current->{'parent'}->{'parent'};
  } elsif ($current->{'type'} and $current->{'type'} eq 'before_item'
            and $current->{'parent'}) {
    $current = $current->{'parent'};
  }
  return $current if ($current->{'cmdname'} 
                       and $current->{'cmdname'} eq 'multitable');
  return undef;
}

# returns next text fragment, be it pending from a macro expansion or 
# text or file
sub _next_text($$$)
{
  my $self = shift;
  my $line_nr = shift;
  my $current = shift;
 
  while (@{$self->{'input'}}) {
    my $input = $self->{'input'}->[0];
    if (@{$input->{'pending'}}) {
      my $new_text = shift @{$input->{'pending'}};
      if ($new_text->[1] and $new_text->[1]->{'end_macro'}) {
        delete $new_text->[1]->{'end_macro'};
        my $top_macro = shift @{$self->{'macro_stack'}};
        print STDERR "SHIFT MACRO_STACK(@{$self->{'macro_stack'}}): $top_macro->{'args'}->[0]->{'text'}\n"
          if ($self->{'DEBUG'});
      }
      return ($new_text->[0], $new_text->[1]);
    } elsif ($input->{'fh'}) {
      my $fh = $input->{'fh'};
      my $line = <$fh>;
      while (defined($line)) {
        # add an end of line if there is none at the end of file
        if (eof($fh) and $line !~ /\n/) {
          $line .= "\n";
        }
        $line =~ s/\x{7F}.*\s*//;
        if ($self->{'CPP_LINE_DIRECTIVES'}
            # no cpp directives in ignored/macro/verbatim
            and defined ($current)
            and not 
             (($current->{'cmdname'}
              and $block_commands{$current->{'cmdname'}}
               and ($block_commands{$current->{'cmdname'}} eq 'raw'
                    or $block_commands{$current->{'cmdname'}} eq 'conditional'))
             or 
              ($current->{'parent'} and $current->{'parent'}->{'cmdname'}
               and $current->{'parent'}->{'cmdname'} eq 'verb')
             )
            and $line =~ /^\s*#\s*(line)? (\d+)(( "([^"]+)")(\s+\d+)*)?\s*$/) {
          $input->{'line_nr'} = $2;
          if (defined($5)) {
            $input->{'name'} = $5;
          }
          $line = <$fh>;
        } else {
          $input->{'line_nr'}++;
          return ($line, {'line_nr' => $input->{'line_nr'}, 
                          'file_name' => $input->{'name'},
                          'macro' => ''});
        }
      }
    }
    my $previous_input = shift(@{$self->{'input'}});
    # Don't close STDIN
    if ($previous_input->{'fh'} and $previous_input->{'name'} ne '-') {
      if (!close($previous_input->{'fh'})) {
        $self->document_warn(sprintf($self->__("error on closing %s: %s"),
                                     $previous_input->{'name'}, $!));

      }
    }
  }

  return (undef, $line_nr);
}

# collect text and line numbers until an end of line is found.
sub _new_line($$$)
{
  my $self = shift;
  my $line_nr = shift;
  my $current = shift;
  my $new_line = '';

  while (1) {
    my $new_text;
    ($new_text, $line_nr) = _next_text($self, $line_nr, $current);
    if (!defined($new_text)) {
      $new_line = undef if ($new_line eq '');
      last;
    }

    $new_line .= $new_text;

    my $chomped_text = $new_text;
    last if chomp($chomped_text);
  }
  return ($new_line, $line_nr);
}

sub _expand_macro_arguments($$$$)
{
  my $self = shift;
  my $macro = shift;
  my $line = shift;
  my $line_nr = shift;
  my $braces_level = 1;
  my $arguments = [ '' ];
  my $arg_nr = 0;
  my $args_total = scalar(@{$macro->{'args'}}) -1;
  my $name = $macro->{'args'}->[0]->{'text'};

  my $line_nr_orig = $line_nr;

  while (1) {
    if ($line =~ s/([^\\{},]*)([\\{},])//) {
      my $separator = $2;
      $arguments->[-1] .= $1;
      if ($separator eq '\\') {
        if ($line =~ s/^(.)//) {
          my $protected_char = $1;
          if ($protected_char !~ /[\\{},]/) {
            $arguments->[-1] .= '\\';
          }
          $arguments->[-1] .= $protected_char;
          
          print STDERR "MACRO ARG: $separator: $protected_char\n" if ($self->{'DEBUG'});
        } else {
          $arguments->[-1] .= '\\';
          print STDERR "MACRO ARG: $separator\n" if ($self->{'DEBUG'});
        }
      } elsif ($separator eq ',') {
        if ($braces_level == 1) {
          if (scalar(@$arguments) < $args_total) {
            push @$arguments, '';
            $line =~ s/^[^\S\f]*//;
            print STDERR "MACRO NEW ARG\n" if ($self->{'DEBUG'});
          } else {
            # implicit quoting when there is one argument.
            if ($args_total != 1) {
              $self->line_error(sprintf($self->__(
                                     "macro `%s' called with too many args"), 
                                        $name), $line_nr);
            }
            $arguments->[-1] .= ',';
          }
        } else {
          $arguments->[-1] .= ',';
        }
      } elsif ($separator eq '}') {
        $braces_level--;
        last if ($braces_level == 0);
        $arguments->[-1] .= $separator;
      } elsif ($separator eq '{') {
        $braces_level++;
        $arguments->[-1] .= $separator;
      }
    } else {
      print STDERR "MACRO ARG end of line\n" if ($self->{'DEBUG'});
      $arguments->[-1] .= $line;

      ($line, $line_nr) = _new_line($self, $line_nr, $macro);
      if (!defined($line)) {
        $self->line_error(sprintf($self->__("\@%s missing close brace"), 
           $name), $line_nr_orig);
        return ($arguments, "\n", $line_nr);
      }
    }
  }
  if ($args_total == 0 and $arguments->[0] =~ /[\S\f]/) {
    $self->line_error(sprintf($self->__(
               "macro `%s' declared without argument called with an argument"), 
                                $name), $line_nr);
  }
  print STDERR "END MACRO ARGS EXPANSION(".scalar(@$arguments)."): ".
                  join("|\n", @$arguments) ."|\n" if ($self->{'DEBUG'});
  return ($arguments, $line, $line_nr);
}

sub _expand_macro_body($$$$) {
  my $self = shift;
  my $macro = shift;
  my $args = shift;
  my $line_nr = shift;

  my $macrobody = $macro->{'extra'}->{'macrobody'};
  my $args_total = scalar(@{$macro->{'args'}}) -1;
  my $args_index = $macro->{'extra'}->{'args_index'};

  my $i;
  for ($i=0; $i<=$args_total; $i++) {
    $args->[$i] = "" unless (defined($args->[$i]));
  }

  my $result = '';
  while ($macrobody ne '') {
    if ($macrobody =~ s/^([^\\]*)\\//o) {
      $result .= $1;
      if ($macrobody =~ s/^\\//) {
        $result .= '\\';
      } elsif ($macrobody =~ s/^([^\\]*)\\//) {
        my $arg = $1;
        if (defined($args_index->{$arg})) {
          $result .= $args->[$args_index->{$arg}];
        } else {
          $self->line_error(sprintf($self->__(
         "\\ in \@%s expansion followed `%s' instead of parameter name or \\"), 
                             $macro->{'args'}->[0]->{'text'}, $arg), $line_nr);
          $result .= '\\' . $arg;
        }
      }
      next;
    }
    $result .= $macrobody;
    last;
  }
  return $result;
}

# each time a new line appeared, a container is opened to hold the text
# consisting only of spaces.  This container is removed here, typically
# this is called when non-space happens on a line.
sub _abort_empty_line($$;$)
{
  my $self = shift;
  my $current = shift;
  my $additional_text = shift;
  $additional_text = '' if (!defined($additional_text));
  if ($current->{'contents'} and @{$current->{'contents'}} 
       and $current->{'contents'}->[-1]->{'type'}
       and ($current->{'contents'}->[-1]->{'type'} eq 'empty_line' 
           or $current->{'contents'}->[-1]->{'type'} eq 'empty_line_after_command'
           or $current->{'contents'}->[-1]->{'type'} eq 'empty_spaces_before_argument'
           or $current->{'contents'}->[-1]->{'type'} eq 'empty_spaces_after_close_brace')) {
    print STDERR "ABORT EMPTY additional text |$additional_text|, current |$current->{'contents'}->[-1]->{'text'}|\n" if ($self->{'DEBUG'});
    $current->{'contents'}->[-1]->{'text'} .= $additional_text;
    # remove empty 'empty*before'.
    if ($current->{'contents'}->[-1]->{'text'} eq '') {
      # as we remove 'empty_spaces_before_argument', 'spaces_before_argument'
      # is removed from 'extra' too.
      if ($current->{'extra'} 
          and $current->{'extra'}->{'spaces_before_argument'}
          and $current->{'extra'}->{'spaces_before_argument'} 
                eq $current->{'contents'}->[-1]) {
        delete ($current->{'extra'}->{'spaces_before_argument'});
        delete ($current->{'extra'}) if !(keys(%{$current->{'extra'}}));
      }
      pop @{$current->{'contents'}} 
    } elsif ($current->{'contents'}->[-1]->{'type'} eq 'empty_line') {
      # exactly the same condition than to begin a paragraph
      if ((!$current->{'type'} or $type_with_paragraph{$current->{'type'}})
         and !$no_paragraph_contexts{$self->{'context_stack'}->[-1]}) {
        $current->{'contents'}->[-1]->{'type'} = 'empty_spaces_before_paragraph';
      } else { 
        delete $current->{'contents'}->[-1]->{'type'};
      }
    } elsif ($current->{'contents'}->[-1]->{'type'} eq 'empty_line_after_command') {
      $current->{'contents'}->[-1]->{'type'} = 'empty_spaces_after_command';
    }
    return 1;
  }
  return 0;
}

# isolate last space in a command to help expansion disregard unuseful spaces.
sub _isolate_last_space($$;$)
{
  my $self = shift;
  my $current = shift;
  my $type = shift;
  $type = 'spaces_at_end' if (!defined($type));
  if ($current->{'contents'} and @{$current->{'contents'}}) {
    my $index = -1;
    # we ignore space before a misc command that is last on line.
    # This is primarily to tag spaces before comments, but this will
    # also tag and, in most converter lead to removal of spaces
    # before any misc command, which is not really problematic as 
    # in most cases, if it is not a comment, we are in an invalid 
    # nesting of misc command on another @-command line.
    $index = -2 
      if (scalar(@{$current->{'contents'}}) > 1 
        and $current->{'contents'}->[-1]->{'cmdname'}
        and $self->{'misc_commands'}->{$current->{'contents'}->[-1]->{'cmdname'}});
    if (defined($current->{'contents'}->[$index]->{'text'}) 
        and !$current->{'contents'}->[$index]->{'type'}
        and $current->{'contents'}->[$index]->{'text'} =~ /\s+$/) {
      if ($current->{'contents'}->[$index]->{'text'} !~ /\S/) {
        $current->{'contents'}->[$index]->{'type'} = $type;
      } else {
        $current->{'contents'}->[$index]->{'text'} =~ s/(\s+)$//;
        my $spaces = $1;
        my $new_spaces = { 'text' => $spaces, 'parent' => $current,
                           'type' => $type };
        if ($index == -1) {
          push @{$current->{'contents'}}, $new_spaces;
        } else {
          splice (@{$current->{'contents'}}, $index+1, 0, $new_spaces);
        }
      }
    }
  }
}

# used to put a node name in error messages.
sub _node_extra_to_texi($)
{
  my $node = shift;
  my $result = '';
  if ($node->{'manual_content'}) {
    $result = '('.Texinfo::Convert::Texinfo::convert({'contents'
                                     => $node->{'manual_content'}}) .')';
  }
  if ($node->{'node_content'}) {
    $result .= Texinfo::Convert::Texinfo::convert ({'contents'
                                          => $node->{'node_content'}});
  }
  return $result;
}

sub _find_end_brace($$)
{
  my $text = shift;
  my $braces_count = shift;

  my $before = '';
  while ($braces_count > 0 and length($text)) {
    if ($text =~ s/([^()]*)([()])//) {
      $before .= $1.$2;
      my $brace = $2;
      if ($brace eq '(') {
        $braces_count++;
      } else {
        $braces_count--;
        if ($braces_count == 0) {
          return ($before, $text, 0);
        }
      }
    } else {
      $before .= $text;
      $text = '';
    }
  }
  return ($before, undef, $braces_count);
}

# This only counts opening braces, and returns 0 once all the parentheses
# are closed
sub _count_opened_tree_braces($$);
sub _count_opened_tree_braces($$)
{
  my $current = shift;
  my $braces_count = shift;
  if (defined($current->{'text'})) {
    my ($before, $after);
    ($before, $after, $braces_count) = _find_end_brace($current->{'text'},
                                                          $braces_count);
  }
  if ($current->{'args'}) {
    foreach my $arg (@{$current->{'args'}}) {
      $braces_count = _count_opened_tree_braces($arg, $braces_count);
      return $braces_count if ($braces_count == 0);
    }
  }
  if ($current->{'contents'}) {
    foreach my $content (@{$current->{'contents'}}) {
      $braces_count = _count_opened_tree_braces($content, $braces_count);
      return $braces_count if ($braces_count == 0);
    }
  }
  return $braces_count;
}

# retrieve a leading manual name in parentheses, if there is one.
sub _parse_node_manual($)
{
  my $node = shift;
  my @contents = @{$node->{'contents'}};
  _trim_spaces_comment_from_content(\@contents);

  my $manual;
  my $result;
#print STDERR "RRR $contents[0] and $contents[0]->{'text'} \n";
  if ($contents[0] and $contents[0]->{'text'} and $contents[0]->{'text'} =~ /^\(/) {
    my $braces_count = 1;
    if ($contents[0]->{'text'} !~ /^\($/) {
      my $brace = shift @contents;
      my $brace_text = $brace->{'text'};
      $brace_text =~ s/^\(//;
      unshift @contents, { 'text' => $brace_text, 'type' => $brace->{'type'},
                           'parent' => $brace->{'parent'} } if $brace_text ne '';
    } else {
      shift @contents;
    }
    while(@contents) {
      my $content = shift @contents;
      if (!defined($content->{'text'}) or $content->{'text'} !~ /\)/) {
        push @$manual, $content;
        $braces_count = _count_opened_tree_braces($content, $braces_count);
        # This is an error, braces were closed in a command
        if ($braces_count == 0) {
          last;
        }
      } else {
        my ($before, $after);
        ($before, $after, $braces_count) = _find_end_brace($content->{'text'},
                                                              $braces_count);
        if ($braces_count == 0) {
          $before =~ s/\)$//;
          push @$manual, { 'text' => $before, 'parent' => $content->{'parent'} }
            if ($before ne '');
          $after =~ s/^\s*//;
          unshift @contents,  { 'text' => $after, 'parent' => $content->{'parent'} }
            if ($after ne '');
          last;
        } else {
          push @$manual, $content;
        }
      }
    }
    $result->{'manual_content'} = $manual if (defined($manual));
  }
  if (@contents) {
    $result->{'node_content'} = \@contents;
    $result->{'normalized'} =
      Texinfo::Convert::NodeNameNormalization::normalize_node({'contents' => \@contents});
  }
  return $result;
}

sub _parse_float_type($)
{
  my $current = shift;
  if (@{$current->{'args'}}) {
    my @type_contents = @{$current->{'args'}->[0]->{'contents'}};
    _trim_spaces_comment_from_content(\@type_contents);
    if (@type_contents) {
      my $normalized 
        = Texinfo::Convert::NodeNameNormalization::normalize_node(
                                               {'contents' => \@type_contents});
      $current->{'extra'}->{'type'}->{'content'} = \@type_contents;
      if ($normalized =~ /[^-]/) {
        $current->{'extra'}->{'type'}->{'normalized'} = $normalized;
        return 1;
      }
    }
  }
  $current->{'extra'}->{'type'}->{'normalized'} = '';
  return 0;
}

# used for definition line parsing
sub _next_bracketed_or_word($$)
{
  my $self = shift;
  my $contents = shift;
  return undef if (!scalar(@{$contents}));
  my $spaces;
  $spaces = shift @{$contents} if (defined($contents->[0]->{'text'}) and 
                                     $contents->[0]->{'text'} !~ /\S/);
  if (defined($spaces)) {
    $spaces->{'type'} = 'spaces';
    chomp $spaces->{'text'};
    $spaces = undef if ($spaces->{'text'} eq '');
  }
  return ($spaces, undef) if (!scalar(@{$contents}));

  #print STDERR "BEFORE PROCESSING ".Texinfo::Convert::Texinfo::convert({'contents' => $contents});
  if ($contents->[0]->{'type'} and $contents->[0]->{'type'} eq 'bracketed') {
    #print STDERR "Return bracketed\n";
    my $bracketed = shift @{$contents};
    $self->_isolate_last_space($bracketed, 'empty_space_at_end_def_bracketed');
    my $bracketed_def_content = { 'contents' => $bracketed->{'contents'},
                                  'parent' => $bracketed->{'parent'},
                                  'type' => 'bracketed_def_content', };
    if ($bracketed->{'extra'} and $bracketed->{'extra'}->{'spaces_before_argument'}) {
      $bracketed_def_content->{'extra'}->{'spaces_before_argument'}
        = $bracketed->{'extra'}->{'spaces_before_argument'};
    }
    return ($spaces, $bracketed_def_content);
  } elsif ($contents->[0]->{'cmdname'}) {
    #print STDERR "Return command $contents->[0]->{'cmdname'}\n";
    return ($spaces, shift @{$contents});
  } else {
    #print STDERR "Process $contents->[0]->{'text'}\n";
    $contents->[0]->{'text'} =~ s/^(\s*)//;
    my $space_text = $1;
    $spaces = {'text' => $space_text, 'type' => 'spaces'} if ($space_text);
    $contents->[0]->{'text'} =~ s/^(\S+)//;
    shift @{$contents} if ($contents->[0]->{'text'} eq '');
    return ($spaces, {'text' => $1});
  }
}

# definition line parsing
sub _parse_def($$$)
{
  my $self = shift;
  my $command = shift;
  my $contents = shift;
  
  my @contents = @$contents;
  shift @contents if ($contents[0] and $contents[0]->{'type'}
                    and $contents[0]->{'type'} eq 'empty_spaces_after_command');
  if ($def_aliases{$command}) {
    my $real_command = $def_aliases{$command};
    my $prepended = $def_map{$command}->{$real_command};
    my @prepended_content;

    my $text;
    my $in_bracketed;
    if ($prepended =~ /^\{/) {
      $text = $prepended;
      $text =~ s/\{([^\}]+)\}/$1/;
      $in_bracketed = 1;
    } else {
      $text = $prepended;
    }
    my $tree = $self->gdt($text);
    if ($in_bracketed or @{$tree->{'contents'}} > 1) {
      my $bracketed = { 'type' => 'bracketed' };
      $bracketed->{'contents'} = $tree->{'contents'};
      foreach my $content (@{$tree->{'contents'}}) {
        $content->{'parent'} = $bracketed;
      }
      @prepended_content = ($bracketed);
    } else {
      @prepended_content = (@{$tree->{'contents'}});
    }
    push @prepended_content, { 'text' => ' ' };

    unshift @contents, @prepended_content;

    $command = $def_aliases{$command};
  }
  foreach (my $i = 0; $i < scalar(@contents); $i++) {
    # copy, to avoid changing the original
    $contents[$i] = {'text' => $contents[$i]->{'text'}} 
       if (defined($contents[$i]->{'text'}));
  }
  my @result;
  my @args = @{$def_map{$command}};
  my $arg_type;
  # Even when $arg_type is not set, that is for def* that is not documented
  # to take args, everything is as is arg_type was set to arg.
  $arg_type = pop @args if ($args[-1] eq 'arg' or $args[-1] eq 'argtype');
  foreach my $arg (@args) {
    #print STDERR "$command $arg"._print_current($contents[0]);
    #foreach my $content (@contents) {print STDERR " "._print_current($content)};
    #print STDERR " contents ->".Texinfo::Convert::Texinfo::convert ({'contents' => \@contents});
    my ($spaces, $next) = $self->_next_bracketed_or_word(\@contents);
    last if (!defined($next));
    #print STDERR "NEXT[$arg] ".Texinfo::Convert::Texinfo::convert($next)."\n";
    push @result, ['spaces', $spaces] if (defined($spaces));
    push @result, [$arg, $next];
  }

  my @args_results;
  while (@contents) {
    my ($spaces, $next) = $self->_next_bracketed_or_word(\@contents);
    push @args_results, ['spaces', $spaces] if (defined($spaces));
    last if (!defined($next));
    if (defined($next->{'text'})) {
      while (1) {
        if ($next->{'text'} =~ s/^([^\[\](),]+)//) {
          push @args_results, ['arg', {'text' => $1}];
        } elsif ($next->{'text'} =~ s/^([\[\](),])//) {
          push @args_results, ['delimiter', 
                      {'text' => $1, 'type' => 'delimiter'}];
        } else {
          last;
        }
      }
    } else {
      push @args_results, [ 'arg', $next ];
    }
  }
  if ($arg_type and $arg_type eq 'argtype') {
    my $next_is_type = 1;
    foreach my $arg(@args_results) {
      if ($arg->[0] eq 'spaces') {
      } elsif ($arg->[0] eq 'delimiter') {
        $next_is_type = 1;
      } elsif ($arg->[1]->{'cmdname'} and $arg->[1]->{'cmdname'} ne 'code') {
        $next_is_type = 1;
      } elsif ($next_is_type) {
        $arg->[0] = 'typearg';
        $next_is_type = 0;
      } else {
        $next_is_type = 1;
      }
    }
  }
  return [@result, @args_results];
}

# register a label, that is something that may be the target of a reference
# and must be unique in the document.  Corresponds with @node, @anchor and 
# @float second arg.
sub _register_label($$$$)
{
  my $self = shift;
  my $current = shift;
  my $label = shift;
  my $line_nr = shift;
  my $normalized = $label->{'normalized'};
  if (_ignore_global_commands($self)) {
    $current->{'extra'}->{'normalized'} = $normalized;
    $current->{'extra'}->{'node_content'} = $label->{'node_content'};
    return 0;
  } elsif ($self->{'labels'}->{$normalized}) {
    $self->line_error(sprintf($self->__("\@%s `%s' previously defined"), 
                         $current->{'cmdname'}, 
                   Texinfo::Convert::Texinfo::convert({'contents' => 
                                                $label->{'node_content'}})), 
                           $line_nr);
    $self->line_error(sprintf($self->__("here is the previous definition as \@%s"),
                               $self->{'labels'}->{$normalized}->{'cmdname'}),
                       $self->{'labels'}->{$normalized}->{'line_nr'}, 1);
    return 0;
  } else {
    $current->{'extra'}->{'normalized'} = $normalized;
    $current->{'extra'}->{'node_content'} = $label->{'node_content'};
    $self->{'labels'}->{$normalized} = $current;
    return 1;
  }
}

sub _non_bracketed_contents($)
{
  my $current = shift;
  if ($current->{'type'} and $current->{'type'} eq 'bracketed') {
    my $new = {};
    $new->{'contents'} = $current->{'contents'} if ($current->{'parent'});
    $new->{'parent'} = $current->{'parent'} if ($current->{'parent'});
    return $new;
  } else {
    return $current;
  }
}

# store an index entry.
# $current is the command element.
# $content holds the actual content.
# for index entries and v|ftable items, it is the index entry content, 
# for def, it is the parsed arguments, based on the definition line 
# arguments.
sub _enter_index_entry($$$$$$$)
{
  my $self = shift;
  my $command_container = shift;
  my $command = shift;
  my $current = shift;
  my $content = shift;
  my $content_normalized = shift;
  my $line_nr = shift;

  $content_normalized = $content if (!defined($content_normalized));

  my $prefix = $self->{'command_index_prefix'}->{$command_container};
  my $index_name = $self->{'prefix_to_index_name'}->{$prefix};
  my $index = $self->{'index_names'}->{$index_name};

  my $number;
  unless (_ignore_global_commands($self)) {
    $number = (defined($index->{'index_entries'})
                 ? (scalar(@{$index->{'index_entries'}}) + 1)
                   : 1);
  }
  my $index_entry = { 'index_name'           => $index_name,
                      'index_at_command'     => $command,
                      'index_type_command'   => $command_container,
                      'index_prefix'         => $prefix,
                      'content'              => $content,
                      'content_normalized'   => $content_normalized,
                      'command'              => $current,
                      'number'               => $number,
                    };
  if (@{$self->{'regions_stack'}}) {
    $index_entry->{'region'} = $self->{'regions_stack'}->[-1];
  } elsif ($self->{'current_node'}) {
    $index_entry->{'node'} = $self->{'current_node'};
  } elsif (!$self->{'current_section'}) {
    $self->line_warn(sprintf($self->__("entry for index `%s' outside of any node"), 
                             $index_name), $line_nr);
  }
  #print STDERR "INDEX ENTRY \@$command->{'cmdname'} $index_name($number)\n";
  unless (_ignore_global_commands($self)) {
    push @{$index->{'index_entries'}}, $index_entry;
  }
  $current->{'extra'}->{'index_entry'} = $index_entry;
}

# This is always called at command closing.
sub _remove_empty_content_arguments($)
{
  my $current = shift;
  my $type;
  if ($current->{'extra'}) {
    if ($current->{'extra'}->{'block_command_line_contents'}) {
      $type = 'block_command_line_contents';
    } elsif ($current->{'extra'}->{'brace_command_contents'}) {
      $type = 'brace_command_contents';
    }
  }
  if ($type) {
    while (@{$current->{'extra'}->{$type}} 
           and not defined($current->{'extra'}->{$type}->[-1])) {
      pop @{$current->{'extra'}->{$type}}; 
    }
    delete $current->{'extra'}->{$type} if (!@{$current->{'extra'}->{$type}});
    delete $current->{'extra'} if (!keys(%{$current->{'extra'}}));
  }
}

sub _strip_macrobody_leading_space($$)
{
  my $self = shift;
  my $text = shift;
  if ($self->{'MACRO_BODY_IGNORES_LEADING_SPACE'}) {
    $text =~ s/^\s*//mg;
  }
  return $text;
}

# close constructs and do stuff at end of line (or end of the document)
sub _end_line($$$);
sub _end_line($$$)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;

  my $current_old = $current;

  my $included_file = 0;

  # a line consisting only of spaces.
  if ($current->{'contents'} and @{$current->{'contents'}} 
      and $current->{'contents'}->[-1]->{'type'} 
      and $current->{'contents'}->[-1]->{'type'} eq 'empty_line') {
    print STDERR "END EMPTY LINE\n" if ($self->{'DEBUG'});
    if ($current->{'type'} and $current->{'type'} eq 'paragraph') {
      my $empty_line = pop @{$current->{'contents'}};
      $current = _end_paragraph($self, $current, $line_nr);
      push @{$current->{'contents'}}, $empty_line;
      $empty_line->{'parent'} = $current;
    } elsif ($current->{'type'}
             and $current->{'type'} eq 'preformatted'
             and $current->{'parent'}->{'type'}
             and $current->{'parent'}->{'type'} eq 'menu_entry_description')  {
      my $empty_line = pop @{$current->{'contents'}};
      if ($current->{'type'} eq 'preformatted') {
        my $empty_preformatted = (!@{$current->{'contents'}});
        $current = $current->{'parent'};
        pop @{$current->{'contents'}} if ($empty_preformatted);
      }
      my $context = pop @{$self->{'context_stack'}};
      if ($context ne 'preformatted') {
        $self->_bug_message("context $context instead of preformatted in empty line after menu_entry_description", 
                            $line_nr, $current);
      }
      
      # first parent is menu_entry
      $current = $current->{'parent'}->{'parent'};
      
      push @{$current->{'contents'}}, { 'type' => 'menu_comment',
                                        'parent' => $current,
                                        'contents' => [] };
      $current = $current->{'contents'}->[-1];
      push @{$current->{'contents'}}, { 'type' => 'preformatted',
                                        'parent' => $current,
                                        'contents' => [] };
      $current = $current->{'contents'}->[-1];
      push @{$current->{'contents'}}, { 'type' => 'after_description_line', 
                                        'text' => $empty_line->{'text'},
                                        'parent' => $current };
      push @{$self->{'context_stack'}}, 'preformatted';
      print STDERR "MENU: END DESCRIPTION, OPEN COMMENT\n" if ($self->{'DEBUG'});
    } elsif (!$no_paragraph_contexts{$self->{'context_stack'}->[-1]}) {
      $current = _end_paragraph($self, $current, $line_nr);
    }

  # end of a menu line.
  } elsif ($current->{'type'} 
    and ($current->{'type'} eq 'menu_entry_name'
     or $current->{'type'} eq 'menu_entry_node')) {
    my $empty_menu_entry_node = 0;
    my $end_comment;
    if ($current->{'type'} eq 'menu_entry_node') {
      if (@{$current->{'contents'}} 
          and $current->{'contents'}->[-1]->{'cmdname'}
          and ($current->{'contents'}->[-1]->{'cmdname'} eq 'c' 
            or $current->{'contents'}->[-1]->{'cmdname'} eq 'comment')) {
        $end_comment = pop @{$current->{'contents'}};
      }
      if (!@{$current->{'contents'}} 
           # empty if only the end of line or spaces
           or (@{$current->{'contents'}} == 1 
               and defined($current->{'contents'}->[-1]->{'text'})
               and $current->{'contents'}->[-1]->{'text'} !~ /\S/)) {
        $empty_menu_entry_node = 1;
        push @{$current->{'contents'}}, $end_comment if ($end_comment);
      }
    }
    # we abort the menu entry if there is no node name
    if ($empty_menu_entry_node 
          or $current->{'type'} eq 'menu_entry_name') {
      my $description_or_menu_comment;
      print STDERR "FINALLY NOT MENU ENTRY\n" if ($self->{'DEBUG'});
      my $menu = $current->{'parent'}->{'parent'};
      my $menu_entry = pop @{$menu->{'contents'}};
      if (@{$menu->{'contents'}} and $menu->{'contents'}->[-1]->{'type'}
         and $menu->{'contents'}->[-1]->{'type'} eq 'menu_entry') {
        my $entry = $menu->{'contents'}->[-1];
        my $description;
        foreach my $entry_element (reverse(@{$entry->{'args'}})) {
          if ($entry_element->{'type'} eq 'menu_entry_description') {
            $description = $entry_element;
            last;
          }
        }
        if ($description) {
          $description_or_menu_comment = $description;
        } else {
          # Normally this cannot happen
          $self->_bug_message("No description in menu_entry", 
                               $line_nr, $current);
          push @{$entry->{'args'}}, {'type' => 'menu_entry_description',
                                     'parent' => $entry,
                                     'contents' => [] };
          $description_or_menu_comment = $entry->{'args'}->[-1];
        }
      } elsif (@{$menu->{'contents'}} and $menu->{'contents'}->[-1]->{'type'}
         and $menu->{'contents'}->[-1]->{'type'} eq 'menu_comment') {
        $description_or_menu_comment = $menu->{'contents'}->[-1];
      }
      if ($description_or_menu_comment) {
        $current = $description_or_menu_comment;
        if ($current->{'contents'}->[-1] and $current->{'contents'}->[-1]->{'type'}
          and $current->{'contents'}->[-1]->{'type'} eq 'preformatted') {
          $current = $current->{'contents'}->[-1];
        } else {
          # this should not happen
          $self->_bug_message("description or menu comment not in preformatted", 
                               $line_nr, $current);
          push @{$current->{'contents'}}, {'type' => 'preformatted',
                                    'parent' => $current,
                                    'contents' => [] };
          $current = $current->{'contents'}->[-1];
        }
        push @{$self->{'context_stack'}}, 'preformatted';
      } else {
        push @{$menu->{'contents'}}, {'type' => 'menu_comment',
                                    'parent' => $menu,
                                    'contents' => [] };
        $current = $menu->{'contents'}->[-1];
        push @{$current->{'contents'}}, {'type' => 'preformatted',
                                  'parent' => $current,
                                  'contents' => [] };
        $current = $current->{'contents'}->[-1];
        push @{$self->{'context_stack'}}, 'preformatted';
        print STDERR "THEN MENU_COMMENT OPEN\n" if ($self->{'DEBUG'});
      }
      while (@{$menu_entry->{'args'}}) {
        my $arg = shift @{$menu_entry->{'args'}};
        if (defined($arg->{'text'})) {
          $current = _merge_text($self, $current, $arg->{'text'});
        } else {
          while (@{$arg->{'contents'}}) {
            my $content = shift @{$arg->{'contents'}};
            if (defined($content->{'text'})) {
              $current = _merge_text($self, $current, 
                                    $content->{'text'});
              $content = undef;
            } else {
              $content->{'parent'} = $current;
              push @{$current->{'contents'}}, $content;
            }
          }
        }
        $arg = undef;
      }
      # MENU_COMMENT open
      $menu_entry = undef;
    } else {
      print STDERR "MENU ENTRY END LINE\n" if ($self->{'DEBUG'});
      $current = $current->{'parent'};
      $current = _enter_menu_entry_node($self, $current, $line_nr);
      if (defined($end_comment)) {
        $end_comment->{'parent'} = $current;
        push @{$current->{'contents'}}, $end_comment;
      }
    }
  # def line
  } elsif ($current->{'parent'}
            and $current->{'parent'}->{'type'}
            and $current->{'parent'}->{'type'} eq 'def_line') {
    my $def_context = pop @{$self->{'context_stack'}};
    if ($def_context ne 'def') {
      $self->_bug_message("context $def_context instead of def", 
                          $line_nr, $current);
      die; 
    }
    my $def_command = $current->{'parent'}->{'extra'}->{'def_command'};
    my $arguments = $self->_parse_def($def_command, 
                                      $current->{'contents'});
    if (scalar(@$arguments)) {
      $current->{'parent'}->{'extra'}->{'def_args'} = $arguments;
      my $def_parsed_hash;
      foreach my $arg (@$arguments) {
        die if (!defined($arg->[0]));
        last if ($arg->[0] eq 'arg' or $arg->[0] eq 'typearg' 
                  or $arg->[0] eq 'delimiter');
        next if ($arg->[0] eq 'spaces');
        # change of type is done in _parse_def.
        #if ($arg->[1]->{'type'} and $arg->[1]->{'type'} eq 'bracketed') {
        #  $def_parsed_hash->{$arg->[0]} = { 'contents' => $arg->[1]->{'contents'},
        #                                    'type' => 'bracketed_def_content',
        #                                    'parent' => $arg->[1]->{'parent'}};
        #} else {
        #  $def_parsed_hash->{$arg->[0]} = $arg->[1];
        #}
        $def_parsed_hash->{$arg->[0]} = $arg->[1];
      }
      $current->{'parent'}->{'extra'}->{'def_parsed_hash'} = $def_parsed_hash;
      # do an standard index entry tree
      my $index_entry;
      if (defined($def_parsed_hash->{'name'})) {
        $index_entry = $def_parsed_hash->{'name'}
         # empty bracketed
          unless ($def_parsed_hash->{'name'}->{'type'}
                  and $def_parsed_hash->{'name'}->{'type'} eq 'bracketed_def_content'
                  and (!$def_parsed_hash->{'name'}->{'contents'}
                       or (!scalar(@{$def_parsed_hash->{'name'}->{'contents'}}))
                       or (scalar(@{$def_parsed_hash->{'name'}->{'contents'}}) == 1
                          and defined($def_parsed_hash->{'name'}->{'contents'}->[0]->{'text'})
                          and $def_parsed_hash->{'name'}->{'contents'}->[0]->{'text'} !~ /\S/)));
      }
      if (defined($index_entry)) {
        my $index_contents_normalized;
        if ($def_parsed_hash->{'class'}) {
          if ($command_index_prefix{$def_command} eq 'f') {
            $index_entry = $self->gdt('{name} on {class}', 
                                  {'name' => $def_parsed_hash->{'name'},
                                   'class' => $def_parsed_hash->{'class'}});
           $index_contents_normalized
             = [_non_bracketed_contents($def_parsed_hash->{'name'}),
                { 'text' => ' on '},
                _non_bracketed_contents($def_parsed_hash->{'class'})];
          } elsif ($command_index_prefix{$def_command} eq 'v'
                  and $def_command ne 'defcv') {
            $index_entry = $self->gdt('{name} of {class}', 
                                     {'name' => $def_parsed_hash->{'name'},
                                     'class' => $def_parsed_hash->{'class'}});
            $index_contents_normalized
              = [_non_bracketed_contents($def_parsed_hash->{'name'}),
                 { 'text' => ' of '},
                 _non_bracketed_contents($def_parsed_hash->{'class'})];
          }
        }
        $index_contents_normalized = [$index_entry]
          if (!defined($index_contents_normalized));
        my $index_contents;
        # 'root_line' is the container returned by gdt.
        if ($index_entry->{'type'} and $index_entry->{'type'} eq 'root_line') {
          $index_contents = $index_entry->{'contents'};
        } else {
          $index_contents = [$index_entry];
        }
        _enter_index_entry($self, 
          $current->{'parent'}->{'extra'}->{'def_command'},
          $current->{'parent'}->{'extra'}->{'original_def_cmdname'},
          $current->{'parent'}, $index_contents, 
          $index_contents_normalized, $line_nr);
      } else {
        $self->_command_warn($current->{'parent'}, $line_nr, 
           $self->__('missing name for @%s'),
           $current->{'parent'}->{'extra'}->{'original_def_cmdname'});
      }
    } else {
      $self->_command_warn($current->{'parent'}, $line_nr, 
         $self->__('missing category for @%s'),
         $current->{'parent'}->{'extra'}->{'original_def_cmdname'});
    }
    $current = $current->{'parent'}->{'parent'};
    $current = $self->_begin_preformatted($current);

  # other block command lines
  } elsif ($current->{'type'}
            and $current->{'type'} eq 'block_line_arg') {
    my $empty_text;
    my $context = pop @{$self->{'context_stack'}};
    if ($context ne 'line') {
      $self->_bug_message("context $context instead of line in block_line_arg", 
                          $line_nr, $current);
    }
    # @multitable args
    if ($current->{'parent'}->{'cmdname'}
               and $current->{'parent'}->{'cmdname'} eq 'multitable') {
      # parse the prototypes and put them in a special arg
      my @prototype_row;
      # do the same but keeping spaces information
      my @prototype_line;
      foreach my $content (@{$current->{'contents'}}) {
        if ($content->{'type'} and $content->{'type'} eq 'bracketed') {
          push @prototype_row, { 'contents' => $content->{'contents'},
                                 'parent' => $content->{'parent'},
                                 'type' => 'bracketed_multitable_prototype'};
          push @prototype_line, $content;
        } elsif ($content->{'text'}) {
          if ($content->{'text'} =~ /\S/) {
            foreach my $prototype (split /\s+/, $content->{'text'}) {
              push @prototype_row, { 'text' => $prototype, 
                            'type' => 'row_prototype' } unless ($prototype eq '');
            }
          }
          # The regexp breaks between characters, with a non space followed
          # by a space or a space followed by non space.  It is like \b, but
          # for \s \S, and not \w \W.
          foreach my $prototype_or_space (split /(?<=\S)(?=\s)|(?=\S)(?<=\s)/, 
                                                          $content->{'text'}) {
            if ($prototype_or_space =~ /\S/) {
              push @prototype_line, {'text' => $prototype_or_space,
                                     'type' => 'row_prototype' };
            } elsif ($prototype_or_space =~ /\s/) {
              push @prototype_line, {'text' => $prototype_or_space,
                                     'type' => 'prototype_space' };
            }
          }
        } else {
          # FIXME could this happen?  Should be a debug message?
          if (!$content->{'cmdname'}) { 
            $self->_command_warn($current, $line_nr, 
                $self->__("unexpected argument on \@%s line: %s"),
                   $current->{'cmdname'}, 
         Texinfo::Convert::Texinfo::convert({ $content->{'contents'} }));
          } elsif ($content->{'cmdname'} eq 'c' 
                   or $content->{'cmdname'} eq 'comment') {
          } else {
            push @prototype_row, $content;
            push @prototype_line, $content;
          }
        }
      }
      
      my $multitable = $current->{'parent'};
      $multitable->{'extra'}->{'max_columns'} = scalar(@prototype_row);
      if (!scalar(@prototype_row)) {
        $self->_command_warn($multitable, $line_nr, 
                             $self->__("empty multitable"));
      }
      $multitable->{'extra'}->{'prototypes'} = \@prototype_row;
      $multitable->{'extra'}->{'prototypes_line'} = \@prototype_line;

    } else {
      $self->_isolate_last_space($current, 'space_at_end_block_command');
      $self->_register_command_arg($current, 'block_command_line_contents');
    } 
    # @float args
    if ($current->{'parent'}->{'cmdname'}
               and $current->{'parent'}->{'cmdname'} eq 'float') {
      my $float = $current->{'parent'};
      $float->{'line_nr'} = $line_nr;
      my $type = '';
      if (@{$float->{'args'}}) {
        if ($float->{'args'}->[1]) {
          my $float_label = _parse_node_manual($float->{'args'}->[1]);
          _check_internal_node($self, $float_label, $line_nr);
          if (defined($float_label) and $float_label->{'node_content'}
             and $float_label->{'normalized'} =~ /[^-]/) {
            _register_label($self, $float, $float_label, $line_nr);
          }
        }
        _parse_float_type($float);
        $type = $float->{'extra'}->{'type'}->{'normalized'};
      }
      push @{$self->{'floats'}->{$type}}, $float
        unless (_ignore_global_commands($self));
      $float->{'float_section'} = $self->{'current_section'} 
        if (defined($self->{'current_section'}));
    }
    $current = $current->{'parent'};
    delete $current->{'remaining_args'};
    # don't consider empty argument of block @-commands as argument,
    # reparent them as contents
    if ($current->{'args'}->[0]->{'contents'}->[0] 
         and $current->{'args'}->[0]->{'contents'}->[0]->{'type'}
         and $current->{'args'}->[0]->{'contents'}->[0]->{'type'} eq 'empty_line_after_command')
    {
      my $empty_text = $current->{'args'}->[0]->{'contents'}->[0];
      $empty_text->{'parent'} = $current;
      unshift @{$current->{'contents'}}, $empty_text;
      delete $current->{'args'};
    }
    # Additionally, remove empty arguments as far as possible
    _remove_empty_content_arguments($current);

    if ($current->{'cmdname'} 
          and $block_item_commands{$current->{'cmdname'}}) {
      if ($current->{'cmdname'} eq 'enumerate') {
        my $spec = 1;
        if ($current->{'extra'}->{'block_command_line_contents'}
            and defined($current->{'extra'}->{'block_command_line_contents'}->[0])) {
          if (scalar(@{$current->{'extra'}->{'block_command_line_contents'}->[0]}) > 1) {
            $self->_command_error($current, $line_nr, 
                        $self->__("superfluous argument to \@%s"),
                        $current->{'cmdname'});
          }
          my $arg = $current->{'extra'}->{'block_command_line_contents'}->[0]->[0];
          if (!defined($arg->{'text'}) or $arg->{'text'} !~ /^(([[:digit:]]+)|([[:alpha:]]+))$/) {
            $self->_command_error($current, $line_nr, 
                        $self->__("bad argument to \@%s"),
                        $current->{'cmdname'});
          } else {
            $spec = $arg->{'text'};
          }
        }
        $current->{'extra'}->{'enumerate_specification'} = $spec;
      } elsif ($item_line_commands{$current->{'cmdname'}}) {
        if (!$current->{'extra'} 
            or !$current->{'extra'}->{'command_as_argument'}) {
          $self->_command_error($current, $line_nr, 
              $self->__("%s requires an argument: the formatter for %citem"),
              $current->{'cmdname'}, ord('@'));
        } elsif (!$brace_commands{$current->{'extra'}->{'command_as_argument'}->{'cmdname'}}
            and !$self->{'definfoenclose'}->{$current->{'extra'}->{'command_as_argument'}->{'cmdname'}}) {
          $self->_command_error($current, $line_nr, 
              $self->__("command \@%s not accepting argument in brace should not be on \@%s line"),
              $current->{'extra'}->{'command_as_argument'}->{'cmdname'},
              $current->{'cmdname'});
          delete $current->{'extra'}->{'command_as_argument'};
        }
      }
      # This code checks that the command_as_argument of the @itemize
      # is alone on the line, otherwise it is not a command_as_argument.
      if ($current->{'extra'}
          and $current->{'extra'}->{'command_as_argument'}
          and $current->{'cmdname'} eq 'itemize') {
        my @args = @{$current->{'args'}->[0]->{'contents'}};
        while (@args) {
          my $arg = shift @args;
          last if ($arg eq $current->{'extra'}->{'command_as_argument'});
        }
        while (@args) {
          my $arg = shift @args;
          if (!(($arg->{'cmdname'} 
               and ($arg->{'cmdname'} eq 'c' 
                     or $arg->{'cmdname'} eq 'comment'))
               or (defined($arg->{'text'}) and $arg->{'text'} !~ /\S/))) {
            #print STDERR " -> stop at "._print_current($arg)."\n";
            delete $current->{'extra'}->{'command_as_argument'}->{'type'};
            delete $current->{'extra'}->{'command_as_argument'};
            last;
          }
        }
      }
      if ($current->{'extra'}
          and $current->{'extra'}->{'command_as_argument'}
          and $accent_commands{$current->{'extra'}->{'command_as_argument'}->{'cmdname'}}
          and ($current->{'cmdname'} eq 'itemize' 
               or $item_line_commands{$current->{'cmdname'}})) {
        $self->_command_warn($current, $line_nr, 
              $self->__("accent command `\@%s' not allowed as \@%s argument"),
              $current->{'extra'}->{'command_as_argument'}->{'cmdname'}, 
              $current->{'cmdname'});
        delete $current->{'extra'}->{'command_as_argument'};
        delete $current->{'extra'}->{'block_command_line_contents'};
      }
      if (!$current->{'extra'}->{'block_command_line_contents'}
          and $current->{'cmdname'} eq 'itemize') {
        $current->{'extra'}->{'block_command_line_contents'} = [
          [ { 'cmdname' => 'bullet', 
            'type' => 'command_as_argument',
            'parent' => $current }
          ]
        ];
        $current->{'extra'}->{'command_as_argument'} = 
          $current->{'extra'}->{'block_command_line_contents'}->[0]->[0];
      } elsif ($item_line_commands{$current->{'cmdname'}} and
              ! $current->{'extra'}->{'command_as_argument'}) {
        $current->{'extra'}->{'block_command_line_contents'} = [
          [ { 'cmdname' => 'asis', 
            'type' => 'command_as_argument',
            'parent' => $current }
          ]
        ];
        $current->{'extra'}->{'command_as_argument'} = 
          $current->{'extra'}->{'block_command_line_contents'}->[0]->[0];
      }
      push @{$current->{'contents'}}, { 'type' => 'before_item',
         'contents' => [], 'parent', $current };
      $current = $current->{'contents'}->[-1];
    }
    if ($current->{'cmdname'} and $menu_commands{$current->{'cmdname'}}) {
      push @{$current->{'contents'}}, {'type' => 'menu_comment',
                                       'parent' => $current,
                                       'contents' => [] };
      $current = $current->{'contents'}->[-1];
      print STDERR "MENU_COMMENT OPEN\n" if ($self->{'DEBUG'});
      push @{$self->{'context_stack'}}, 'preformatted';
    }
    $current = $self->_begin_preformatted($current);

  # if we are after a @end verbatim, we must restart a preformatted if needed,
  # since there is no @end command explicitly associated to raw commands
  # it won't be done elsewhere.
  } elsif ($current->{'contents'}
           and $current->{'contents'}->[-1]
           and $current->{'contents'}->[-1]->{'type'}
           and $current->{'contents'}->[-1]->{'type'} eq 'empty_line_after_command'
           and $current->{'contents'}->[-1]->{'extra'}
           and $current->{'contents'}->[-1]->{'extra'}->{'command'}
           and $current->{'contents'}->[-1]->{'extra'}->{'command'}->{'cmdname'} eq 'verbatim') {
    $current = $self->_begin_preformatted($current);
  # misc command line arguments
  # Never go here if skipline/noarg/...
  } elsif ($current->{'type'} 
           and $current->{'type'} eq 'misc_line_arg') {
    my $context = pop @{$self->{'context_stack'}};
    if ($context ne 'line') {
      $self->_bug_message("context $context instead of line in misc_line_arg", 
                          $line_nr, $current);
    }
    $self->_isolate_last_space($current);

    # first parent is the @command, second is the parent
    $current = $current->{'parent'};
    my $misc_cmd = $current;
    my $command = $current->{'cmdname'};
    my $end_command;
    print STDERR "MISC END \@$command\n" if ($self->{'DEBUG'});
    if ($self->{'misc_commands'}->{$command} =~ /^\d$/) {
      my $args = _parse_line_command_args($self, $current, $line_nr);
      $current->{'extra'}->{'misc_args'} = $args if (defined($args));
    } elsif ($self->{'misc_commands'}->{$command} eq 'text') {
      my $text = Texinfo::Convert::Text::convert($current->{'args'}->[0],
                                                 {'code' => 1, 
                                          Texinfo::Common::_convert_text_options($self)});
      if ($text eq '') {
        $self->_command_warn($current, $line_nr, 
                             $self->__("\@%s missing argument"), $command);
        $current->{'extra'}->{'missing_argument'} = 1;
      } else {
        $current->{'extra'}->{'text_arg'} = $text;
        if ($command eq 'end') {
          # REMACRO
          my $line = $text;
          if ($line =~ s/^([[:alnum:]][[:alnum:]-]+)//) {
            $end_command = $1;
            
            if (!exists $block_commands{$end_command}) {
              $self->_command_warn($current, $line_nr, 
                             $self->__("unknown \@end %s"), $end_command);
              $end_command = undef;
            } else {
              print STDERR "END BLOCK $end_command\n" if ($self->{'DEBUG'});
              if ($block_commands{$end_command} eq 'conditional') {
                if (@{$self->{'conditionals_stack'}}
                  and $self->{'conditionals_stack'}->[-1] eq $end_command) {
                  pop @{$self->{'conditionals_stack'}};
                } else {
                  $self->_command_error($current, $line_nr, 
                             $self->__("unmatched `%c%s'"), ord('@'), 'end');
                  $end_command = undef;
                }
              }
              $current->{'extra'}->{'command_argument'} = $end_command
                if (defined($end_command));
            }
            if ($line =~ /\S/ and defined($end_command)) {
              my $texi_line 
                = Texinfo::Convert::Texinfo::convert($current->{'args'}->[0]);
              $texi_line =~ s/^\s*([[:alnum:]][[:alnum:]-]+)//;
              $self->_command_error($current, $line_nr, 
                             $self->__("superfluous argument to \@%s %s: %s"),
                             $command, $end_command, $texi_line);
            }
          } else {
            $self->_command_error($current, $line_nr,
                              $self->__("bad argument to \@%s: %s"),
                              $command, $line);
          }
        } elsif ($command eq 'include') {
          my $file = Texinfo::Common::locate_include_file($self, $text) ;
          if (defined($file)) {
            my $filehandle = do { local *FH };
            if (open ($filehandle, $file)) {
              $included_file = 1;
              binmode($filehandle, ":encoding($self->{'INPUT_PERL_ENCODING'})")
                if (defined($self->{'INPUT_PERL_ENCODING'}));
              print STDERR "Included $file($filehandle)\n" if ($self->{'DEBUG'});
              unshift @{$self->{'input'}}, { 
                'name' => $file,
                'line_nr' => 0,
                'pending' => [],
                'fh' => $filehandle };
            } else {
              $self->_command_error($current, $line_nr,
                              $self->__("\@%s: could not open %s: %s"),
                              $command, $text, $!);
            }
          } else {
            $self->_command_error($current, $line_nr,
                              $self->__("\@%s: could not find %s"),
                              $command, $text);
          }
        } elsif ($command eq 'documentencoding') {
          my ($texinfo_encoding, $perl_encoding, $input_encoding)
            = Texinfo::Encoding::encoding_alias($text);
          $self->_command_warn($current, $line_nr,
                 $self->__("encoding `%s' is not a canonical texinfo encoding"),
                               $text)
            if (!$texinfo_encoding or $texinfo_encoding ne lc($text));
          if (! _ignore_global_commands($self)) {
            if ($input_encoding) {
              $current->{'extra'}->{'input_encoding_name'} = $input_encoding;
            }
            if (!$perl_encoding) {
              $self->_command_warn($current, $line_nr,
                   $self->__("unrecognized encoding name `%s'"), $text);
            } else {
              $current->{'extra'}->{'input_perl_encoding'} = $perl_encoding;

              if ($input_encoding) {
                if (!$self->{'set'}->{'INPUT_ENCODING_NAME'}) {
                  $self->{'INPUT_ENCODING_NAME'} = $input_encoding;
                  $self->{'info'}->{'input_encoding_name'} = $input_encoding;
                }
              }

              if (!$self->{'set'}->{'INPUT_PERL_ENCODING'}) {
                $self->{'INPUT_PERL_ENCODING'} = $perl_encoding;
                $self->{'info'}->{'input_perl_encoding'} = $perl_encoding;
                foreach my $input (@{$self->{'input'}}) {
                  binmode($input->{'fh'}, ":encoding($perl_encoding)") if ($input->{'fh'});
                }
              }
            }
          }
        } elsif ($command eq 'documentlanguage') {
          my @messages = Texinfo::Common::warn_unknown_language($text,
                                                          $self->{'gettext'});
          foreach my $message(@messages) {
            $self->_command_warn($current, $line_nr, $message);
          }
          if (!$self->{'set'}->{'documentlanguage'} 
              and !_ignore_global_commands($self)) {
            $self->{'documentlanguage'} = $text;
          }
        }
      }
    } elsif ($command eq 'node') {
      foreach my $arg (@{$current->{'args'}}) {
        my $node = _parse_node_manual($arg);
        push @{$current->{'extra'}->{'nodes_manuals'}}, $node;
      }
      if (_check_node_label($self, $current->{'extra'}->{'nodes_manuals'}->[0],
                        $command, $line_nr)) {
        if (_register_label($self, $current, 
                    $current->{'extra'}->{'nodes_manuals'}->[0], $line_nr)) {
          $self->{'current_node'} = $current;
          push @{$self->{'nodes'}}, $current;
        }
      }
    } elsif ($command eq 'listoffloats') {
      # Empty listoffloats is allowed
      _parse_float_type($current);
      #if (!_parse_float_type($current)) {
      #  $self->line_error (sprintf($self->__("\@%s missing argument"), 
      #     $command), $line_nr);
      #}
    # handle all the other 'line' commands.  Here just check that they 
    # have an argument and prepare contents without spaces.
    } else {
      my @contents = @{$current->{'args'}->[0]->{'contents'}};
      _trim_spaces_comment_from_content(\@contents);
      # empty @top is allowed
      if (!scalar(@contents) and $command ne 'top') {
        $self->_command_warn($current, $line_nr,
               $self->__("\@%s missing argument"), $command);
        $current->{'extra'}->{'missing_argument'} = 1;
      } else {
        $current->{'extra'}->{'misc_content'} = \@contents;
        if (($command eq 'item' or $command eq 'itemx')
            and $self->{'command_index_prefix'}->{$current->{'parent'}->{'cmdname'}}) {
          _enter_index_entry($self, $current->{'parent'}->{'cmdname'}, 
                             $command, $current,
                             $current->{'extra'}->{'misc_content'}, 
                             undef, $line_nr);
        } elsif ($self->{'command_index_prefix'}->{$current->{'cmdname'}}) {
          _enter_index_entry($self, $current->{'cmdname'}, 
                             $current->{'cmdname'}, $current,
                             $current->{'extra'}->{'misc_content'}, 
                             undef, $line_nr);
          $current->{'type'} = 'index_entry_command';
        }
      }
      if (defined($command_structuring_level{$command})) {
        $current->{'level'} = $command_structuring_level{$command};
      }
    }
    $current = $current->{'parent'};
    if ($end_command) {
      print STDERR "END COMMAND $end_command\n" if ($self->{'DEBUG'});
      my $end = pop @{$current->{'contents'}};
      if ($block_commands{$end_command} ne 'conditional') {
        my $closed_command;
        ($closed_command, $current)
          = _close_commands($self, $current, $line_nr, $end_command);
        my $inline_copying;
        if ($closed_command) {
          $misc_cmd->{'extra'}->{'command'} = $closed_command;
          $closed_command->{'extra'}->{'end_command'} = $misc_cmd;
          $self->_close_command_cleanup($closed_command);
          $end->{'parent'} = $closed_command;

          # register @insertcopying as a macro if INLINE_INSERTCOPYING is set.
          if ($end_command eq 'copying' and $self->{'INLINE_INSERTCOPYING'}) {
            # remove the end of line following @copying.
            my @contents = @{$closed_command->{'contents'}};
            shift @contents if ($contents[0] and $contents[0]->{'type'}
               and ($contents[0]->{'type'} eq 'empty_line_after_command'
                    or $contents[0]->{'type'} eq 'empty_spaces_after_command'));
            # the macrobody is the @copying content converted to Texinfo.
            my $body = Texinfo::Convert::Texinfo::convert(
                         {'contents' => \@contents});
            
            #chomp ($body);
            $self->{'macros'}->{'insertcopying'} = {
                    'args' => [{'text' => 'insertcopying', 'type' => 'macro_name'}],
                    'cmdname' => 'macro',
                    'extra' => {'macrobody' => 
                                 $self->_strip_macrobody_leading_space($body)}
            };
            $inline_copying = 1;
            print STDERR "INLINE_INSERTCOPYING as macro\n" if ($self->{'DEBUG'});
          }
          push @{$closed_command->{'contents'}}, $end;

          # closing a menu command, but still in a menu. Open a menu_comment
          if ($menu_commands{$closed_command->{'cmdname'}} 
              and $self->{'context_stack'}->[-1] eq 'menu') {
            print STDERR "CLOSE MENU but still in menu context\n"
              if ($self->{'DEBUG'});
            push @{$current->{'contents'}}, {'type' => 'menu_comment',
                                             'parent' => $current,
                                             'contents' => [] };
            $current = $current->{'contents'}->[-1];
            push @{$self->{'context_stack'}}, 'preformatted';
          }
        } else {
          #print STDERR "LLLLLLLLLLLL Cannot be here...\n";
        }
        $current = $self->_begin_preformatted($current)
          if ($close_preformatted_commands{$end_command});
      }
    } else {
      $current = $self->_begin_preformatted($current) 
        if ($close_preformatted_commands{$command});
    }
    # if a file was included, remove completly the include file command.
    # Also ignore @setfilename in included file, as said in the manual.
    if ($included_file or ($command eq 'setfilename'
                           and scalar(@{$self->{'input'}}) > 1)) {
      # TODO keep the information with sourcemark
      pop @{$current->{'contents'}};
    } elsif ($command eq 'setfilename'
             and ($self->{'current_node'} or $self->{'current_section'})) {
      $self->_command_warn($misc_cmd, $line_nr,
               $self->__("\@%s after the first element"), $command);
    # columnfractions 
    } elsif ($command eq 'columnfractions') {
      # in a multitable, we are in a block_line_arg
      if (!$current->{'parent'} or !$current->{'parent'}->{'cmdname'} 
                   or $current->{'parent'}->{'cmdname'} ne 'multitable') {
        $self->_command_error($current, $line_nr,
               $self->__("\@%s only meaningful on a \@multitable line"), 
               $command);
      } else {
        # This is the multitable block_line_arg line context
        my $context = pop @{$self->{'context_stack'}};
        if ($context ne 'line') {
          $self->_bug_message("context $context instead of line for multitable", 
                               $line_nr, $current);
        }
        $current = $current->{'parent'};
        $current->{'extra'}->{'max_columns'} = 0;
        if (defined($misc_cmd->{'extra'}->{'misc_args'})) {
          $current->{'extra'}->{'max_columns'} = 
            scalar(@{$misc_cmd->{'extra'}->{'misc_args'}});
          $current->{'extra'}->{'columnfractions'} = $misc_cmd->{'extra'}->{'misc_args'};
        }
        push @{$current->{'contents'}}, { 'type' => 'before_item',
           'contents' => [], 'parent', $current };
        $current = $current->{'contents'}->[-1];
      }
    } elsif ($root_commands{$command}) {
      $current = $current->{'contents'}->[-1];
      delete $current->{'remaining_args'};
      $current->{'contents'} = [];

      # we never should be in a raw format bock, so we don't check for
      # _ignore_global_commands($self)
      # associate the section (not part) with the current node.
      if ($command ne 'node' and $command ne 'part') {
        if ($self->{'current_node'}
           and !$self->{'current_node'}->{'extra'}->{'associated_section'}) {
          $self->{'current_node'}->{'extra'}->{'associated_section'} = $current;
          $current->{'extra'}->{'associated_node'} = $self->{'current_node'};
        }
        if ($self->{'current_parts'}) {
          $current->{'extra'}->{'associated_part'} = $self->{'current_parts'}->[-1];
          foreach my $part (@{$self->{'current_parts'}}) {
            $part->{'extra'}->{'part_associated_section'} = $current;
            if ($current->{'cmdname'} eq 'top') {
              $self->line_warn(sprintf($self->__(
                  "\@%s should not be associated with \@top"),
                   $part->{'cmdname'}), $part->{'line_nr'});
            }
          }
          delete $self->{'current_parts'};
        }
        $self->{'current_section'} = $current;
      } elsif ($command eq 'part') {
        push @{$self->{'current_parts'}}, $current;
        if ($self->{'current_node'}
           and !$self->{'current_node'}->{'extra'}->{'associated_section'}) {
          $self->line_warn (sprintf($self->__(
           "\@node precedes \@%s, but parts may not be associated with nodes"), 
                                    $command), $line_nr);
        }
      }
    }
   # do that last in order to have the line processed if one of the above
   # case is also set.
  } elsif (
      $current->{'contents'} 
      and (scalar(@{$current->{'contents'}}) == 1
           and (($current->{'contents'}->[-1]->{'type'}
               and $current->{'contents'}->[-1]->{'type'} eq 'empty_line_after_command'))
          or (scalar(@{$current->{'contents'}}) == 2
               and $current->{'contents'}->[-1]->{'cmdname'}
               and ($current->{'contents'}->[-1]->{'cmdname'} eq 'c'
                    or $current->{'contents'}->[-1]->{'cmdname'} eq 'comment')
               and $current->{'contents'}->[-2] 
               and $current->{'contents'}->[-2]->{'type'}
               and $current->{'contents'}->[-2]->{'type'} eq 'empty_line_after_command'))) {
    # empty line after a @menu or before a preformatted. Reparent to the menu
    # or other format
    if ($current->{'type'}
        and $preformatted_contexts{$current->{'type'}}) {
      my $parent = $current->{'parent'};
      if ($parent->{'type'} and $parent->{'type'} eq 'menu_comment'
          and scalar(@{$parent->{'contents'}}) == 1) {
        $parent = $parent->{'parent'};
      }
      my $to_reparent = pop @{$parent->{'contents'}};
      print STDERR "LINE AFTER COMMAND IN PREFORMATTED ($to_reparent->{'type'})\n" if ($self->{'DEBUG'});
      while (@{$current->{'contents'}}) {
        my $content = shift @{$current->{'contents'}};
        $content->{'parent'} = $parent;
        push @{$parent->{'contents'}}, $content;
      }
      push @{$parent->{'contents'}}, $to_reparent;
    }
  }

  # this happens if there is a nesting of line @-commands on a line.
  # they are reprocessed here.
  if ($self->{'context_stack'}->[-1] eq 'line' 
            or $self->{'context_stack'}->[-1] eq 'def') {
    print STDERR "Still opened line command $self->{'context_stack'}->[-1]:"._print_current($current) 
      if ($self->{'DEBUG'});
    if ($self->{'context_stack'}->[-1] eq 'def') {
      while ($current->{'parent'} and !($current->{'parent'}->{'type'}
            and $current->{'parent'}->{'type'} eq 'def_line')) {
        $current = $self->_close_current($current, $line_nr);
      }
    } else {
      while ($current->{'parent'} and !($current->{'type'}
             and ($current->{'type'} eq 'block_line_arg'
                  or $current->{'type'} eq 'misc_line_arg'))) {
        $current = $self->_close_current($current, $line_nr);
      }
    }

    # check for infinite loop bugs...
    if ($current eq $current_old) {
      my $indent = '- ';
      my $tree_msg = $indent . _print_current($current);
      while ($current->{'parent'}) {
        $indent = '-'.$indent;
        $current = $current->{'parent'};
        $tree_msg .= $indent . _print_current($current);
      }
      $self->_bug_message("Nothing closed while a line context remains\n$tree_msg",
                           $line_nr);
      die;
    }

    $current = $self->_end_line($current, $line_nr);
  }
  return $current;
}

# $command may be undef if we are after a wrong misc command such as 
# a buggy @tab.
sub _start_empty_line_after_command($$$) {
  my $line = shift;
  my $current = shift;
  my $command = shift;
  $line =~ s/^([^\S\r\n]*)//;
  push @{$current->{'contents'}}, { 'type' => 'empty_line_after_command',
                                    'text' => $1,
                                    'parent' => $current, 
                                  };
  if (defined($command)) {
    $current->{'contents'}->[-1]->{'extra'} = {'command' => $command};
    $command->{'extra'}->{'spaces_after_command'} = $current->{'contents'}->[-1];
  }
  return $line;
}

sub _check_empty_node($$$$)
{
  my $self = shift;
  my $parsed_node = shift;
  my $command = shift;
  my $line_nr = shift;
  if (!defined($parsed_node) or !$parsed_node->{'node_content'}) {
    $self->line_error (sprintf($self->__("empty argument in \@%s"),
                $command), $line_nr);
    return 0;
  } elsif ($parsed_node->{'normalized'} !~ /[^-]/) {
    $self->line_error (sprintf($self->__("empty node name after expansion `%s'"),
                Texinfo::Convert::Texinfo::convert({'contents' 
                                        => $parsed_node->{'node_content'}})), 
                $line_nr);
    return 0;
  } else {
    return 1;
  }
}

sub _check_internal_node($$$)
{
  my $self = shift;
  my $parsed_node = shift;
  my $line_nr = shift;
  if ($parsed_node and $parsed_node->{'manual_content'}) {
    $self->line_error (sprintf($self->__("syntax for an external node used for `%s'"),
          _node_extra_to_texi($parsed_node)), $line_nr)
  }
}

sub _check_node_label($$$$)
{
  my $self = shift;
  my $parsed_node = shift;
  my $command = shift;
  my $line_nr = shift;
  _check_internal_node($self, $parsed_node, $line_nr);
  return _check_empty_node($self, $parsed_node, $command, $line_nr);
}

sub _register_extra_menu_entry_information($$;$)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;

  foreach my $arg (@{$current->{'args'}}) {
    if ($arg->{'type'} eq 'menu_entry_name') {
      $current->{'extra'}->{'menu_entry_name'} = $arg;
      my $normalized_menu_entry_name = 
        Texinfo::Convert::NodeNameNormalization::normalize_node($arg);
      if ($normalized_menu_entry_name !~ /[^-]/) {
        $self->line_warn(sprintf($self->__("empty menu entry name in `%s'"),
          Texinfo::Convert::Texinfo::convert($current)), $line_nr);
      }
    } elsif ($arg->{'type'} eq 'menu_entry_node') {
      $self->_isolate_last_space($arg, 'space_at_end_menu_node');
      my $parsed_entry_node = _parse_node_manual($arg);
      if (! defined($parsed_entry_node)) {
        if ($self->{'SHOW_MENU'}) {
          $self->line_error ($self->__("empty node name in menu entry"), $line_nr);
        }
      } else {
        $current->{'extra'}->{'menu_entry_node'} = $parsed_entry_node;
      }
    } elsif ($arg->{'type'} eq 'menu_entry_description') {
      $current->{'extra'}->{'menu_entry_description'} = $arg;
    }
  } 
}

sub _enter_menu_entry_node($$$)
{
  my $self = shift;
  my $current = shift;
  my $line_nr = shift;
  my $description = { 'type' => 'menu_entry_description',
                      'contents' => [],
                      'parent' => $current };
  push @{$current->{'args'}}, $description;
  _register_extra_menu_entry_information($self, $current, $line_nr);
  $current->{'line_nr'} = $line_nr;
  $current = $description;
  push @{$current->{'contents'}}, {'type' => 'preformatted',
                                   'parent' => $current,
                                   'contents' => [] };
  $current = $current->{'contents'}->[-1];
  push @{$self->{'context_stack'}}, 'preformatted';
  return $current;
}

sub _register_command_arg($$$)
{
  my $self = shift;
  my $current = shift;
  my $type = shift;
  my @contents = @{$current->{'contents'}};
  _trim_spaces_comment_from_content(\@contents);
  if (scalar(@contents)) {
    push @{$current->{'parent'}->{'extra'}->{$type}}, \@contents;
  } else {
    push @{$current->{'parent'}->{'extra'}->{$type}}, undef;
  }
}

sub _command_with_command_as_argument($)
{
  my $current = shift;
  return ($current and $current->{'type'}
      and $current->{'type'} eq 'block_line_arg'
      and $current->{'parent'} 
      and $current->{'parent'}->{'cmdname'} and
     ($current->{'parent'}->{'cmdname'} eq 'itemize'
      or $item_line_commands{$current->{'parent'}->{'cmdname'}})
      and (scalar(@{$current->{'contents'}}) == 1
           or (scalar(@{$current->{'contents'}}) == 2
            and defined($current->{'contents'}->[0]->{'text'})
            and $current->{'contents'}->[0]->{'text'}
                               =~ /^[^\S\r\n]*/)))
}

# $marked_as_invalid_command may be undef, if there is no
# tree element because the @-command construct is incorrect, for example
# wrong @tab.
sub _mark_and_warn_invalid($$$$$)
{
  my $self = shift;
  my $command = shift;
  my $invalid_parent = shift;
  my $line_nr = shift;
  my $marked_as_invalid_command = shift;

  if (defined($invalid_parent)) {
    $self->line_warn(sprintf($self->__("\@%s should not appear in \@%s"), 
              $command, $invalid_parent), $line_nr);
    $marked_as_invalid_command->{'extra'}->{'invalid_nesting'} = 1
      if (defined($marked_as_invalid_command));
  }
}
# the different types
#c 'menu_entry'
#c 'menu_entry'
# t 'menu_entry_leading_text'
#
#t 'macro_arg_name'
#t 'macro_arg_args'
#
#t 'raw'
#
#t 'misc_arg'
#c 'misc_line_arg'
#
#c 'block_line_arg'
#
#c 'brace_command_arg'
#c 'brace_command_context'
#
#c 'before_item'   what comes after @*table, @itemize, @enumerate before
#                an @item
#
#c 'paragraph'
#
#a 'def_line'
#
#special for @verb, type is the character

# the main subroutine
sub _parse_texi($;$)
{
  my $self = shift;
  my $root = shift;

  $root = { 'contents' => [], 'type' => 'text_root' } if (!defined($root));
  my $current = $root;

  my $line_nr;
  
 NEXT_LINE:
  while (1) {
    my $line;
    ($line, $line_nr) = _next_text($self, $line_nr, $current);
    last if (!defined($line));

    if ($self->{'DEBUG'}) {
      $current->{'HERE !!!!'} = 1; # marks where we are in the tree
      if ($self->{'DEBUG'} >= 100) {
        local $Data::Dumper::Indent = 1;
        local $Data::Dumper::Purity = 1;
        print STDERR "".Data::Dumper->Dump([$root], ['$root']);
      }
      my $line_text = '';
      $line_text = "$line_nr->{'line_nr'}.$line_nr->{'macro'}" if ($line_nr);
      print STDERR "NEW LINE(".join('|', @{$self->{'context_stack'}}).":@{$self->{'conditionals_stack'}}:$line_text): $line";
      #print STDERR "CONTEXT_STACK ".join('|',@{$self->{'context_stack'}})."\n";
      delete $current->{'HERE !!!!'};
    }

    if (not 
        # raw format or verb
          (($current->{'cmdname'}
           and $block_commands{$current->{'cmdname'}}
            and ($block_commands{$current->{'cmdname'}} eq 'raw'
                 or $block_commands{$current->{'cmdname'}} eq 'conditional'))
          or 
           ($current->{'parent'} and $current->{'parent'}->{'cmdname'}
            and $current->{'parent'}->{'cmdname'} eq 'verb')
          )
        # not def line
        and $self->{'context_stack'}->[-1] ne 'def') {
      print STDERR "BEGIN LINE\n" if ($self->{'DEBUG'});
      $line =~ s/^([^\S\r\n]*)//;
      push @{$current->{'contents'}}, { 'type' => 'empty_line', 
                                        'text' => $1,
                                        'parent' => $current };
    }

    while (1) {
      # in a raw or ignored conditional block command
      if ($current->{'cmdname'} and 
            $block_commands{$current->{'cmdname'}} and 
            ($block_commands{$current->{'cmdname'}} eq 'raw'
             or $block_commands{$current->{'cmdname'}} eq 'conditional')) {
        # r?macro may be nested
        if (($current->{'cmdname'} eq 'macro' 
              or $current->{'cmdname'} eq 'rmacro') 
             and $line =~ /^\s*\@r?macro\s+/) {
          $line =~ s/\s*\@(r?macro)//;
          push @{$current->{'contents'}}, { 'cmdname' => $1,
                                            'parent' => $current,
                                            'contents' => [],
                         'extra' => {'arg_line' => $line }};
          $current = $current->{'contents'}->[-1];
          last;
        # ifclear/ifset may be nested
        } elsif (($current->{'cmdname'} eq 'ifclear' 
                  or $current->{'cmdname'} eq 'ifset'
                  or $current->{'cmdname'} eq 'ifcommanddefined'
                  or $current->{'cmdname'} eq 'ifcommandnotdefined')
                and $line =~ /^\s*\@$current->{'cmdname'}/) {
          $line =~ s/\s*\@($current->{'cmdname'})//;
          push @{$current->{'contents'}}, { 'cmdname' => $1,
                                            'parent' => $current,
                                            'contents' => [],
                         'extra' => {'line' => $line }};
          $current = $current->{'contents'}->[-1];
          last;
        } elsif ($line =~ /^(\s*?)\@end\s+([a-zA-Z][\w-]*)/
                 and ($2 eq $current->{'cmdname'})) {
          my $end_command = $2;
          my $raw_command = $current;
          $line =~ s/^(\s*?)(\@end\s+$current->{'cmdname'})//;
          if ($1 eq '') {
            # FIXME exclude other formats, like @macro, @ifset, @ignore?
            if ($current->{'cmdname'} ne 'verbatim'
                and @{$current->{'contents'}}
                and $current->{'contents'}->[-1]->{'type'}
                and $current->{'contents'}->[-1]->{'type'} eq 'raw') {
              if ($current->{'contents'}->[-1]->{'text'} =~ s/(\n)//) {
                push @{$current->{'contents'}}, {'type' => 'last_raw_newline',
                                          'text' => $1, 'parent' => $current};
              }
            }
          } else {
            push @{$current->{'contents'}}, 
              { 'text' => $1, 'type' => 'raw', 'parent' => $current };
            $self->line_warn(sprintf($self->__("\@end %s should only appear at a line beginning"), 
                                     $end_command), $line_nr);
          }
          # if there is a user defined macro that expandes to spaces, there
          # will be a spurious warning.
          $self->line_warn(sprintf($self->
                __("superfluous argument to \@%s %s: %s"), 'end', $end_command,
                                    $line), $line_nr)
            if ($line =~ /\S/ and $line !~ /^\s*\@c(omment)?\b/);
          # store toplevel macro specification
          if (($end_command eq 'macro' or $end_command eq 'rmacro') 
               and (! $current->{'parent'} 
                    or !$current->{'parent'}->{'cmdname'} 
                    or ($current->{'parent'}->{'cmdname'} ne 'macro'
                        and $current->{'parent'}->{'cmdname'} ne 'rmacro'))) {
            $current->{'extra'}->{'macrobody'} = 
             $self->_strip_macrobody_leading_space(
               Texinfo::Convert::Texinfo::convert({ 'contents' 
                                             => $current->{'contents'} }));
            if ($current->{'args'} and $current->{'args'}->[0]
                and !_ignore_global_commands($self)) {
              my $name = $current->{'args'}->[0]->{'text'};
              if (exists($self->{'macros'}->{$name})) {
                $self->line_warn(sprintf($self->__("macro `%s' previously defined"), 
                                          $name), $current->{'line_nr'});
                $self->line_warn(sprintf($self->__(
                                   "here is the previous definition of `%s'"), 
                               $name), $self->{'macros'}->{$name}->{'line_nr'});
              }
              if ($all_commands{$name}) {
                $self->line_warn(sprintf($self->__(
                                  "redefining Texinfo language command: \@%s"), 
                                          $name), $current->{'line_nr'});
              }
              $self->{'macros'}->{$name} = $current
                unless ($current->{'extra'}->{'invalid_syntax'});
            }
          }
          $current = $current->{'parent'};
          if ($block_commands{$end_command} eq 'conditional') {
            # don't store ignored @if*
            my $conditional = pop @{$current->{'contents'}};
            if (!defined($conditional->{'cmdname'}
                or $conditional->{'cmdname'} ne $end_command)) {
              $self->_bug_message("Ignored command is not the conditional $end_command", 
                                   $line_nr, $conditional);
              die;
            }
            # Ignore until end of line
            if ($line !~ /\n/) {
              ($line, $line_nr) = _new_line($self, $line_nr, $conditional);
              print STDERR "IGNORE CLOSE line: $line" if ($self->{'DEBUG'});
            }
            print STDERR "CLOSED conditional $end_command\n" if ($self->{'DEBUG'});
            last;
          } else {
            print STDERR "CLOSED raw $end_command\n" if ($self->{'DEBUG'});
            $line = _start_empty_line_after_command($line, $current, $raw_command);
          }
        } else {
          if (@{$current->{'contents'}} 
              and $current->{'contents'}->[-1]->{'type'}
              and $current->{'contents'}->[-1]->{'type'} eq 'empty_line_after_command'
              and $current->{'contents'}->[-1]->{'text'} !~ /\n/
              and $line !~ /\S/) {
            $current->{'contents'}->[-1]->{'text'} .= $line;
          } else {
            push @{$current->{'contents'}}, 
              { 'text' => $line, 'type' => 'raw', 'parent' => $current };
          }
          last;
        }
      # in @verb. type should be 'brace_command_arg'
      } elsif ($current->{'parent'} and $current->{'parent'}->{'cmdname'}
             and $current->{'parent'}->{'cmdname'} eq 'verb') {
        # collect the first character if not already done
        if (!defined($current->{'parent'}->{'type'})) {
          if ($line =~ /^$/) {
            $current->{'parent'}->{'type'} = '';
            $self->line_error(sprintf($self->
                __("\@%s without associated character"), 'verb'), $line_nr);
          } else {
            $line =~ s/^(.)//;
            $current->{'parent'}->{'type'} = $1;
          }
        }
        my $char = quotemeta($current->{'parent'}->{'type'});
        if ($line =~ s/^(.*?)$char\}/\}/) {
          push @{$current->{'contents'}}, 
              { 'text' => $1, 'type' => 'raw', 'parent' => $current } 
                if ($1 ne '');
          print STDERR "END VERB\n" if ($self->{'DEBUG'});
        } else {
          push @{$current->{'contents'}}, 
             { 'text' => $line, 'type' => 'raw', 'parent' => $current };
          print STDERR "LINE VERB: $line" if ($self->{'DEBUG'});
          last;
        }
      }

      # this mostly happens in the following cases:
      #   after expansion of user defined macro that doesn't end with EOL
      #   after a protection of @\n in @def* line
      #   at the end of an expanded Texinfo fragment
      while ($line eq '') {
        print STDERR "EMPTY TEXT\n"
          if ($self->{'DEBUG'});
        ($line, $line_nr) = _next_text($self, $line_nr, $current);
        if (!defined($line)) {
          # end of the file or of a text fragment.
          $current = _end_line ($self, $current, $line_nr);
          # It may happen that there is an @include file on the line, it 
          # will be picked up at NEXT_LINE, beginning a new line
          next NEXT_LINE;
        }
      }

      # handle user defined macros before anything else since
      # their expansion may lead to changes in the line
      # REMACRO
      if ($line =~ /^\@([[:alnum:]][[:alnum:]-]*)/ 
                and ($self->{'macros'}->{$1} 
                     or (exists $self->{'aliases'}->{$1} and 
                       $self->{'macros'}->{$self->{'aliases'}->{$1}}))) {
        $line =~ s/^\@([[:alnum:]][[:alnum:]-]*)//o;
        my $command = $1;
        my $alias_command;
        if (exists($self->{'aliases'}->{$command})) {
          $alias_command = $command;
          $command = $self->{'aliases'}->{$command};
        }

        my $expanded_macro = $self->{'macros'}->{$command};
        my $args_number = scalar(@{$expanded_macro->{'args'}}) -1;
        my $arguments = [];
        if ($line =~ s/^\s*{[^\S\f]*//) { # macro with args
          ($arguments, $line, $line_nr) = 
            _expand_macro_arguments($self, $expanded_macro, $line, $line_nr);
        } elsif (($args_number >= 2) or ($args_number <1)) {
        # as agreed on the bug-texinfo mailing list, no warn when zero
        # arg and not called with {}.
          $self->line_warn(sprintf($self->__(
   "\@%s defined with zero or more than one argument should be invoked with {}"), 
                                    $command), $line_nr)
             if ($args_number >= 2);
        } else {
          if ($line !~ /\n/) {
            ($line, $line_nr) = _new_line($self, $line_nr, $expanded_macro);
            $line = '' if (!defined($line));
          }
          $line =~ s/^[^\S\f]*// if ($line =~ /[\S\f]/);
          my $has_end_of_line = chomp $line;
          $arguments = [$line];
          $line = "\n" if ($has_end_of_line);
        }
        my $expanded = _expand_macro_body ($self, $expanded_macro, 
                                   $arguments, $line_nr);
        print STDERR "MACROBODY: $expanded".'||||||'."\n" 
           if ($self->{'DEBUG'}); 
        # empty result.  It is ignored here.
        if ($expanded eq '') {
          next;
        }
        if ($self->{'MAX_MACRO_CALL_NESTING'}
            and scalar(@{$self->{'macro_stack'}}) > $self->{'MAX_MACRO_CALL_NESTING'}) {
          $self->line_warn(sprintf($self->__(
"macro call nested too deeply (set MAX_NESTED_MACROS to override; current value %d)"), 
                                $self->{'MAX_MACRO_CALL_NESTING'}), $line_nr);
          next;
        }
        if ($expanded_macro->{'cmdname'} eq 'macro') {
          my $found = 0;
          foreach my $macro (@{$self->{'macro_stack'}}) {
            if ($macro->{'args'}->[0]->{'text'} eq $command) {
              $self->line_error(sprintf($self->__(
             "recursive call of macro %s is not allowed; use \@rmacro if needed"),
                                         $command), $line_nr);
              $found = 1;
              last;
            }
          }
          next if ($found);
        }

        unshift @{$self->{'macro_stack'}}, $expanded_macro;
        print STDERR "UNSHIFT MACRO_STACK: $expanded_macro->{'args'}->[0]->{'text'}\n"
          if ($self->{'DEBUG'});
        my $expanded_lines = _text_to_lines($expanded);
        chomp ($expanded_lines->[-1]);
        pop @$expanded_lines if ($expanded_lines->[-1] eq '');
        print STDERR "MACRO EXPANSION LINES: ".join('|', @$expanded_lines)
                                     ."|\nEND LINES MACRO EXPANSION\n" if ($self->{'DEBUG'});
        next if (!@$expanded_lines);
        my $new_lines = _complete_line_nr($expanded_lines, 
                            $line_nr->{'line_nr'}, $line_nr->{'file_name'},
                            $expanded_macro->{'args'}->[0]->{'text'}, 1);
        $line_nr->{'end_macro'} = 1;
        unshift @{$self->{'input'}->[0]->{'pending'}}, [$line, $line_nr];
        my $new_text = shift @$new_lines;
        ($line, $line_nr) = ($new_text->[0], $new_text->[1]);
        unshift @{$self->{'input'}->[0]->{'pending'}}, @$new_lines;

      # Now handle all the cases that may lead to command closing
      # or following character association with an @-command, especially
      # accent command, that is handle @-command with braces that don't
      # always need a brace.

      # The condition below is only caught right after command opening,
      # otherwise we are in the 'args' and not right in the command container.
      } elsif ($current->{'cmdname'} and 
          (defined($brace_commands{$current->{'cmdname'}}) or 
            $self->{'definfoenclose'}->{$current->{'cmdname'}})
           and $line !~ /^{/) {
        # special case for @-command as argument of @itemize or @*table.
        if (_command_with_command_as_argument($current->{'parent'})) {
          delete $current->{'contents'};
          print STDERR "FOR PARENT \@$current->{'parent'}->{'parent'}->{'cmdname'} command_as_argument $current->{'cmdname'}\n" if ($self->{'DEBUG'});
          $current->{'type'} = 'command_as_argument' if (!$current->{'type'});
          $current->{'parent'}->{'parent'}->{'extra'}->{'command_as_argument'} 
            = $current;
          $current = $current->{'parent'};
        # now accent commands
        } elsif ($accent_commands{$current->{'cmdname'}}) {
          if ($line =~ /^[^\S\r\n]/) {
            if ($current->{'cmdname'} =~ /^[a-zA-Z]/) {
              $line =~ s/^([^\S\r\n]+)//;
              $current->{'extra'}->{'spaces'} = '' 
                if (!defined($current->{'extra'}->{'spaces'}));
              $current->{'extra'}->{'spaces'} .= $1;
            } else {
              $self->line_warn(sprintf($self->
                __("accent command `\@%s' must not be followed by whitespace"),
                $current->{'cmdname'}), $line_nr);
              $current = $current->{'parent'};
            }
          } elsif ($line =~ /^\@/) {
            $self->line_error(sprintf($self->
              __("use braces to give a command as an argument to \@%s"),
                $current->{'cmdname'}), $line_nr);
            $current = $current->{'parent'};
          } elsif ($line =~ s/^(.)//o) {
            print STDERR "ACCENT \@$current->{'cmdname'}\n" 
              if ($self->{'DEBUG'});
            my $following_arg = {'type' => 'following_arg',
                                 'parent' => $current};
            $following_arg->{'contents'} = [{ 'text' => $1,
                                             'parent' => $following_arg } ];
            $current->{'args'} = [ $following_arg ];
            if ($current->{'cmdname'} eq 'dotless' and $1 ne 'i' and $1 ne 'j') {
              $self->line_error(sprintf($self->
                 __("%c%s expects `i' or `j' as argument, not `%s'"), 
                 ord('@'), $current->{'cmdname'}, $1), $line_nr);
            }
            if ($current->{'cmdname'} =~ /^[a-zA-Z]/) {
              $current->{'args'}->[-1]->{'type'} = 'space_command_arg';
            }
            delete $current->{'contents'};
            $current = $current->{'parent'};
          } else { # The accent is at end of line
            # whitespace for commands with letter.
            print STDERR "STRANGE ACC \@$current->{'cmdname'}\n" if ($self->{'DEBUG'});
            $self->line_warn(sprintf($self->
               __("accent command `\@%s' must not be followed by new line"),
               $current->{'cmdname'}), $line_nr);
            $current = $current->{'parent'};
          }
          next;
        } else {
          # ignore space after a braced @-command like TeX does
          if ($self->{'IGNORE_SPACE_AFTER_BRACED_COMMAND_NAME'}
              and $line =~ s/^\s+//) {
            next;
          }
          $self->line_error(sprintf($self->__("\@%s expected braces"), 
                           $current->{'cmdname'}), $line_nr);
          $current = $current->{'parent'};
        }
      # maybe a menu entry beginning: a * at the beginning of a menu line
      } elsif ($line =~ /^\*/ and $current->{'type'}
                and $current->{'type'} eq 'preformatted'
                and $current->{'parent'}->{'type'} 
                and ($current->{'parent'}->{'type'} eq 'menu_comment'
                     or $current->{'parent'}->{'type'} eq 'menu_entry_description')
                and @{$current->{'contents'}} 
                and $current->{'contents'}->[-1]->{'type'}
                and $current->{'contents'}->[-1]->{'type'} eq 'empty_line'
                and $current->{'contents'}->[-1]->{'text'} eq '') {
        print STDERR "MENU STAR\n" if ($self->{'DEBUG'});
        _abort_empty_line($self, $current);
        $line =~ s/^\*//;
        push @{$current->{'contents'}}, { 'type' => 'menu_star',
                                          'text' => '*' };
      # a space after a * at the beginning of a menu line
      } elsif ($line =~ /^\s+/ and $current->{'contents'} 
               and @{$current->{'contents'}} 
               and $current->{'contents'}->[-1]->{'type'}
               and $current->{'contents'}->[-1]->{'type'} eq 'menu_star') {
        print STDERR "MENU ENTRY (certainly)\n" if ($self->{'DEBUG'});
        # this is the menu star collected previously
        pop @{$current->{'contents'}};
        $line =~ s/^(\s+)//;
        my $leading_text = '*' . $1;
        if ($current->{'type'} eq 'preformatted'
            and $current->{'parent'}->{'type'} 
            and $current->{'parent'}->{'type'} eq 'menu_comment') {
          my $menu = $current->{'parent'}->{'parent'};
          if (!@{$current->{'contents'}}) {
            pop @{$current->{'parent'}->{'contents'}};
            if (!scalar(@{$current->{'parent'}->{'contents'}})) {
              pop @{$menu->{'contents'}}; 
            }
          }
          $current = $menu;
          #print STDERR "Close MENU_COMMENT because new menu entry\n";
        } else {
          # first parent preformatted, third is menu_entry
          if ($current->{'type'} ne 'preformatted' 
              or $current->{'parent'}->{'type'} ne 'menu_entry_description'
              or $current->{'parent'}->{'parent'}->{'type'} ne 'menu_entry'
              or !$menu_commands{$current->{'parent'}->{'parent'}->{'parent'}->{'cmdname'}}) {
            $self->_bug_message("Not in menu comment nor description", 
                                 $line_nr, $current);
          }
          $current = $current->{'parent'}->{'parent'}->{'parent'};
        }
        my $context = pop @{$self->{'context_stack'}};
        if ($context ne 'preformatted') {
          $self->_bug_message("context $context instead of preformatted after menu leading star", 
                              $line_nr, $current);
        }
        push @{$current->{'contents'}}, { 'type' => 'menu_entry',
                                          'parent' => $current,
                                        };
        $current = $current->{'contents'}->[-1];
        $current->{'args'} = [ { 'type' => 'menu_entry_leading_text',
                                 'text' => $leading_text,
                                 'parent' => $current },
                               { 'type' => 'menu_entry_name',
                                 'contents' => [],
                                 'parent' => $current } ];
        $current = $current->{'args'}->[-1];
      # * followed by something else than a space.
      } elsif ($current->{'contents'} and @{$current->{'contents'}} 
               and $current->{'contents'}->[-1]->{'type'}
               and $current->{'contents'}->[-1]->{'type'} eq 'menu_star') {
        print STDERR "ABORT MENU STAR ($line)\n" if ($self->{'DEBUG'});
        delete $current->{'contents'}->[-1]->{'type'};
      # after a separator in menu
      } elsif ($current->{'args'} and @{$current->{'args'}} 
               and $current->{'args'}->[-1]->{'type'}
               and $current->{'args'}->[-1]->{'type'} eq 'menu_entry_separator') {
        my $separator = $current->{'args'}->[-1]->{'text'};
        # separator is ::, we concatenate and let the while restart
        # in order to collect spaces below
        if ($separator eq ':' and $line =~ s/^(:)//) {
          $current->{'args'}->[-1]->{'text'} .= $1;
        # a . not followed by a space.  Not a separator.
        } elsif ($separator eq '.' and $line =~ /^\S/) {
          pop @{$current->{'args'}};
          $current = $current->{'args'}->[-1];
          $current = _merge_text($self, $current, $separator);
        # here we collect spaces following separators.
        } elsif ($line =~ s/^([^\S\r\n]+)//) {
          # FIXME a trailing end of line could be considered to be part
          # of the separator. Right now it is part of the description,
          # since it is catched (in the next while) as one of the case below
          $current->{'args'}->[-1]->{'text'} .= $1;
        # now handle the menu part that was closed
        } elsif ($separator =~ /^::/) {
          print STDERR "MENU NODE no entry $separator\n" if ($self->{'DEBUG'});
          # it was previously registered as menu_entry_name, it is 
          # changed to node
          $current->{'args'}->[-2]->{'type'} = 'menu_entry_node';
          $current = _enter_menu_entry_node($self, $current, $line_nr);
        # end of the menu entry name  
        } elsif ($separator =~ /^:/) {
          print STDERR "MENU ENTRY $separator\n" if ($self->{'DEBUG'});
          push @{$current->{'args'}}, { 'type' => 'menu_entry_node',
                                        'contents' => [],
                                        'parent' => $current };
          $current = $current->{'args'}->[-1];
        # anything else is the end of the menu node following a menu_entry_name
        } else {
          print STDERR "MENU NODE $separator\n" if ($self->{'DEBUG'});
          $current = _enter_menu_entry_node($self, $current, $line_nr);
        }
        # REMACRO
      } elsif ($line =~ s/^\@(["'~\@\}\{,\.!\?\s\*\-\^`=:\|\/\\])//o 
               or $line =~ s/^\@([[:alnum:]][[:alnum:]-]*)//o) {
        my $command = $1;
        my $alias_command;
        if (exists($self->{'aliases'}->{$command})) {
          $alias_command = $command;
          $command = $self->{'aliases'}->{$command};
        }
        print STDERR "COMMAND $command\n" if ($self->{'DEBUG'});


        if ($command eq 'value') {
          $line =~ s/^\s*// 
             if ($self->{'IGNORE_SPACE_AFTER_BRACED_COMMAND_NAME'});
          # REVALUE
          if ($line =~ s/^{([\w\-][^\s{\\}~`\^+"<>|@]*)}//) {
            my $value = $1;
            if (exists($self->{'values'}->{$value})) {
              if (!defined($self->{'values'}->{$value})) {
                print STDERR "BUG? $value exists but not defined\n";
              } elsif (!ref($self->{'values'}->{$value})) {
                $line = $self->{'values'}->{$value} . $line;
              # the push @{$current->{'contents'}}, {}; prevents a trailing
              # text to be merged, to avoid having the value tree modified.
              } elsif (ref($self->{'values'}->{$value}) eq 'ARRAY') {
                # we don't know for sure, but if we don't do it here it 
                # won't be done
                _abort_empty_line($self, $current);
                foreach my $content (@{$self->{'values'}->{$value}}) {
                  push @{$current->{'contents'}}, $content;
                }
                push @{$current->{'contents'}}, {};
              } elsif (ref($self->{'values'}->{$value}) eq 'HASH') {
                # we don't know for sure, but if we don't do it here it 
                # won't be done
                _abort_empty_line($self, $current);
                my $content = $self->{'values'}->{$value};
                push @{$current->{'contents'}}, $content;
                push @{$current->{'contents'}}, {};
              }
            } else {
              # caller should expand something along 
              # gdt('@{No value for `{value}\'@}', {'value' => $value}, {'keep_texi'=> 1});
              push @{$current->{'contents'}}, { 'cmdname' => 'value',
                                                'type' => $value };
              $self->line_warn(
                  sprintf($self->__("undefined flag: %s"), $value), $line_nr);
            }
          } else {
            $self->line_error($self->__("bad syntax for \@value"), $line_nr);
          }
          next;
        }

        if (defined($deprecated_commands{$command})) {
          if ($deprecated_commands{$command} eq '') {
            $self->line_warn(sprintf($self->__("%c%s is obsolete."), 
                                ord('@'), $command), $line_nr);
          } else {
            $self->line_warn(sprintf($self->__("%c%s is obsolete; %s"),
                   ord('@'), $command, 
                   $self->__($deprecated_commands{$command})), $line_nr);
          }
        }

        if (not _abort_empty_line($self, $current) 
               and $begin_line_commands{$command}) {
          $self->line_warn( 
              sprintf($self->__("\@%s should only appear at a line beginning"), 
                      $command), $line_nr);
        }

        my $invalid_parent;
        # error messages for forbidden constructs, like @node in @r, 
        # block command on line command, @xref in @anchor or node...
        if ($current->{'parent'}) { 
          if ($current->{'parent'}->{'cmdname'}) {
            if (defined($self->{'valid_nestings'}->{$current->{'parent'}->{'cmdname'}})
                and !$self->{'valid_nestings'}->{$current->{'parent'}->{'cmdname'}}->{$command}
                # we make sure that we are on a root @-command line and 
                # not in contents
                and (!$root_commands{$current->{'parent'}->{'cmdname'}}
                     or ($current->{'type'}
                         and $current->{'type'} eq 'misc_line_arg'))
                # we make sure that we are on a block @-command line and 
                # not in contents
                and (!($block_commands{$current->{'parent'}->{'cmdname'}})
                     or ($current->{'type'} 
                          and $current->{'type'} eq 'block_line_arg'))
                # we make sure that we are on an @item/@itemx line and
                # not in an @enumerate, @multitable or @itemize @item.
                and (($current->{'parent'}->{'cmdname'} ne 'itemx'
                     and $current->{'parent'}->{'cmdname'} ne 'item')
                     or ($current->{'type'}
                              and $current->{'type'} eq 'misc_line_arg'))) {
              $invalid_parent = $current->{'parent'}->{'cmdname'};
            }
          } elsif ($self->{'context_stack'}->[-1] eq 'def'
            # FIXME instead of hardcoding in_full_line_commands_no_refs
            # it would be better to use the parent command valid_nesting.
                   and !$in_full_line_commands_no_refs{$command}) {
            my $def_block = $current;
            while ($def_block->{'parent'} and (!$def_block->{'parent'}->{'type'} 
                                 or $def_block->{'parent'}->{'type'} ne 'def_line')) {
              $def_block = $def_block->{'parent'};
            }

            $invalid_parent = $def_block->{'parent'}->{'parent'}->{'cmdname'};
          }
        }

        # special case with @ followed by a newline protecting end of lines
        # in @def*
        last if ($self->{'context_stack'}->[-1] eq 'def' and $command eq "\n");

        unless ($self->{'no_paragraph_commands'}->{$command}) {
          my $paragraph = _begin_paragraph($self, $current, $line_nr);
          $current = $paragraph if ($paragraph);
        }

        if ($self->{'close_paragraph_commands'}->{$command}) {
          $current = _end_paragraph($self, $current, $line_nr);
        }
        if ($self->{'close_preformatted_commands'}->{$command}) {
          $current = _end_preformatted($self, $current, $line_nr);
        }

        # commands without braces and not block commands, ie no @end
        if (defined($self->{'misc_commands'}->{$command})) {
          if ($root_commands{$command} or $command eq 'bye') {
            $current = _close_commands($self, $current, $line_nr, undef, 
                                       $command);
            # root_level commands leads to setting a new root
            # for the whole document and stuffing the preceding text
            # as the first content, this is done only once.
            if ($current->{'type'} and $current->{'type'} eq 'text_root') {
              if ($command ne 'bye') {
                $root = { 'type' => 'document_root', 'contents' => [$current] };
                $current->{'parent'} = $root;
                $current = $root;
              }
            } else {
              die if (!defined($current->{'parent'}));
              $current = $current->{'parent'};
            }
          }

          # noarg skipline skipspace text line lineraw /^\d$/
          my $arg_spec = $self->{'misc_commands'}->{$command};
          my $misc;

          if ($arg_spec eq 'noarg') {
            my $ignored = 0;
            my $only_in_headings = 0;
            if ($command eq 'insertcopying') {
              my $parent = $current;
              while ($parent) {
                if ($parent->{'cmdname'} and $parent->{'cmdname'} eq 'copying') {
                  $self->line_error(
                     sprintf($self->__("\@%s not allowed inside `\@%s' block"), 
                             $command, $parent->{'cmdname'}), $line_nr);
                  $ignored = 1;
                  last;
                }
                $parent = $parent->{'parent'};
              }
            } elsif ($in_heading_commands{$command}) {
              $self->line_error(
                sprintf($self->__("\@%s should only appear in heading or footing"),
                        $command), $line_nr);
              $only_in_headings = 1;
            }
            if (!$ignored) {
              $misc = {'cmdname' => $command,
                       'parent' => $current};
              push @{$current->{'contents'}}, $misc;
              # also sets invalid_nesting in that case
              $misc->{'extra'}->{'invalid_nesting'} = 1 if ($only_in_headings);
              $self->_register_global_command($command, $misc, $line_nr);
            }
            $self->_mark_and_warn_invalid($command, $invalid_parent,
                                              $line_nr, $misc);
            $current = $self->_begin_preformatted($current)
              if ($close_preformatted_commands{$command});

          # all the cases using the raw line
          } elsif ($arg_spec eq 'skipline' or $arg_spec eq 'lineraw'
                   or $arg_spec eq 'special') {
            # complete the line if there was a user macro expansion
            if ($line !~ /\n/) {
              my ($new_line, $new_line_nr) = _new_line($self, $line_nr, undef);
              $line .= $new_line if (defined($new_line));
            }
            $misc = {'cmdname' => $command,
                     'parent' => $current};
            my $args = [];
            my $has_comment;
            if ($arg_spec eq 'lineraw' or $arg_spec eq 'skipline') {
              $args = [ $line ];
            } elsif ($arg_spec eq 'special') {
              ($args, $has_comment) 
                = $self->_parse_special_misc_command($line, $command, $line_nr);
              $misc->{'extra'}->{'arg_line'} = $line;
            }

            # if using the @set txi* instead of a proper @-command, replace
            # by the tree obtained with the @-command.  Even though
            # _end_line is called below, as $current is not misc_line_arg
            # there should not be anything done in addition than what is
            # done for @clear or @set.
            if (($command eq 'set' or $command eq 'clear')
                 and scalar(@$args) >= 1
                 and $set_flag_command_equivalent{$args->[0]}) {
              my $arg; 
              if ($command eq 'set') {
                $arg = 'on';
              } else {
                $arg = 'off';
              }
              $command = $set_flag_command_equivalent{$args->[0]};
              $misc = {'cmdname' => $command,
                       'parent' => $current,
                       'line_nr' => $line_nr,
                       'extra' => {'misc_args' => [$arg]}};
              my $misc_line_args = {'type' => 'misc_line_arg',
                     'parent' => $misc};
              $misc->{'args'} = [$misc_line_args];
              my $spaces_after_command 
                = { 'type' => 'empty_spaces_after_command',
                    'text' => ' ',
                    'parent' => $misc_line_args,
                    'extra' => {'command' => $misc} };
              $misc->{'extra'}->{'spaces_after_command'} 
                 = $spaces_after_command;
              $misc_line_args->{'contents'} = [ $spaces_after_command,
                { 'text' => $arg,
                  'parent' => $misc_line_args, },
                { 'text' => "\n",
                  'parent' => $misc_line_args,
                  'type' => 'spaces_at_end', } ];
              push @{$current->{'contents'}}, $misc;
            } else {
              push @{$current->{'contents'}}, $misc;
              foreach my $arg (@$args) {
                push @{$misc->{'args'}},
                  { 'type' => 'misc_arg', 'text' => $arg, 
                    'parent' => $current->{'contents'}->[-1] };
              }
              $misc->{'extra'}->{'misc_args'} = $args 
                 if (scalar(@$args) and $arg_spec ne 'skipline');
            }
            if (! _ignore_global_commands($self)) {
              if ($command eq 'raisesections') {
                $self->{'sections_level'}++;
              } elsif ($command eq 'lowersections') {
                $self->{'sections_level'}--;
              } elsif ($command eq 'novalidate') {
                $self->{'novalidate'} = 1;
              }
            }
            $self->_mark_and_warn_invalid($command, $invalid_parent,
                                              $line_nr, $misc);
            $self->_register_global_command($command, $misc, $line_nr);
            # the end of line is ignored for special commands
            if ($arg_spec ne 'special' or !$has_comment) {
              $current = _end_line($self, $current, $line_nr);
            }

            last NEXT_LINE if ($command eq 'bye');
            # Even if _end_line is called, it is not done since there is 
            # no misc_line_arg
            $current = $self->_begin_preformatted($current)
              if ($close_preformatted_commands{$command});
            last;
          } else {
            # $arg_spec is text, line, skipspace or a number
            my $line_arg = 0;
            $line_arg = 1 if ($arg_spec ne 'skipspace');
            if ($command eq 'item' or $command eq 'itemx' 
               or $command eq 'headitem' or $command eq 'tab') {
              my $parent;
              # itemize or enumerate
              if ($parent = _item_container_parent($current)) {
                if ($command eq 'item') {
                  print STDERR "ITEM_CONTAINER\n" if ($self->{'DEBUG'});
                  $parent->{'items_count'}++;
                  $misc = { 'cmdname' => $command, 'parent' => $parent,
                            'contents' => [],
                            'extra' => 
                              {'item_number' => $parent->{'items_count'}} };
                  push @{$parent->{'contents'}}, $misc;
                  $current = $parent->{'contents'}->[-1];
                } else {
                  $self->line_error(sprintf($self->__(
                                "\@%s not meaningful inside `\@%s' block"), 
                                   $command, $parent->{'cmdname'}), $line_nr);
                }
                $current = $self->_begin_preformatted($current);
              # *table
              } elsif ($parent = _item_line_parent($current)) {
                if ($command eq 'item' or $command eq 'itemx') {
                  print STDERR "ITEM_LINE\n" if ($self->{'DEBUG'});
                  $current = $parent;
                  $self->_gather_previous_item($current, $command, $line_nr);
                  $misc = { 'cmdname' => $command, 'parent' => $current };
                  push @{$current->{'contents'}}, $misc;
                  # since in the %misc_commands hash the entry for those 
                  # commands is 'skipspace' we set $line_arg here.
                  $line_arg = 1;
                } else {
                  $self->line_error(sprintf($self->__(
                                  "\@%s not meaningful inside `\@%s' block"), 
                                    $command, $parent->{'cmdname'}), $line_nr);
                  $current = $self->_begin_preformatted($current);
                }
              # multitable
              } elsif ($parent = _item_multitable_parent($current)) {
                if ($command eq 'item' or $command eq 'headitem'
                     or $command eq 'tab') {
                  if (!$parent->{'extra'}->{'max_columns'}) {
                    $self->line_warn(
                       sprintf($self->__("\@%s in empty multitable"), 
                               $command), $line_nr);
                  } elsif ($command eq 'tab') {
                    my $row = $parent->{'contents'}->[-1];
                    die if (!$row->{'type'});
                    if ($row->{'type'} eq 'before_item') {
                      $self->line_error($self->__("\@tab before \@item"), $line_nr);
                    } elsif ($row->{'cells_count'} >= $parent->{'extra'}->{'max_columns'}) {
                      $self->line_error(sprintf($self->__(
                              "too many columns in multitable item (max %d)"), 
                             $parent->{'extra'}->{'max_columns'}), $line_nr);
                    } else {
                      $row->{'cells_count'}++;
                      $misc = { 'cmdname' => $command,
                                'parent' => $row,
                                'contents' => [],
                                'extra' =>
                            {'cell_number' => $row->{'cells_count'}} };
                      push @{$row->{'contents'}}, $misc;
                      $current = $row->{'contents'}->[-1];
                      #$current = $self->_begin_preformatted($current);
                      print STDERR "TAB\n" if ($self->{'DEBUG'});
                    }
                  } else {
                    print STDERR "ROW\n" if ($self->{'DEBUG'});
                    $parent->{'rows_count'}++;
                    my $row = { 'type' => 'row', 'contents' => [],
                                'cells_count' => 1,
                                'extra' => {'row_number' => $parent->{'rows_count'} },
                                'parent' => $parent };
                    push @{$parent->{'contents'}}, $row;
                    $misc =  { 'cmdname' => $command,
                               'parent' => $row,
                               'contents' => [],
                               'extra' => {'cell_number' => 1}};
                    push @{$row->{'contents'}}, $misc;
                    $current = $row->{'contents'}->[-1];
                  }
                } else {
                  $self->line_error(sprintf($self->__(
                           "\@%s not meaningful inside `\@%s' block"), 
                               $command, $parent->{'cmdname'}), $line_nr);
                }
                $current = $self->_begin_preformatted($current);
              } elsif ($command eq 'tab') {
                $self->line_error($self->__(
                           "ignoring \@tab outside of multitable"), $line_nr);
                $current = $self->_begin_preformatted($current);
              } else {
                $self->line_error (sprintf($self->__(
                   "\@%s outside of table or list"), $command), $line_nr);
                $current = $self->_begin_preformatted($current);
              }
              $misc->{'line_nr'} = $line_nr if (defined($misc));
            } else {
              $misc = { 'cmdname' => $command, 'parent' => $current,
                  'line_nr' => $line_nr };
              push @{$current->{'contents'}}, $misc;
              if ($self->{'sections_level'} and $root_commands{$command}
                   and $command ne 'node' and $command ne 'part') {
                $current->{'contents'}->[-1]->{'extra'}->{'sections_level'}
                  = $self->{'sections_level'};
              }
              # def*x
              if ($def_commands{$command}) {
                my $base_command = $command;
                $base_command =~ s/x$//;
                # check that the def*x is first after @def*, no paragraph
                # in-between.
                my $after_paragraph = _check_no_text($current);
                push @{$self->{'context_stack'}}, 'def';
                $current->{'contents'}->[-1]->{'type'} = 'def_line';
                $current->{'contents'}->[-1]->{'extra'} = 
                   {'def_command' => $base_command,
                    'original_def_cmdname' => $command};
                if ($current->{'cmdname'} 
                    and $current->{'cmdname'} eq $base_command) {
                  pop @{$current->{'contents'}};
                  _gather_def_item($current, $command);
                  push @{$current->{'contents'}}, $misc; 
                }
                if (!$current->{'cmdname'} 
                     or $current->{'cmdname'} ne $base_command
                     or $after_paragraph) {
                  $self->line_error(sprintf($self->__(
                                       "must be after `\@%s' to use `\@%s'"), 
                                          $base_command, $command), $line_nr);
                  $current->{'contents'}->[-1]->{'extra'}->{'not_after_command'} = 1;
                }
              }
            }
            # a container for what is on the @-command line, considered to
            # be the @-command argument
            if ($line_arg) {
              $current = $current->{'contents'}->[-1];
              $current->{'args'} = [{ 'type' => 'misc_line_arg', 
                                      'contents' => [], 
                                      'parent' => $current }];
              # @node is the only misc command with args separated with comma
              # FIXME a 3 lingering here deep into the code may not
              # be very wise...  However having a hash only for one @-command
              # is not very appealing either...
              if ($command eq 'node') {
                $current->{'remaining_args'} = 3;
              } elsif ($command eq 'author') {
                my $parent = $current;
                my $found;
                while ($parent->{'parent'}) {
                  $parent = $parent->{'parent'};
                  last if ($parent->{'type'}
                          and $parent->{'type'} eq 'brace_command_context');
                  if ($parent->{'cmdname'}) {
                    if ($parent->{'cmdname'} eq 'titlepage') {
                      push @{$self->{'extra'}->{'author'}}, $current;
                      $current->{'extra'}->{'titlepage'} = $parent;
                      $found = 1;
                    } elsif ($parent->{'cmdname'} eq 'quotation' or
                        $parent->{'cmdname'} eq 'smallquotation') {
                      push @{$parent->{'extra'}->{'authors'}}, $current;
                      $current->{'extra'}->{'quotation'} = $parent;
                      $found = 1;
                    }
                    last if ($found);
                  }
                }
                if (!$found) {
                  $self->line_warn(sprintf($self->__(
               "\@%s not meaningful outside `\@titlepage' and `\@quotation' environments"),
                                 $command), $current->{'line_nr'});
                }
              } elsif ($command eq 'dircategory' and $self->{'current_node'}) {
                  $self->line_warn($self->__("\@dircategory after first node"),
                               $line_nr);
              }

              $current = $current->{'args'}->[-1];
              push @{$self->{'context_stack'}}, 'line' 
                unless ($def_commands{$command});
            }
            $line = _start_empty_line_after_command($line, $current, $misc);
          }
          $self->_mark_and_warn_invalid($command, $invalid_parent,
                                            $line_nr, $misc);

          $self->_register_global_command($command, $misc, $line_nr);
          if ($command eq 'dircategory' 
              and ! _ignore_global_commands($self)) {
            push @{$self->{'info'}->{'dircategory_direntry'}}, $misc;
          }
        # @-command with matching @end opening
        } elsif (exists($block_commands{$command})) {
          if ($command eq 'macro' or $command eq 'rmacro') {
            my $macro = _parse_macro_command_line($self, $command, $line, 
                                 $current, $line_nr);
            push @{$current->{'contents'}}, $macro;
            $self->_mark_and_warn_invalid($command, $invalid_parent,
                                       $line_nr, $current->{'contents'}->[-1]);
            $current = $current->{'contents'}->[-1];
            last;
          } elsif ($block_commands{$command} eq 'conditional') {
            my $ifvalue_true = 0;
            if ($command eq 'ifclear' or $command eq 'ifset') {
              # REVALUE
              if ($line =~ /^\s+([\w\-][^\s{\\}~`\^+"<>|@]*)\s*(\@(c|comment)((\@|\s+).*)?)?$/) {
                my $name = $1;
                if ((exists($self->{'values'}->{$name}) and $command eq 'ifset')
                    or (!exists($self->{'values'}->{$name}) 
                         and $command eq 'ifclear')) {
                  $ifvalue_true = 1;
                }
                print STDERR "CONDITIONAL \@$command $name: $ifvalue_true\n" if ($self->{'DEBUG'});
              } elsif ($line !~ /\S/) {
                  $self->line_error(sprintf($self->
                    __("%c%s requires a name"), ord('@'), $command), $line_nr);
              } else {
                $self->line_error(sprintf($self->
                    __("bad name for \@%s"), $command), $line_nr);
              }
            } elsif ($command eq 'ifcommanddefined' 
                     or $command eq 'ifcommandnotdefined') {
              # REMACRO
              if ($line =~ /^\s+([[:alnum:]][[:alnum:]\-]*)\s*(\@(c|comment)((\@|\s+).*)?)?$/) {
                my $name = $1;
                my $command_is_defined = (
                  exists($Texinfo::Common::all_commands{$name})
                  or $self->{'macros'}->{$name}
                  or $self->{'definfoenclose'}->{$name}
                  or $self->{'aliases'}->{$name}
                  or $self->{'command_index_prefix'}->{$name}
                );
                if (($command_is_defined
                     and $command eq 'ifcommanddefined')
                    or (! $command_is_defined
                         and $command eq 'ifcommandnotdefined')) {
                  $ifvalue_true = 1;
                }
                print STDERR "CONDITIONAL \@$command $name: $ifvalue_true\n" if ($self->{'DEBUG'});
              } elsif ($line !~ /\S/) {
                  $self->line_error(sprintf($self->
                    __("%c%s requires a name"), ord('@'), $command), $line_nr);
              } else {
                $self->line_error(sprintf($self->
                    __("bad name for \@%s"), $command), $line_nr);
              }
            } elsif ($command =~ /^ifnot(.*)/) {
              $ifvalue_true = 1 if !($self->{'expanded_formats_hash'}->{$1}
                    # exception as explained in the texinfo manual
                    or ($1 eq 'info' 
                        and $self->{'expanded_formats_hash'}->{'plaintext'}));
              print STDERR "CONDITIONAL \@$command format $1: $ifvalue_true\n" if ($self->{'DEBUG'});
            } else {
              die unless ($command =~ /^if(.*)/);
              $ifvalue_true = 1 if ($self->{'expanded_formats_hash'}->{$1}
                      or ($1 eq 'info' 
                          and $self->{'expanded_formats_hash'}->{'plaintext'}));
              print STDERR "CONDITIONAL \@$command format $1: $ifvalue_true\n" if ($self->{'DEBUG'});
            }
            if ($ifvalue_true) {
              push @{$self->{'conditionals_stack'}}, $command;
            } else {
              push @{$current->{'contents'}}, { 'cmdname' => $command, 
                                                'parent' => $current,
                                                'contents' => [] };
              $current = $current->{'contents'}->[-1];
            }
            # FIXME(Karl) ignore what is remaining on the line, to eat 
            # the end of line?
            last;
          } else {
            my $block;
            # a menu command closes a menu_comment, but not the other
            # block commands. This won't catch menu commands buried in 
            # other formats (that are incorrect anyway).
            if ($menu_commands{$command} and $current->{'type'} 
                and ($current->{'type'} eq 'menu_comment' 
                     or $current->{'type'} eq 'menu_entry_description')) {

              my $menu;

              $menu = $current->{'parent'};
              pop @{$menu->{'contents'}}
                if (!@{$current->{'contents'}});

              my $context = pop @{$self->{'context_stack'}};
              if ($context ne 'preformatted') {
                $self->_bug_message("context $context instead of preformatted in new menu", 
                                   $line_nr, $current);
              }
              
              if ($menu->{'type'} and $menu->{'type'} eq 'menu_entry') {
                $menu = $menu->{'parent'};
              }

              $current = $menu;
            }
            # the def command holds a line_def* which corresponds with the
            # definition line.  This allows to have a treatement similar
            # with def*x.
            if ($def_commands{$command}) {
              push @{$self->{'context_stack'}}, 'def';
              $block = { 'parent' => $current,
                         'cmdname' => $command,
                         'contents' => [] };
              push @{$current->{'contents'}}, $block;
              $current = $current->{'contents'}->[-1];
              push @{$current->{'contents'}}, { 
                                                'type' => 'def_line',
                                                'parent' => $current,
                                                'line_nr' => $line_nr,
                                                'extra' => 
                                                 {'def_command' => $command,
                                                  'original_def_cmdname' => $command}
                                                };
            } else {
              $block = { 'cmdname' => $command,
                         'parent' => $current,
                         'contents' => [] };
              push @{$current->{'contents'}}, $block;
            }
            $current = $current->{'contents'}->[-1];

            if ($block_arg_commands{$command}) {
              if ($preformatted_commands{$command}) {
                push @{$self->{'context_stack'}}, 'preformatted';
              } elsif ($format_raw_commands{$command}) {
                push @{$self->{'context_stack'}}, 'rawpreformatted';
                if ($self->{'expanded_formats_hash'}->{$command} 
                    and $self->{'expanded_formats_stack'}->[-1]) {
                  push @{$self->{'expanded_formats_stack'}}, $command;
                } else {
                  push @{$self->{'expanded_formats_stack'}}, 0;
                }
              }
              if ($region_commands{$command}) {
                if (@{$self->{'regions_stack'}}) {
                  $self->line_error(
              sprintf($self->__("region %s inside region %s is not allowed"),
                      $command, $self->{'regions_stack'}->[-1]->{'cmdname'}), 
                                    $line_nr);
                }
                push @{$self->{'regions_stack'}}, $block;
              }
              if ($menu_commands{$command}) {
                if ($self->{'context_stack'}->[-1] eq 'preformatted') {
                  push @{$self->{'context_stack'}}, 'preformatted';
                } else {
                  push @{$self->{'context_stack'}}, 'menu';
                }
                if (! _ignore_global_commands($self)) {
                  push @{$self->{'info'}->{'dircategory_direntry'}}, $block
                    if ($command eq 'direntry');
                  if ($self->{'current_node'}) {
                    if ($command eq 'direntry') {
                      if ($self->{'SHOW_MENU'}) {
                        $self->line_warn($self->__("\@direntry after first node"),
                                  $line_nr);
                      }
                    } elsif ($command eq 'menu') {
                      push @{$self->{'current_node'}->{'menus'}}, $current;
                    }
                  } elsif ($command ne 'direntry') {
                    if ($self->{'SHOW_MENU'}) {
                      $self->line_error(sprintf($self->__("\@%s seen before first \@node"), 
                                                $command), $line_nr);
                      $self->line_error($self->__(
      "perhaps your \@top node should be wrapped in \@ifnottex rather than \@ifinfo?"), 
                                    $line_nr, 1);
                    }
                    if ($command eq 'menu') {
                      push @{$self->{'info'}->{'unassociated_menus'}}, $current;
                    }
                  }
                }
              }
              $current->{'args'} = [ {
                 'type' => 'block_line_arg',
                 'contents' => [],
                 'parent' => $current } ];
              
              $current->{'remaining_args'} = $block_commands{$command} -1 
                if ($block_commands{$command} =~ /^\d+$/ 
                    and $block_commands{$command} -1 > 0);
              $current = $current->{'args'}->[-1];
              push @{$self->{'context_stack'}}, 'line' 
                unless ($def_commands{$command});
            }
            $block->{'line_nr'} = $line_nr;
            $self->_mark_and_warn_invalid($command, $invalid_parent,
                                              $line_nr, $block);
            $self->_register_global_command($command, $block, $line_nr);

            $line = _start_empty_line_after_command($line, $current, $block);
          }
        } elsif (defined($brace_commands{$command})
               or defined($self->{'definfoenclose'}->{$command})) {
          
          push @{$current->{'contents'}}, { 'cmdname' => $command, 
                                            'parent' => $current, 
                                            'contents' => [] };
          $current->{'contents'}->[-1]->{'line_nr'} = $line_nr
            if ($keep_line_nr_brace_commands{$command});
          $self->_mark_and_warn_invalid($command, $invalid_parent,
                                       $line_nr, $current->{'contents'}->[-1]);
          $current = $current->{'contents'}->[-1];
          if ($command eq 'click') {
            $current->{'extra'}->{'clickstyle'} = $self->{'clickstyle'};
          } elsif ($command eq 'kbd') {
            if ($self->{'context_stack'}->[-1] eq 'preformatted'
                and $self->{'kbdinputstyle'} ne 'distinct') {
              $current->{'extra'}->{'code'} = 1;
            } elsif ($self->{'kbdinputstyle'} eq 'code'
                     or ($self->{'kbdinputstyle'} eq 'example'
                         and $self->_in_code($current->{'parent'}))) {
              $current->{'extra'}->{'code'} = 1;
            }
          }
          if ($self->{'definfoenclose'}->{$command}) {
            $current->{'type'} = 'definfoenclose_command';
            $current->{'extra'} = { 
                 'begin' => $self->{'definfoenclose'}->{$command}->[0], 
                 'end' => $self->{'definfoenclose'}->{$command}->[1] };
          }
        } elsif (exists ($no_brace_commands{$command})) {
          push @{$current->{'contents'}},
                 { 'cmdname' => $command, 'parent' => $current };
          # FIXME generalize?
          if ($command eq '\\' and $self->{'context_stack'}->[-1] ne 'math') {
            $self->line_warn(sprintf($self->__("\@%s should only appear in math context"), 
                                        $command), $line_nr);
          }
          if ($command eq "\n") {
            $current = _end_line($self, $current, $line_nr);
            last;
          }
        } else {
          $self->line_error(sprintf($self->__("unknown command `%s'"), 
                                      $command), $line_nr);
        }

      } elsif ($line =~ s/^([{}@,:\t.\f])//) {
        my $separator = $1;
        print STDERR "SEPARATOR: $separator\n" if ($self->{'DEBUG'});
        if ($separator eq '@') {
          # this may happen with a @ at the very end of a file, therefore
          # not followed by anything.
          $self->line_error($self->__("unexpected \@"), $line_nr);
        } elsif ($separator eq '{') {
          _abort_empty_line($self, $current);
          if ($current->{'cmdname'} 
               and (defined($brace_commands{$current->{'cmdname'}})
                     or $self->{'definfoenclose'}->{$current->{'cmdname'}})) {
            my $command = $current->{'cmdname'};
            $current->{'args'} = [ { 'parent' => $current,
                                   'contents' => [] } ];
            $current->{'remaining_args'} = $brace_commands{$command} -1
                  if ($brace_commands{$command} and $brace_commands{$command} -1);
            $current->{'line_nr'} = $line_nr if ($brace_commands{$command});
            if ($self->{'definfoenclose'}->{$command}) {
              $current->{'remaining_args'} = 0;
            }
            $current = $current->{'args'}->[-1];
            if ($context_brace_commands{$command}) {
              if ($command eq 'caption' or $command eq 'shortcaption') {
                my $float;
                if (!$current->{'parent'}->{'parent'} 
                    or !$current->{'parent'}->{'parent'}->{'cmdname'}
                    or $current->{'parent'}->{'parent'}->{'cmdname'} ne 'float') {
                  $float = $current->{'parent'};
                  while ($float->{'parent'} and !($float->{'cmdname'}
                                                  and $float->{'cmdname'} eq 'float')) {
                    $float = $float->{'parent'};
                  }
                  if (!($float->{'cmdname'} and $float->{'cmdname'} eq 'float')) {
                    $self->line_error(sprintf($self->__(
                       "\@%s is not meaningful outside `\@float' environment"),
                                               $command), $line_nr);
                    $float = undef;
                  } else {
                    $self->line_warn(sprintf($self->__(
                                       "\@%s should be right below `\@float'"),
                                               $command), $line_nr);
                  }
                } else {
                  $float = $current->{'parent'}->{'parent'};
                }
                if ($float) {
                  if ($float->{'extra'}->{$command}) {
                    $self->line_warn(sprintf($self->__("ignoring multiple \@%s"),
                                              $command), $line_nr);
                  } else {
                    $current->{'parent'}->{'extra'}->{'float'} = $float;
                    $float->{'extra'}->{$command} = $current->{'parent'};
                  }
                }
              }
              push @{$self->{'context_stack'}}, $command;
              $line =~ s/([^\S\f\n]*)//;
              $current->{'type'} = 'brace_command_context';
              push @{$current->{'contents'}}, { 'type' => 'empty_spaces_before_argument', 
                                        'text' => $1,
                                        'parent' => $current };
              $current->{'parent'}->{'extra'}->{'spaces_before_argument'}
                 = $current->{'contents'}->[-1];
            } else {
              $current->{'type'} = 'brace_command_arg';
              if ($brace_commands{$command}
                  and ($brace_commands{$command} > 1
                       or $simple_text_commands{$command})) {
                push @{$current->{'contents'}}, 
                  {'type' => 'empty_spaces_before_argument',
                   'text' => '' };
                $current->{'parent'}->{'extra'}->{'spaces_before_argument'}
                   = $current->{'contents'}->[-1];
              }
              if ($inline_commands{$command}) {
                # this is changed when the first argument is known.
                push @{$self->{'expanded_formats_stack'}}, 0;
                push @{$self->{'context_stack'}}, $command
                  if ($command eq 'inlineraw');
              }
            }
            print STDERR "OPENED \@$current->{'parent'}->{'cmdname'}, remaining: "
              .(defined($current->{'parent'}->{'remaining_args'}) ? "remaining: $current->{'parent'}->{'remaining_args'}, " : '')
              .($current->{'type'} ? "type: $current->{'type'}" : '')."\n"
               if ($self->{'DEBUG'});
          } elsif ($current->{'parent'}
                    and (($current->{'parent'}->{'cmdname'}
                          and $current->{'parent'}->{'cmdname'} eq 'multitable')
                         or ($current->{'parent'}->{'type'} 
                             and $current->{'parent'}->{'type'} eq 'def_line'))) {
            push @{$current->{'contents'}},
                 { 'type' => 'bracketed', 'contents' => [],
                   'parent' => $current };
            $current = $current->{'contents'}->[-1];
            # we need the line number here in case @ protects end of line
            $current->{'line_nr'} = $line_nr 
              if ($current->{'parent'}->{'parent'}->{'type'}
                  and $current->{'parent'}->{'parent'}->{'type'} eq 'def_line');
            push @{$current->{'contents'}}, 
                {'type' => 'empty_spaces_before_argument',
                 'text' => '' };
            print STDERR "BRACKETED in def/multitable\n" if ($self->{'DEBUG'});
            $current->{'extra'}->{'spaces_before_argument'}
               = $current->{'contents'}->[-1];

          # lone braces accepted right in a rawpreformatted
          } elsif ($current->{'type'} 
                   and $current->{'type'} eq 'rawpreformatted') {
            push @{$current->{'contents'}}, {'text' => '{' };
          # matching braces accepted in a rawpreformatted or math or ignored
          # code
          } elsif ($self->{'context_stack'}->[-1] eq 'math'
                   or $self->{'context_stack'}->[-1] eq 'rawpreformatted'
                   or $self->{'context_stack'}->[-1] eq 'inlineraw'
                   or $self->_ignore_global_commands()) {
            push @{$current->{'contents'}},
                 { 'type' => 'bracketed', 'contents' => [],
                   'parent' => $current, 'line_nr' => $line_nr };
            $current = $current->{'contents'}->[-1];
            print STDERR "BRACKETED in math\n" if ($self->{'DEBUG'});
          } else {
            $self->line_error(sprintf($self->__("misplaced %c"),
                                             ord('{')), $line_nr);
          }

        } elsif ($separator eq '}') {
          _abort_empty_line($self, $current);
          #print STDERR "GGGGG". _print_current ($current);
          if ($current->{'type'} and ($current->{'type'} eq 'bracketed')) {
            $current = $current->{'parent'};
           # the following will not happen for footnote if there is 
           # a paragraph withing the footnote
          } elsif ($current->{'parent'}
                   and $current->{'parent'}->{'cmdname'}
                   and (exists $brace_commands{$current->{'parent'}->{'cmdname'}}
                         or $self->{'definfoenclose'}->{$current->{'parent'}->{'cmdname'}})) {
            # for math and footnote out of paragraph
            if ($context_brace_commands{$current->{'parent'}->{'cmdname'}}) {
              my $context_command = pop @{$self->{'context_stack'}};
              if ($context_command ne $current->{'parent'}->{'cmdname'}) {
                $self->_bug_message("context $context_command instead of brace command $current->{'parent'}->{'cmdname'}", 
                                   $line_nr, $current);
                die;
              }
            }
            # first is the arg.
            
            if ($brace_commands{$current->{'parent'}->{'cmdname'}} 
                and ($brace_commands{$current->{'parent'}->{'cmdname'}} > 1
                   or $simple_text_commands{$current->{'parent'}->{'cmdname'}})
                and $current->{'parent'}->{'cmdname'} ne 'math') {
              # @inline* always have end spaces considered as normal text 
              $self->_isolate_last_space($current) 
                unless ($inline_commands{$current->{'parent'}->{'cmdname'}});
              $self->_register_command_arg($current, 'brace_command_contents');
              # Remove empty arguments, as far as possible
              _remove_empty_content_arguments($current);
            }
            my $closed_command = $current->{'parent'}->{'cmdname'};
            print STDERR "CLOSING(brace) \@$current->{'parent'}->{'cmdname'}\n" 
              if ($self->{'DEBUG'});
            delete $current->{'parent'}->{'remaining_args'};
            if (defined($brace_commands{$closed_command}) 
                 and $brace_commands{$closed_command} == 0
                 and @{$current->{'contents'}}) {
              $self->line_warn(sprintf($self->__(
                                 "command \@%s does not accept arguments"), 
                                       $closed_command), $line_nr);
            }
            if ($current->{'parent'}->{'cmdname'} eq 'anchor') {
              $current->{'parent'}->{'line_nr'} = $line_nr;
              my $parsed_anchor = _parse_node_manual($current);
              if (_check_node_label($self, $parsed_anchor,
                                $current->{'parent'}->{'cmdname'}, $line_nr)) {
                _register_label($self, $current->{'parent'},
                  $parsed_anchor, $line_nr);
                if (@{$self->{'regions_stack'}}) {
                  $current->{'extra'}->{'region'} = $self->{'regions_stack'}->[-1];
                }
              }
            } elsif ($ref_commands{$current->{'parent'}->{'cmdname'}}) {
              my $ref = $current->{'parent'};
              if (@{$ref->{'args'}}) {
                my @args = @{$ref->{'extra'}->{'brace_command_contents'}};
                if (($closed_command eq 'inforef' 
                     and !defined($args[0]) and !defined($args[2]))
                    or ($closed_command ne 'inforef'
                     and !defined($args[0]) and !defined($args[3])
                     and !defined($args[4]))) {
                  $self->line_warn(sprintf($self->__(
                     "command \@%s missing a node or external manual argument"),
                                        $closed_command), $line_nr);
                } else {
                  my $parsed_ref_node = _parse_node_manual($ref->{'args'}->[0]);
                  $ref->{'extra'}->{'node_argument'} = $parsed_ref_node
                     if (defined($parsed_ref_node));
                  if ($closed_command ne 'inforef' 
                      and !defined($args[3]) and !defined($args[4])
                      and !$parsed_ref_node->{'manual_content'}
                      and ! _ignore_global_commands($self)) {
                    push @{$self->{'internal_references'}}, $ref;
                  }
                }
                if (defined($args[1])) {
                  my $normalized_cross_ref_name = 
                    Texinfo::Convert::NodeNameNormalization::normalize_node(
                                                      {'contents' => $args[1]});
                  if ($normalized_cross_ref_name !~ /[^-]/) {
                    $self->line_warn(sprintf($self->__(
                      "in \@%s empty cross reference name after expansion `%s'"),
                          $closed_command,
                          Texinfo::Convert::Texinfo::convert({'contents' => $args[1]})), 
                            $line_nr);
                  }
                }
                if ($closed_command ne 'inforef' and defined($args[2])) {
                  my $normalized_cross_ref_title =
                    Texinfo::Convert::NodeNameNormalization::normalize_node({'contents' => $args[2]});
                  if ($normalized_cross_ref_title !~ /[^-]/) {
                    $self->line_warn(sprintf($self->__(
                     "in \@%s empty cross reference title after expansion `%s'"),
                          $closed_command,
                          Texinfo::Convert::Texinfo::convert({'contents' => $args[2]})), 
                            $line_nr);
                  }
                }
              }
            } elsif ($current->{'parent'}->{'cmdname'} eq 'image') {
              my $image = $current->{'parent'};
              if (!@{$image->{'args'}} 
                  or !@{$image->{'extra'}->{'brace_command_contents'}}
                  or !defined($image->{'extra'}->{'brace_command_contents'}->[0])) {
                $self->line_error(
                   $self->__("\@image missing filename argument"), $line_nr);
              }
            } elsif($current->{'parent'}->{'cmdname'} eq 'dotless') {
              my $dotless = $current->{'parent'};
              if (@{$current->{'contents'}}) {
                my $text = $current->{'contents'}->[0]->{'text'};
                if (!defined ($text)
                  or ($text ne 'i' and $text ne 'j')) {
                  $self->line_error(sprintf($self->
                    __("%c%s expects `i' or `j' as argument, not `%s'"), 
                    ord('@'), $dotless->{'cmdname'}, 
                    Texinfo::Convert::Texinfo::convert($current)), $line_nr);
                }
              }
            } elsif ($explained_commands{$current->{'parent'}->{'cmdname'}}
                     or $inline_commands{$current->{'parent'}->{'cmdname'}}) {
              my $current_command = $current->{'parent'};
              if ($inline_commands{$current_command->{'cmdname'}}) {
                if ($current_command->{'cmdname'} eq 'inlineraw') {
                  my $context_command = pop @{$self->{'context_stack'}};
                  if ($context_command ne $current_command->{'cmdname'}) {
                    $self->_bug_message("context $context_command instead of inlineraw $current_command->{'cmdname'}", 
                                     $line_nr, $current);
                    die;
                  }
                }
                pop @{$self->{'expanded_formats_stack'}};
              }
              if (!@{$current_command->{'args'}} 
                  or !@{$current_command->{'extra'}->{'brace_command_contents'}}
                  or !defined($current_command->{'extra'}->{'brace_command_contents'}->[0])) {
                $self->line_warn(
                   sprintf($self->__("\@%s missing first argument"),
                           $current_command->{'cmdname'}), $line_nr);
              } else {
                if ($explained_commands{$current_command->{'cmdname'}}) {
                  my $normalized_type
                    = Texinfo::Convert::NodeNameNormalization::normalize_node(
                        {'contents' =>
                         $current_command->{'extra'}->{'brace_command_contents'}->[0]});
                  $current_command->{'extra'}->{'normalized'} = $normalized_type;
                  if (!$current_command->{'extra'}->{'brace_command_contents'}->[1]) {
                    if ($self->{'explained_commands'}->{$current_command->{'cmdname'}}->{$normalized_type}) {
                      $current_command->{'extra'}->{'explanation_contents'} 
                        = $self->{'explained_commands'}->{$current_command->{'cmdname'}}->{$normalized_type};
                    }
                  } elsif (! _ignore_global_commands($self)) {
                    $self->{'explained_commands'}->{$current_command->{'cmdname'}}->{$normalized_type} 
                      = $current_command->{'extra'}->{'brace_command_contents'}->[1];
                  }
                }# else {
                #  my $argument
                #   = Texinfo::Convert::Text::convert({'contents' =>
                #       $current_command->{'extra'}->{'brace_command_contents'}->[0]},
                #       {Texinfo::Common::_convert_text_options($self)});
                #  $current_command->{'extra'}->{'format'} = $argument; 
                #}
              }
            } elsif ($current->{'parent'}->{'cmdname'} eq 'errormsg') {
              if (! _ignore_global_commands($self)) {
                my $error_message_text 
                 = Texinfo::Convert::Text::convert($current,
                            {Texinfo::Common::_convert_text_options($self)});
                $self->line_error($error_message_text, $line_nr);
              }
            } elsif (_command_with_command_as_argument($current->{'parent'}->{'parent'})
                 and scalar(@{$current->{'contents'}}) == 0) {
               print STDERR "FOR PARENT \@$current->{'parent'}->{'parent'}->{'parent'}->{'cmdname'} command_as_argument braces $current->{'cmdname'}\n" if ($self->{'DEBUG'});
               $current->{'parent'}->{'type'} = 'command_as_argument' 
                  if (!$current->{'parent'}->{'type'});
               $current->{'parent'}->{'parent'}->{'parent'}->{'extra'}->{'command_as_argument'} 
                  = $current->{'parent'};
            }
            $self->_register_global_command($current->{'parent'}->{'cmdname'},
                                            $current->{'parent'}, $line_nr);
            if ($command_ignore_space_after{$current->{'parent'}->{'cmdname'}}) {
              push @{$current->{'parent'}->{'parent'}->{'contents'}}, 
                 {'type' => 'empty_spaces_after_close_brace',
                  'text' => '' };                          
            }
            $current = $current->{'parent'}->{'parent'};
            $current = $self->_begin_preformatted ($current)
               if ($close_preformatted_commands{$closed_command});
          # lone braces accepted right in a rawpreformatted
          } elsif ($current->{'type'}
                   and $current->{'type'} eq 'rawpreformatted') {
            push @{$current->{'contents'}}, {'text' => '}' };
          # footnote caption closing, when there is a paragraph inside.
          } elsif ($context_brace_commands{$self->{'context_stack'}->[-1]}) {
             # closing the context under broader situations
             $current = _end_paragraph($self, $current, $line_nr);
             if ($current->{'parent'}
                 and $current->{'parent'}->{'cmdname'}
                 and $context_brace_commands{$current->{'parent'}->{'cmdname'}}
                 and $current->{'parent'}->{'cmdname'} eq $self->{'context_stack'}->[-1]) {
              my $context_command = pop @{$self->{'context_stack'}};
              if ($context_command ne $current->{'parent'}->{'cmdname'}) {
                $self->_bug_message("context $context_command instead of brace isolated $current->{'parent'}->{'cmdname'}", 
                                   $line_nr, $current);
                die;
              }
              print STDERR "CLOSING(context command) \@$current->{'parent'}->{'cmdname'}\n" if ($self->{'DEBUG'});
              my $closed_command = $current->{'parent'}->{'cmdname'};
              $self->_register_global_command($current->{'parent'}->{'cmdname'},
                                              $current->{'parent'}, $line_nr);
              $current = $current->{'parent'}->{'parent'};
              $current = $self->_begin_preformatted ($current)
                 if ($close_preformatted_commands{$closed_command});
            }
          } else {
            $self->line_error(sprintf($self->__("misplaced %c"),
                                     ord('}')), $line_nr);
          }
        } elsif ($separator eq ','
                 and $current->{'parent'}->{'remaining_args'}) {
          _abort_empty_line ($self, $current);
          if ($brace_commands{$current->{'parent'}->{'cmdname'}} 
              and ($brace_commands{$current->{'parent'}->{'cmdname'}} > 1
                 or $simple_text_commands{$current->{'parent'}->{'cmdname'}})) {
            $self->_isolate_last_space($current);
            $self->_register_command_arg($current, 'brace_command_contents');
          } else {
            $self->_isolate_last_space($current);
            if (exists $block_commands{$current->{'parent'}->{'cmdname'}}) {
              $self->_register_command_arg($current, 'block_command_line_contents');
            }
          }
          my $type = $current->{'type'};
          $current = $current->{'parent'};
          if ($inline_commands{$current->{'cmdname'}}) {
            if (! $current->{'extra'}->{'format'}) {
              # change the top of the raw_formats_stack now that we know the
              # first arg of the inlineraw
              my $inline_type
                = Texinfo::Convert::Text::convert({'contents' =>
                  $current->{'extra'}->{'brace_command_contents'}->[0]},
                          {Texinfo::Common::_convert_text_options($self)});
              if ($self->{'expanded_formats_stack'}->[-2]) {
                if ($inline_format_commands{$current->{'cmdname'}}) {
                  if ($self->{'expanded_formats_hash'}->{$inline_type}) { 
                    $self->{'expanded_formats_stack'}->[-1] = $inline_type;
                    $current->{'extra'}->{'expand_index'} = 1;
                  } else {
                    $self->{'expanded_formats_stack'}->[-1] = 0;
                  }
                } elsif (($current->{'cmdname'} eq 'inlineifset'
                          and exists($self->{'values'}->{$inline_type}))
                         or ($current->{'cmdname'} eq 'inlineifclear' 
                             and ! exists($self->{'values'}->{$inline_type}))) {
                  $self->{'expanded_formats_stack'}->[-1] 
                         = "$current->{'cmdname'} $inline_type";
                  $current->{'extra'}->{'expand_index'} = 1;
                } else {
                  $self->{'expanded_formats_stack'}->[-1] = 0;
                }
              } else {
                $self->{'expanded_formats_stack'}->[-1] = 0;
              }
              $current->{'extra'}->{'format'} = $inline_type;
            } else {
              # happens for the second arg of inlinefmtifelse
              my $inline_type = $current->{'extra'}->{'format'};
              if ($self->{'expanded_formats_stack'}->[-2]
                  and ! ($self->{'expanded_formats_hash'}->{$inline_type})) {
                $self->{'expanded_formats_stack'}->[-1] = $inline_type;
                $current->{'extra'}->{'expand_index'} = 2;
              } else {
                $self->{'expanded_formats_stack'}->[-1] = 0;
              }
            }
          }
          $current->{'remaining_args'}--;
          push @{$current->{'args'}},
               { 'type' => $type, 'parent' => $current, 'contents' => [] };
          #if ($inline_commands{$current->{'cmdname'}} 
          #    and ! $self->{'expanded_formats_stack'}->[-1]) {
          #  $current->{'args'}->[-1]->{'extra'}->{'ignore'} = 1;
          #} 
          $current = $current->{'args'}->[-1];
          push @{$current->{'contents'}}, 
                 {'type' => 'empty_spaces_before_argument',
                  'text' => '' };                          
        } elsif ($separator eq ',' and $current->{'type'}
            and $current->{'type'} eq 'misc_line_arg'
            and $current->{'parent'}->{'cmdname'} 
            and $current->{'parent'}->{'cmdname'} eq 'node') {
          $self->line_warn($self->__("superfluous arguments for node"), $line_nr);
        # end of menu node (. must be followed by a space to stop the node).
        } elsif (($separator =~ /[,\t.]/ and $current->{'type'}
               and $current->{'type'} eq 'menu_entry_node')
               or ($separator eq ':' and $current->{'type'}
                 and $current->{'type'} eq 'menu_entry_name')) {
          $current = $current->{'parent'};
          push @{$current->{'args'}}, { 'type' => 'menu_entry_separator',
                                 'text' => $separator,
                                 'parent' => $current };
        } elsif ($separator eq "\f" and $current->{'type'}
                 and $current->{'type'} eq 'paragraph') {
          # form feed stops and restart a paragraph.
          $current = $self->_end_paragraph($current);
          push @{$current->{'contents'}}, {'text' => $separator, 
                                           'type' => 'empty_line',
                                            'parent' => $current };
          push @{$current->{'contents'}}, { 'type' => 'empty_line', 
                                            'text' => '',
                                            'parent' => $current };
          
        } else {
          $current = _merge_text($self, $current, $separator);
        }
      # Misc text except end of line
      } elsif ($line =~ s/^([^{}@,:\t.\n\f]+)//) {
        my $new_text = $1;
        $current = _merge_text($self, $current, $new_text);
      # end of line
      } else {
        if ($self->{'DEBUG'}) {
          print STDERR "END LINE: ". _print_current($current)."\n";
        }
        if ($line =~ s/^(\n)//) {
          $current = _merge_text($self, $current, $1);
        } else {
          if (scalar(@{$self->{'input'}})) {
            $self->_bug_message("Text remaining without normal text but `$line'", 
                                $line_nr, $current);
            die;
          }
        }
        #print STDERR "END LINE AFTER MERGE END OF LINE: ". _print_current($current)."\n";
        $current = _end_line($self, $current, $line_nr);
        last;
      }
    }
  }
  while (@{$self->{'conditionals_stack'}}) { 
    my $end_conditional = pop @{$self->{'conditionals_stack'}};
    $self->line_error(sprintf($self->__("expected \@end %s"), $end_conditional), 
                      $line_nr);
  }
  $current = _close_commands($self, $current, $line_nr);

  if (@{$self->{'context_stack'}} != 1) {
    # This happens in 2 cases in the tests:
    #   @verb not closed on misc commands line
    #   def line escaped with @ ending the file
    if ($self->{'DEBUG'}) {
      print STDERR "CONTEXT_STACK no empty end _parse_texi: ".join('|', @{$self->{'context_stack'}})."\n";
    }
    @{$self->{'context_stack'}} = ($self->{'context'});
  }
  if (@{$self->{'expanded_formats_stack'}} != 1) {
    if ($self->{'DEBUG'}) {
      print STDERR "EXPANDED_FORMATS_STACK no empty end _parse_texi: ".join('|', @{$self->{'expanded_formats_stack'}})."\n";
    }
    @{$self->{'expanded_formats_stack'}} = ($self->{'expanded_formats_stack'}->[0]);
  }
  return $root;
}


# parse special line @-commands, unmacro, set, clear, clickstyle.
# Also remove spaces or ignore text, as specified in the misc_commands hash.
sub _parse_special_misc_command($$$$)
{
  my $self = shift;
  my $line = shift;
  my $command = shift;
  my $line_nr = shift;
  my $args = [];

  my $has_comment = 0;
  my $remaining;
  if ($command eq 'set') {
    # REVALUE
    #if ($line =~ s/^\s+([\w\-]+)(\s+(.*?))\s*$//) {
    if ($line =~ /^\s+([\w\-][^\s{\\}~`\^+"<>|@]*)(\@(c|comment)((\@|\s+).*)?|[^\S\f]+(.*?))?[^\S\f]*$/) {
      if ($line =~ s/\@(c|comment)((\@|\s+).*)?$//) {
        $has_comment = 1;
      }
      $line =~ /^\s+([\w\-][^\s{\\}~`\^+"<>|@]*)([^\S\f]+(.*?))?[^\S\f]*$/;
      my $name = $1;
      my $arg = $3;
      $arg = '' if (!defined($arg));
      $args = [$name, $arg];
      $self->{'values'}->{$name} = $arg
        unless(_ignore_global_commands($self));
    } elsif ($line !~ /\S/) {
      $self->line_error(sprintf($self->
                  __("%c%s requires a name"), ord('@'), $command), $line_nr);
    } else {
      $self->line_error(sprintf($self->
                    __("bad name for \@%s"), $command), $line_nr);
    }
  } elsif ($command eq 'clear') {
    # REVALUE
    if ($line =~ /^\s+([\w\-][^\s{\\}~`\^+"<>|@]*)\s*(\@(c|comment)((\@|\s+).*)?)?$/) {
      $args = [$1];
      delete $self->{'values'}->{$1}
        unless(_ignore_global_commands($self));
      $has_comment = 1 if (defined($3));
      #$remaining = $line;
      #$remaining =~ s/^\s+([\w\-]+)\s*(\@(c|comment)((\@|\s+).*)?)?//;
    } elsif ($line !~ /\S/) {
      $self->line_error(sprintf($self->
                  __("%c%s requires a name"), ord('@'), $command), $line_nr);
    } else {
      $self->line_error(sprintf($self->
                    __("bad name for \@%s"), $command), $line_nr);
    }
  } elsif ($command eq 'unmacro') {
    # REMACRO
    if ($line =~ /^\s+([[:alnum:]][[:alnum:]\-]*)\s*(\@(c|comment)((\@|\s+).*)?)?$/) {
      $args = [$1];
      delete $self->{'macros'}->{$1}
        unless(_ignore_global_commands($self));
      $has_comment = 1 if (defined($3));
      print STDERR "UNMACRO $1\n" if ($self->{'DEBUG'});
    } elsif ($line !~ /\S/) {
      $self->line_error(sprintf($self->
                  __("%c%s requires a name"), ord('@'), $command), $line_nr);
    } else {
      $self->line_error(sprintf($self->
                    __("bad name for \@%s"), $command), $line_nr);
    }
  } elsif ($command eq 'clickstyle') {
    # REMACRO
    if ($line =~ /^\s+@([[:alnum:]][[:alnum:]\-]*)({})?\s*/) {
      $args = ['@'.$1];
      $self->{'clickstyle'} = $1
        unless(_ignore_global_commands($self));
      $remaining = $line;
      $remaining =~ s/^\s+@([[:alnum:]][[:alnum:]\-]*)({})?\s*(\@(c|comment)((\@|\s+).*)?)?//;
      $has_comment = 1 if (defined($4));
    } else {
      $self->line_error (sprintf($self->__(
                 "\@%s should only accept a \@-command as argument, not `%s'"),
                                 $command, $line), $line_nr);
    }
  } else {
    die $self->_bug_message("Unknown special command $command", $line_nr);
  }
  if (defined($remaining)) {
    chomp($remaining);
    if ($remaining ne '') {
      $self->line_warn(sprintf($self->__(
                         "remaining argument on \@%s line: %s"), 
                           $command, $remaining), $line_nr);
    }
  }
  return ($args, $has_comment);
}

sub _trim_spaces_comment_from_content($)
{
  Texinfo::Common::trim_spaces_comment_from_content($_[0]);
}

# at the end of a @-command line with arguments, parse the resulting 
# text, to collect aliases, definfoenclose and collect errors on 
# wrong arguments.
sub _parse_line_command_args($$$)
{
  my $self = shift;
  my $line_command = shift;
  my $line_nr = shift;

  my $args;

  my $command = $line_command->{'cmdname'};
  my $arg = $line_command->{'args'}->[0];

  if ($self->{'DEBUG'}) {
    print STDERR "MISC ARGS \@$command\n";
    if (@{$arg->{'contents'}}) {
      my $idx = 0;
      foreach my $content (@{$arg->{'contents'}}) {
        my $name = '';
        $name = '@' . $content->{'cmdname'} if ($content->{'cmdname'});
        my $type = ', t: ';
        $type .= $content->{'type'} if ($content->{'type'});
        my $text = ', ';
        $type .= $content->{'text'} if ($content->{'text'});
        print STDERR "   -> $idx $name $type $text\n";
        $idx++;
      }
    }
  }

  my @contents = @{$arg->{'contents'}};

  _trim_spaces_comment_from_content(\@contents);

  if (! @contents) {
    $self->_command_error($line_command, $line_nr,
               $self->__("\@%s missing argument"), $command);
    $line_command->{'extra'}->{'missing_argument'} = 1;
    return undef;
  }

  if (@contents > 1
         or (!defined($contents[0]->{'text'}))) {
    $self->line_error (sprintf($self->__("superfluous argument to \@%s"),
       $command), $line_nr);
  }
  return undef if (!defined($contents[0]->{'text'}));
  
  my $line = $contents[0]->{'text'};  

  if ($command eq 'alias') {
    # REMACRO
    if ($line =~ s/^([[:alnum:]][[:alnum:]-]*)(\s*=\s*)([[:alnum:]][[:alnum:]-]*)$//) {
      my $new_command = $1;
      my $existing_command = $3;
      $args = [$1, $3];
      $self->{'aliases'}->{$new_command} = $existing_command
        unless (_ignore_global_commands($self));
      if (exists($block_commands{$existing_command})) {
        $self->line_warn(sprintf($self->
                             __("environment command %s as argument to \@%s"), 
                             $existing_command, $command), $line_nr);
      }
    } else {
      $self->line_error(sprintf($self->
                             __("bad argument to \@%s"), $command), $line_nr);
    }

  } elsif ($command eq 'definfoenclose') {
    # REMACRO
    if ($line =~ s/^([[:alnum:]][[:alnum:]\-]*)\s*,\s*([^\s,]*)\s*,\s*([^\s,]*)$//) {
      $args = [$1, $2, $3 ];
      $self->{'definfoenclose'}->{$1} = [ $2, $3 ]
        unless (_ignore_global_commands($self));
      print STDERR "DEFINFOENCLOSE \@$1: $2, $3\n" if ($self->{'DEBUG'});
    } else {
      $self->line_error(sprintf($self->
                              __("bad argument to \@%s"), $command), $line_nr);
    }
  } elsif ($command eq 'columnfractions') {
    my @possible_fractions = split (/\s+/, $line);
    if (!@possible_fractions) {
      $self->line_error (sprintf($self->__("empty \@%s"), $command), 
                             $line_nr);
    } else {
      foreach my $fraction (@possible_fractions) {
        if ($fraction =~ /^(\d*\.\d+)|(\d+)\.?$/) {
          push @$args, $fraction;
        } else {
          $self->line_error (sprintf($self->
                              __("column fraction not a number: %s"),
                              $fraction), $line_nr);
        }
      }
    }
  } elsif ($command eq 'sp') {
    if ($line =~ /^([0-9]+)$/) {
      $args = [$1];
    } else {
      $self->line_error(sprintf($self->__("\@sp arg must be numeric, not `%s'"), 
                                $line), $line_nr);
    }
  } elsif ($command eq 'defindex' || $command eq 'defcodeindex') {
    # REMACRO
    if ($line =~ /^([[:alnum:]][[:alnum:]\-]*)$/) {
      my $name = $1;
      if ($forbidden_index_name{$name}) {
        $self->line_error(sprintf($self->
                                __("reserved index name %s"),$name), $line_nr);
      } else {
        my $in_code = 0;
        $in_code = 1 if ($command eq 'defcodeindex');
        $args = [$name];
        if (! _ignore_global_commands($self)) {
          $self->{'index_names'}->{$name} = {'in_code' => $in_code};
          $self->_register_index_commands($name);
        }
      }
    } else {
      $self->line_error(sprintf($self->
                   __("bad argument to \@%s: %s"), $command, $line), $line_nr);
    }
  } elsif ($command eq 'synindex' || $command eq 'syncodeindex') {
    # REMACRO
    if ($line =~ /^([[:alnum:]][[:alnum:]\-]*)\s+([[:alnum:]][[:alnum:]\-]*)$/) {
      my $index_from = $1;
      my $index_to = $2;
      $self->line_error(sprintf($self->__("unknown source index in \@%s: %s"),
                                  $command, $index_from), $line_nr)
        unless $self->{'index_names'}->{$index_from};
      $self->line_error(sprintf($self->__("unknown destination index in \@%s: %s"), 
                                 $command, $index_to), $line_nr)
        unless $self->{'index_names'}->{$index_to};
      if ($self->{'index_names'}->{$index_from} 
           and $self->{'index_names'}->{$index_to}) {
        my $current_to = $index_to;
        # find the merged indices recursively avoiding loops
        while ($current_to ne $index_from 
               and $self->{'merged_indices'}->{$current_to}) {
          $current_to = $self->{'merged_indices'}->{$current_to};
        }
        if ($current_to ne $index_from) {
          my $index_from_info = $self->{'index_names'}->{$index_from};
          my $index_to_info = $self->{'index_names'}->{$current_to};

          my $in_code = 0;
          $in_code = 1 if ($command eq 'syncodeindex');
          if (! _ignore_global_commands($self)) {
            $self->{'merged_indices'}->{$index_from} = $current_to;
            $index_from_info->{'in_code'} = $in_code;
            foreach my $contained_index (keys %{$index_from_info->{'contained_indices'}}) {
              $index_to_info->{'contained_indices'}->{$contained_index} = 1;
              $self->{'index_names'}->{$contained_index}->{'merged_in'} = $current_to;
            }
            $index_from_info->{'merged_in'} = $current_to;
            $index_to_info->{'contained_indices'}->{$index_from} = 1;
          }
          $args = [$index_from, $index_to];
        } else {
          $self->line_warn(sprintf($self->__(
                         "\@%s leads to a merging of %s in itself, ignoring"), 
                             $command, $index_from), $line_nr);
        }
      }
    } else {
      $self->line_error(sprintf($self->__("bad argument to \@%s: %s"), 
                                $command, $line), $line_nr);
    }
  } elsif ($command eq 'printindex') {
    # REMACRO
    if ($line =~ /^([[:alnum:]][[:alnum:]\-]*)$/) {
      my $name = $1;
      if (!exists($self->{'index_names'}->{$name})) {
        $self->line_error(sprintf($self->__("unknown index `%s' in \@printindex"),
                                    $name), $line_nr);
      
      } else {
        if ($self->{'merged_indices'}->{$name}) {
          $self->line_warn(sprintf($self->__(
                       "printing an index `%s' merged in another one `%s'"), 
                                   $name, $self->{'merged_indices'}->{$name}),
                           $line_nr); 
        }
        if (!defined($self->{'current_node'}) 
            and !defined($self->{'current_section'})
            and !scalar(@{$self->{'regions_stack'}})) {
          $self->line_warn(sprintf($self->__(
                     "printindex before document beginning: \@printindex %s"), 
                                    $name), $line_nr);
        }
        $args = [$name];
      }
    } else {
      $self->line_error(sprintf($self->
                   __("bad argument to \@%s: %s"), $command, $line), $line_nr);
    }
  } elsif (grep {$_ eq $command} ('everyheadingmarks', 'everyfootingmarks',
                                  'evenheadingmarks', 'oddheadingmarks',
                                  'evenfootingmarks', 'oddfootingmarks')) {
    if ($line eq 'top' or $line eq 'bottom') {
      $args = [$line];
    } else {
      $self->line_error(sprintf($self->__(
                      "\@%s arg must be `top' or `bottom', not `%s'"), 
                                $command, $line), $line_nr);
    }
  } elsif ($command eq 'fonttextsize') {
    if ($line eq '10' or $line eq '11') {
      $args = [$line];
    } else {
      $self->line_error(sprintf($self->__(
                        "Only \@%s 10 or 11 is supported, not `%s'"),
                                $command, $line), $line_nr);
    }
  } elsif ($command eq 'footnotestyle') {
    if ($line eq 'separate' or $line eq 'end') {
      $args = [$line];
    } else {
      $self->line_error(sprintf($self->__(
                            "\@%s arg must be `separate' or `end', not `%s'"), 
                                $command, $line), $line_nr);
    }
  } elsif ($command eq 'setchapternewpage') {
    if ($line eq 'on' or $line eq 'off' or $line eq 'odd') {
      $args = [$1];
    } else {
      $self->line_error(sprintf($self->__(
                           "\@%s arg must be `on', `off' or `odd', not `%s'"), 
                                 $command, $line), $line_nr);
    }
  } elsif ($command eq 'need') { # only a warning
    if (($line =~ /^([0-9]+(\.[0-9]*)?)$/) or
             ($line =~ /^(\.[0-9]+)$/)) {
      $args = [$1];
    } else {
      $self->line_error(sprintf($self->__("bad argument to \@%s: %s"), 
                                 $command, $line), $line_nr);
    }
  } elsif ($command eq 'paragraphindent') {
    if ($line =~ /^([\w\-]+)$/) {
      my $value = $1;
      if ($value =~ /^([0-9]+)$/ or $value eq 'none' or $value eq 'asis') {
        $args = [$1];
      } else {
        $self->line_error(sprintf($self->__(
           "\@paragraphindent arg must be numeric/`none'/`asis', not `%s'"), 
                                             $value), $line_nr);
      } 
    } else {
      $self->line_error(sprintf($self->__(
             "\@paragraphindent arg must be numeric/`none'/`asis', not `%s'"), 
                                           $line), $line_nr);
    }
  } elsif ($command eq 'firstparagraphindent') {
    if ($line eq 'none' or $line eq 'insert') {
      $args = [$line];
    } else {
      $self->line_error(sprintf($self->__(
         "\@firstparagraphindent arg must be `none' or `insert', not `%s'"), 
                                           $line), $line_nr);
    }
  } elsif ($command eq 'exampleindent') {
    if ($line =~ /^([0-9]+)/) {
      $args = [$1];
    } elsif ($line =~ /^(asis)$/) {
      $args = [$1];
    } else {
      $self->line_error(sprintf($self->__(
           "\@exampleindent arg must be numeric/`asis', not `%s'"), 
                                           $line), $line_nr);
    }
  } elsif ($command eq 'frenchspacing' 
           or $command eq 'xrefautomaticsectiontitle'
           or $command eq 'codequoteundirected'
           or $command eq 'codequotebacktick'
           or $command eq 'deftypefnnewline') {
    if ($line eq 'on' or $line eq 'off') {
      $args = [$line];
    } else {
      $self->line_error(sprintf($self->__("expected \@%s on or off, not `%s'"), 
                                           $command, $line), $line_nr);
    }
  } elsif ($command eq 'kbdinputstyle') {
    if ($line eq 'code' or $line eq 'example' or $line eq 'distinct') {
      $self->{'kbdinputstyle'} = $line
          unless (_ignore_global_commands($self));
      $args = [$line];
    } else {
      $self->line_error(sprintf($self->__(
      "\@kbdinputstyle arg must be `code'/`example'/`distinct', not `%s'"), 
                                           $line), $line_nr);
    }
  } elsif ($command eq 'allowcodebreaks') {
    if ($line eq 'true' or $line eq 'false') {
      $args = [$line];
    } else {
      $self->line_error(sprintf($self->__(
               "\@allowcodebreaks arg must be `true' or `false', not `%s'"), 
                                           $line), $line_nr);
    }
  } elsif ($command eq 'urefbreakstyle') {
    if ($line eq 'after' or $line eq 'before' or $line eq 'none') {
      $args = [$line];
    } else {
      $self->line_error(sprintf($self->__(
         "\@urefbreakstyle arg must be `after'/`before'/`none', not `%s'"), 
                                           $line), $line_nr);
    }
  } elsif ($command eq 'headings') {
    if ($line eq 'off' or $line eq 'on' or $line eq 'single' 
       or $line eq 'double' or  $line eq 'singleafter' or $line eq 'doubleafter') {
      $args = [$line];
    } else {
      $self->line_error(sprintf($self->__("bad argument to \@%s: %s"), 
                                 $command, $line), $line_nr);
    }
  }
  return $args;
}

1;
__END__
=head1 NAME

Texinfo::Parser - Parse Texinfo code in a Perl tree

=head1 SYNOPSIS

  use Texinfo::Parser;
  my $parser = Texinfo::Parser::parser();
  my $tree = $parser->parse_texi_file("somefile.texi");
  my ($errors, $errors_count) = $parser->errors();
  foreach my $error_message (@$errors) {
    warn $error_message->{'error_line'};
  }

  my ($index_names, $merged_indices_hash)
      = $parser->indices_information();
  my $float_types_arrays = $parser->floats_information();
  my $internal_references_array
    = $parser->internal_references_information();
  # An hash reference on normalized node/float/anchor names
  my $labels_information = $parser->labels_information();
  # A hash reference, keys are @-command names, value is an 
  # array reference holding all the corresponding @-commands.
  my $global_commands_information = $parser->global_commands_information();
  # a hash reference on some document informations (encodings, 
  # input file name, dircategory and direntry list, for exampel).
  my $global_informations = $parser->global_informations();

=head1 DESCRIPTION

Texinfo::Parser will parse Texinfo text into a perl tree.  In one pass
it expands user defined @-commands, conditionals (@ifset, @ifinfo...)
and @value and constructs the tree.  Some extra information is gathered
while doing the tree, for example the block command associated with @end,
the number of row in a multitable, the node associated with a section.



=head1 METHODS

No method is exported in the default case.  The module allows both
an object oriented syntax, or traditional function, with the parser
as an opaque data structure given in argument to every function.

=head2 Initialization

The following method is used to construct a new C<Texinfo::Parser> object:

=over

=item $parser = Texinfo::Parser::parser($options);

This method creates a new parser.  The options may be provided as a hash
reference.  There are two types of options.  The first type of options
change the way the parser behave, they are described right here.  The 
other type of options allow to give to the parser some information as if 
it came from texinfo code, for example allow to set aliases (as with 
C<@alias>), values (as with C<@set>), merged indices (as with 
C<@synindex>).  These options are described below in L</Texinfo Parser options>.

=over

=item expanded_formats

An array reference of the output formats for which C<@ifI<FORMAT>> 
conditional blocks should be expanded.  Default is empty.

The raw block formats (within C<@html> blocks, for example) are 
always kept.

=item gettext

If set, the function reference is used to translate error and warning
messages.  It takes a string as argument and returns a string.  The default 
function returns the error message as is.

=item GLOBAL_COMMANDS

The associated value is a reference on an array.  All the commands in the
array are collected during parsing.  They are afterwards available 
through L<global_informations|/$info = global_informations($parser)>.

=item include_directories

An array reference of directories in which C<@include> files should be 
searched for.  Default contains the working directory, F<.>.

=item INLINE_INSERTCOPYING

If set, C<@insertcopying> is replaced by the C<@copying> content as if
C<@insertcopying> was a user-defined macro.  In the default case, it is 
considered to be a simple @-command and kept as is in the tree.

=item IGNORE_BEFORE_SETFILENAME

If set, and C<@setfilename> exists, everything before C<@setfilename>
is put in a special container type, @C<preamble_before_setfilename>.
This option is set in the default case.

=item IGNORE_SPACE_AFTER_BRACED_COMMAND_NAME

If set, spaces after an @-commande name that take braces are ignored.
Default on.

=item MACRO_BODY_IGNORES_LEADING_SPACE

If set, leading spaces are stripped from user defined macro bodies.

=item MAX_MACRO_CALL_NESTING

Maximal number of nested user-defined macro calls.  Default is 100000.

=item SHOW_MENU

If false, no menu related error are reported.  Default is true.

=begin :comment

Used by Sectioning only
=item TOP_NODE_UP

Text for the up node of the Top node.  The default is C<(dir)>.  The
string may contain @-commands.

=end :comment

=back

=back

=head2 Parsing Texinfo text

There are three methods that may be called to parse some Texinfo code,
C<parse_texi_line> for a line, C<parse_texi_text> for a text fragment, 
and C<parse_texi_file> for a file.

For all those functions, if the I<$parser> argument is undef, a new 
parser object is generated to parse the line, otherwise the parser given 
in argument is used to parse into a tree.

When C<parse_texi_text> is used, the resulting tree is rooted at 
a C<root_line> type container.  Otherwise, the resulting tree should be 
rooted at a C<text_root> type container if it do not contain nodes or 
sections, at a C<document_root> type container otherwise.

=over

=item $tree = parse_texi_line($parser, $text, $first_line_number, $file_name, $macro_name, $fixed_line_number)

This function is used to parse a short fragment of Texinfo code.

I<$text> may be either an array reference of lines, or a text.

The other arguments are optional and allow to specify the position
information of the Texinfo code.  I<$first_line_number> is the line number
of the first text line.  I<$file_name> is the name of the file the
text comes from.  I<$macro> is for the user-defined macro name the text
is expanded from.  If I<$fixed_line_number> is set, the line number is
not increased for the different lines, as if the text was the expansion
of a macro.

=item $tree = parse_texi_text ($parser, $text, $line_numbers_specification, $file_name, $macro_name, $fixed_line_number)

This function is used to parse some Texinfo text.

I<$text> may be either an array reference of lines, or a text.

The other arguments are optional and allow to specify the position
information of the Texinfo code.  There are two distinct cases for 
I<$line_numbers_specification>.  

=over

=item 1.

If it is an array reference, it is considered to hold objects describing 
the position information of each text lines.

=item 2.

If I<$line_numbers_specification> is a scalar, it is the line number of 
the first text line.  In that case (like for C<parse_texi_text>), 
I<$file_name> is the name of the file the text comes from.  
I<$macro> is for the user-defined macro name the text
is expanded from.  If I<$fixed_line_number> is set, the line number is
not increased for the different lines, as if the text was the expansion
of a macro.

=back

=item $tree = parse_texi_file($parser, $file_name)

The file with name I<$file_name> is considered to be a Texinfo file and
is parsed into a tree.

undef is returned if the file couldn't be read.

=back

The errors collected during the tree parsing are available through the
C<errors> method.  This method comes from C<Texinfo::Report>, and is 
described in L<errors|Texinfo::Report/($error_warnings_list, $error_count) = errors ($converter)>.

=head2 Getting informations on the document

After parsing some informations about the Texinfo code that was processed
are available from the parser.

Some global informations is available through C<global_informations>

=over

=item $info = global_informations($parser)

The I<$info> returned is a hash reference.  The possible keys are

=over

=item input_file_name

The name of the main Texinfo input file.

=item input_encoding_name

=item input_perl_encoding

C<input_encoding_name> string is the encoding name used for the 
Texinfo code.
C<input_perl_encoding> string is a corresponding perl encoding name.

=item dircategory_direntry

An array of successive C<@dircategory> and C<@direntry> as they appear
in the document.

=item unassociated_menus

An array of menus that are not associated with a node.

=back

=back

Some command lists are available, such that it is possible to go through
the corresponding tree elements without walking the tree.  They are
available through C<global_commands_information>

=over

=item $commands = global_commands_information($parser)

I<$commands> is an hash reference.  The keys are @-command names.  The 
associated values are array references containing all the corresponding
tree elements.

=back

All the @-commands that have an associated label, that can be the
target of cross references, C<@node>, C<@anchor> and C<@float> with
label have a normalized name associated, constructed as described in the
B<HTML Xref> node in the Texinfo manual.  Those normalized labels and
the association with @-commands is available through C<labels_information>:

=over

=item $labels_information = labels_information($parser)

I<$labels_information> is a hash reference whose keys are normalized
labels, and the associated value is the corresponding @-command.

=back

Information on C<@float> is also available, grouped by type of 
floats, each type correponding to potential C<@listoffloats>.
This information is available through the method C<floats_information>.

=over

=item $float_types = floats_information($parser)

I<$float_types> is a hash reference whose keys are normalized float
types (the first float argument, or the C<@listoffloats> argument).
The normalization is the same than for node names. The value is the list
of float tree elements appearing in the texinfo document.

=back

Internal references, that is, @-commands that refers to node, anchors
or floats within the document are also available:

=over

=item $internal_references_array = internal_references_information($parser);

The function returns a list of cross reference commands referring to
the same document.

=back

Information about defined indices, merged indices and index entries is
also available through the C<indices_information> method.

=over

=item indices_information

  ($index_names, $merged_indices_hash)
    = indices_information($parser);

The index names is a hash reference.  The keys are

=over

=item in_code

1 if the index entries should be formatted as code, 0 in the opposite case.

=item name

The index name.

=item prefix

An array reference of prefix associated to the index.

=item merged_in

In case the index is merged to another index, this key holds the name of 
the index the index is merged into.  It takes into account indirectly
merged indices.

=item contained_indices

An hash reference holding names of indices that are merged to the index,
including itself.  It also contains indirectly merged indices.  This key 
is present even if the index is itself later merged to another index.

=item index_entries

An array reference containing index entry structures for index entries 
associated with the index.  The index entry could be associated to 
@-commands like C<@cindex>, or C<@item> in C<@vtable>, or definition 
commands entries like C<@deffn>.

The keys of the index entry structures are

=over

=item index_name

The index name.

=item index_prefix

The associated index prefix.

=item index_at_command

The name of the @-command associated with the index entry.

=item index_type_command

The @-command associated with the index entry allowing to 
find the index type.

=item content

An array reference corresponding to the index entry content.

=item content_normalized

An array reference corresponding to the index entry content, independent
of the current language.

=item command

The element in the parsed tree associated with the @-command holding the 
index entry.

=item node

The node in the parsed tree containing the index entry.

=item number

The number of the index entry.

=item region

The region command (C<@copying>, C<@titlepage>) containing the index entry,
if it is in such an environement.

=back

=back

The following shows the references corresponding with the default indexes
I<cp> and I<fn>, the I<fn> index having its entries formatted as code and 
the indices corresponding to the following texinfo

  @defindex some
  @defcodeindex code

  $index_names = {'cp' => {'name' => 'cp', 'in_code' => 0, 
                                           'prefix' => ['c', 'cp']},
                  'fn' => {'name' => 'fn', 'in_code' => 1, 
                                           'prefix' => ['f', 'fn']},
                  'some' => {'in_code' => 0},
                  'code' => {'in_code' => 1}};

If C<name> is not set, it is set to the index name.  If C<prefix> is 
not set, it is  set to an array containing the index name.

I<$merged_indices_hash> is a hash reference, the key is an index
name merged in the value.

=back

=head2 Texinfo Parser options

Setting those options is the same as seeing some Texinfo constructs in the 
document.

=over

=item aliases

A hash reference.  The key is a command name, the value is the alias, as
could be set by C<@alias>.

=item clickstyle

A string, the command name associated with C<@clickstyle>.

=item documentlanguage

A string corresponding to a document language set by C<@documentlanguage>.

=item explained_commands

A hash reference of explained commands (currently abbr or acronym).
The value is also a hash reference.  The key of this hash is a normalized
first argument of these commands, the value is a content array
corresponding to the explanation associated with this first argument.

For example giving as an option:

  'explained_commands' 
    => {'acronym' => {'EU' => [{'text' => 'European Union'}]} 

is the same as having the following texinfo code in the document:

  @acronym{EU, European Union}

=item INPUT_ENCODING_NAME

=item INPUT_PERL_ENCODING

C<INPUT_ENCODING_NAME> string is the encoding name as set 
by C<@documentencoding>.
C<INPUT_PERL_ENCODING> string is a corresponding perl encoding name.  
In general those two strings should be set simultaneously.

=item indices

If it is a hash reference, the keys are index names, the values are
index prefix hash references.  The index prefix hash reference values are
prefix, the value is set if the corresponding index entries should be
formatted as if in C<@code>.  An example is as L</indices_information>.

If it is an array reference, it is a list of index names, as if they were
entered as

  @defindex name

=item kbdinputstyle

A string, the C<@kbdinputstyle> style.

=item labels

A hash reference.  Keys are normalized node names as described in the
B<HTML Xref> node in the Texinfo manual.  Instead of a node, it may also
be a float label or an anchor name.  The value is the corresponding 
@-command element in the tree.

=item macros

The associated hash reference has as key user-defined macro names.  The
value is the reference on a macro definition element as obtained by 
the Parser when parsing a C<@macro>.  For example

  @macro mymacro{arg}
  coucou \arg\ after arg
  @end macro

Is associated to a macro definition element

  {'cmdname' => 'macro',
   'args' => [{'text' => 'mymacro', 'type' => 'macro_name'},
              {'text' => 'arg', 'type' => 'macro_arg}],
   'contents' => [{'text' => "coucou \arg\ after arg\n", 'type' => 'raw'}],
   'extra' => {'arg_line' => " mymacro{arg}\n",
               'macrobody' => "coucou \arg\ after arg\n"}}

= item merged_indices

The associated hash reference holds merged indices information, each key 
is merged in the value.  Same as setting C<@synindex> or C<syncodeindex>.

=item novalidate

If set, it is as if C<@novalidate> was set in the document.

=item sections_level

Modifier of the sections level.  Same as calling C<@lowersections> or
C<@raisesections>.

=item values

A hash reference.  Keys are names, values are the corresponding values.
Same as values set by C<@set>.

=back

=head1 TEXINFO TREE

A Texinfo tree element (called element because node is overloaded in 
the Texinfo world) is an hash reference.  There are three main category
of tree element.  Tree elements associated with an @-command have a 
C<cmdname> key holding the @-command name.  Tree element corresponding
to text fragments have a C<text> key holding the corresponding text.
The last category corresponds to other containers (hereafter called 
containers).  In most case these containers have a C<type> key holding 
their name.  Text fragments and @-command elements may also have an 
associated type when such information is needed.

The children of @-command or container elements are in the array
correponding with the C<args> key or with the C<contents> key.  The
C<args> key is for arguments of @-commands, in braces or on the @-command
line.  C<args> is also used for the elements of a menu entry, as a menu
entry is well structured with a limited number of arguments.  
The C<contents> key array holds the contents of the texinfo 
code appearing within a block @-command, within a container, 
within a C<@node> or sectioning @-command.

Another important key for the elements is the C<extra> key which is 
associated to a hash reference and holds all kinds of informations gathered
during the parsing and that may help with the conversion.

=head2 Element keys

=over

=item cmdname

The command name of @-command elements.

=item text

The text fragment of text elements.

=item type

The type of the element.  For C<@verb> it is the delimiter.  But otherwise
it is the type of element considered as a container.  Frequent types 
encountered are I<paragraph> for a paragraph container, 
I<brace_command_arg> for the container holding the brace @-commands 
contents, I<misc_line_arg> and I<block_line_arg> contain the arguments 
appearing on the line of @-commands.  Text fragments may have a type to
give an information of the kind of text fragment, for example 
C<empty_spaces_before_argument> is associated to spaces after a brace 
opening and before the argument.  Many @-commands elements don't have
a type associated.

=item args

Arguments in braces or on @-command line, and the elements of a menu entry.

=item contents

The Texinfo appearing in the element.  For block commands, other 
containers, C<@node> and sectioning commands.

=item parent

The parent element.

=item line_nr

An hash reference corresponding to information on the location of the 
element in the Texinfo input manual.  It should only be available for 
@-command elements, and only for @-commands that are considered to be 
complex enough that the location in the document is needed, for example 
to prepare an error message.  

The keys of the line number hash references are

=over

=item line_nr

The line number of the @-command.

=item file_name

The file name where @-command appeared.

=item macro

The user macro name the @-command is expanded from.

=back

=item extra

A hash reference holding any additional information. 
See L</Information available in the extra key>.

=back

=head2 The containers and types

Some types are associated with @-commands.  As said above, for C<@verb> 
the type is the delimiter.  For a C<@value> command that is not 
expanded because there is no corresponding value set, the type is the 
value argument string.  

The following types also happen for @-commands:

=over

=item def_line

This type may be associated with a definition command with a x form,
like C<@defunx>, C<@defvrx>.  For the form without x, the associated
I<def_line> is the first C<contents> element.  It is described in more
details below.

=item command_as_argument

This is the type of a command given in argument of C<@itemize>, 
C<@table>, C<@vtable> or C<@ftable>.  For example in 

 @itemize @bullet
 @item item
 @end itemize

the element corresponding with bullet has the following keys:

  'cmdname' => 'bullet'
  'type' => 'command_as_argument'

The parent @-command has an entry in extra for the I<command_as_argument>
element:

  'cmdname' => 'itemize'
  'extra => {'command_as_argument' => $command_element_as_argument}

=item index_entry_command

This is the type of index entry command like C<@cindex>, and, more
importantly user defined index entry commands.  So for example if there
is 

 @defindex foo
  ...

 @fooindex index entry

the C<@fooindex> @-command element will have the I<index_entry_command>
type.

=item following_arg

This type is set for non alphabetic accent @-commands that don't use brace 
but instead have their argument right after them, as

  @~n

=item space_command_arg

This type is set for accent @-commands that don't use brace but instead
have their argument after some space, as

  @ringaccent A

=item definfoenclose_command

This type is set for an @-command that is redefined by C<@definfoenclose>.
The beginning is in C<< {'extra'}->{'begin'} >> and the end in 
C<< {'extra'}->{'end'} >>.

=back

The text elements may be associated to the following types:

=over

=item empty_line

An empty line.

=item raw

Text in an environment where it should be kept as is (in C<@verbatim>,
C<@verb>, C<@html>, C<@macro> body).

=item last_raw_newline

The last end of line in a raw block (except for C<@verbatim>).

=item empty_line_after_command

=item empty_spaces_after_command

The text is spaces for I<empty_spaces_after_command> 
or spaces followed by a newline for 
I<empty_line_after_command>, appearing after a @-command that 
takes an argument on the line or a block 
@-commands.

=item spaces_at_end

Space at the end of a @-command line, at the end of some @-commands
with braces or at the end of a bracketed content on a 
C<@multitable> line.

=item empty_space_at_end_def_bracketed

Space at the end of a bracketed content on definition line.

=item space_at_end_block_command

Space at the end of a block @-command line.

=item empty_spaces_before_argument

The text is spaces appearing after an opening brace of after a 
comma separated @-command arguments.

=item empty_spaces_after_close_brace

Spaces appearing after a closing brace, for some rare commands for which
this space should be ignorable (like C<@caption>).

=item empty_spaces_before_paragraph

Space appearing before a paragraph beginning.

=item preamble_text

Text appearing before real content, including the C<\input texinfo.tex>. 

=item space_at_end_menu_node

=item after_description_line

Space after a node in the menu entry, when there is no description,
and space appearing after the description line.

=back

Other special types are described in the following.

=over

=item text_root

=item document_root

=item root_line

These types correspond to document roots.  C<text_root> is the document
root when there is no C<@node> or sectioning command.  When
such a command appears, a new root container is used, C<document_root>,
and C<text_root> becomes the first content of C<document_root>.
C<root_line> is the type of the root tree when parsing Texinfo line
fragments using C<parse_texi_line>.

=item preamble

This container holds the text appearing before the first content, including
the C<\input texinfo.tex> line and following blank lines.

=item preamble_before_setfilename

This container holds everything that appears before C<@setfilename>
if I<IGNORE_BEFORE_SETFILENAME> parser option is set.

=item paragraph

A paragraph.

=item preformatted

Texinfo code within a format that is not filled.  Happens within some
block commands as C<@example>, but also in menu (in menu descriptions,
menu comments...).

=item brace_command_arg

=item brace_command_context

=item block_line_arg

=item misc_line_arg

Those containers are within C<args> of @-commands with braces for 
I<brace_command_arg>, @-commands with braces that start a new context 
(C<@footnote>, C<@caption>, C<@math>) for I<brace_command_context>, 
block command argument on their line for I<block_line_arg> and 
other commands that take texinfo code as argument on their line 
(C<@settitle>, C<@node>, C<@section> and similar) for I<misc_line_arg>.
They hold the content of the command argument.

For example

 @code{in code}

leads to

 {'cmdname' => 'code',
  'args' => [{'type' => 'brace_command_arg',
              'contents' => [{'text' => 'in code'}]}]}

=item misc_arg

Argument of @-command taking specific textual arguments on the line.
For example C<@set>, C<@clickstyle>, C<@unmacro>, C<@comment>.
The argument is associated to the I<text> key.

=item menu_entry

=item menu_entry_leading_text

=item menu_entry_name

=item menu_entry_separator

=item menu_entry_node

=item menu_entry_description

A I<menu_entry> holds a full menu entry, like

  * node::    description.

The different elements of the menu entry are directly in the 
I<menu_entry> C<args> array reference.

I<menu_entry_leading_text> holds the star and following spaces. 
I<menu_entry_name> is the menu entry name (if present), I<menu_entry_node>
corresponds to the node in the menu entry, I<menu_entry_separator> holds
the text after the node and before the description, in most case
C<::   >.  Last I<menu_entry_description> is for the description.

=item menu_comment

The I<menu_comment> container holds what is between menu entries 
in menus.  For example in 

  @menu
  Menu title

  * entry::

  Between entries
  * other::
  @end menu

Both 

  Menu title

and

  Between entries

will be in I<menu_comment>.

=item macro_name

=item macro_arg

Taken from C<@macro> definition and put in the C<args> key array of
the macro, I<macro_name> is the type of the text fragment corresponding 
to the macro name, I<macro_arg> is the type of the text fragments 
correponding to macro formal arguments.

=item before_item

A container for content before the first C<@item> of block @-commands
with items (C<@table>, C<@multitable>, C<@enumerate>...).

=item table_entry

=item table_term

=item table_item

=item inter_item

Those containers appear in C<@table>, C<@ftable> and C<@vtable>.
A I<table_entry> container contains C<@item> and C<@itemx> and
the text following the C<@item> and C<@itemx> entries.  A I<table_term>
container holds all the C<@item> and C<@itemx> of the I<table_entry>.
The I<table_item> container holds the content following the I<table_term>.
If there is some content before an C<@itemx> (normally only comments, 
empty lines or maybe index entriees are allowed), it will be in 
a container with type I<inter_item>. 

=item def_line

=item def_item

=item inter_def_item

The I<def_line> type is either associated with a container within a
definition command, or is the type of a definition command with a x
form, like C<@deffnx>.  It holds the definition line arguments.
The container with type I<def_item> holds the definition text content.
Content appearing before a definition command with a x form is in
an I<inter_def_item> container.

=item multitable_head

=item multitable_body

=item row

In C<@multitable>, a I<multitable_head> container contains all the row
with C<@headitem>, while I<multitable_body> contains the rows associated 
with C<@item>.  A I<row> container contains the C<@item> and @<tab> 
forming a row.

=item bracketed

This a special type containing content in brackets in the context
where they are valid, in C<@math>.

=item bracketed_def_content

Content in brackets on definition command lines.

=item bracketed_multitable_prototype

=item row_prototype

On C<@multitable> line, content in brackets is in 
I<bracketed_multitable_prototype>, text not in brackets
is in I<row_prototype>.

=back

=head2 Information available in the extra key

Some extra keys are available for more than one @-command:

=over

=item block_command_line_contents

=item brace_command_contents

An array associated with block @-commands or @-commands with braces
taking more than one argument or with a simple text content
(C<@anchor>, C<@titlefont>, C<@dmn>).  Each of the element of the
array is either undef, if there is no argument at that place,
or an array reference holding the argument contents.

=item misc_content

The contents of an @-command taking regular Texinfo code as
argument, like C<@sttitle> or C<@exdent>.

=item end_command

The C<@end> associated to the block @-command.

=item missing_argument

Set for some @-commands with line arguments and a missing argument.

=item invalid_nesting

Set if the @-command appears in a context it shouldn't appear in,
like a block @-command on sectioning @-command line.

=item arg_line

The string correspond to the line after the @-command 
for @-commands that have special arguments on their line,
and for C<@macro> line.

=item text_arg

The string correspond to the line after the @-command for @-commands 
that have an argument interpreted as simple text, like C<@setfilename>,
C<@end> or C<@documentencoding>.

=item index_entry

The index entry information (described in L</index_entries>
in details) is associated to @-commands that have an associated
index entry.

=item misc_args

An array holding strings, the arguments of @-commands taking simple
textual arguments as arguments, like C<@everyheadingmarks>, 
C<@frenchspacing>, C<@alias>, C<@synindex>, C<@columnfractions>.
Also filled for C<@set>, C<@clickstyle>, C<@unmacro> or C<@comment>
arguments.

=item spaces_after_command

For @-commands followed by spaces, a reference to the corresponding
text element.

=item spaces_before_argument

For @-commands with opening brace followed by spaces held in a 
C<empty_spaces_before_argument> element, a reference to that element.

=item spaces

For accent commands consisting in letter only, like C<@ringaccent>
appearing like

 @ringaccent A

there is a I<spaces> key which holds the spaces appearing between
the command and the argument.

=back

Then there are extra keys specific of @-commands or containers.

=over

=item C<@macro>

I<invalid_syntax> is set if there was an error on the C<@macro>
line.  The key I<args_index> associates format arguments with
their index on the @macro line formal arguments definition.
The I<macrobody> holds the @macro body.  I<arg_line> holds the
line after C<@macro>.

=item C<@node>

The arguments are in the I<nodes_manuals> array. Each
of the entry has a I<node_content> key for
an array holding the corresponding content, a I<manual_content>
if there is an associated external manual name, and I<normalized>
key for the normalized label, built as specified in the Texinfo manual
in the B<HTML Xref> node.

An I<associated_section> key holds the tree element of the 
sectioning command that follows the node.

=item C<@part>

The next sectioning command is in I<part_associated_section>.

=item sectioning command

The node preceding the command is in I<associated_node>.
The part preceding the command is in I<associated_part>.
If the level of the document was modified by C<@raisections>
or C<@lowersections>, the differential level is in I<sections_level>.

=item C<@float>

=item C<@listoffloats>

If float has a second argument, and for C<@listoffloats>
argument there is a I<type> key which is also a hash reference, 
with two keys. I<content> is an array holding the associated
contents, I<normalized> holds the normalized float type.

I<caption> and I<shortcaption> holds the corresponding 
tree elements for float.  The C<@caption> or C<@shortcaption>
have the float tree element stored in I<float>.

=item C<@float>

=item C<@anchor>

@-Commands that are targets for cross references have a I<normalized>
key for the normalized label, built as specified in the Texinfo manual
in the B<HTML Xref> node.  There is also a I<node_content> key for
an array holding the corresponding content.

C<@anchor> also has I<region> set to the special region name if
in a special region (C<@copying>, C<@titlepage>).

=item C<@ref>

=item C<@xref>

=item C<@pxref>

=item C<@inforef>

The I<node_argument> entry holds a parsed node entry, like
the one appearing in the I<nodes_manuals> array for C<@node>.

=item C<@abbr>

=item C<@acronym>

The first argument normalized is in I<normalized>.  If there is no
second argument, but a second argument appeared previously for the
same first argument, the second argument content of the previous
command is stored in I<explanation_contents>.

=item definition command

I<def_command> holds the command name, without x if it is
an x form of a definition command.
I<original_def_cmdname> is the original def command.

If it is an x form, it has I<not_after_command> set if not 
appearing after the definition command without x.

=item def_line

The I<def_arg> extra key holds an array reference corresponding to
the parsed definition line argument.  Each of the the element of the
array is a two element array reference.  The first element is the type
which could be I<spaces> for a space, types specific of the 
definition, like I<category>, I<name>, I<class>, I<type>, and, at the
end, a mix of I<arg>, I<typearg>, I<delimiter> depending on the definition.
The second element is a hash reference holding the content of the 
type.

The I<def_parsed_hash> hash reference has as key the type specific
of the definition, and as value the corresponding content tree.

=item C<@multitable>

The key I<max_columns> holds the maximal number of columns.  If there
are prototypes on the line they are in the array associated with 
I<prototypes>.  In that case, I<prototypes_line> also holds this 
information, and, in addition, keeps spaces with type C<prototype_space>.  
If there is a C<@columnfractions> as argument, then the 
I<columnfractions> key is associated with the array of columnfractions
arguments, holding all the column fractions.

=item C<@enumerate>

The extra key I<enumerate_specification> contains the enumerate 
argument.

=item C<@itemize>

=item C<@table>

=item C<@vtable>

=item C<@ftable>

The I<command_as_argument> extra key points on the @-command on
as argument on the @-command line.

=item paragraph

The I<indent> or I<noindent> key value is set if the corresponding 
@-commands are associated with that paragraph. 

=item C<@item> in C<@enumerate> or C<@itemize>

The I<item_number> extra key holds the number of this item.

=item C<@item> and C<@tab> in C<@multitable>

The I<cell_count> index key holds the index of the column of
the cell.

=item row

The I<row_number> index key holds the index of the row in 
the C<@multitable>.

=item C<@author>

If in a C<@titlepage>, the titlepage is in I<titlepage>, if in
C<@quotation> or C<@smallquotation>, the corresponding tree element
is in I<quotation>.

The author tree element is in the I<author> array of the C<@titlepage>
or the C<@quotation> or C<@smallquotation> it is associated with.

=item C<@ifclear>

=item C<@ifset>

The original line is in I<line>.

=item C<@end>

The textual argument is in I<command_argument>.
The corresponding @-command is in I<command>.

=item C<@documentencoding>

The argument, normalized is in I<input_encoding_name> if it is recognized.
The corresponding perl encoding name is in I<input_perl_encoding>.

=item C<@click>

In I<clickstyle> there is the current clickstyle command.

=item C<@kbd>

I<code> is set depending on the context and C<@kbdinputstyle>.

=item definfoenclose defined commands

I<begin> holds the string beginning the definfoenclose, 
I<end> holds the string ending the definfoenclose.

=item menu_entry

The I<menu_entry_description> and I<menu_entry_name> keys
are associated with the corresponding tree elements. The
I<menu_entry_node> holds the parsed node entry, like
the one appearing in the I<nodes_manuals> array for C<@node>.

=item empty_line_after_command

The corresponding command is in I<command>.

=item C<@inlinefmt>

=item C<@inlineraw>

=item C<@inlinefmtifelse>

=item C<@inlineifclear>

=item C<@inlineifset>

The first argument is in I<format>.  If an argument has been determined
as being expanded by the Parser, the index of this argument is in
I<expand_index>.  Index numbering begins at 0, but the first argument is
always the format or flag name, so, if set, it should be 1 or 2 for
C<@inlinefmtifelse>, and 1 for other commands.

=back

=head1 SEE ALSO

L<Texinfo manual|http://www.gnu.org/s/texinfo/manual/texinfo/>

=begin :comment

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=end :comment

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2010, 2011, 2012 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License,
or (at your option) any later version.

=cut
