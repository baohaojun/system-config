# Common.pm: definition of commands. Common code of other Texinfo modules.
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

package Texinfo::Common;

use strict;

# for unicode/layer support in binmode
use 5.006;

# to determine the null file
use Config;
use File::Spec;

use Texinfo::Documentlanguages;

# debugging
use Carp qw(cluck);

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Texinfo::Covert::Text ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
expand_verbatiminclude
definition_category
expand_today
numbered_heading
trim_spaces_comment_from_content
float_name_caption
normalize_top_node_name
protect_comma_in_tree
protect_first_parenthesis
protect_hashchar_at_line_beginning
protect_colon_in_tree
protect_node_after_label_in_tree
valid_tree_transformation
move_index_entries_after_items_in_tree
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '5.1.90';

# i18n
sub N__($)
{
  return $_[0];
}

# determine the null devices
my $default_null_device = File::Spec->devnull();
our %null_device_file = (
  $default_null_device => 1
);
# special case, djgpp recognizes both null devices
if ($Config{osname} eq 'dos' and $Config{osvers} eq 'djgpp') {
  $null_device_file{'/dev/null'} = 1;
  $null_device_file{'NUL'} = 1;
}

# these are the default values for the parser state that may be 
# initialized to values given by the user.
# They are defined here, because they are used below and we 
# don't want Texinfo::Common to use Texinfo::Parser.
our %default_parser_state_configuration = (
  # this is the initial context.  It is put at the bottom of the 
  # 'context_stack'.  It is not clear if this is really useful to be
  # able to customize that value.
  'context' => '_root',
  'expanded_formats' => [],
  'gettext' => sub {return $_[0];},
  'pgettext' => sub {return $_[1];},
  'include_directories' => [ '.' ],
  # these are the user-added indices.  May be an array reference on names
  # or an hash reference in the same format than %index_names below
  'indices' => [],
  # the following are dynamically modified during the document parsing.
  'aliases' => {},            # key is a command name value is the alias
  'clickstyle' => 'arrow',
  'documentlanguage' => undef,
                              # Current documentlanguage set by 
                              # @documentlanguage
  'explained_commands' => {}, # the key is a command name, either acronym
                              # or abbr, the value is a hash.  The key hash 
                              # is a normalized first argument of the 
                              # corresponding command, the value is the 
                              # contents array of the previous command with
                              # this first arg and a second arg.
  'kbdinputstyle' => 'distinct',
  'labels'          => {},    # keys are normalized label names, as described
                              # in the `HTML Xref' node.  Value should be
                              # a node/anchor or float in the tree.
  'macros' => {},             # the key is the user-defined macro name.  The 
                              # value is the reference on a macro element 
                              # as obtained by parsing the @macro
  'merged_indices' => {},     # the key is merged in the value
  'novalidate' => 0,          # same as setting @novalidate.
  'sections_level' => 0,      # modified by raise/lowersections
  'values' => {'txicommandconditionals' => 1},
                              # the key is the name, the value the @set name 
                              # argument.  A Texinfo tree may also be used.
);

# command-line options
#my @command_line_settable_at_commands = ('footnotestyle', 'novalidate',
#  'documentlanguage', 'paragraphindent');


# FIXME maybe this should better be set as texinfo passed to texi2any as
# texi2dvi --command 

# customization options
our %document_settable_at_commands = (
  'allowcodebreaks' => 'true',
  'clickstyle' => '@arrow',
  'codequotebacktick' => 'off',
  'codequoteundirected' => 'off',
  'contents' => 0,
  'deftypefnnewline' => 'off',
  'documentencoding' => 'us-ascii',
  'documentlanguage' => 'en',
  # is N ems in TeX, 0.4 in.
  'exampleindent' => 5,
  'firstparagraphindent' => 'none',
  'frenchspacing' => 'off',
  'headings' => 'on',
  'kbdinputstyle' => 'distinct',
  'paragraphindent' => 3,
  'shortcontents' => 0,
  'urefbreakstyle' => 'after',
  'xrefautomaticsectiontitle' => 'off',
);

# those should be unique
our %document_settable_unique_at_commands = (
  # when passed through a configuration variable, documentdescription
  # should be already formatted for HTML
  'documentdescription' => undef,
  'evenfootingmarks' => undef,
  'evenheadingmarks' => undef,
  'everyfootingmarks' => 'bottom', 
  'everyheadingmarks' => 'bottom',
  'fonttextsize' => 11, 
  'footnotestyle' => 'end', 
  'novalidate' => 0,
  'oddfootingmarks' => undef,
  'oddheadingmarks' => undef,
  # FIXME not clear here.
  'pagesizes' => undef,
  'setchapternewpage' => 'on',
  'setcontentsaftertitlepage' => 0,
  'setfilename' => undef,
  'setshortcontentsaftertitlepage' => 0,
  'everyheading'      => undef,
  'everyfooting'      => undef,
  'evenheading'       => undef,
  'evenfooting'       => undef,
  'oddheading'        => undef,
  'oddfooting'        => undef,
);

my @command_line_settables = ('FILLCOLUMN', 'SPLIT', 'SPLIT_SIZE',
  'HEADERS',
  'MACRO_EXPAND', 'NUMBER_SECTIONS',
  'NUMBER_FOOTNOTES', 'NODE_FILES',
  'NO_WARN', 'VERBOSE',
  'TRANSLITERATE_FILE_NAMES', 'ERROR_LIMIT', 'ENABLE_ENCODING',
  'FORCE', 'INTERNAL_LINKS', 'OUTFILE', 'SUBDIR', 'OUT',
  'SILENT', 'CASE_INSENSITIVE_FILENAMES',
);

# documented in the Texinfo::Parser pod section
# all are lower cased in texi2any.pl
my @parser_options = map {uc($_)} (keys(%default_parser_state_configuration));

my @obsolete_variables = ('TOP_HEADING_AT_BEGINNING', 'USE_SECTIONS',
  'IDX_SUMMARY', 'I18N_PERL_HASH', 'USE_UNICODE', 'USE_NLS',
  'USE_UP_FOR_ADJACENT_NODES', 'SEPARATE_DESCRIPTION', 
  'NEW_CROSSREF_STYLE', 'SHORT_REF', 'IGNORE_PREAMBLE_TEXT',
  'OUT_ENCODING', 
  'IN_ENCODING', 'DEFAULT_ENCODING');

my @variable_settables_not_used = ('COMPLETE_IMAGE_PATHS', 'TOC_FILE',
  'SPLIT_INDEX');

my @formats_settable = (
);

my @variable_string_settables = (
  'DEBUG', 'FRAMES', 'FRAMESET_DOCTYPE', 'DOCTYPE', 'TEST', 'DUMP_TEXI',
  'TOP_FILE', 'SHOW_MENU', 'USE_NODES', 'TOC_LINKS', 'SHORTEXTN',
  'PREFIX', 'DEF_TABLE', 'L2H', 'MONOLITHIC',
  'L2H_L2H', 'L2H_SKIP', 'L2H_TMP', 'L2H_FILE', 'L2H_CLEAN',
  'L2H_HTML_VERSION', 'EXTERNAL_DIR', 'USE_ISO',
  'VERTICAL_HEAD_NAVIGATION', 'INLINE_CONTENTS', 'NODE_FILE_EXTENSION',
  'NO_CSS', 'INLINE_CSS_STYLE', 'USE_TITLEPAGE_FOR_TITLE',
  'SIMPLE_MENU', 'EXTENSION', 'INLINE_INSERTCOPYING', 'USE_NUMERIC_ENTITY',
  'ENABLE_ENCODING_USE_ENTITY', 'ICONS',
  'USE_UNIDECODE', 'DATE_IN_HEADER', 'OPEN_QUOTE_SYMBOL',
  'CLOSE_QUOTE_SYMBOL', 'TOP_NODE_UP', 'TOP_NODE_UP_URL', 'TOP_NODE_FILE',
  'TOP_NODE_FILE_TARGET', 'SHOW_TITLE', 'WORDS_IN_PAGE',
  'HEADER_IN_TABLE', 'USE_ACCESSKEY', 'USE_REL_REV', 'USE_LINKS',
  'OVERVIEW_LINK_TO_TOC', 'AVOID_MENU_REDUNDANCY', 'NODE_NAME_IN_MENU',
  'NODE_NAME_IN_INDEX', 'NO_USE_SETFILENAME', 'USE_SETFILENAME_EXTENSION',
  'COMPLEX_FORMAT_IN_TABLE',
  'IGNORE_BEFORE_SETFILENAME', 'IGNORE_SPACE_AFTER_BRACED_COMMAND_NAME',
  'USE_NODE_TARGET',
  'PROGRAM_NAME_IN_FOOTER', 'NODE_FILENAMES',
  'EXTERNAL_CROSSREF_SPLIT', 'BODYTEXT',
  'CSS_LINES', 'RENAMED_NODES_REDIRECTIONS', 'RENAMED_NODES_FILE',
  'CPP_LINE_DIRECTIVES',
  'TEXI2DVI', 'DUMP_TREE', 'MAX_MACRO_CALL_NESTING',
  'INPUT_ENCODING_NAME', 'INPUT_PERL_ENCODING', 
  'OUTPUT_ENCODING_NAME', 'OUTPUT_PERL_ENCODING', 
  'PACKAGE_VERSION',
  'PACKAGE_AND_VERSION', 'PACKAGE_URL', 'PACKAGE', 'PACKAGE_NAME', 'PROGRAM',
  'PRE_BODY_CLOSE', 'AFTER_BODY_OPEN', 'PRE_ABOUT', 'AFTER_ABOUT',
  'EXTRA_HEAD', 'DO_ABOUT',
  'DEFAULT_RULE', 'BIG_RULE',
  'MENU_ENTRY_COLON', 'INDEX_ENTRY_COLON', 'MENU_SYMBOL',
  'MAX_HEADER_LEVEL', 'CHAPTER_HEADER_LEVEL',
  'FOOTNOTE_END_HEADER_LEVEL', 'FOOTNOTE_SEPARATE_HEADER_LEVEL',
  'USE_UP_NODE_FOR_ELEMENT_UP',
  'BEFORE_OVERVIEW', 'AFTER_OVERVIEW',
  'BEFORE_TOC_LINES', 'AFTER_TOC_LINES',
  'SORT_ELEMENT_COUNT', 'SORT_ELEMENT_COUNT_WORDS',
  'KEEP_TOP_EXTERNAL_REF',
  'TEXI2HTML', 'IMAGE_LINK_PREFIX', 'FIX_TEXINFO',
  'TREE_TRANSFORMATIONS', 'BASEFILENAME_LENGTH',
  'TEXTCONTENT_COMMENT', 'XREF_USE_FLOAT_LABEL', 'XREF_USE_NODE_NAME_ARG',
  'MACRO_BODY_IGNORES_LEADING_SPACE', 'CHECK_HTMLXREF',
  'TEXINFO_DTD_VERSION', 'TEXINFO_COLUMN_FOR_DESCRIPTION',
  'TEXINFO_OUTPUT_FORMAT', 'INFO_SPECIAL_CHARS_WARNING',
);
# Not strings. 
# FIXME To be documented somewhere, but where?
my @variable_other_settables = (
  'LINKS_BUTTONS', 'TOP_BUTTONS', 'SECTION_BUTTONS', 'BUTTONS_TEXT',
  'BUTTONS_ACCESSKEY', 'BUTTONS_REL', 'BUTTONS_GOTO',
  'CHAPTER_FOOTER_BUTTONS', 'SECTION_FOOTER_BUTTONS',
  'NODE_FOOTER_BUTTONS',
  'MISC_BUTTONS', 'CHAPTER_BUTTONS', 'BUTTONS_NAME',
  'BUTTONS_EXAMPLE', 'SPECIAL_ELEMENTS_NAME', 'SPECIAL_ELEMENTS_CLASS',
  'ACTIVE_ICONS', 'PASSIVE_ICONS',
  'CSS_FILES', 'CSS_REFS', 
  'GLOBAL_COMMANDS',
);

my %valid_options;
foreach my $var (keys(%document_settable_at_commands), 
         keys(%document_settable_unique_at_commands),
         @command_line_settables, @variable_string_settables, 
         @variable_other_settables, @parser_options,
         @formats_settable,
         @obsolete_variables, @variable_settables_not_used) {
  $valid_options{$var} = 1;
}

my %obsolete_options;
foreach my $var (@obsolete_variables) {
  $obsolete_options{$var} = 1;
}

sub valid_option($)
{
  my $option = shift;
  return $valid_options{$option};
}

sub obsolete_option($)
{
  my $option = shift;
  return $obsolete_options{$option};
}

my %customization_variable_classes = (
  'document_settable_at_commands' => [ sort(keys(%document_settable_at_commands)) ],
  'document_settable_unique_at_commands' => [ sort(keys(%document_settable_unique_at_commands)) ],
  'command_line_settables' => \@command_line_settables,
  'variable_string_settables' => \@variable_string_settables,
  'variable_other_settables' => \@variable_other_settables,
  'parser_options' => \@parser_options,
  #'formats_settable' => \@formats_settable,
  'obsolete_variables' => \@obsolete_variables,
  'variable_settables_not_used' => \@variable_settables_not_used,
);

my @secondary_customization_variables = (
  'obsolete_variables', 'variable_settables_not_used'
);
sub _customization_variable_classes(;$)
{
  my $print_all = shift;
  my $result = '';
  foreach my $type (sort(keys(%customization_variable_classes))) {
    next if (!$print_all 
             and grep {$_ eq $type} @secondary_customization_variables);
    foreach my $variable (@{$customization_variable_classes{$type}}) {
      $result .= "$variable\t$type\n";
    }
  }
  return $result;
}

my %valid_tree_transformations;
foreach my $valid_transformation ('simple_menus', 
    'fill_gaps_in_sectioning', 'move_index_entries_after_items',
    'insert_nodes_for_sectioning_commands',
    'complete_tree_nodes_menus', 'regenerate_master_menu',
    'indent_menu_descriptions') {
  $valid_tree_transformations{$valid_transformation} = 1;
}

sub valid_tree_transformation ($)
{
  my $transformation = shift;
  return 1 if (defined($transformation) 
               and $valid_tree_transformations{$transformation});
  return 0;
}

our %no_brace_commands;             # commands never taking braces
%no_brace_commands = (
           '*', "\n",
           ' ', ' ',
           "\t", ' ',
           "\n", ' ',
           '-', '',  # hyphenation hint
           '|', '',  # used in formatting commands @evenfooting and friends
           '/', '',
           ':', '',
           '!', '!',
           '?', '?',
           '.', '.',
           '@', '@',
           '}', '}',
           '{', '{',
           '\\', '\\',  # should only appear in math
);


# commands taking a line as argument or no argument.
# sectioning commands and def* commands are added below.
# index commands are added dynamically.
#
# The values signification is:
# special:     no value and macro expansion, all the line is used, and 
#              analysed during parsing (_parse_special_misc_command)
# lineraw:     no value and macro expansion, the line is kept as-is, not 
#              analysed
# skipline:    no argument, everything else on the line is skipped
# skipspace:   no argument, following spaces are skipped.
# noarg:       no argument
# text:        the line is parsed as texinfo, and the argument is converted
#              to simple text (in _end_line)
# line:        the line is parsed as texinfo
# a number:    the line is parsed as texinfo and the result should be plain 
#              text maybe followed by a comment; the result is analysed
#              during parsing (_parse_line_command_args).  
#              The number is an indication of the number of arguments of 
#              the command.
#
# Beware that @item and @itemx may be like 'line' or 'skipspace' depending
# on the context.
our %misc_commands = (
  'node'              => 'line', # special arg
  'bye'               => 'skipline', # no arg
  'end'               => 'text',
  # set, clear
  'set'               => 'special', # special arg
  'clear'             => 'special', # special arg
  'unmacro'           => 'special', 
  # comments
  'comment'           => 'lineraw',
  'c'                 => 'lineraw',
  # special
  'definfoenclose'    => 3,
  'alias'             => 2, 
  # number of arguments is not known in advance.
  'columnfractions'   => 1, 
  # file names
  'setfilename'       => 'text',
  'verbatiminclude'   => 'text',
  'include'           => 'text',

  'raisesections'     => 'skipline',  # no arg
  'lowersections'     => 'skipline', # no arg
  'contents'          => 'skipline', # no arg
  'shortcontents'     => 'skipline', # no arg
  'summarycontents'   => 'skipline', # no arg
  'insertcopying'     => 'noarg', # no arg
  'clickstyle'        => 'special', # arg should be an @-command
  # more relevant in preamble
  'setcontentsaftertitlepage'      => 'skipline', # no arg
  'setshortcontentsaftertitlepage' => 'skipline', # no arg
  'documentencoding'  => 'text', # or 1?
  'novalidate'        => 'skipline', # no arg
  'dircategory'       => 'line', # line. Position with regard 
                                 # with direntry is significant
  'pagesizes'         => 'line', # can have 2 args 
                           # or one? 200mm,150mm 11.5in
  'finalout'          => 'skipline', # no arg
  'paragraphindent'   => 1, # arg none asis 
                       # or a number and forbids anything else on the line
  'firstparagraphindent' => 1, # none insert
  'frenchspacing'     => 1, # on off
  'codequoteundirected'       => 1, # on off
  'codequotebacktick'         => 1, # on off
  'xrefautomaticsectiontitle' => 1, # on off
  'deftypefnnewline'  => 1, # on off
  'fonttextsize'      => 1, # 10 11
  'allowcodebreaks'   => 1, # false or true
  'exampleindent'     => 1, # asis or a number
  'footnotestyle'     => 1, # end and separate, nothing else on the line
  'urefbreakstyle'    => 1, # after|before|none
  'afourpaper'        => 'skipline', # no arg
  'afivepaper'        => 'skipline', # no arg
  'afourlatex'        => 'skipline', # no arg
  'afourwide'         => 'skipline', # no arg
  'headings'          => 1, #off on single double singleafter doubleafter
                            # interacts with setchapternewpage
  'setchapternewpage' => 1, # off on odd

  # only relevant in TeX, and special
  'everyheading'      => 'lineraw',  # @*heading @*footing use @|
  'everyfooting'      => 'lineraw',  # + @thispage @thissectionname
  'evenheading'       => 'lineraw',  # @thissectionnum @thissection
  'evenfooting'       => 'lineraw',  # @thischaptername @thischapternum
  'oddheading'        => 'lineraw',  # @thischapter @thistitle @thisfile
  'oddfooting'        => 'lineraw',

  'smallbook'         => 'skipline', # no arg
  'syncodeindex'      => 2,   # args are index identifiers
  'synindex'          => 2,
  'defindex'          => 1, # one identifier arg
  'defcodeindex'      => 1, # one identifier arg
  'documentlanguage'  => 'text',     # language code arg
  'kbdinputstyle'     => 1,          # code example distinct
  'everyheadingmarks' => 1, # top bottom
  'everyfootingmarks' => 1,
  'evenheadingmarks'  => 1,
  'oddheadingmarks'   => 1,
  'evenfootingmarks'  => 1,
  'oddfootingmarks'   => 1,
  # not valid for info (should be in @iftex)
  'cropmarks'         => 'skipline', # no arg

  # formatting
  'center'            => 'line',
  'printindex'        => 1,
  'listoffloats'      => 'line',
  # especially in titlepage
#  'shorttitle'        => 'line',
  'shorttitlepage'    => 'line',
  'settitle'          => 'line',
  'author'            => 'line',
  'subtitle'          => 'line',
  'title'             => 'line',
  'sp'                => 1, # numerical arg
  'page'              => 'skipline', # no arg (pagebreak)
  'need'              => 1, # one numerical/real arg
  # formatting
  'noindent'          => 'skipspace', # no arg
  'indent'            => 'skipspace',
  'exdent'            => 'line',
  'headitem'          => 'skipspace',
  'item'              => 'skipspace', # or line, depending on the context
  'itemx'             => 'skipspace', # or line, depending on the context
  'tab'               => 'skipspace', 
  # only valid in heading or footing
  'thischapter'       => 'noarg',
  'thischaptername'   => 'noarg',
  'thischapternum'    => 'noarg',
  'thisfile'          => 'noarg',
  'thispage'          => 'noarg',
  'thistitle'         => 'noarg',
  # not valid for info (should be in @iftex)
  'vskip'             => 'lineraw', # arg line in TeX
  # obsolete @-commands.
  'refill'            => 'noarg',   # no arg (obsolete, to be ignored)
  # Remove spaces and end of lines after the 
  # commands? If no, they can lead to empty lines
  'quote-arg'         => 'skipline',
  'allow-recursion'   => 'skipline',
);

# key is index name, keys of the reference value are the prefixes.
# value associated with the prefix is 0 if the prefix is not a code-like
# prefix, 1 if it is a code-like prefix (set by defcodeindex/syncodeindex).
#our %index_names = (
# 'cp' => {'cp' => 0, 'c' => 0},
# 'fn' => {'fn' => 1, 'f' => 1},
# 'vr' => {'vr' => 1, 'v' => 1},
# 'ky' => {'ky' => 1, 'k' => 1},
# 'pg' => {'pg' => 1, 'p' => 1},
# 'tp' => {'tp' => 1, 't' => 1}
#);

our %index_names = (
 'cp' => {'prefix' => ['c'], 'in_code' => 0},
 'fn' => {'prefix' => ['f'], 'in_code' => 1},
 'vr' => {'prefix' => ['v'], 'in_code' => 1},
 'ky' => {'prefix' => ['k'], 'in_code' => 1},
 'pg' => {'prefix' => ['p'], 'in_code' => 1},
 'tp' => {'prefix' => ['t'], 'in_code' => 1},
);

foreach my $index(keys(%index_names)) {
  $index_names{$index}->{'name'} = $index;
  push @{$index_names{$index}->{'prefix'}}, $index;
}

our %default_index_commands;
# all the commands are readded dynamically in the Parser.
foreach my $index_name (keys (%index_names)) {
  foreach my $index_prefix (@{$index_names{$index_name}->{'prefix'}}) {
    next if ($index_prefix eq $index_name);
    # only put the one letter versions in the hash.
    $misc_commands{$index_prefix.'index'} = 'line';
    $default_index_commands{$index_prefix.'index'} = 1;
  }
}

# command with braces. Value is the max number of arguments.
our %brace_commands;    

our %letter_no_arg_commands;
foreach my $letter_no_arg_command ('aa','AA','ae','oe','AE','OE','o','O',
                                   'ss','l','L','DH','dh','TH','th') {
  $letter_no_arg_commands{$letter_no_arg_command} = 1;
  $brace_commands{$letter_no_arg_command} = 0;
}

foreach my $no_arg_command ('TeX','LaTeX','bullet','copyright',
  'registeredsymbol','dots','enddots','equiv','error','expansion','arrow',
  'minus','point','print','result','today',
  'exclamdown','questiondown','pounds','ordf','ordm',
  'atchar', 'lbracechar', 'rbracechar', 'backslashchar', 'hashchar', 'comma',
  'euro', 'geq','leq','tie','textdegree','click',
  'quotedblleft','quotedblright','quoteleft','quoteright','quotedblbase',
  'quotesinglbase','guillemetleft','guillemetright','guillemotleft',
  'guillemotright','guilsinglleft','guilsinglright') {
  $brace_commands{$no_arg_command} = 0;
}

# accent commands. They may be called with and without braces.
our %accent_commands;
foreach my $accent_command ('"','~','^','`',"'",',','=',
                           'ringaccent','H','dotaccent','u','ubaraccent',
                           'udotaccent','v','ogonek','tieaccent', 'dotless') {
  $accent_commands{$accent_command} = 1;
  $brace_commands{$accent_command} = 1;
}

our %style_commands;
foreach my $style_command ('asis','cite','clicksequence',
  'dfn', 'emph',
  'sc', 't', 'var',
  'headitemfont', 'code', 'command', 'env', 'file', 'kbd',
  'option', 'samp', 'strong') {
  $brace_commands{$style_command} = 1;
  $style_commands{$style_command} = 1;
}

our %regular_font_style_commands;
foreach my $command ('r', 'i', 'b', 'sansserif', 'slanted') {
  $regular_font_style_commands{$command} = 1;
  $brace_commands{$command} = 1;
  $style_commands{$command} = 1;
}

foreach my $one_arg_command (
  'ctrl','dmn', 'w', 'key',
  'titlefont','hyphenation','anchor','errormsg') {
  $brace_commands{$one_arg_command} = 1;
}

our %code_style_commands;
foreach my $command ('code', 'command', 'env', 'file', 'kbd', 'key', 'option',
   'samp', 'indicateurl', 'verb', 't') {
  $code_style_commands{$command} = 1;
  $brace_commands{$command} = 1;
}


# Commands that enclose full texts
our %context_brace_commands;
foreach my $context_brace_command ('footnote', 'caption', 'shortcaption', 'math') {
  $context_brace_commands{$context_brace_command} = $context_brace_command;
  $brace_commands{$context_brace_command} = 1;
}

our %explained_commands;
foreach my $explained_command ('abbr', 'acronym') {
  $explained_commands{$explained_command} = 1;
  $brace_commands{$explained_command} = 2;
}

our %inline_format_commands;
our %inline_commands;
foreach my $inline_format_command ('inlineraw', 'inlinefmt', 
        'inlinefmtifelse') {
  $inline_format_commands{$inline_format_command} = 1;
  $brace_commands{$inline_format_command} = 2;
  $inline_commands{$inline_format_command} = 1;
}

$brace_commands{'inlinefmtifelse'} = 3;

our %inline_conditional_commands;
foreach my $inline_conditional_command ('inlineifclear', 'inlineifset') {
  $inline_conditional_commands{$inline_conditional_command} = 1;
  $brace_commands{$inline_conditional_command} = 2;
  $inline_commands{$inline_conditional_command} = 1;
}

# 'inlineset', 'inlineclear'
#$brace_commands{'inlineclear'} = 1;

foreach my $two_arg_command('email') {
  $brace_commands{$two_arg_command} = 2;
}

foreach my $three_arg_command('uref','url','inforef') {
  $brace_commands{$three_arg_command} = 3;
}

foreach my $five_arg_command('xref','ref','pxref','image') {
  $brace_commands{$five_arg_command} = 5;
}


# some classification to help converters
our %ref_commands;
foreach my $ref_command ('xref','ref','pxref','inforef') {
  $ref_commands{$ref_command} = 1;
}


our %in_heading_commands;
foreach my $in_heading_command ('thischapter', 'thischaptername',
  'thischapternum', 'thisfile', 'thispage', 'thistitle') {
  $in_heading_commands{$in_heading_command} = 1;
}

# commands delimiting blocks, with an @end.
# Value is either the number of arguments on the line separated by
# commas or the type of command, 'raw', 'def' or 'multitable'.
our %block_commands;

# commands that have a possible content before an item
our %block_item_commands;

sub gdt($)
{
  return $_[0];
}

our %def_map = (
    # basic commands. 
    # 'arg' and 'argtype' are for everything appearing after the other
    # arguments.
    'deffn',     [ 'category', 'name', 'arg' ],
    'defvr',     [ 'category', 'name' ],
    'deftypefn', [ 'category', 'type', 'name', 'argtype' ],
    'deftypeop', [ 'category', 'class' , 'type', 'name', 'argtype' ],
    'deftypevr', [ 'category', 'type', 'name' ],
    'defcv',     [ 'category', 'class' , 'name' ],
    'deftypecv', [ 'category', 'class' , 'type', 'name' ],
    'defop',     [ 'category', 'class' , 'name', 'arg' ],
    'deftp',     [ 'category', 'name', 'argtype' ],
    # shortcuts
    'defun',         {'deffn'     => gdt('Function')},
    'defmac',        {'deffn'     => gdt('Macro')},
    'defspec',       {'deffn'     => '{'.gdt('Special Form').'}'},
    'defvar',        {'defvr'     => gdt('Variable')},
    'defopt',        {'defvr'     => '{'.gdt('User Option').'}'},
    'deftypefun',    {'deftypefn' => gdt('Function')},
    'deftypevar',    {'deftypevr' => gdt('Variable')},
    'defivar',       {'defcv'     => '{'.gdt('Instance Variable').'}'},
    'deftypeivar',   {'deftypecv' => '{'.gdt('Instance Variable').'}'},
    'defmethod',     {'defop'     => gdt('Method')},
    'deftypemethod', {'deftypeop' => gdt('Method')},
);

# the type of index, f: function, v: variable, t: type
my %index_type_def = (
 'f' => ['deffn', 'deftypefn', 'deftypeop', 'defop'],
 'v' => ['defvr', 'deftypevr', 'defcv', 'deftypecv' ],
 't' => ['deftp']
);

our %command_index_prefix;

$command_index_prefix{'vtable'} = 'v';
$command_index_prefix{'ftable'} = 'f';

foreach my $index_type (keys %index_type_def) {
  foreach my $def (@{$index_type_def{$index_type}}) {
    $command_index_prefix{$def} = $index_type;
  }
}

our %def_commands;
our %def_aliases;
foreach my $def_command(keys %def_map) {
  if (ref($def_map{$def_command}) eq 'HASH') {
    my ($real_command) = keys (%{$def_map{$def_command}});
    $command_index_prefix{$def_command} = $command_index_prefix{$real_command};
    $def_aliases{$def_command} = $real_command;
  }
  $block_commands{$def_command} = 'def';
  $misc_commands{$def_command.'x'} = 'line';
  $def_commands{$def_command} = 1;
  $def_commands{$def_command.'x'} = 1;
  $command_index_prefix{$def_command.'x'} = $command_index_prefix{$def_command};
}

#print STDERR "".Data::Dumper->Dump([\%def_aliases]);
#print STDERR "".Data::Dumper->Dump([\%def_prepended_content]);

$block_commands{'multitable'} = 'multitable';
$block_item_commands{'multitable'} = 1;

# block commands in which menu entry and menu comments appear
our %menu_commands;
foreach my $menu_command ('menu', 'detailmenu', 'direntry') {
  $menu_commands{$menu_command} = 1;
  $block_commands{$menu_command} = 0;
};

our %align_commands;
foreach my $align_command('raggedright', 'flushleft', 'flushright') {
  $block_commands{$align_command} = 0;
  $align_commands{$align_command} = 1;
}
$align_commands{'center'} = 1;

foreach my $block_command(
    'cartouche', 'group', 'indentedblock', 'smallindentedblock') {
  $block_commands{$block_command} = 0;
}

our %region_commands;
foreach my $block_command('titlepage', 'copying', 'documentdescription') {
  $block_commands{$block_command} = 0;
  $region_commands{$block_command} = 1;
}
  
our %preformatted_commands;
our %preformatted_code_commands;
foreach my $preformatted_command(
    'example', 'smallexample', 'lisp', 'smalllisp') {
  $block_commands{$preformatted_command} = 0;
  $preformatted_commands{$preformatted_command} = 1;
  $preformatted_code_commands{$preformatted_command} = 1;
}

foreach my $preformatted_command(
    'display', 'smalldisplay', 'format', 'smallformat') {
  $block_commands{$preformatted_command} = 0;
  $preformatted_commands{$preformatted_command} = 1;
}

our %format_raw_commands;
foreach my $format_raw_command('html', 'tex', 'xml', 'docbook') {
  $block_commands{$format_raw_command} = 0;
  $format_raw_commands{$format_raw_command} = 1;
}

our %raw_commands;
# macro/rmacro are special
foreach my $raw_command ('verbatim',
                         'ignore', 'macro', 'rmacro') {
  $block_commands{$raw_command} = 'raw';
  $raw_commands{$raw_command} = 1;
}

our %texinfo_output_formats;
foreach my $command (keys(%format_raw_commands), 'info', 'plaintext') {
  $block_commands{'if' . $command} = 'conditional';
  $block_commands{'ifnot' . $command} = 'conditional';
  $texinfo_output_formats{$command} = $command;
}

$block_commands{'ifset'} = 'conditional';
$block_commands{'ifclear'} = 'conditional';

$block_commands{'ifcommanddefined'} = 'conditional';
$block_commands{'ifcommandnotdefined'} = 'conditional';

# 'macro' ?
foreach my $block_command_one_arg('table', 'ftable', 'vtable',
  'itemize', 'enumerate', 'quotation', 'smallquotation') {
  $block_commands{$block_command_one_arg} = 1;
  $block_item_commands{$block_command_one_arg} = 1 
    unless ($block_command_one_arg =~ /quotation/);
}

$block_commands{'float'} = 2;

# commands that forces closing an opened paragraph.
our %close_paragraph_commands;

foreach my $block_command (keys(%block_commands)) {
  $close_paragraph_commands{$block_command} = 1
     unless ($block_commands{$block_command} eq 'raw' or
             $block_commands{$block_command} eq 'conditional'
             or $format_raw_commands{$block_command});
}

$close_paragraph_commands{'verbatim'} = 1;

foreach my $close_paragraph_command ('titlefont', 'insertcopying', 'sp',
  'verbatiminclude', 'page', 'item', 'itemx', 'tab', 'headitem',
  'printindex', 'listoffloats', 'center', 'dircategory', 'contents',
  'shortcontents', 'summarycontents', 'caption', 'shortcaption',
  'setfilename', 'exdent') {
  $close_paragraph_commands{$close_paragraph_command} = 1;
}

foreach my $close_paragraph_command (keys(%def_commands)) {
  $close_paragraph_commands{$close_paragraph_command} = 1;
}

our %item_container_commands;
foreach my $item_container_command ('itemize', 'enumerate') {
  $item_container_commands{$item_container_command} = 1;
}
our %item_line_commands;
foreach my $item_line_command ('table', 'ftable', 'vtable') {
  $item_line_commands{$item_line_command} = 1;
}

our %deprecated_commands = (
  'ctrl' => '',
  'allow-recursion' => N__('recursion is always allowed'),
  'quote-arg' => N__('arguments are quoted by default'),
);

# commands that should only appear at the root level and contain up to
# the next root command.  @node and sectioning commands.
our %root_commands;

our %command_structuring_level = (
              'top', 0,
              'chapter', 1,
              'unnumbered', 1,
              'chapheading', 1,
              'appendix', 1,
              'section', 2,
              'unnumberedsec', 2,
              'heading', 2,
              'appendixsec', 2,
              'subsection', 3,
              'unnumberedsubsec', 3,
              'subheading', 3,
              'appendixsubsec', 3,
              'subsubsection', 4,
              'unnumberedsubsubsec', 4,
              'subsubheading', 4,
              'appendixsubsubsec', 4,
         );

our %level_to_structuring_command;

{
  my $sections = [ ];
  my $appendices = [ ];
  my $unnumbered = [ ];
  my $headings = [ ];
  foreach my $command (keys (%command_structuring_level)) {
    if ($command =~ /^appendix/) {
      $level_to_structuring_command{$command} = $appendices;
    } elsif ($command =~ /^unnumbered/ or $command eq 'top') {
      $level_to_structuring_command{$command} = $unnumbered;
    } elsif ($command =~ /section$/ or $command eq 'chapter') {
      $level_to_structuring_command{$command} = $sections;
    } else {
      $level_to_structuring_command{$command} = $headings;
    }
    $level_to_structuring_command{$command}->[$command_structuring_level{$command}] 
      = $command;
  }
  $level_to_structuring_command{'appendixsection'} = $appendices;
  $level_to_structuring_command{'majorheading'} = $headings;
  $level_to_structuring_command{'centerchap'} = $unnumbered;
}


# out of the main hierarchy
$command_structuring_level{'part'} = 0;
# this are synonyms
$command_structuring_level{'appendixsection'} = 2;
# command_structuring_level{'majorheading'} is also 1 and not 0
$command_structuring_level{'majorheading'} = 1;
$command_structuring_level{'centerchap'} = 1;

our %sectioning_commands;

foreach my $sectioning_command (keys (%command_structuring_level)) {
  $misc_commands{$sectioning_command} = 'line';
  if ($sectioning_command =~ /heading/) {
    $close_paragraph_commands{$sectioning_command} = 1;
  } else {
    $root_commands{$sectioning_command} = 1;
  }
  $sectioning_commands{$sectioning_command} = 1;
}

$root_commands{'node'} = 1;

our %all_commands;
foreach my $command (
  keys(%Texinfo::Common::block_commands),
  keys(%Texinfo::Common::brace_commands),
  keys(%Texinfo::Common::misc_commands),
  keys(%Texinfo::Common::no_brace_commands), 
  qw(value),
 ) {
  $all_commands{$command} = 1;
} 

our @MONTH_NAMES =
    (
     'January', 'February', 'March', 'April', 'May',
     'June', 'July', 'August', 'September', 'October',
     'November', 'December'
    );

sub locate_include_file($$)
{
  my $self = shift;
  my $text = shift;
  my $file;

  my $ignore_include_directories = 0;

  my ($volume, $directories, $filename) = File::Spec->splitpath($text);
  my @directories = File::Spec->splitdir($directories);

  #print STDERR "$self $text @{$self->{'include_directories'}}\n";
  # If the path is absolute or begins with . or .., do not search in
  # include directories.
  if (File::Spec->file_name_is_absolute($text)) {
    $ignore_include_directories = 1;
  } else {
    foreach my $dir (@directories) {
      if ($dir eq File::Spec->updir() or $dir eq File::Spec->curdir()) {
        $ignore_include_directories = 1;
        last;
      } elsif ($dir ne '') {
        last;
      }
    }
  }

  #if ($text =~ m,^(/|\./|\.\./),) {
  if ($ignore_include_directories) {
    $file = $text if (-e $text and -r $text);
  } else {
    my @dirs;
    if ($self) {
      @dirs = @{$self->{'include_directories'}};
    } else {
      # no object with directory list and not an absolute path, never succeed
      return undef;
    }
    foreach my $include_dir (@{$self->{'include_directories'}}) {
      my ($include_volume, $include_directories, $include_filename) 
         = File::Spec->splitpath($include_dir, 1);
      
      my $possible_file = File::Spec->catpath($include_volume, 
        File::Spec->catdir(File::Spec->splitdir($include_directories), 
                           @directories), $filename);
      #$file = "$include_dir/$text" if (-e "$include_dir/$text" and -r "$include_dir/$text");
      $file = "$possible_file" if (-e "$possible_file" and -r "$possible_file");
      last if (defined($file));
    }
  }
  return $file;
}

sub open_out($$;$)
{
  my $self = shift;
  my $file = shift;
  my $encoding = shift;

  if (!defined($encoding) and $self 
      and defined($self->get_conf('OUTPUT_PERL_ENCODING'))) {
    $encoding = $self->get_conf('OUTPUT_PERL_ENCODING');
  }

  if ($file eq '-') {
    binmode(STDOUT, ":encoding($encoding)") if ($encoding);
    if ($self) {
      $self->{'unclosed_files'}->{$file} = \*STDOUT;
    }
    return \*STDOUT;
  }
  my $filehandle = do { local *FH };
  if (!open ($filehandle, '>', $file)) {
    return undef; 
  }
  if ($encoding) {
    if ($encoding eq 'utf8' or $encoding eq 'utf-8-strict') {
      binmode($filehandle, ':utf8');
    } else { # FIXME also right for shiftijs or similar encodings?
      binmode($filehandle, ':bytes');
    }
    binmode($filehandle, ":encoding($encoding)");
  }
  if ($self) {
    push @{$self->{'opened_files'}}, $file;
    $self->{'unclosed_files'}->{$file} = $filehandle;
    #print STDERR "OOOOOOO $file ".join('|',@{$self->{'opened_files'}})."\n";
    #cluck;
  }
  return $filehandle;
}

sub warn_unknown_language($$) {
  my $lang = shift;
  my $gettext = shift;

  my @messages = ();
  my $lang_code = $lang;
  my $region_code;

  if ($lang =~ /^([a-z]+)_([A-Z]+)/) {
    $lang_code = $1;
    $region_code = $2;
  }

  if (! $Texinfo::Documentlanguages::language_codes{$lang_code}) {
    push @messages, sprintf(&$gettext(N__("%s is not a valid language code")), 
                            $lang_code);
  }
  if (defined($region_code) 
       and ! $Texinfo::Documentlanguages::region_codes{$region_code}) {
    push @messages, sprintf(&$gettext(N__("%s is not a valid region code")), 
                            $region_code);
  }
  return @messages;
}

my %possible_split = (
  'chapter' => 1,
  'section' => 1,
  'node' => 1,
);

sub warn_unknown_split($$) {
  my $split = shift;
  my $gettext = shift;

  my @messages = ();
  if ($split and !$possible_split{$split}) {
    push @messages, sprintf(&$gettext(N__("%s is not a valid split possibility")),
                            $split);
  }
  return @messages;
}

# This should do the job, or at least don't do wrong if $self
# is not defined, as could be the case if called from 
# Texinfo::Convert::Text.
sub expand_verbatiminclude($$)
{
  my $self = shift;
  my $current = shift;

  return unless ($current->{'extra'} and defined($current->{'extra'}->{'text_arg'}));
  my $text = $current->{'extra'}->{'text_arg'};
  my $file = locate_include_file($self, $text);

  my $verbatiminclude;

  if (defined($file)) {
    if (!open(VERBINCLUDE, $file)) {
      if ($self) {
        $self->line_error(sprintf($self->__("could not read %s: %s"), $file, $!), 
                            $current->{'line_nr'});
      }
    } else {
      if ($self and defined($self->get_conf('INPUT_PERL_ENCODING'))) {
        binmode(VERBINCLUDE, ":encoding(".
                            $self->get_conf('INPUT_PERL_ENCODING').")");
      }
      $verbatiminclude = { 'cmdname' => 'verbatim',
                           'parent' => $current->{'parent'},
                           'extra' => 
                        {'text_arg' => $current->{'extra'}->{'text_arg'}} };
      while (<VERBINCLUDE>) {
        push @{$verbatiminclude->{'contents'}}, 
                  {'type' => 'raw', 'text' => $_ };
      }
      if (!close (VERBINCLUDE)) {
        $self->document_warn(sprintf($self->__(
                      "error on closing \@verbatiminclude file %s: %s"),
                             $file, $!));
      }
    }
  } elsif ($self) {
    $self->line_error(sprintf($self->__("\@%s: could not find %s"), 
                    $current->{'cmdname'}, $text), $current->{'line_nr'});
  }
  return $verbatiminclude;
}

sub definition_category($$)
{
  my $self = shift;
  my $current = shift;

  return undef if (!$current->{'extra'} or !$current->{'extra'}->{'def_args'});

  my $arg_category = $current->{'extra'}->{'def_parsed_hash'}->{'category'};
  my $arg_class = $current->{'extra'}->{'def_parsed_hash'}->{'class'};

  return $arg_category
    if (!defined($arg_class));
  
  my $style = 
    $command_index_prefix{$current->{'extra'}->{'def_command'}};
  if ($style eq 'f') {
    if ($self) {
      return $self->gdt('{category} on {class}', { 'category' => $arg_category,
                                          'class' => $arg_class });
    } else {
      return {'contents' => [$arg_category, {'text' => ' on '}, $arg_class]};
    }
  } elsif ($style eq 'v') {
    if ($self) {
      return $self->gdt('{category} of {class}', { 'category' => $arg_category,
                                          'class' => $arg_class });
    } else {
      return {'contents' => [$arg_category, {'text' => ' of '}, $arg_class]};
    }
  }
}

sub expand_today($)
{
  my $self = shift;
  if ($self->get_conf('TEST')) {
    return {'text' => 'a sunny day'};
  }
  my($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst)
   = localtime(time);
  $year += ($year < 70) ? 2000 : 1900;
  return $self->gdt('{month} {day}, {year}',
          { 'month' => $self->gdt($MONTH_NAMES[$mon]),
            'day' => $mday, 'year' => $year });
}

sub translated_command_tree($$)
{
  my $self = shift;
  my $cmdname = shift;
  if ($self->{'translated_commands'}->{$cmdname}) {
    return $self->gdt($self->{'translated_commands'}->{$cmdname});
  }
  return undef;
}

sub numbered_heading($$$;$)
{
  my $self = shift;
  my $current = shift;
  my $text = shift;
  my $numbered = shift;

  my $number;
  if (defined($current->{'number'}) and ($numbered or !defined($numbered))) {
    $number = $current->{'number'};
  }

  my $result;
  if ($self) {
    if (defined($number)) {
      if ($current->{'cmdname'} eq 'appendix' and $current->{'level'} == 1) {
        $result = $self->gdt('Appendix {number} {section_title}',
                   {'number' => $number, 'section_title' => $text}, 
                   'translated_text');
      } else {
        $result = $self->gdt('{number} {section_title}',
                   {'number' => $number, 'section_title' => $text},
                   'translated_text');
      }
    } else {
      $result = $text;
    }
  } else {
    $result = $text;
    $result = $number.' '.$result if (defined($number));
    if ($current->{'cmdname'} eq 'appendix' and $current->{'level'} == 1) {
      $result = 'Appendix '.$result;
    }
  }
  chomp ($result);
  return $result;
}

sub definition_arguments_content($)
{
  my $root = shift;
  my $result;

  return undef if (!defined($root->{'extra'}) 
                    or !defined($root->{'extra'}->{'def_args'}));
  my @args = @{$root->{'extra'}->{'def_args'}};
  while (@args) {
    last if ($args[0]->[0] ne 'spaces'
             and !$root->{'extra'}->{'def_parsed_hash'}->{$args[0]->[0]});
    shift @args;
  }
  if (@args) {
    foreach my $arg (@args) {
      push @$result, $arg->[1];
    }
  }
  return $result;
}

# find the accent commands stack and the innermost text contents
sub find_innermost_accent_contents($;$)
{
  my $current = shift;
  my $encoding = shift;
  my @accent_commands = ();
  my $debug = 0;
 ACCENT:
  while (1) {
    # the following can happen if called with a bad tree
    if (!$current->{'cmdname'} 
        or !$accent_commands{$current->{'cmdname'}}) {
      #print STDERR "BUG: Not an accent command in accent\n";
      cluck "BUG: Not an accent command in accent\n";
      #print STDERR Texinfo::Convert::Texinfo::convert($current)."\n";
      #print STDERR Data::Dumper->Dump([$current]);
      last;
    }
    push @accent_commands, $current;
    # A bogus accent, that may happen
    if (!$current->{'args'}) {
      return ([], \@accent_commands);
    }
    my $arg = $current->{'args'}->[0];
    if (!$arg->{'contents'}) {
      print STDERR "BUG: No content in accent command\n";
      #print STDERR Data::Dumper->Dump([$current]);
      #print STDERR Texinfo::Convert::Texinfo::convert($current)."\n";
      return ([], \@accent_commands);
    }
    # inside the argument of an accent
    my $text_contents = [];
    foreach my $content (@{$arg->{'contents'}}) {
      if (!($content->{'extra'} and $content->{'extra'}->{'invalid_nesting'})
         and !($content->{'cmdname'} and ($content->{'cmdname'} eq 'c'
                                  or $content->{'cmdname'} eq 'comment'))) {
        if ($content->{'cmdname'} and $accent_commands{$content->{'cmdname'}}) {
          $current = $content;
          next ACCENT;
        } else {
          push @$text_contents, $content;
        }
      }
    }
    # we go here if there was no nested accent
    return ($text_contents, \@accent_commands);
  }
}

sub trim_spaces_comment_from_content($)
{
  my $contents = shift;
  shift @$contents 
    if ($contents->[0] and $contents->[0]->{'type'}
       and ($contents->[0]->{'type'} eq 'empty_line_after_command'
            or $contents->[0]->{'type'} eq 'empty_spaces_after_command'
            or $contents->[0]->{'type'} eq 'empty_spaces_before_argument'
            or $contents->[0]->{'type'} eq 'empty_space_at_end_def_bracketed'
            or $contents->[0]->{'type'} eq 'empty_spaces_after_close_brace'));

  while (@$contents 
         and (($contents->[-1]->{'cmdname'}
               and ($contents->[-1]->{'cmdname'} eq 'c' 
                    or $contents->[-1]->{'cmdname'} eq 'comment'))
              or ($contents->[-1]->{'type'}
                  and ($contents->[-1]->{'type'} eq 'spaces_at_end'
                       or $contents->[-1]->{'type'} eq 'space_at_end_block_command')))) {
    pop @$contents;
  }
}

sub float_name_caption($$)
{
  my $self = shift;
  my $root = shift;

  my $caption;
  if ($root->{'extra'}->{'caption'}) {
    $caption = $root->{'extra'}->{'caption'};
  } elsif ($root->{'extra'}->{'shortcaption'}) {
    $caption = $root->{'extra'}->{'shortcaption'};
  }
  #if ($self->get_conf('DEBUG')) {
  #  my $caption_texi = 
  #    Texinfo::Convert::Texinfo::convert({ 'contents' => $caption->{'contents'}});
  #  print STDERR "  CAPTION: $caption_texi\n";
  #}
  my $type;
  if ($root->{'extra'}->{'type'}->{'normalized'} ne '') {
    $type = {'contents' => $root->{'extra'}->{'type'}->{'content'}};
  }

  my $prepended;
  if ($type) {
    if ($caption) {
      if (defined($root->{'number'})) {
        $prepended = $self->gdt('{float_type} {float_number}: ',
            {'float_type' => $type,
             'float_number' => $root->{'number'}});
      } else {
        $prepended = $self->gdt('{float_type}: ',
          {'float_type' => $type});
      }
    } else {
      if (defined($root->{'number'})) {
        $prepended = $self->gdt("{float_type} {float_number}\n",
            {'float_type' => $type,
              'float_number' => $root->{'number'}});
      } else {
        $prepended = $self->gdt("{float_type}\n",
            {'float_type' => $type});
      }
    }
  } elsif (defined($root->{'number'})) {
    if ($caption) {
      $prepended = $self->gdt('{float_number}: ',
          {'float_number' => $root->{'number'}});
    } else {
      $prepended = $self->gdt("{float_number}\n",
           {'float_number' => $root->{'number'}});
    }
  }
  return ($caption, $prepended);
}

# decompose a decimal number on a given base.
sub _decompose_integer($$)
{
  my $number = shift;
  my $base = shift;
  my @result = ();

  while ($number >= 0) {
    my $factor = $number % $base;
    push (@result, $factor);
    $number = int(($number - $factor) / $base) - 1;
  }
  return @result;
}

sub enumerate_item_representation($$)
{
  my $specification = shift;
  my $number = shift;

  if ($specification =~ /^[0-9]+$/) {
    return $specification + $number -1;
  }

  my $result = '';
  my $base_letter = ord('a');
  $base_letter = ord('A') if (ucfirst($specification) eq $specification);
  my @letter_ords = _decompose_integer(ord($specification) - $base_letter + $number - 1, 26);
  foreach my $ord (@letter_ords) {
    $result = chr($base_letter + $ord) . $result;
  }
  return $result;
}


our %htmlxref_entries = (
 'node' => [ 'node', 'section', 'chapter', 'mono' ],
 'section' => [ 'section', 'chapter','node', 'mono' ],
 'chapter' => [ 'chapter', 'section', 'node', 'mono' ],
 'mono' => [ 'mono', 'chapter', 'section', 'node' ],
);

sub parse_htmlxref_files($$)
{
  my $self = shift;
  my $files = shift;
  my $htmlxref;

  foreach my $file (@$files) {
    print STDERR "html refs config file: $file\n" if ($self->get_conf('DEBUG'));
    unless (open (HTMLXREF, $file)) {
      $self->document_warn(
        sprintf($self->__("could not open html refs config file %s: %s"),
          $file, $!));
      next;
    }
    my $line_nr = 0;
    my %variables;
    while (my $hline = <HTMLXREF>) {
      my $line = $hline;
      $line_nr++;
      next if $hline =~ /^\s*#/;
      #$hline =~ s/[#]\s.*//;
      $hline =~ s/^\s*//;
      next if $hline =~ /^\s*$/;
      chomp ($hline);
      if ($hline =~ s/^\s*(\w+)\s*=\s*//) {
        # handle variables
        my $var = $1;
        my $re = join '|', map { quotemeta $_ } keys %variables;
        $hline =~ s/\$\{($re)\}/defined $variables{$1} ? $variables{$1} 
                                                       : "\${$1}"/ge;
        $variables{$var} = $hline;
        next;
      }
      my @htmlxref = split /\s+/, $hline;
      my $manual = shift @htmlxref;
      my $split_or_mono = shift @htmlxref;
      #print STDERR "$split_or_mono $Texi2HTML::Config::htmlxref_entries{$split_or_mono} $line_nr\n";
      if (!defined($split_or_mono)) {
        $self->file_line_warn($self->__("missing type"), $file, $line_nr);
        next;
      } elsif (!defined($htmlxref_entries{$split_or_mono})) {
        $self->file_line_warn(sprintf($self->__("unrecognized type: %s"), 
                               $split_or_mono), $file, $line_nr);
        next;
      }
      my $href = shift @htmlxref;
      next if (exists($htmlxref->{$manual}->{$split_or_mono}));

      if (defined($href)) { # substitute 'variables'
        my $re = join '|', map { quotemeta $_ } keys %variables;
        $href =~ s/\$\{($re)\}/defined $variables{$1} ? $variables{$1} 
                                                      : "\${$1}"/ge;
        $href =~ s/\/*$// if ($split_or_mono ne 'mono');
      }
      $htmlxref->{$manual}->{$split_or_mono} = $href;
    }
    if (!close (HTMLXREF)) {
      $self->document_warn(sprintf($self->__(
                       "error on closing html refs config file %s: %s"),
                             $file, $!));
    }
  }
  return $htmlxref;
}

sub parse_renamed_nodes_file($$;$$)
{
  my $self = shift;
  my $renamed_nodes_file = shift;
  # if not given they are automatically created
  my $renamed_nodes = shift;
  my $renamed_nodes_lines = shift;

  if (open(RENAMEDFILE, "<$renamed_nodes_file")) {
    if ($self->get_conf('INPUT_PERL_ENCODING')) {
      binmode(RENAMEDFILE, ":encoding(".
                       $self->get_conf('INPUT_PERL_ENCODING').")");
    }
    my $renamed_nodes_line_nr = 0;
    my @old_names = ();
    while (<RENAMEDFILE>) {
      $renamed_nodes_line_nr++;
      next unless (/\S/);
      next if (/^\s*\@c\b/);
      if (s/^\s*\@\@\{\}\s+(\S)/$1/) {
        chomp;
        if (scalar(@old_names)) {
          foreach my $old_node_name (@old_names) {
            $renamed_nodes->{$old_node_name} = $_;
          }
          $renamed_nodes_lines->{$_} = $renamed_nodes_line_nr;
          @old_names = ();
        } else {
          $self->file_line_warn($self->__("no node to be renamed"),
                        $renamed_nodes_file, $renamed_nodes_line_nr);
        }
      } else {
        chomp;
        s/^\s*//;
        $renamed_nodes_lines->{$_} = $renamed_nodes_line_nr;
        push @old_names, $_;
      }
    }
    if (scalar(@old_names)) {
      $self->file_line_warn($self->__("nodes without a new name at the end of file"),
             $renamed_nodes_file, $renamed_nodes_line_nr);
    }
    if (!close(RENAMEDFILE)) {
      $self->document_warn(sprintf($self->__p(
          "see HTML Xref Link Preservation in the Texinfo manual for context",
          "error on closing node-renaming configuration file %s: %s"), 
                            $renamed_nodes_file, $!));
    }
  } else {
    $self->document_warn(sprintf($self->__("could not open %s: %s"), 
                         $renamed_nodes_file, $!));
  }
  return ($renamed_nodes, $renamed_nodes_lines);
}

sub collect_renamed_nodes($$;$$)
{
  my $self = shift;
  my $basename = shift;
  my $renamed_nodes = shift;
  my $renamed_nodes_lines = shift;

  my $renamed_nodes_file;
  if (defined($self->get_conf('RENAMED_NODES_FILE'))) {
    $renamed_nodes_file = $self->get_conf('RENAMED_NODES_FILE');
  } elsif (-f $basename . '-noderename.cnf') {
    $renamed_nodes_file = $basename . '-noderename.cnf';
  }
  if (defined($renamed_nodes_file)) {
    my ($renamed_nodes, $renamed_nodes_lines)
     = parse_renamed_nodes_file($self, $renamed_nodes_file, $renamed_nodes,
                                $renamed_nodes_lines);
    return ($renamed_nodes, $renamed_nodes_lines, $renamed_nodes_file);
  }
  return (undef, undef, undef);
}

sub normalize_top_node_name($)
{
  my $node = shift;
  if ($node =~ /^top$/i) {
    return 'Top';
  }
  return $node;
}

sub _convert_text_options($)
{
  my $self = shift;
  my %options;
  if ($self->get_conf('ENABLE_ENCODING')) {
    if ($self->get_conf('OUTPUT_ENCODING_NAME')) {
      $options{'enabled_encoding'} = $self->get_conf('OUTPUT_ENCODING_NAME');
    } elsif ($self->get_conf('INPUT_ENCODING_NAME')) {
      $options{'enabled_encoding'} = $self->get_conf('INPUT_ENCODING_NAME');
    }
  }
  $options{'TEST'} = 1 if ($self->get_conf('TEST'));
  $options{'NUMBER_SECTIONS'} = $self->get_conf('NUMBER_SECTIONS');
  $options{'converter'} = $self;
  $options{'expanded_formats_hash'} = $self->{'expanded_formats_hash'};
  return %options;
}

sub count_bytes($$;$) 
{
  my $self = shift;
  my $string = shift;
  my $encoding = shift;

  if (!defined($encoding) and $self and $self->get_conf('OUTPUT_PERL_ENCODING')) {
    $encoding = $self->get_conf('OUTPUT_PERL_ENCODING');
  }

  if ($encoding and $encoding ne 'ascii') {
    return length(Encode::encode($encoding, $string));
  } else {
    return length($string);
    #my $length = length($string);
    #$string =~ s/\n/\\n/g;
    #$string =~ s/\f/\\f/g;
    #print STDERR "Count($length): $string\n";
    #return $length;
  }
  # FIXME is the following required for correct count of end of lines?
  #if ($encoding) {
  #  return length(Encode::encode($encoding, $string));
  #} else {
  #  return length(Encode::encode('ascii', $string));
  #}
}

# TODO
# also recurse into
# extra->misc_args, extra->args_index
# extra->index_entry extra->type
#
# extra that should point to other elements: 
# command_as_argument
# @block_command_line_contents @brace_command_contents @misc_content end_command
# associated_section part_associated_section associated_node associated_part
# @prototypes @columnfractions titlepage quotation @author command
# menu_entry_description menu_entry_name
# 
# should point to other elements, or be copied.  And some should be recursed
# into too.
# extra->type->content
# extra->nodes_manuals->[]
# extra->node_content
# extra->node_argument
# extra->explanation_contents
# extra->menu_entry_node
# extra->def_arg


sub _copy_tree($$$);
sub _copy_tree($$$)
{
  my $current = shift;
  my $parent = shift;
  my $reference_associations = shift;
  my $new = {};
  $reference_associations->{$current} = $new;
  $new->{'parent'} = $parent if ($parent);
  foreach my $key ('type', 'cmdname', 'text') {
    $new->{$key} = $current->{$key} if (exists($current->{$key}));
  }
  foreach my $key ('args', 'contents') {
    if ($current->{$key}) {
      if (ref($current->{$key}) ne 'ARRAY') {
        my $command_or_type = '';
        if ($new->{'cmdname'}) {
          $command_or_type = '@'.$new->{'cmdname'};
        } elsif ($new->{'type'}) {
          $command_or_type = $new->{'type'};
        }
        print STDERR "Not an array [$command_or_type] $key ".ref($current->{$key})."\n";
      }
      $new->{$key} = [];
      $reference_associations->{$current->{$key}} = $new->{$key};
      foreach my $child (@{$current->{$key}}) {
        push @{$new->{$key}}, _copy_tree($child, $new, $reference_associations);
      }
    }
  }
  if ($current->{'extra'}) {
    $new->{'extra'} = {};
    foreach my $key (keys %{$current->{'extra'}}) {
      if ($current->{'cmdname'} and $current->{'cmdname'} eq 'multitable'
          and $key eq 'prototypes') {
        $new->{'extra'}->{$key} = [];
        $reference_associations->{$current->{'extra'}->{$key}} = $new->{$key};
        foreach my $child (@{$current->{'extra'}->{$key}}) {
          push @{$new->{'extra'}->{$key}}, 
                  _copy_tree($child, $new, $reference_associations);
        }
      } elsif (!ref($current->{'extra'}->{$key})) {
        $new->{'extra'}->{$key} = $current->{'extra'}->{$key};
      }
    }
  }
  return $new;
}

# Not used.
sub _collect_references($$);
sub _collect_references($$)
{
  my $current = shift;
  my $references = shift;
  foreach my $key ('args', 'contents') {
    if ($current->{$key}) {
      $references->{$current->{$key}} = $current->{$key};
      foreach my $child (@{$current->{$key}}) {
        $references->{$child} = $child;
        _collect_references($child, $references);
      }
    }
  }
}

sub _substitute_references_in_array($$$);
sub _substitute_references_in_array($$$)
{
  my $array = shift;
  my $reference_associations = shift;
  my $context = shift;

  my $result = [];
  my $index = 0;
  foreach my $item (@{$array}) {
    if (!ref($item)) {
      push @{$result}, $item;
    } elsif ($reference_associations->{$item}) {
      push @{$result}, $reference_associations->{$item};
    } elsif (ref($item) eq 'ARRAY') {
      push @$result, 
        _substitute_references_in_array($item, $reference_associations,
                                        "$context [$index]");
    } elsif (defined($item->{'text'})) {
      my $new_text = _copy_tree($item, undef, $reference_associations);
      substitute_references($item, $new_text, $reference_associations);
      push @{$result}, $new_text;
    } else {
      print STDERR "Trouble with $context [$index] (".ref($item).")\n";
      push @{$result}, undef;
    }
    $index++;
  }
  return $result;
}

sub substitute_references($$$);
sub substitute_references($$$)
{
  my $current = shift;
  my $new = shift;
  my $reference_associations = shift;
  
  foreach my $key ('args', 'contents') {
    if ($new->{$key}) {
      my $index = 0;
      foreach my $child (@{$new->{$key}}) {
        substitute_references($child, $current->{$key}->[$index],
                              $reference_associations);
        $index++;
      }
    }
  }
  if ($current->{'extra'}) {
    foreach my $key (keys %{$current->{'extra'}}) {
      if (ref($current->{'extra'}->{$key})) {
        my $command_or_type = '';
        if ($new->{'cmdname'}) {
          $command_or_type = '@'.$new->{'cmdname'};
        } elsif ($new->{'type'}) {
          $command_or_type = $new->{'type'};
        }
        
        if ($current->{'cmdname'} and $current->{'cmdname'} eq 'multitable'
            and $key eq 'prototypes') {
          my $index = 0;
          foreach my $child (@{$new->{'extra'}->{$key}}) {
            substitute_references($child, $current->{'extra'}->{$key}->[$index],
                                  $reference_associations);
            $index++;
          }
        } elsif ($reference_associations->{$current->{'extra'}->{$key}}) {
          $new->{'extra'}->{$key} 
            = $reference_associations->{$current->{'extra'}->{$key}};
          #print STDERR "Done [$command_or_type]: $key\n";
        } else {
          if (ref($current->{'extra'}->{$key}) eq 'ARRAY') {
            
            #print STDERR "Array $command_or_type -> $key\n";
            $new->{'extra'}->{$key} = _substitute_references_in_array(
              $current->{'extra'}->{$key}, $reference_associations,
              "[$command_or_type]{$key}");
          } else {
            if (($current->{'cmdname'} 
                 and ($current->{'cmdname'} eq 'listoffloats'
                     or $current->{'cmdname'} eq 'float') 
                 and $key eq 'type')
                 or ($key eq 'index_entry')
                 or ($current->{'type'} 
                     and $current->{'type'} eq 'menu_entry'
                     and $key eq 'menu_entry_node')) {
              foreach my $type_key (keys(%{$current->{'extra'}->{$key}})) {
                if (!ref($current->{'extra'}->{$key}->{$type_key})) {
                  $new->{'extra'}->{$key}->{$type_key} 
                    = $current->{'extra'}->{$key}->{$type_key};
                } elsif ($reference_associations->{$current->{'extra'}->{$key}->{$type_key}}) {
                  $new->{'extra'}->{$key}->{$type_key}
                    = $reference_associations->{$current->{'extra'}->{$key}->{$type_key}};
                } elsif (ref($current->{'extra'}->{$key}->{$type_key}) eq 'ARRAY') {
                  $new->{'extra'}->{$key}->{$type_key}
                    = _substitute_references_in_array(
                      $current->{'extra'}->{$key}->{$type_key}, 
                      $reference_associations,
                      "[$command_or_type]{$key}{$type_key}");
                } else {
                  print STDERR "Not substituting [$command_or_type]{$key}: $type_key\n";
                }
              }
            } else {
              print STDERR "Not substituting [$command_or_type]: $key ($current->{'extra'}->{$key})\n";
            }
          }
        }
      }
    }
  }
}

sub copy_tree($;$)
{
  my $current = shift;
  my $parent = shift;
  my $reference_associations = {};
  my $copy = _copy_tree($current, $parent, $reference_associations);
  substitute_references($current, $copy, $reference_associations);
  return $copy;
}

sub modify_tree($$$;$);
sub modify_tree($$$;$)
{
  my $self = shift;
  my $tree = shift;
  my $operation = shift;
  my $argument = shift;
  #print STDERR "modify_tree tree: $tree\n";

  if ($tree->{'args'}) {
    my @args = @{$tree->{'args'}};
    for (my $i = 0; $i <= $#args; $i++) {
      my @new_args = &$operation($self, 'arg', $args[$i], $argument);
      modify_tree($self, $args[$i], $operation, $argument);
      # this puts the new args at the place of the old arg using the 
      # offset from the end of the array
      splice (@{$tree->{'args'}}, $i - $#args -1, 1, @new_args);
      #foreach my $arg (@new_args) {
      #  modify_tree($self, $arg, $operation);
      #}
    }
  }
  if ($tree->{'contents'}) {
    my @contents = @{$tree->{'contents'}};
    for (my $i = 0; $i <= $#contents; $i++) {
      my @new_contents = &$operation($self, 'content', $contents[$i], $argument);
      modify_tree($self, $contents[$i], $operation, $argument);
      # this puts the new contents at the place of the old content using the 
      # offset from the end of the array
      splice (@{$tree->{'contents'}}, $i - $#contents -1, 1, @new_contents);
      #foreach my $content (@new_contents) {
      #  modify_tree($self, $content, $operation);
      #}
    }
  }
  return $tree;
}

sub _protect_comma($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  return _protect_text($current, quotemeta(','));
}

sub protect_comma_in_tree($)
{
  my $tree = shift;
  return modify_tree(undef, $tree, \&_protect_comma);
}

sub _new_asis_command_with_text($$;$)
{
  my $text = shift;
  my $parent = shift;
  my $text_type = shift;
  my $new_command = {'cmdname' => 'asis', 'parent' => $parent };
  push @{$new_command->{'args'}}, {'type' => 'brace_command_arg',
                                   'parent' => $new_command};
  push @{$new_command->{'args'}->[0]->{'contents'}}, {
    'text' => $text,
    'parent' => $new_command->{'args'}->[0]};
  if (defined($text_type)) {
    $new_command->{'args'}->[0]->{'contents'}->[0]->{'type'} = $text_type;
  }
  return $new_command;
}

sub _protect_text($$)
{
  my $current = shift;
  my $to_protect = shift;

  #print STDERR "$to_protect: $current "._print_current($current)."\n";
  if (defined($current->{'text'}) and $current->{'text'} =~ /$to_protect/
      and !(defined($current->{'type'}) and $current->{'type'} eq 'raw')) {
    my @result = ();
    my $remaining_text = $current->{'text'};
    while ($remaining_text) {
      if ($remaining_text =~ s/^(.*?)(($to_protect)+)//) {
        if ($1 ne '') {
          push @result, {'text' => $1, 'parent' => $current->{'parent'}};
          $result[-1]->{'type'} = $current->{'type'} 
            if defined($current->{'type'});
        }
        if ($to_protect eq quotemeta(',')) {
          for (my $i = 0; $i < length($2); $i++) {
            push @result, {'cmdname' => 'comma', 'parent' => $current->{'parent'},
                           'args' => [{'type' => 'brace_command_arg'}]};
          }
        } else {
          push @result, _new_asis_command_with_text($2, $current->{'parent'},
                                                    $current->{'type'});
        }
      } else {
        push @result, {'text' => $remaining_text, 'parent' => $current->{'parent'}};
        $result[-1]->{'type'} = $current->{'type'} 
          if defined($current->{'type'});
        last;
      }
    }
    #print STDERR "Result: @result\n";
    return @result;
  } else {
    #print STDERR "No change: $current\n";
    return ($current);
  }
}

sub _protect_colon($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  return _protect_text ($current, quotemeta(':'));
}

sub protect_colon_in_tree($)
{
  my $tree = shift;
  return modify_tree(undef, $tree, \&_protect_colon);
}

sub _protect_node_after_label($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  return _protect_text ($current, '['. quotemeta(".\t,") .']');
}

sub protect_node_after_label_in_tree($)
{
  my $tree = shift;
  return modify_tree(undef, $tree, \&_protect_node_after_label);
}

sub _is_cpp_line($)
{
  my $text = shift;
  return 1 if ($text =~ /^\s*#\s*(line)? (\d+)(( "([^"]+)")(\s+\d+)*)?\s*$/);
  return 0;
}

sub _protect_hashchar_at_line_beginning($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  #print STDERR "$type $current "._print_current($current)."\n";
  # if the next is a hash character at line beginning, mark it
  if (defined($current->{'text'}) and $current->{'text'} =~ /\n$/
      and $current->{'parent'} and $current->{'parent'}->{'contents'}) {
    my $parent = $current->{'parent'};
    #print STDERR "End of line in $current, parent $parent: (@{$parent->{'contents'}})\n";
    my $current_found = 0;
    foreach my $content (@{$parent->{'contents'}}) {
      if ($current_found) {
        #print STDERR "after $current: $content $content->{'text'}\n";
        if ($content->{'text'} and _is_cpp_line($content->{'text'})) {
          $content->{'extra'}->{'_protect_hashchar'} = 1;
        }
        last;
      } elsif ($content eq $current) {
        $current_found = 1;
      }
    }
  }

  my $protect_hash = 0;
  # if marked, or first and a cpp_line protect a leading hash character
  if ($current->{'extra'} and $current->{'extra'}->{'_protect_hashchar'}) {
    delete $current->{'extra'}->{'_protect_hashchar'};
    if (!scalar(keys(%{$current->{'extra'}}))) {
      delete $current->{'extra'};
    }
    $protect_hash = 1;
  } elsif ($current->{'parent'} and $current->{'parent'}->{'contents'}
           and $current->{'parent'}->{'contents'}->[0]
           and $current->{'parent'}->{'contents'}->[0] eq $current
           and $current->{'text'}
           and _is_cpp_line($current->{'text'})) {
    $protect_hash = 1;
  }
  if ($protect_hash) {
    my @result = ();
    if ($current->{'type'} and $current->{'type'} eq 'raw') {
      if ($self) {
        my $parent = $current->{'parent'};
        while ($parent) {
          if ($parent->{'cmdname'} and $parent->{'line_nr'}) {
            $self->line_warn(sprintf($self->__(
                  "could not protect hash character in \@%s"), 
                             $parent->{'cmdname'}), $parent->{'line_nr'});
            last;
          }
          $parent = $parent->{'parent'};
        }
      }
    } else {
      $current->{'text'} =~ s/^(\s*)#//;
      if ($1 ne '') {
        push @result, {'text' => $1, 'parent' => $current->{'parent'}};
      }
      push @result, {'cmdname' => 'hashchar', 'parent' => $current->{'parent'},
                     'args' => [{'type' => 'brace_command_arg'}]};
    }
    push @result, $current;
    return @result;
  } else {
    return ($current);
  }
}

sub protect_hashchar_at_line_beginning($$)
{
  my $self = shift;
  my $tree = shift;
  return modify_tree($self, $tree, \&_protect_hashchar_at_line_beginning);
}

sub protect_first_parenthesis($)
{
  my $contents = shift;
  return undef if (!defined ($contents));
  my @contents = @$contents;
  my $brace;
  if ($contents[0] and $contents->[0]{'text'} and $contents[0]->{'text'} =~ /^\(/) {
    if ($contents[0]->{'text'} !~ /^\($/) {
      $brace = shift @contents;
      my $brace_text = $brace->{'text'};
      $brace_text =~ s/^\(//;
      unshift @contents, { 'text' => $brace_text, 'type' => $brace->{'type'},
                           'parent' => $brace->{'parent'} } if $brace_text ne '';
    } else {
      $brace = shift @contents;
    }
    unshift @contents, _new_asis_command_with_text('(', $brace->{'parent'},
                                                    $brace->{'type'});
  }
  return \@contents;
}

sub find_parent_root_command($$)
{
  my $parser = shift;
  my $current = shift;

  my $root_command;
  while (1) {
    if ($current->{'cmdname'}) {
      if ($root_commands{$current->{'cmdname'}}) {
        return $current;
      } elsif ($region_commands{$current->{'cmdname'}}) {
        if ($current->{'cmdname'} eq 'copying' and $parser
            and $parser->{'extra'} and $parser->{'extra'}->{'insertcopying'}) {
          foreach my $insertcopying(@{$parser->{'extra'}->{'insertcopying'}}) {
            my $root_command
              = $parser->find_parent_root_command($insertcopying);
            return $root_command if (defined($root_command));
          }
        } else {
          return undef;
        }
      }
    }
    if ($current->{'parent'}) {
      $current = $current->{'parent'};
    } else {
      return undef;
    }
  }
  # Should never get there
  return undef;
}

# for debugging
sub _print_current($)
{
  my $current = shift;
  if (ref($current) ne 'HASH') {
    return  "_print_current: $current not a hash\n";
  }
  my $type = '';
  my $cmd = '';
  my $parent_string = '';
  my $text = '';
  $type = "($current->{'type'})" if (defined($current->{'type'}));
  $cmd = "\@$current->{'cmdname'}" if (defined($current->{'cmdname'}));
  $cmd .= "($current->{'level'})" if (defined($current->{'level'}));
  $text = "[text: $current->{'text'}]" if (defined($current->{'text'}));
  if ($current->{'parent'}) {
    my $parent = $current->{'parent'};
    my $parent_cmd = '';
    my $parent_type = '';
    $parent_cmd = "\@$parent->{'cmdname'}" if (defined($parent->{'cmdname'}));
    $parent_type = "($parent->{'type'})" if (defined($parent->{'type'}));
    $parent_string = " <- $parent_cmd$parent_type\n";
  }
  my $args = '';
  my $contents = '';
  $args = "args(".scalar(@{$current->{'args'}}).')' if $current->{'args'};
  $contents = "contents(".scalar(@{$current->{'contents'}}).')'
    if $current->{'contents'};
  if ("$cmd$type" ne '') {
    return "$cmd$type : $text $args $contents\n$parent_string";
  } else {
    return "$text $args $contents\n$parent_string";
  }
}

sub move_index_entries_after_items($) {
  # enumerate or itemize
  my $current = shift;

  return unless ($current->{'contents'});

  my $previous;
  foreach my $item (@{$current->{'contents'}}) {
    #print STDERR "Before proceeding: $previous $item->{'cmdname'} (@{$previous->{'contents'}})\n" if ($previous and $previous->{'contents'});
    if (defined($previous) and $item->{'cmdname'} 
        and $item->{'cmdname'} eq 'item' 
        and $previous->{'contents'} and scalar(@{$previous->{'contents'}})) {

      my $previous_ending_container;
      if ($previous->{'contents'}->[-1]->{'type'}
          and ($previous->{'contents'}->[-1]->{'type'} eq 'paragraph'
               or $previous->{'contents'}->[-1]->{'type'} eq 'preformatted')) {
        $previous_ending_container = $previous->{'contents'}->[-1];
      } else {
        $previous_ending_container = $previous;
      }

      my @gathered_index_entries;

      #print STDERR "Gathering for item $item in previous $previous ($previous_ending_container)\n";
      while ($previous_ending_container->{'contents'}->[-1]
             and (($previous_ending_container->{'contents'}->[-1]->{'type'}
                   and $previous_ending_container->{'contents'}->[-1]->{'type'} eq 'index_entry_command')
                  or ($previous_ending_container->{'contents'}->[-1]->{'cmdname'}
                      and ($previous_ending_container->{'contents'}->[-1]->{'cmdname'} eq 'c'
                           or $previous_ending_container->{'contents'}->[-1]->{'cmdname'} eq 'comment')))) {
        unshift @gathered_index_entries, pop @{$previous_ending_container->{'contents'}};
      }
      #print STDERR "Gathered: @gathered_index_entries\n";
      if (scalar(@gathered_index_entries)) {
        # put back leading comments
        while ($gathered_index_entries[0]
               and (!$gathered_index_entries[0]->{'type'}
                    or $gathered_index_entries[0]->{'type'} ne 'index_entry_command')) {
          #print STDERR "Putting back $gathered_index_entries[0] $gathered_index_entries[0]->{'cmdname'}\n";
          push @{$previous_ending_container->{'contents'}}, 
             shift @gathered_index_entries;
        }

        # We have the index entries of the previous @item or before item.
        # Now put them right after the current @item command.
        if (scalar(@gathered_index_entries)) {
          my $item_container;
          if ($item->{'contents'} and $item->{'contents'}->[0]
              and $item->{'contents'}->[0]->{'type'}
              and $item->{'contents'}->[0]->{'type'} eq 'preformatted') {
            $item_container = $item->{'contents'}->[0];
          } else {
            $item_container = $item;
          }
          foreach my $entry(@gathered_index_entries) {
            $entry->{'parent'} = $item_container;
          }
          if ($item_container->{'contents'} 
              and $item_container->{'contents'}->[0]
              and $item_container->{'contents'}->[0]->{'type'}) {
            if ($item_container->{'contents'}->[0]->{'type'} eq 'empty_line_after_command') {
              
              unshift @gathered_index_entries, shift @{$item_container->{'contents'}};
            } elsif ($item_container->{'contents'}->[0]->{'type'} eq 'empty_spaces_after_command') {
               unshift @gathered_index_entries, shift @{$item_container->{'contents'}};
               $gathered_index_entries[0]->{'type'} = 'empty_line_after_command';
               $gathered_index_entries[0]->{'text'} .= "\n";
            }
          }
          unshift @{$item_container->{'contents'}}, @gathered_index_entries;
        }
      }
    }
    $previous = $item;
  }
}

sub _move_index_entries_after_items($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  if ($current->{'cmdname'} and ($current->{'cmdname'} eq 'enumerate'
                                 or $current->{'cmdname'} eq 'itemize')) {
    move_index_entries_after_items($current);
  }
  return ($current);
}

sub move_index_entries_after_items_in_tree($)
{
  my $tree = shift;
  return modify_tree(undef, $tree, \&_move_index_entries_after_items);
}

1;

__END__

=head1 NAME

Texinfo::Common - Classification of commands and miscellaneous methods

=head1 SYNOPSIS

  use Texinfo::Common qw(expand_today expand_verbatiminclude);
  if ($Texinfo::Common::accent_commands{$a_command}) {
    print STDERR "$a_command is an accent command\n";
  }
  
  my $today_tree = expand_today($converter);
  my $verbatiminclude_tree 
     = expand_verbatiminclude(undef, $verbatiminclude);

=head1 DESCRIPTION

Texinfo::Common holds interesting hashes classifying Texinfo @-commands,
as well as miscellaneous methods that may be useful for any backend
converting texinfo trees.

It also defines, as our variable a hash for default indices,
named C<%index_names>.  The format of this hash is described in 
L<Texinfo::Parser/indices_information>.

=head1 COMMAND CLASSES

Hashes are defined as C<our> variables, and are therefore available
outside of the module.

The key of the hashes are @-command names without the @.  The 
following hashes are available:

=over

=item %all_commands

All the @-commands.

=item %no_brace_commands

Commands without brace with a single character as name, like C<*>
or C<:>.  The value is an ascii representation of the command.  It
may be an empty string.

=item %misc_commands

Command that do not take braces and are not block commands either, like
C<@node>, C<@chapter>, C<@cindex>, C<@deffnx>, C<@end>, C<@footnotestyle>, 
C<@set>, C<@settitle>, C<@indent>, C<@definfoenclose>, C<@comment> and many 
others.

=item %default_index_commands

Index entry commands corresponding to default indices. For example 
C<@cindex>.

=item %root_commands

Commands that are at the root of a Texinfo document, namely
C<@node> and sectioning commands, except heading commands.

=item %sectioning_commands

All the sectioning and heading commands.

=item %brace_commands

The commands that take braces.  The associated value is the maximum
number of arguments.

=item %letter_no_arg_commands

@-commands with braces but no argument corresponding to letters, 
like C<@AA{}> or C<@ss{}> or C<@o{}>.

=item %accent_commands

Accent @-commands taking an argument, like C<@'> or C<@ringaccent> 
including C<@dotless> and C<@tieaccent>.

=item %style_commands

Commands that mark a fragment of texinfo, like C<@strong>,
C<@cite>, C<@code> or C<@asis>.

=item %code_style_commands

I<style_commands> that have their argument in code style, like 
C<@code>.

=item %regular_font_style_commands

I<style_commands> that have their argument in regular font, like
C<@r> or C<@slanted>.

=item %context_brace_commands

@-commands with brace like C<@footnote>, C<@caption> and C<@math>
whose argument is outside of the main text flow in one way or another.

=item %ref_commands

Cross reference @-command referencing nodes, like C<@xref>.

=item %explained_commands

@-commands whose second argument explain first argument and further
@-command call without first argument, as C<@abbr> and C<@acronym>.

=item %block commands

Commands delimiting a block with a closing C<@end>.  The value
is I<conditional> for C<@if> commands, I<def> for definition
commands like C<@deffn>, I<raw> for @-commands that have no expansion
of @-commands in their bodies and I<multitable> for C<@multitable>.  
Otherwise it is set to the number of arguments separated by commas 
that may appear on the @-command line. That means 0 in most cases, 
1 for C<@quotation> and 2 for C<@float>.

=item %raw_commands

@-commands that have no expansion of @-commands in their bodies,
as C<@macro>, C<@verbatim> or C<@ignore>.

=item %format_raw_commands

@-commands associated with raw output format, like C<@html>, or
C<@docbook>.

=item %texinfo_output_formats

Cannonical output formats that have associated conditionals.  In
practice C<%format_raw_commands> plus C<info> and C<plaintext>.

=item %def_commands

=item %def_aliases

Definition commands.  C<%def_aliases> associates an aliased command
to the original command, for example C<defun> is associated to C<deffn>.

=item %menu_commands

@-commands with menu entries.

=item %align_commands

@-commands related with alignement of text.

=item %region_commands

Block @-commands that enclose full text regions, like C<@titlepage>.

=item %preformatted_commands

=item %preformatted_code_commands

I<%preformatted_commands> is for commands whose content should not 
be filled, like C<@example> or C<@display>.  If the command is meant 
for code, it is also in I<%preformatted_code_commands>, like C<@example>.

=item %item_container_commands

Commands holding C<@item> with C<@item> that contains blocks of text, 
like C<@itemize>.

=item %item_line_commands

Commands with C<@item> that have their arguments on their lines, like
C<@ftable>.

=back

=head1 METHODS

No method is exported in the default case.

Most methods takes a I<$converter> as argument, sometime optionally, 
to get some information and use methods for error reporting, 
see L<Texinfo::Convert::Converter> and L<Texinfo::Report>.

=over

=item $tree = expand_today($converter)

Expand today's date, as a texinfo tree with translations.

=item $tree = expand_verbatiminclude($converter, $verbatiminclude)

The I<$converter> argument may be undef.  I<$verbatiminclude> is a
C<@verbatiminclude> tree element.  This function returns a 
C<@verbatim> tree elements after finding the included file and
reading it.

=item $tree = definition_category($converter, $def_line)

The I<$converter> argument may be undef.  I<$def_line> is a 
C<def_line> texinfo tree container.  This function
returns a texinfo tree corresponding to the category of the
I<$def_line> taking the class into account, if there is one.

=item $result = numbered_heading ($converter, $heading_element, $heading_text, $do_number)

The I<$converter> argument may be undef.  I<$heading_element> is 
a heading command tree element.  I<$heading_text> is the already 
formatted heading text.  if the I<$do_number> optional argument is 
defined and false, no number is used and the text is returned as is.
This function returns the heading with a number and the appendix 
part if needed.

=item ($caption, $prepended) = float_name_caption ($converter, $float)

I<$float> is a texinfo tree C<@float> element.  This function 
returns the caption that should be used for the float formatting 
and the I<$prepended> texinfo tree combining the type and label
of the float.

=item $text = enumerate_item_representation($specification, $number)

This function returns the number or letter correponding to item
number I<$number> for an C<@enumerate> specification I<$specification>,
appearing on an C<@enumerate> line.  For example

  enumerate_item_representation('c', 3)

is C<e>.

=item trim_spaces_comment_from_content($contents)

Remove empty spaces after commands or braces at begin and
spaces and comments at end from a content array, modifying it.

=item $normalized_name = normalize_top_node_name ($node_string)

Normalize the node name string given in argument, by normalizing
Top node case.

=item protect_comma_in_tree($tree)

Protect comma characters, replacing C<,> with @comma{} in tree.

=item protect_colon_in_tree($tree)

=item protect_node_after_label_in_tree($tree)

Protect colon with C<protect_colon_in_tree> and characters that 
are special in node names after a label in menu entries (tab
dot and comma) with C<protect_node_after_label_in_tree>.  
The protection is achieved by putting protected characters 
in C<@asis{}>.

=item $contents_result = protect_first_parenthesis ($contents)

Return a contents array reference with first parenthesis in the 
contents array reference protected.

=item protect_hashchar_at_line_beginning($parser, $tree)

Protect hash character at beginning of line if the line is a cpp
line directive.  The I<$parser> argument maybe undef, if it is 
defined it is used for error reporting in case an hash character
could not be protected because it appeared in a raw environment.

=item move_index_entries_after_items_in_tree($tree)

In C<@enumerate> and C<@itemize> from the tree, move index entries 
appearing just before C<@item> after the C<@item>.  Comment lines 
between index entries are moved too.

=item $command = find_parent_root_command($parser, $tree_element)

Find the parent root command of a tree element (sectioning command or node).
The C<$parser> argument is optional, it is used to continue 
through C<@insertcopying> if in a C<@copying>.

=item valid_tree_transformation($name)

Return true if the I<$name> is a known tree transformation name
that may be passed with C<TREE_TRANSFORMATIONS> to modify a texinfo
tree.

=back

=head1 SEE ALSO

L<Texinfo::Parser>, L<Texinfo::Convert::Converter> and L<Texinfo::Report>. 

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2010, 2011, 2012 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License,
or (at your option) any later version.

=cut

