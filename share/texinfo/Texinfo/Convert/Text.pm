# Text.pm: output tree as simple text.
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

package Texinfo::Convert::Text;

use 5.00405;
use strict;

# accent commands list.
use Texinfo::Common;
use Texinfo::Convert::Unicode;
# for debugging
use Texinfo::Convert::Texinfo;
use Data::Dumper;
use Carp qw(cluck carp);

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Texinfo::Convert::Text ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
  convert
  ascii_accent
  text_accents
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '5.1.90';

# this is in fact not needed for 'footnote', 'shortcaption', 'caption'
# when they have no brace_command_arg, see below.
my %ignored_brace_commands;
foreach my $ignored_brace_command (#'xref','ref','pxref','inforef',
   'anchor',
   'footnote', 'shortcaption', 'caption', 'hyphenation', 'errormsg') {
  $ignored_brace_commands{$ignored_brace_command} = 1;
}

my %ignored_block_commands;
foreach my $ignored_command ('titlepage', 'copying', 'documentdescription',
  'html', 'tex', 'xml', 'docbook', 'ignore', 'macro', 'rmacro') {
  $ignored_block_commands{$ignored_command} = 1;
}

# used by Texinfo::Convert::NodeNormalization
our %text_brace_no_arg_commands = (
               'TeX'                => 'TeX',
               'LaTeX'              => 'LaTeX',
               'bullet'             => '*',
               'copyright'          => '(C)',
               'registeredsymbol'   => '(R)',
               'dots'         => '...',
               'enddots'      => '...',
               'equiv'        => '==',
               'error'        => 'error-->',
               'expansion'    => '==>',
               'arrow'        => '->',
               'minus'        => '-',
               'point'        => '-!-',
               'print'        => '-|',
               'result'       => '=>',
               'today'        => '',
               'aa'           => 'aa',
               'AA'           => 'AA',
               'ae'           => 'ae',
               'oe'           => 'oe',
               'AE'           => 'AE',
               'OE'           => 'OE',
               'o'            => '/o',
               'O'            => '/O',
               'ss'           => 'ss',
               'l'            => '/l',
               'L'            => '/L',
               'DH'           => 'D',
               'dh'           => 'd',
               'TH'           => 'TH', # http://www.evertype.com/standards/wynnyogh/thorn.html

               'th'           => 'th',
               'exclamdown'   => '!',
               'questiondown' => '?',
               'pounds'       => '#',
               'ordf'         => 'a',
               'ordm'         => 'o',
               'comma'        => ',',
               'atchar'       => '@',
               'lbracechar'   => '{',
               'rbracechar'   => '}',
               'backslashchar' => '\\',
               'hashchar'      => '#',
               'euro'         => 'Euro',
               'geq'          => '>=',
               'leq'          => '<=',
               'tie'          => ' ',
               'textdegree'      => 'o',
               'quotedblleft'    => '``',
               'quotedblright'   => "''",
               'quoteleft'       => '`',
               'quoteright'      => "'",
               'quotedblbase'    => ',,',
               'quotesinglbase'  => ',',
               'guillemetleft'   => '<<',
               'guillemetright'  => '>>',
               'guillemotleft'   => '<<',
               'guillemotright'  => '>>',
               'guilsinglleft'   => '<',
               'guilsinglright'  => '>',
               'click'           => '', # specially treated
);

my %sort_brace_no_arg_commands = (
  'copyright' => 'C',
  'registeredsymbol' => 'R',
  'today' => 't', 
);

foreach my $accent_letter ('o','O','l','L') {
  $sort_brace_no_arg_commands{$accent_letter} = $accent_letter;
}

my %accent_commands = %Texinfo::Common::accent_commands;
my %no_brace_commands = %Texinfo::Common::no_brace_commands;

our %formatting_misc_commands;
foreach my $command ('verbatiminclude', 'sp', 'center', 'exdent', 
                     'item', 'itemx', 'tab', 'headitem',
    'node', keys(%Texinfo::Common::sectioning_commands)) {
  $formatting_misc_commands{$command} = 1;
}
 
my %ignored_types;
foreach my $type ('empty_line_after_command', 'preamble',
            'empty_spaces_after_command', 'spaces_at_end',
            'empty_spaces_before_argument', 'empty_spaces_before_paragraph',
            'empty_spaces_after_close_brace', 
            'empty_space_at_end_def_bracketed') {
  $ignored_types{$type} = 1;
}


sub ascii_accent($$)
{
  my $text = shift;
  my $command = shift;
  my $accent = $command->{'cmdname'};
  return $text if ($accent eq 'dotless');
  return $text . "''" if ($accent eq 'H');
  return $text . '.' if ($accent eq 'dotaccent');
  return $text . '*' if ($accent eq 'ringaccent');
  return $text . '[' if ($accent eq 'tieaccent');
  return $text . '(' if ($accent eq 'u');
  return $text . '_' if ($accent eq 'ubaraccent');
  return '.' . $text  if ($accent eq 'udotaccent');
  return $text . '<' if ($accent eq 'v');
  return $text . ';' if ($accent eq 'ogonek');
  return $text . $accent;
}

# format a stack of accents as ascii
sub ascii_accents($$;$)
{
  my $result = shift;
  my $stack = shift;
  my $set_case = shift;

  if ($set_case and $result =~ /^\w$/) {
    if ($set_case > 0) {
      $result = uc($result);
    } else {
      $result = lc($result);
    }
  }
  foreach my $accent_command (reverse(@$stack)) {
    $result = ascii_accent($result, $accent_command);
  }
  return $result;
}

# Same as ascii_accent, but with a converter as first argument to be consistent
# with calling conventions of fallback accent formatting functions given
# to convert_accents/encoded_accents
sub ascii_accent_fallback($$$)
{
  my $converter = shift;
  my $text = shift;
  my $command = shift;

  return ascii_accent($text, $command);
}

# format an accent command and nested accents within as Text.
sub text_accents($;$$)
{
  my $accent = shift;
  my $encoding = shift;
  my $set_case = shift;
  
  my ($contents, $stack)
      = Texinfo::Common::find_innermost_accent_contents($accent);

  my $options = {};
  $options->{'enabled_encoding'} = $encoding if (defined($encoding));
  $options->{'sc'} = $set_case if (defined($set_case));
  my $text = convert({'contents' => $contents}, $options);

  my $result = Texinfo::Convert::Unicode::encoded_accents(undef, $text, 
                     $stack, $encoding, \&ascii_accent_fallback, $set_case);
  if (defined($result)) {
    return $result;
  } else {
    return ascii_accents($text, $stack, $set_case);
  }
}

sub brace_no_arg_command($;$)
{
  my $root = shift;
  my $options = shift;
  my $encoding;
  $encoding = $options->{'enabled_encoding'}
    if ($options and $options->{'enabled_encoding'});

  my $command = $root->{'cmdname'};
  $command = $root->{'extra'}->{'clickstyle'}
     if ($root->{'extra'}
      and defined($root->{'extra'}->{'clickstyle'})
      and defined($text_brace_no_arg_commands{$root->{'extra'}->{'clickstyle'}}));
  my $result = Texinfo::Convert::Unicode::unicode_for_brace_no_arg_command(
                       $command, $encoding);
  if (!defined($result and $options and $options->{'converter'})) {
    my $tree = Texinfo::Common::translated_command_tree(
                  $options->{'converter'}, $command);
    if ($tree) {
      $result = _convert($tree, $options);
    }
  }
  if (!defined($result)) {
    if ($options and $options->{'sort_string'} 
        and $sort_brace_no_arg_commands{$command}) {
      $result = $sort_brace_no_arg_commands{$command};
    } else {
      $result = $text_brace_no_arg_commands{$command};
    }
  }
  if ($options and $Texinfo::Common::letter_no_arg_commands{$command}) {
    if ($options->{'sc'}) {
      $result = uc($result);
    } elsif ($options->{'lc'}) {
      $result = lc($result);
    }
  }
  return $result;
}

my %underline_symbol = (
  0 => '*',
  1 => '*',
  2 => '=',
  3 => '-',
  4 => '.'
);

sub heading($$$;$)
{
  my $current = shift;
  my $text = shift;
  my $converter = shift;
  my $numbered = shift;

  $text = Texinfo::Common::numbered_heading($converter, $current, $text, 
                                            $numbered);
  return '' if ($text !~ /\S/);
  my $result = $text ."\n";
  $result .=($underline_symbol{$current->{'level'}} 
     x Texinfo::Convert::Unicode::string_width($text))."\n";
  return $result;
}

sub _code_options($)
{
  my $options = shift;
  my $code_options;
  if (defined($options)) {
    $code_options = { %$options };
  } else {
    $code_options = {};
  }
  $code_options->{'code'} = 1;
  return $code_options;
}

sub convert($;$)
{
  my $root = shift;
  # means it was called object oriented
  if (ref($root) ne 'HASH') {
    if (ref($root) eq 'ARRAY') {
      carp ("convert argument $root not blessed reference or HASH");
      return undef;
    }
    $root = shift;
  }
  my $options = shift;
  #print STDERR "CONVERT\n";
  return _convert($root, $options);
}

sub _convert($;$);

sub _convert($;$)
{
  my $root = shift;
  my $options = shift;

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
                     and !(defined($options->{'expanded_formats_hash'})
                           and $options->{'expanded_formats_hash'}->{$root->{'cmdname'}}))
                 or ($Texinfo::Common::inline_commands{$root->{'cmdname'}}
                     and $root->{'cmdname'} ne 'inlinefmtifelse'
                     and (($Texinfo::Common::inline_format_commands{$root->{'cmdname'}}
                          and (!$root->{'extra'}->{'format'}
                               or !$options->{'expanded_formats_hash'}->{$root->{'extra'}->{'format'}}))
                         or (!$Texinfo::Common::inline_format_commands{$root->{'cmdname'}}
                             and !defined($root->{'extra'}->{'expand_index'}))))
             # here ignore most of the misc commands
                 or ($root->{'args'} and $root->{'args'}->[0] 
                     and $root->{'args'}->[0]->{'type'} 
                     and ($root->{'args'}->[0]->{'type'} eq 'misc_line_arg'
                         or $root->{'args'}->[0]->{'type'} eq 'misc_arg') 
                     and !$formatting_misc_commands{$root->{'cmdname'}})))));
  my $result = '';
  if (defined($root->{'text'})) {
    $result = $root->{'text'};
    if ((! defined($root->{'type'}) 
         or $root->{'type'} ne 'raw')
         and !$options->{'raw'}) {
      if ($options->{'sc'}) {
        $result = uc($result);
      }
      if (!$options->{'code'}) {
        $result =~ s/``/"/g;
        $result =~ s/\'\'/"/g;
        $result =~ s/---/\x{1F}/g;
        $result =~ s/--/-/g;
        $result =~ s/\x{1F}/--/g;
      }
    }
  }
  if ($root->{'cmdname'}) {
    my $command = $root->{'cmdname'};
    if (defined($no_brace_commands{$root->{'cmdname'}})) {
      return $no_brace_commands{$root->{'cmdname'}};
    } elsif ($root->{'cmdname'} eq 'today') {
      if ($options->{'sort_string'} 
          and $sort_brace_no_arg_commands{$root->{'cmdname'}}) {
        return $sort_brace_no_arg_commands{$root->{'cmdname'}};
      } elsif ($options->{'converter'}) {
        return _convert(Texinfo::Common::expand_today($options->{'converter'}),
                       $options);
      } elsif ($options->{'TEST'}) {
        return 'a sunny day';
      } else {
        my($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst)
          = localtime(time);
        $year += ($year < 70) ? 2000 : 1900;
        return "$Texinfo::Common::MONTH_NAMES[$mon] $mday, $year";
      }
    } elsif (defined($text_brace_no_arg_commands{$root->{'cmdname'}})) {
      return brace_no_arg_command($root, $options);
    # commands with braces
    } elsif ($accent_commands{$root->{'cmdname'}}) {
      my $result = text_accents ($root, $options->{'enabled_encoding'}, 
                                        $options->{'sc'});
      return $result;
    } elsif ($root->{'cmdname'} eq 'image') {
      return _convert($root->{'args'}->[0], _code_options($options));
    } elsif ($root->{'cmdname'} eq 'email') {
      my $mail = _convert($root->{'args'}->[0], _code_options($options));
      my $text;
      $text = _convert($root->{'args'}->[1], $options)
         if (defined($root->{'args'}->[1]));
      return $text if (defined($text) and ($text ne ''));
      return $mail;
    } elsif ($root->{'cmdname'} eq 'uref' or $root->{'cmdname'} eq 'url') {
      my $replacement;
      $replacement = _convert($root->{'args'}->[2], $options)
        if (defined($root->{'args'}->[2]));
      return $replacement if (defined($replacement) and $replacement ne '');
      my $text;
      $text = _convert($root->{'args'}->[1], $options)
        if (defined($root->{'args'}->[1]));
      my $url = _convert($root->{'args'}->[0], _code_options($options));
      if (defined($text) and $text ne '') {
        return "$url ($text)";
      } else {
        return $url;
      }
    } elsif ($Texinfo::Common::explained_commands{$root->{'cmdname'}}
             and $root->{'args'} and $root->{'args'}->[1]) {
      my $explanation = _convert($root->{'args'}->[1], $options);
      if ($explanation ne '') {
        return _convert($root->{'args'}->[0], $options) ." ($explanation)";
      } else {
        return _convert($root->{'args'}->[0], $options);
      }
    } elsif ($Texinfo::Common::inline_commands{$root->{'cmdname'}}) {
      $options->{'raw'} = 1 if ($root->{'cmdname'} eq 'inlineraw');
      my $arg_index = 1;
      if ($root->{'cmdname'} eq 'inlinefmtifelse'
          and (!$root->{'extra'}->{'format'}
               or !$options->{'expanded_formats_hash'}->{$root->{'extra'}->{'format'}})) {
        $arg_index = 2;
      }
      if (scalar(@{$root->{'args'}}) > $arg_index) {
        return _convert($root->{'args'}->[$arg_index], $options);
      } else {
        return '';
      }
    } elsif ($root->{'args'} and $root->{'args'}->[0] 
           and (($root->{'args'}->[0]->{'type'}
                and $root->{'args'}->[0]->{'type'} eq 'brace_command_arg')
                or $root->{'cmdname'} eq 'math')) {
      my $result;
      if ($root->{'cmdname'} eq 'sc') {
        $options = {%$options, 'sc' => 1};
      } elsif ($Texinfo::Common::code_style_commands{$root->{'cmdname'}}
               or $root->{'cmdname'} eq 'math') {
        $options = _code_options($options);
      }
      $result = _convert($root->{'args'}->[0], $options);
      return $result;
    # block commands
    } elsif ($root->{'cmdname'} eq 'quotation'
             or $root->{'cmdname'} eq 'smallquotation'
             or $root->{'cmdname'} eq 'float') {
      if ($root->{'args'}) {
        foreach my $arg (@{$root->{'args'}}) {
          my $converted_arg = _convert($arg, $options);
          if ($converted_arg =~ /\S/) {
            $result .= $converted_arg.", ";
          }
        }
        $result =~ s/, $//;
        chomp ($result);
        $result .= "\n" if ($result =~ /\S/);
      }
    } elsif ($options->{'expanded_formats_hash'}->{$root->{'cmdname'}}) {
      $options->{'raw'} = 1;
    } elsif ($formatting_misc_commands{$root->{'cmdname'}} and $root->{'args'}) {
      if ($root->{'cmdname'} eq 'sp') {
        if ($root->{'extra'} and $root->{'extra'}->{'misc_args'}
            and $root->{'extra'}->{'misc_args'}->[0]) {
          # this useless copy avoids perl changing the type to integer!
          my $sp_nr = $root->{'extra'}->{'misc_args'}->[0];
          $result = "\n" x $sp_nr;
        }
      } elsif ($root->{'cmdname'} eq 'verbatiminclude') {
        my $verbatim_include_verbatim
          = Texinfo::Common::expand_verbatiminclude($options->{'converter'},
                                                    $root);
        if (defined($verbatim_include_verbatim)) {
          $result .= _convert($verbatim_include_verbatim, $options);
        }
      } elsif ($root->{'cmdname'} ne 'node') {
        $result = _convert($root->{'args'}->[0], $options);
        if ($Texinfo::Common::sectioning_commands{$root->{'cmdname'}}) {
          $result = heading ($root, $result, $options->{'converter'}, 
                             $options->{'NUMBER_SECTIONS'});
        } else {
        # we always want an end of line even if is was eaten by a command
          chomp ($result);
          $result .= "\n";
        }
      }
    } elsif ($root->{'cmdname'} eq 'item' 
            and $root->{'parent'}->{'cmdname'} 
            and $root->{'parent'}->{'cmdname'} eq 'enumerate') {
      $result .= Texinfo::Common::enumerate_item_representation(
         $root->{'parent'}->{'extra'}->{'enumerate_specification'},
         $root->{'extra'}->{'item_number'}) . '. ';
    }
  }
  if ($root->{'type'} and $root->{'type'} eq 'def_line') {
    #print STDERR "$root->{'extra'}->{'def_command'}\n";
    if ($root->{'extra'} and $root->{'extra'}->{'def_args'}
             and @{$root->{'extra'}->{'def_args'}}) {
      my $parsed_definition_category
        = Texinfo::Common::definition_category ($options->{'converter'}, $root);
      my @contents = ($parsed_definition_category, {'text' => ': '});
      if ($root->{'extra'}->{'def_parsed_hash'}->{'type'}) {
        push @contents, ($root->{'extra'}->{'def_parsed_hash'}->{'type'},
                         {'text' => ' '});
      }
      push @contents, $root->{'extra'}->{'def_parsed_hash'}->{'name'};

      my $arguments = Texinfo::Common::definition_arguments_content($root);
      if ($arguments) {
        push @contents, {'text' => ' '};
        push @contents, @$arguments;
      }
      push @contents, {'text' => "\n"};
      $result = _convert({'contents' => \@contents}, _code_options($options));
    }
    #$result = convert($root->{'args'}->[0], $options) if ($root->{'args'});
  } elsif ($root->{'type'} and $root->{'type'} eq 'menu_entry') {
    foreach my $arg (@{$root->{'args'}}) {
      if ($arg->{'type'} eq 'menu_entry_node') {
        $result .= _convert($arg, _code_options($options));
      } else {
        $result .= _convert($arg, $options);
      }
    }
    if (!$root->{'parent'}->{'type'} 
        or ($root->{'parent'}->{'type'} ne 'preformatted'
            and $root->{'parent'}->{'type'} ne 'rawpreformatted')) {
      chomp($result);
      $result .= "\n";
    }
  }
  if ($root->{'contents'}) {
    if ($root->{'cmdname'} 
        and $Texinfo::Common::preformatted_code_commands{$root->{'cmdname'}}) {
      $options = _code_options($options);
    }
    if (ref($root->{'contents'}) ne 'ARRAY') {
      cluck "contents not an array($root->{'contents'}).";
    }
    foreach my $content (@{$root->{'contents'}}) {
      $result .= _convert($content, $options);
    }
  }
  $result = '{'.$result.'}' 
     if ($root->{'type'} and $root->{'type'} eq 'bracketed'
         and (!$root->{'parent'}->{'type'} or
              ($root->{'parent'}->{'type'} ne 'block_line_arg'
               and $root->{'parent'}->{'type'} ne 'misc_line_arg')));
  #print STDERR "  RR ($root) -> $result\n";
  return $result;
}



# Implement the converters API, but as simply as possible
# initialization
sub converter($)
{
  my $class = shift;
  my $conf;
  my $converter = {};
  if (ref($class) eq 'HASH') {
    $conf = $class;
    bless $converter;
  } elsif (defined($class)) {
    bless $converter, $class;
    $conf = shift;
  } else {
    bless $converter;
    $conf = shift;
  }

  if ($conf) {
    %{$converter} = %{$conf};
  }

  my $expanded_formats = $converter->{'expanded_formats'};;
  if ($converter->{'parser'}) {
    $converter->{'info'} = $converter->{'parser'}->global_informations();
    $converter->{'extra'} = $converter->{'parser'}->global_commands_information();
    foreach my $global_command ('documentencoding') {
      if (defined($converter->{'extra'}->{$global_command})) {
        my $root = $converter->{'extra'}->{$global_command}->[0];
        if ($global_command eq 'documentencoding'
            and defined($root->{'extra'})
            and defined($root->{'extra'}->{'input_perl_encoding'})) {
          $converter->{'OUTPUT_ENCODING_NAME'} 
             = $root->{'extra'}->{'input_encoding_name'};
          $converter->{'OUTPUT_PERL_ENCODING'} 
             = $root->{'extra'}->{'input_perl_encoding'};
        }
      }
    }
    if (!$expanded_formats and $converter->{'parser'}->{'expanded_formats'}) {
      $expanded_formats = $converter->{'parser'}->{'expanded_formats'};
    }
  }
  if ($expanded_formats) {
    foreach my $expanded_format(@$expanded_formats) {
      $converter->{'expanded_formats_hash'}->{$expanded_format} = 1;
    }
  }

  bless $converter;
  return $converter;
}

sub convert_tree($$)
{
  my $self = shift;
  my $root = shift;

  return _convert($root);
}

# determine outfile and output to that file
my $STDIN_DOCU_NAME = 'stdin';
sub output($$)
{
  my $self = shift;
  my $tree = shift;
  #print STDERR "OUTPUT\n";
  my $input_basename;
  if (defined($self->{'info'}->{'input_file_name'})) {
    my ($directories, $suffix);
    ($input_basename, $directories, $suffix)
       = fileparse($self->{'info'}->{'input_file_name'});
  } else {
    # This could happen if called on a piece of texinfo
    $input_basename = '';
  }
  $self->{'input_basename'} = $input_basename;
  $input_basename = $STDIN_DOCU_NAME if ($input_basename eq '-');
  $input_basename =~ s/\.te?x(i|info)?$//;

  my $setfilename;
  $setfilename = $self->{'extra'}->{'setfilename'}->{'extra'}->{'text_arg'}
    if ($self->{'extra'} and $self->{'extra'}->{'setfilename'}
        and $self->{'extra'}->{'setfilename'}->{'extra'}
        and defined($self->{'extra'}->{'setfilename'}->{'extra'}->{'text_arg'}));
  my $outfile;
  if (!defined($self->{'OUTFILE'})) {
    if (defined($setfilename)) {
      $outfile = $setfilename;
      $outfile =~ s/\.[^\.]*$//;
    } elsif ($input_basename ne '') {
      $outfile = $input_basename;
    }
    if (defined($outfile)) {
      $outfile .= '.txt';
    }
  } else {
    $outfile = $self->{'OUTFILE'};
  }
  my $fh;
  if (defined($outfile)) {
    $fh = $self->Texinfo::Common::open_out($outfile);
    return undef if (!$fh);
  }
  my %options = $self->Texinfo::Common::_convert_text_options();
  my $result = _convert($tree, \%options);
  if ($fh) {
    print $fh $result;
    return undef if (!close($fh));
    $result = '';
  }
  return $result;
}

sub get_conf($$)
{
  my $self = shift;
  my $key = shift;

  return $self->{$key};
}

sub errors()
{
  return undef;
}

sub converter_unclosed_files()
{
  return undef;
}

sub converter_opened_files()
{
  return ();
}

sub converter_defaults()
{
  return ();
}

1;

__END__

=head1 NAME

Texinfo::Convert::Text - Convert Texinfo tree to simple text

=head1 SYNOPSIS

  use Texinfo::Convert::Text qw(convert ascii_accent text_accents);

  my $result = convert($tree);
  my $result_encoded = convert($tree, 
             {'enabled_encoding' => 'utf-8'});
  my $result_converter = convert($tree,
             {'converter' => $converter});

  my $result_accent_text = ascii_accent('e', $accent_command);
  my $accents_text = text_accents($accents, 'utf-8');

=head1 DESCRIPTION

Texinfo::Convert::Text is a simple backend that converts a Texinfo tree
to simple text.  It is used for some command argument expansion in 
C<Texinfo::Parser>, for instance the file names, or encoding names.
The converter is very simple, and, in the default case, cannot handle 
output strings translation or error handling.

=head1 METHODS

=over

=item $result = convert($tree, $options)

Convert a Texinfo tree to simple text.  I<$options> is a hash reference of 
options.  The converter is very simple, and has no internal state besides
the options.  It cannot handle as is output strings translation or error 
storing.

If the I<converter> option is set, some additional features may be available
for the conversion of some @-commands, like output strings translation or
error reporting.

The following options may be set:

=over

=item enabled_encoding

If set, the value is considered to be the encoding name texinfo accented
letters should be converted to.  This option corresponds to the 
C<--enable-encoding> option, or the C<ENABLE_ENCODING> customization 
variable.

=item sc

If set, the text is upper-cased.

=item code

If set the text is in code style.  (mostly --, ---, '' and `` are kept as 
is).

=item NUMBER_SECTIONS

If set, sections are numbered when output.

=item sort_string

A somehow internal option to convert to text more suitable for alphabetical
sorting rather than presentation.

=item converter

If this converter object is passed to the function, some features of this
object may be used during conversion.  Mostly error reporting and strings
translation, as the converter object is also supposed to be a 
L<Texinfo::Report> objet.  See also L<Texinfo::Convert::Converter>.

=item expanded_formats_hash

A reference on a hash.  The keys should be format names (like C<html>, 
C<tex>), and if thecorresponding  value is set, the format is expanded.

=back

=item $result_accent_text = ascii_accent($text, $accent_command)

I<$text> is the text appearing within an accent command.  I<$accent_command>
should be a Texinfo tree element corresponding to an accent command taking
an argument.  The function returns a transliteration of the accented
character.

=item $result_accent_text = ascii_accent_fallback($converter, $text, $accent_command)

Same as C<ascii_accent> but  with an additional first argument
converter, which is in ignored, but needed if this function is to 
be in argument of functions that need a fallback for accents
conversion.

=item $accents_text = text_accents($accents, $encoding, $set_case)

I<$accents> is an accent command that may contain other nested accent 
commands.  The function will format the whole stack of nested accent 
commands and the innermost text.  If I<$encoding> is set, the formatted
text is converted to this encoding as much as possible instead of being
converted as simple ascii.  If I<$set_case> is positive, the result
is meant to be upper-cased, if it is negative, the result is to be 
lower-cased.

=back

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2010, 2011, 2012 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License,
or (at your option) any later version.

=cut
