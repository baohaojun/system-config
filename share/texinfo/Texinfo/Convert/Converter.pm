# Converter.pm: Common code for Converters.
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

package Texinfo::Convert::Converter;

use 5.00405;
use strict;

# for fileparse
use File::Basename;
# for file names portability
use File::Spec;
use Encode;

use Texinfo::Report;
use Texinfo::Common;
use Texinfo::Convert::Text;
use Texinfo::Convert::Texinfo;
use Texinfo::Structuring;

use Carp qw(cluck);

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter Texinfo::Report);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Texinfo::Convert::Converter ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
xml_protect_text
xml_comment
xml_accent
xml_accents
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '5.1.90';

my %defaults = (
  'ENABLE_ENCODING'      => 1,
  'OUTFILE'              => undef,
  'SUBDIR'               => undef,
  'documentlanguage'     => undef,
);

# defaults for all converters.  Maybe more could be added, especially what
# can be set with --set and should be the same for all the formats.
our %all_converters_defaults = (
  'htmlxref_files'       => undef,
  'output_format'        => undef,
  'SPLIT_SIZE'           => 300000,
  'paragraphindent'      => 3,
  'fillcolumn'           => 72,
  'expanded_formats'     => undef,
  'include_directories'  => undef,
  'IMAGE_LINK_PREFIX'    => undef,
  'NUMBER_SECTIONS'      => 1,
  'NUMBER_FOOTNOTES'     => 1,
  'frenchspacing'        => 'off',
  'paragraphindent'      => 3,
  'firstparagraphindent' => 'none',
  'allowcodebreaks'      => 'true',
  'footnotestyle'        => 'end',
  'deftypefnnewline'     => 'off',
  'BASEFILENAME_LENGTH'  => 255 - 10,
# This is not used much as converters do their best to give a sane output
  'FIX_TEXINFO'          => 1,
# This is the default, mainly for tests; the caller should set them.  These
# values are what is used in tests of the Converters.
  'PACKAGE_AND_VERSION'  => 'texinfo',
  'PACKAGE_VERSION'      => '',
  'PACKAGE_URL'          => 'http://www.gnu.org/software/texinfo/',
  'PROGRAM'              => '',

  'DEBUG'                => 0,
  'TEST'                 => 0,
  'translated_commands'  => {'error' => 'error@arrow{}',},
  'TEXINFO_COLUMN_FOR_DESCRIPTION' => 32, # same as emacs
);

# For translation of in document string.
if (0) {
  my $self;
  $self->gdt('error@arrow{}');
}

sub converter_defaults($$)
{
  return %defaults;
}

sub converter_initialize($)
{
}

sub converter_global_commands($)
{
  return ('documentlanguage', 'documentencoding');
}

sub output_internal_links($)
{
  my $self = shift;
  return undef;
}

sub _informative_command_value($$)
{
  my $self = shift;
  my $root = shift;

  my $cmdname = $root->{'cmdname'};

  if ($Texinfo::Common::misc_commands{$cmdname} eq 'skipline') {
    return 1;
  } elsif (exists($root->{'extra'}->{'text_arg'})) {
    return $root->{'extra'}->{'text_arg'};
  } elsif ($root->{'extra'} and $root->{'extra'}->{'misc_args'}
           and exists($root->{'extra'}->{'misc_args'}->[0])) {
    return $root->{'extra'}->{'misc_args'}->[0];
  }
  return undef;
}

# FIXME documentencoding handling is not reverted by resetting
# a value with set_conf, so _unset_global_multiple_commands won't
# reverse what _set_global_multiple_commands did through 
# _informative_command.
sub _informative_command($$)
{
  my $self = shift;
  my $root = shift;

  my $cmdname = $root->{'cmdname'};
  $cmdname = 'shortcontents' if ($cmdname eq 'summarycontents');

  return if ($self->{'set'}->{$cmdname});

  my $value = $self->_informative_command_value($root);
  if (defined($value)) {
    $self->set_conf($cmdname, $value);
  }
}

sub register_close_file($$)
{
  my $self = shift;
  my $filename = shift;
  if ($self->{'unclosed_files'}->{$filename}) {
    delete $self->{'unclosed_files'}->{$filename};
  } else {
    cluck "$filename not opened\n";
  }
}

sub converter(;$)
{
  my $class = shift;
  my $converter = { 'set' => {} };

  my $conf;
  my $name = 'converter';

  if (ref($class) eq 'HASH') {
    $conf = $class;
    bless $converter;
  } elsif (defined($class)) {
    bless $converter, $class;
    $name = ref($converter);
    $conf = shift;
  } else {
    bless $converter;
    $conf = shift;
    $name = ref($converter);
  }
  my %defaults = $converter->converter_defaults($conf);
  foreach my $key (keys(%all_converters_defaults)) {
    $defaults{$key} = $all_converters_defaults{$key} 
      if (!exists($defaults{$key}));
  }
  foreach my $key (keys(%defaults)) {
    if (Texinfo::Common::valid_option($key)) {
      $converter->{'conf'}->{$key} = $defaults{$key};
    } else {
      $converter->{$key} = $defaults{$key};
    }
  }
  $converter->{'conf_default'} = \%defaults;
  if (defined($conf)) {
    if ($conf->{'parser'}) {
      $converter->{'parser'} = $conf->{'parser'};
      $converter->{'extra'} 
         = $converter->{'parser'}->global_commands_information();
      $converter->{'info'} = $converter->{'parser'}->global_informations();
      if ($converter->{'info'} 
          and $converter->{'info'}->{'input_perl_encoding'}
          and !defined($conf->{'INPUT_PERL_ENCODING'})) {
        $conf->{'INPUT_PERL_ENCODING'}
              = $converter->{'info'}->{'input_perl_encoding'};
      }
      if ($converter->{'info'} 
          and $converter->{'info'}->{'input_encoding_name'}
          and !defined($conf->{'INPUT_ENCODING_NAME'})) {
        $conf->{'INPUT_ENCODING_NAME'} 
             = $converter->{'info'}->{'input_encoding_name'};
      }
      my $floats = $converter->{'parser'}->floats_information();
      my $labels = $converter->{'parser'}->labels_information();
      $converter->{'structuring'} = $converter->{'parser'}->{'structuring'};

      $converter->{'floats'} = $floats if ($floats);
      $converter->{'labels'} = $labels if ($labels);
      foreach my $global_command ($converter->converter_global_commands()) {
        if (defined($converter->{'extra'}->{$global_command})) {
          my $root = $converter->{'extra'}->{$global_command};
          # always set unique commands
          if (ref($root) ne 'ARRAY') {
            $converter->_informative_command($root);
          }
        }
      }
      $converter->set_conf('setcontentsaftertitlepage', 1)
         if ($converter->get_conf('contents')
               and $converter->{'extra'}->{'setcontentsaftertitlepage'}
               and $converter->{'structuring'}
               and $converter->{'structuring'}->{'sectioning_root'});
      $converter->set_conf('setshortcontentsaftertitlepage', 1)
         if ($converter->get_conf('shortcontents') 
               and $converter->{'extra'}->{'setshortcontentsaftertitlepage'}
               and $converter->{'structuring'}
               and $converter->{'structuring'}->{'sectioning_root'});
      $converter->{'gettext'} = $converter->{'parser'}->{'gettext'};
      $converter->{'pgettext'} = $converter->{'parser'}->{'pgettext'};
      delete $conf->{'parser'};
    }
    foreach my $key (keys(%$conf)) {
      if (Texinfo::Common::valid_option($key)) {
        $converter->{'conf'}->{$key} = $conf->{$key};
      } elsif (!exists($defaults{$key})) {
        warn "$key not a possible configuration in $name\n";
      } else {
        $converter->{$key} = $conf->{$key};
      }
      $converter->{'set'}->{$key} = 1;
    }
  }
  $converter->set_conf('OUTPUT_ENCODING_NAME', 
                       $converter->get_conf('INPUT_ENCODING_NAME'))
     if ($converter->get_conf('INPUT_ENCODING_NAME'));
  if (!$converter->get_conf('OUTPUT_PERL_ENCODING')
       and $converter->get_conf('OUTPUT_ENCODING_NAME')) {
    my $perl_encoding 
      = Encode::resolve_alias($converter->get_conf('OUTPUT_ENCODING_NAME'));
    if ($perl_encoding) {
      $converter->set_conf('OUTPUT_PERL_ENCODING', $perl_encoding);
    }
  }
  if (!defined($converter->{'expanded_formats'})) {
    if ($converter->{'parser'}) {
      $converter->{'expanded_formats'} = $converter->{'parser'}->{'expanded_formats'};
    } else {
      $converter->{'expanded_formats'} = [];
    }
  }
  if (!defined($converter->{'include_directories'})) {
    if ($converter->{'parser'}) {
      $converter->{'include_directories'} = $converter->{'parser'}->{'include_directories'};
    } else {
      $converter->{'include_directories'} = [ '.' ];
    }
  }

  # turn the array to a hash for speed.  Not sure it really matters for such
  # a small array.
  foreach my $expanded_format(@{$converter->{'expanded_formats'}}) {
    $converter->{'expanded_formats_hash'}->{$expanded_format} = 1;
  }

  $converter->Texinfo::Report::new();

  $converter->converter_initialize();

  return $converter;
}

sub converter_unclosed_files($)
{
  my $self = shift;
  return $self->{'unclosed_files'};
}

sub converter_opened_files($)
{
  my $self = shift;
  if (defined($self->{'opened_files'})) {
    return @{$self->{'opened_files'}};
  } else {
    return ();
  }
}

sub _set_global_multiple_commands($;$)
{
  my $self = shift;
  my $multiple_commands_index = shift;
  $multiple_commands_index = 0 if (!defined($multiple_commands_index));

  foreach my $global_command ($self->converter_global_commands()) {
    if (defined($self->{'extra'}->{$global_command})
        and ref($self->{'extra'}->{$global_command}) eq 'ARRAY') {
      my $root = $self->{'extra'}->{$global_command}->[$multiple_commands_index];
      if ($self->get_conf('DEBUG')) {
        print STDERR "SET_global_multiple_commands($multiple_commands_index) $global_command\n";
      }
      $self->_informative_command($root);
    }
  }
}

# Notice that set_conf is used, which means that it is not possible to
# customize what is done for those commands.  For documentencoding, for
# example the values are not reset correctly, see the FIXME above.
sub _unset_global_multiple_commands($)
{
  my $self = shift;

  foreach my $global_command ($self->converter_global_commands()) {
    if (defined($self->{'extra'}->{$global_command})
        and ref($self->{'extra'}->{$global_command}) eq 'ARRAY') {
      if (exists($self->{'conf_default'}->{$global_command})) {
        if ($self->get_conf('DEBUG')) {
          my $default = 'UNDEF';
          $default = $self->{'conf_default'}->{$global_command} 
            if (defined($self->{'conf_default'}->{$global_command}));
          my $set = 0;
          $set = 1 if ($self->{'set'}->{$global_command});
          print STDERR "UNSET_global_multiple_commands $global_command ($set): $default\n";
        }
        if (Texinfo::Common::valid_option($global_command)) {
          $self->set_conf($global_command, $self->{'conf_default'}->{$global_command});
        } else {
          $self->{$global_command} = $self->{'conf_default'}->{$global_command};
        }
      }
    }
  }
}

sub get_conf($$)
{
  my $self = shift;
  my $conf = shift;
  if (!Texinfo::Common::valid_option($conf)) {
    warn "BUG: unknown option $conf\n";
    return undef;
  }
  return $self->{'conf'}->{$conf};
}

sub set_conf($$$)
{
  my $self = shift;
  my $conf = shift;
  my $value = shift;
  if (!Texinfo::Common::valid_option($conf)) {
    warn "BUG: unknown option $conf\n";
    return undef;
  } elsif (Texinfo::Common::obsolete_option($conf)) {
    warn(sprintf(main::__("Obsolete variable %s\n"), $conf));
  }
  if ($self->{'set'}->{$conf}) {
    return 0;
  } else {
    $self->{'conf'}->{$conf} = $value;
    return 1;
  }
}

sub force_conf($$$)
{
  my $self = shift;
  my $conf = shift; 
  my $value = shift;
  if (!Texinfo::Common::valid_option($conf)) {
    warn "BUG: unknown option $conf\n";
    return undef;
  } elsif (Texinfo::Common::obsolete_option($conf)) {
    warn(sprintf(main::__("Obsolete variable %s\n"), $conf));
  }
  $self->{'conf'}->{$conf} = $value;
  return 1;
}

my $STDIN_DOCU_NAME = 'stdin';

sub _set_outfile($$$)
{
  my $self = shift;

  # determine input file base name
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
  if ($input_basename eq '-') {
    $input_basename = $STDIN_DOCU_NAME 
  } else {
    $input_basename =~ s/\.te?x(i|info)?$//;
  }
  $self->{'input_basename_name'} = $input_basename;

  my $setfilename;
  if (defined($self->get_conf('setfilename'))) {
    $setfilename = $self->get_conf('setfilename');
  } elsif ($self->{'extra'} and $self->{'extra'}->{'setfilename'}
          and $self->{'extra'}->{'setfilename'}->{'extra'}
          and defined($self->{'extra'}->{'setfilename'}->{'extra'}->{'text_arg'})) {
     $setfilename = $self->{'extra'}->{'setfilename'}->{'extra'}->{'text_arg'}
  }

  # PREFIX overrides both setfilename and the input file base name
  if (defined($self->get_conf('PREFIX'))) {
    $setfilename = undef;
    $input_basename = $self->get_conf('PREFIX');
  }

  # the name of the document, which is more or less the basename, without 
  # extension
  my $document_name;
  # determine output file and output file name
  my $outfile;
  if (!defined($self->get_conf('OUTFILE'))) {
    if (defined($setfilename) and !$self->get_conf('NO_USE_SETFILENAME')) {
      $outfile = $setfilename;
      $document_name = $setfilename;
      $document_name =~ s/\.[^\.]*$//;
      if (!$self->get_conf('USE_SETFILENAME_EXTENSION')) {
        $outfile =~ s/\.[^\.]*$//;
        $outfile .= '.'.$self->get_conf('EXTENSION') 
          if (defined($self->get_conf('EXTENSION')) 
              and $self->get_conf('EXTENSION') ne '');
      }
    } elsif ($input_basename ne '') {
      $outfile = $input_basename;
      $document_name = $input_basename;
      $outfile .= '.'.$self->get_conf('EXTENSION') 
        if (defined($self->get_conf('EXTENSION')) 
            and $self->get_conf('EXTENSION') ne '');
    } else {
      $outfile = '';
      $document_name = $outfile;
    }
    if (defined($self->get_conf('SUBDIR')) and $outfile ne '') {
      my $dir = File::Spec->canonpath($self->get_conf('SUBDIR'));
      $outfile = File::Spec->catfile($dir, $outfile);
    }
    #$self->set_conf('OUTFILE', $outfile);
  } else {
    $document_name = $self->get_conf('OUTFILE');
    $document_name =~ s/\.[^\.]*$//;
    $outfile = $self->get_conf('OUTFILE');
  }

  # the output file without directories part.
  my $output_filename = $outfile;
  # this is a case that should happen rarely: one wants to get 
  # the result in a string and there is a setfilename.
  if ($outfile eq '' and defined($setfilename)
      and !$self->get_conf('NO_USE_SETFILENAME')) {
    $output_filename = $setfilename;
    $document_name = $setfilename;
    $document_name =~ s/\.[^\.]*$//;
  }
  my ($directories, $suffix);
  # We may be handling setfilename there, so it is not obvious that we
  # want to use fileparse and not consider unixish separators.  However, 
  # if this is setfilename, it should be a simple file name, so it 
  # should hopefully be harmless to use fileparse
  ($document_name, $directories, $suffix) = fileparse($document_name);
  $self->{'document_name'} = $document_name;
  ($output_filename, $directories, $suffix) = fileparse($output_filename);
  $self->{'output_filename'} = $output_filename;
  if ($self->get_conf('SPLIT')) {
    if (defined($self->get_conf('OUTFILE'))) {
      $self->{'destination_directory'} = $self->get_conf('OUTFILE');
    } elsif (defined($self->get_conf('SUBDIR'))) {
      $self->{'destination_directory'} = $self->get_conf('SUBDIR');
    } else {
      $self->{'destination_directory'} = $document_name;
    }
  } else {
    my ($out_filename, $output_dir, $suffix) = fileparse($outfile);
    if ($output_dir ne '') {
      $self->{'destination_directory'} = $output_dir;
    }
  }
  if (defined($self->{'destination_directory'}) 
      and $self->{'destination_directory'} ne '') {
    $self->{'destination_directory'} 
      = File::Spec->canonpath($self->{'destination_directory'});
  }
  $self->{'output_file'} = $outfile;
}

sub _bug_message($$;$)
{
  my $self = shift;
  my $message = shift;
  my $current = shift;

  my $line_message = '';
  my $current_element_message = '';
  if ($current) {
    if ($current->{'line_nr'}) {
      my $line_number = $current->{'line_nr'};
      my $file = $line_number->{'file_name'};
      $line_message
        = "in: $line_number->{'file_name'}:$line_number->{'line_nr'}";
      if ($line_number->{'macro'} ne '') {
        $line_message .= " (possibly involving $line_number->{'macro'})";
      }
      $line_message .= "\n";
    }
    if ($current) {
      $current_element_message = "current: ". 
        Texinfo::Parser::_print_current($current);
    }
  }
  my $additional_information = '';
  if ($line_message.$current_element_message ne '') {
    $additional_information = "Additional informations:\n".
       $line_message.$current_element_message;
  }
  warn "You found a bug: $message\n\n".$additional_information;
}

# This is not used as code, but used to mark months as strings to be
# translated
if (0) {
  my $self;
  my @mark_month_for_translation = (
   $self->gdt('January'),
   $self->gdt('February'),
   $self->gdt('March'),
   $self->gdt('April'),
   $self->gdt('May'),
   $self->gdt('June'),
   $self->gdt('July'),
   $self->gdt('August'),
   $self->gdt('September'),
   $self->gdt('October'),
   $self->gdt('November'),
   $self->gdt('December')
  );
}

sub _create_destination_directory($)
{
  my $self = shift;
  if (defined($self->{'destination_directory'})
      and ! -d $self->{'destination_directory'}) {
    if (!mkdir($self->{'destination_directory'}, oct(755))) {
      if ($self->get_conf('SPLIT') 
          and $self->get_conf('EXTENSION') 
          and $self->get_conf('EXTENSION') ne '') {
        my ($volume, $directories, $file) 
           = File::Spec->splitpath($self->{'destination_directory'}, 1);
        my $new_directory = File::Spec->catpath($volume, 
                 $directories . '.' . $self->get_conf('EXTENSION'), $file);
        if (! -d $new_directory) {
          if (!mkdir($new_directory, oct(755))) {
            $self->document_error(sprintf($self->__(
              "could not create directories `%s' or `%s': %s"), 
              $self->{'destination_directory'}, $new_directory, $!));
            return undef;
          }
        }
        $self->{'destination_directory'} = $new_directory;
      } else {
        $self->document_error(sprintf($self->__(
             "could not create directory `%s': %s"), 
             $self->{'destination_directory'}, $!));
        return undef;
      }
    }
  }
  return 1;
}

sub _float_type_number($$)
{
  my $self = shift;
  my $float = shift;

  my $type;
  if ($float->{'extra'}->{'type'}
      and $float->{'extra'}->{'type'}->{'normalized'} ne '') { 
    $type = {'contents' => $float->{'extra'}->{'type'}->{'content'}};
  }

  my $tree;
  if ($type) {            
    if (defined($float->{'number'})) {
      $tree = $self->gdt("{float_type} {float_number}",
          {'float_type' => $type,
            'float_number' => $float->{'number'}});
    } else {
      $tree = $self->gdt("{float_type}",
          {'float_type' => $type});
    }
  } elsif (defined($float->{'number'})) {
    $tree = $self->gdt("{float_number}",
       {'float_number' => $float->{'number'}});
  }
  return $tree;
}

# This is used when the formatted text has no comment nor new line, but
# one want to add the comment or new line from the original arg
sub _end_line_or_comment($$)
{
  my $self = shift;
  my $contents_possible_comment = shift;
  my $end_line;
  if ($contents_possible_comment
      and $contents_possible_comment->[-1]->{'cmdname'}
      and ($contents_possible_comment->[-1]->{'cmdname'} eq 'c'
          or $contents_possible_comment->[-1]->{'cmdname'} eq 'comment')) {
    $end_line = $self->convert_tree($contents_possible_comment->[-1]);
  } elsif ($contents_possible_comment      
           and $contents_possible_comment->[-1]->{'text'}) {
    my $text = $contents_possible_comment->[-1]->{'text'};
    if (chomp($text)) {
      $end_line = "\n";
    } else {
      $end_line = '';
    }
  } else {
    $end_line = '';
  }
  return $end_line;
}

sub _tree_without_comment($)
{
  my $contents_possible_comment = shift;
  my $comment;
  my $tree;

  if ($contents_possible_comment->{'contents'}
      and $contents_possible_comment->{'contents'}->[-1]->{'cmdname'}
      and ($contents_possible_comment->{'contents'}->[-1]->{'cmdname'} eq 'c'
           or $contents_possible_comment->{'contents'}->[-1]->{'cmdname'} eq 'comment')) {
    my @contents = @{$contents_possible_comment->{'contents'}};
    $comment = pop @contents;
    $tree = {'contents' => \@contents};
    # FIXME why this selection, and not everything?
    foreach my $key ('extra', 'type', 'cmdname', 'parent', 'line_nr') {
      $tree->{$key} = $contents_possible_comment->{$key}
        if (exists($contents_possible_comment->{$key}));
    }
  } else {
   $tree = $contents_possible_comment;
  }
  return ($comment, $tree);
}

sub _convert_argument_and_end_line($$)
{
  my $self = shift;
  my $root = shift;
  my ($comment, $tree) 
    = _tree_without_comment($root);
  my $converted = $self->convert_tree($tree);
  my $end_line;
  if ($comment) {
    $end_line = $self->convert_tree($comment);
  } else {
    if (chomp($converted)) {
      $end_line = "\n";
    } else {
      $end_line = "";
    }
  }
  return ($converted, $end_line);
}

sub _collect_leading_trailing_spaces_arg($$)
{
  my $self = shift;
  my $arg = shift;
  #print STDERR "$arg->{'type'}: @{$arg->{'contents'}}\n";
  my @result;
  if ($arg->{'contents'} and $arg->{'contents'}->[0] 
      and defined($arg->{'contents'}->[0]->{'text'})
      and $arg->{'contents'}->[0]->{'text'} !~ /\S/
      and defined($arg->{'contents'}->[0]->{'type'})) {
    #print STDERR "$arg->{'contents'}->[0]->{'type'}\n";
    warn "Unknown leading space type $arg->{'contents'}->[0]->{'type'}\n"
      if ($arg->{'contents'}->[0]->{'type'} ne 'empty_spaces_after_command'
          and $arg->{'contents'}->[0]->{'type'} ne 'empty_spaces_before_argument'
          # FIXME should we really catch this too?
          and $arg->{'contents'}->[0]->{'type'} ne 'empty_line_after_command'
         );
    $result[0] = $arg->{'contents'}->[0]->{'text'};
    return @result if (scalar(@{$arg->{'contents'}}) == 1);
  }
  if ($arg->{'contents'}) {
    my $index = -1;
    $index-- if ($arg->{'contents'}->[-1] 
                 and $arg->{'contents'}->[-1]->{'cmdname'}
                 and ($arg->{'contents'}->[-1]->{'cmdname'} eq 'c'
                      or $arg->{'contents'}->[-1]->{'cmdname'} eq 'comment'));
    if (scalar(@{$arg->{'contents'}}) + $index > 0) {
      if ($arg->{'contents'}->[$index] 
          and defined($arg->{'contents'}->[$index]->{'text'})
          and $arg->{'contents'}->[$index]->{'text'} !~ /\S/
          and defined($arg->{'contents'}->[$index]->{'type'})) {
      #print STDERR "$arg->{'contents'}->[$index]->{'type'}\n";
        warn "Unknown trailing space type $arg->{'contents'}->[$index]->{'type'}\n"
          if ($arg->{'contents'}->[$index]->{'type'} ne 'spaces_at_end'
              and $arg->{'contents'}->[$index]->{'type'} ne 'space_at_end_block_command'
             );
        $result[1] = $arg->{'contents'}->[$index]->{'text'};
      }
    }
  }
  return @result;
}

sub _table_item_content_tree($$$)
{
  my $self = shift;
  my $root = shift;
  my $contents = shift;

  my $converted_tree = {'parent' => $root};
  my $table_command = $root->{'parent'}->{'parent'}->{'parent'};
  if ($table_command->{'extra'}
     and $table_command->{'extra'}->{'command_as_argument'}) {
    my $command_as_argument
      = $table_command->{'extra'}->{'command_as_argument'};
    my $command = {'cmdname' => $command_as_argument->{'cmdname'},
               'line_nr' => $root->{'line_nr'},
               'parent' => $converted_tree };
    if ($command_as_argument->{'type'} eq 'definfoenclose_command') {
      $command->{'type'} = $command_as_argument->{'type'};
      $command->{'extra'}->{'begin'} = $command_as_argument->{'extra'}->{'begin'};
      $command->{'extra'}->{'end'} = $command_as_argument->{'extra'}->{'end'};
    }
    my $arg = {'type' => 'brace_command_arg',
               'contents' => $contents,
               'parent' => $command,};
    $command->{'args'} = [$arg];
    $self->Texinfo::Parser::_register_command_arg($arg, 'brace_command_contents');
    $contents = [$command];
  }
  $converted_tree->{'contents'} = $contents;
  return $converted_tree;
}

sub _level_corrected_section($$)
{
  my $self = shift;
  my $root = shift;
  my $heading_level = $root->{'level'};
  my $command;
  if ($heading_level ne $Texinfo::Common::command_structuring_level{$root->{'cmdname'}}) {
    $command
      = $Texinfo::Common::level_to_structuring_command{$root->{'cmdname'}}->[$heading_level];
  } else {
    $command = $root->{'cmdname'};
  }
  return $command;
}

# generic output method
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

  if ($self->get_conf('USE_NODES')) {
    return $self->convert_document_nodes($root, $fh);
  } else {
    return $self->convert_document_sections($root, $fh);
  }
}

# output fo $fh if defined, otherwise return the text.
sub _output_text($$$)
{
  my $self = shift;
  my $text = shift;
  my $fh = shift;
  if ($fh) { 
    print $fh $text;
    return '';
  } else {
    return $text;
  } 
}

sub _convert_document_elements($$;$$)
{
  my $self = shift;
  my $root = shift;
  my $elements = shift;
  my $fh = shift;

  if ($elements) {
    my $result = '';
    foreach my $element (@$elements) {
      $result .= $self->_output_text($self->convert_tree($element), $fh);
    }
    return $result;
  } else {
    return $self->_output_text($self->convert_tree($root), $fh);
  }
}

sub convert_document_sections($$;$)
{
  my $self = shift;
  my $root = shift;
  my $fh = shift;

  my $elements = Texinfo::Structuring::split_by_section($root);
  return $self->_convert_document_elements($root, $elements, $fh);
}

sub convert_document_nodes($$;$)
{
  my $self = shift;
  my $root = shift;
  my $fh = shift;

  my $elements = Texinfo::Structuring::split_by_node($root);
  return $self->_convert_document_elements($root, $elements, $fh);
}

# if in this container, we are 'inline', within a running text
my @inline_types = ('def_line', 'paragraph', 'preformatted',
  'misc_command_arg', 'misc_line_arg', 'block_line_arg',
  'menu_entry_name', 'menu_entry_node');

my %inline_types;
foreach my $type (@inline_types) {
  $inline_types{$type} = 1;
}

my %not_inline_commands = (%Texinfo::Common::root_commands, 
  %Texinfo::Common::block_commands, %Texinfo::Common::context_brace_command);

# Return 1 if inline in a running text, 0 if right in top-level or block
# environment, and undef otherwise.
sub _inline_or_block($$)
{
  my $self = shift;
  my $current = shift;
  if ($current->{'type'} and $inline_types{$current->{'type'}}) {
    return 1;
  } elsif ($current->{'cmdname'} 
           and exists($not_inline_commands{$current->{'cmdname'}})) {
    return 0;
  } else {
    return undef;
  }
}

# return true if in running text context
sub _is_inline($$)
{
  my $self = shift;
  my $current = shift;
  while ($current->{'parent'}) {
    $current = $current->{'parent'};
    my $inline_or_block = $self->_inline_or_block($current);
    return ($inline_or_block) if (defined($inline_or_block));
  }
  return 0;
}

# return true if container or parent may hold running text
sub _in_inline($$)
{
  my $self = shift;
  my $current = shift;
  my $inline_or_block = $self->_inline_or_block($current);
  return ($inline_or_block) if (defined($inline_or_block));
  return $self->_is_inline($current);
}

our %default_args_code_style = (
  'email' => [1],
  'anchor' => [1],
  'uref' => [1],
  'url' => [1],
  'math' => [1],
  'inforef' => [1,undef,1],
  'image' => [1, 1, 1, undef, 1],
# and type?
  'float' => [1],
);

foreach my $code_style_command (keys(%Texinfo::Common::code_style_commands)) {
  $default_args_code_style{$code_style_command} = [1];
}
foreach my $ref_cmd ('pxref', 'xref', 'ref') {
  $default_args_code_style{$ref_cmd} = [1, undef, undef, 1];
}

sub convert_accents($$$;$)
{
  my $self = shift;
  my $accent = shift;
  my $format_accents = shift;
  my $in_upper_case = shift;

  my ($contents, $stack)
      = Texinfo::Common::find_innermost_accent_contents($accent);
  my $result = $self->convert_tree({'contents' => $contents});  

  my $encoded;
  if ($self->get_conf('ENABLE_ENCODING')) {
    $encoded = Texinfo::Convert::Unicode::encoded_accents($self, $result, $stack,
                                       $self->get_conf('OUTPUT_ENCODING_NAME'),
                                       $format_accents,
                                       $in_upper_case);
  }
  if (!defined($encoded)) {
    foreach my $accent_command (reverse(@$stack)) {
      $result = &$format_accents ($self, $result, $accent_command, 
                                  $in_upper_case);
    }
    return $result;
  } else {
    return $encoded;
  }
}

# This method allows to count words in elements and returns an array
# and a text already formatted.
sub sort_element_counts($$;$$)
{
  my $converter =  shift;
  my $tree = shift;
  my $use_sections = shift;
  my $count_words = shift;

  my $elements;
  if ($use_sections) {
    $elements = Texinfo::Structuring::split_by_section($tree);
  } else {
    $elements = Texinfo::Structuring::split_by_node($tree);
  }

  if (!$elements) {
    @$elements = ($tree);
  } elsif (scalar(@$elements) >= 1 
           and (!$elements->[0]->{'extra'}->{'node'}
                and !$elements->[0]->{'extra'}->{'section'})) {
    shift @$elements;
  }

  my $max_count = 0;
  my @name_counts_array;
  foreach my $element (@$elements) {
    my $name = 'UNNAMED element';
    if ($element->{'extra'} 
        and ($element->{'extra'}->{'node'} or $element->{'extra'}->{'section'})) {
      my $command = $element->{'extra'}->{'element_command'};
      if ($command->{'cmdname'} eq 'node') {
        $name = Texinfo::Convert::Texinfo::convert({'contents' 
          => $command->{'extra'}->{'nodes_manuals'}->[0]->{'node_content'}});
      } else {
        $name = "\@$command->{'cmdname'}"
          .Texinfo::Convert::Texinfo::convert($command->{'args'}->[0]);
      }
    }
    chomp($name);
    my $count;
    my $element_content = $converter->convert_tree($element);
    if ($count_words) {
      my @res = split /\W+/, $element_content;
      $count = scalar(@res);
    } else {
      my @res = split /^/, $element_content;
      $count = scalar(@res);
    }
    push @name_counts_array, [$count, $name];
    if ($count > $max_count) {
      $max_count = $count;
    }
  }

  my @sorted_name_counts_array = sort {$a->[0] <=> $b->[0]} @name_counts_array;
  @sorted_name_counts_array = reverse(@sorted_name_counts_array);

  my $max_length = length($max_count);
  my $result = '';
  foreach my $sorted_count (@sorted_name_counts_array) {
    $result .=  sprintf("%${max_length}d  $sorted_count->[1]\n", $sorted_count->[0]);
  }
  return (\@sorted_name_counts_array, $result);
}

# XML related methods and variables that may be used in different
# XML Converters.
sub xml_protect_text($$)
{
  my $self = shift;
  my $text = shift;
  if (!defined($text)) {
    cluck;
  }
  $text =~ s/&/&amp;/g;
  $text =~ s/</&lt;/g;
  $text =~ s/>/&gt;/g;
  $text =~ s/\"/&quot;/g;
  return $text;
}

# 'today' is not set here.
our %default_xml_commands_formatting; 
$default_xml_commands_formatting{'normal'} = {
               'TeX'          => 'TeX',
               'LaTeX'          => 'LaTeX',
               'bullet'       => '&bull;',
               'copyright'    => '&copy;',
               'registeredsymbol'   => '&reg;',
               'dots'         => '&hellip;',
               'enddots'      => '...',
               'equiv'        => '&equiv;',
               # in general the following is not used since error
               # appears in 'translated_commands'
               'error'        => 'error--&gt;',
               'expansion'    => '&rarr;',
               'arrow'        => '&rarr;',
               'click'        => '&rarr;',
               'minus'        => '-',
               'point'        => '&lowast;',
               'print'        => '-|',
               'result'       => '&rArr;',
               'aa'           => '&aring;',
               'AA'           => '&Aring;',
               'ae'           => '&aelig;',
               'oe'           => '&oelig;', #pertusus: also &#156;. &oelig; not in html 3.2
               'AE'           => '&AElig;',
               'OE'           => '&OElig;', #pertusus: also &#140;. &OElig; not in html 3.2
               'o'            =>  '&oslash;',
               'O'            =>  '&Oslash;',
               'ss'           => '&szlig;',
               'DH'           => '&ETH;',
               'dh'           => '&eth;',
               'TH'           => '&THORN;',
               'th'           => '&thorn;',
               'l'            => '&#322;',
               'L'            => '&#321;',
               'exclamdown'   => '&iexcl;',
               'questiondown' => '&iquest;',
               'pounds'       => '&pound;',
               'ordf'         => '&ordf;',
               'ordm'         => '&ordm;',
               'comma'        => ',',
               'atchar'       => '@',
               'lbracechar'   => '{',
               'rbracechar'   => '}',
               'backslashchar' => '\\',
               'hashchar' => '#',
               'euro'         => '&euro;',
               'geq'          => '&ge;',
               'leq'          => '&le;',
               'tie'          => '&nbsp;',
               'textdegree'          => '&deg;',
               'quotedblleft'          => '&ldquo;',
               'quotedblright'          => '&rdquo;',
               'quoteleft'          => '&lsquo;',
               'quoteright'          => '&rsquo;',
               'quotedblbase'          => '&bdquo;',
               'quotesinglbase'          => '&sbquo;',
               'guillemetleft'          => '&laquo;',
               'guillemetright'          => '&raquo;',
               'guillemotleft'          => '&laquo;',
               'guillemotright'          => '&raquo;',
               'guilsinglleft'          => '&lsaquo;',
               'guilsinglright'          => '&rsaquo;',
};

foreach my $no_brace_command (keys(%Texinfo::Common::no_brace_commands)) {
  $default_xml_commands_formatting{'normal'}->{$no_brace_command}
    = $Texinfo::Common::no_brace_commands{$no_brace_command};
}

sub xml_comment($$)
{
  my $self = shift;
  my $text = shift;
  chomp $text;
  $text =~ s/--+/-/go;
  return '<!--' . $text . ' -->' . "\n";
}

our %xml_accent_entities = (
          '"',  'uml',
          '~',  'tilde',
          '^',  'circ',
          '`',  'grave',
          "'", 'acute',
          ",", 'cedil',
          'ringaccent', 'ring',
          'ogonek', 'ogon',
         );

my %xml_accent_text_with_entities = (
      'ringaccent' => 'aA',
      "'"          => 'aeiouyAEIOUY',
      ','          => 'cC',
      '^'          => 'aeiouAEIOU',
      '`'          => 'aeiouAEIOU',
      '~'          => 'nNaoAO',
      '"'          => 'aeiouyAEIOU',
# according to http://www2.lib.virginia.edu/small/vhp/download/ISO.txt
# however this doesn't seems to work in firefox
#      'ogonek'     => 'aeiuAEIU',
);


sub xml_accent($$$;$$)
{
  my $self = shift;
  my $text = shift;
  my $command = shift;
  my $in_upper_case = shift;
  my $use_numeric_entities = shift;
  my $accent = $command->{'cmdname'};
  
  if ($in_upper_case and $text =~ /^\w$/) {
    $text = uc ($text);
  }
 
  return "&${text}$xml_accent_entities{$accent};" 
    if (defined($xml_accent_entities{$accent}) 
        and defined($xml_accent_text_with_entities{$accent}) 
        and ($text =~ /^[$xml_accent_text_with_entities{$accent}]$/));
  if ($use_numeric_entities
      and exists($Texinfo::Convert::Unicode::unicode_accented_letters{$accent}) 
      and exists($Texinfo::Convert::Unicode::unicode_accented_letters{$accent}->{$text})) {
    return '&#' . 
      hex($Texinfo::Convert::Unicode::unicode_accented_letters{$accent}->{$text}). ';';
  }
  return $text . '&lt;' if ($accent eq 'v');
  # FIXME it is not possible to call xml_protect_text since what is in $text
  # may already be xml.  But this means that each time ascii_accent changes
  # it should be changed here too.
  return Texinfo::Convert::Text::ascii_accent($text, $command);
}

sub _xml_accent_numeric_entities($$;$)
{
  my $self = shift;
  my $text = shift;
  my $command = shift;
  my $in_upper_case = shift;
  return $self->xml_accent($text, $command, $in_upper_case, 1);
}

sub xml_accents($$;$)
{
  my $self = shift;
  my $accent = shift;
  my $in_upper_case = shift;

  my $format_accents;
  if ($self->get_conf('USE_NUMERIC_ENTITY')) {
    $format_accents = \&_xml_accent_numeric_entities;
  } else {
    $format_accents = \&xml_accent;
  }
  
  return $self->convert_accents($accent, $format_accents, $in_upper_case);
}

1;

__END__

=head1 NAME

Texinfo::Convert::Converter - Parent class for Texinfo tree converters

=head1 SYNOPSIS

  package Texinfo::Convert::MyConverter;

  use Texinfo::Convert::Converter;
  @ISA = qw(Texinfo::Convert::Converter);

  sub converter_defaults ($$) {
    return %myconverter_defaults;
  }
  sub converter_initialize($) {
    my $self = shift;
    $self->{'document_context'} = [{}];
  }
  sub converter_global_commands($) {
    return ('documentlanguage', documentencoding', 'paragraphindent');
  }

  sub convert($$) {
    ...
  }
  sub convert_tree($$) {
    ...
  }
  sub output ($$) {
    ...
  }

  # end of Texinfo::Convert::MyConverter

  my $converter = Texinfo::Convert::MyConverter->converter(
                                               {'parser' => $parser});
  $converter->output($texinfo_tree);

=head1 DESCRIPTION

Texinfo::Convert::Converter is a super class that can be used to
simplify converters initialization.  The class also provide some 
useful methods.

In turn, the converter should define some methods.  Three are 
optional, C<converter_defaults>, C<converter_initialize> and 
C<converter_global_commands> and used for initialization, to 
give C<Texinfo::Convert::Converter> some informations.

The C<convert_tree> method is more or less mandatory and should 
convert portions of Texinfo tree.  The C<output> and C<convert> 
are not required, but customarily used by converters as entry 
points for conversion to a file with headers and so on, or 
conversion of a whole Texinfo tree.

Existing backends may be used as examples that implement those
methods.  C<Texinfo::Convert::Texinfo> together with 
C<Texinfo::Convert::PlainTexinfo>, as well as 
C<Texinfo::Convert::TextContent> are trivial examples.  
C<Texinfo::Convert::Text> is less trivial, although still simplistic, 
while C<Texinfo::Convert::DocBook> is a real converter
that is also not too complex.  

L<Texinfo::Common>, L<Texinfo::Convert::Unicode> 
and L<Texinfo::Report> document modules or additional function 
that may be useful for backends, while the parsed Texinfo tree is 
described in L<Texinfo::Parser>.


=head1 METHODS

=head2 Initialization

A module subclassing C<Texinfo::Convert::Converter> is created by calling
the C<converter> method that should be inherited from 
C<Texinfo::Convert::Converter>.

=over

=item $converter = MyConverter->converter($options)

The I<$options> hash reference holds options for the converter.  In
this option hash reference a L<parser object|Texinfo::Parser> 
may be associated with the I<parser> key.  The other options 
should be configuration options described in the Texinfo manual.
Those options, when appropriate, override the document content.

The C<converter> function returns a converter object (a blessed hash 
reference) after checking the options and performing some initializations,
especially when a parser is given among the options.  The converter is
also initialized as a L<Texinfo::Report>.

=back

To help with these initializations, the modules can define three methods:

=over

=item %defaults = $converter->converter_defaults($options)

The converter can provide a defaults hash for configurations options.
The I<$options> hash reference holds options for the converter.

=item @global_commands = $converter->converter_global_commands()

The list returned is the list of Texinfo global commands (like 
C<@paragraphindent>, C<@documentlanguage>...) that are relevant for the
converter.

=item converter_initialize

This method is called at the end of the converter initialization.

=back

=head2 Helper methods

C<Texinfo::Convert::Converter> provides methods
that may be useful for every converter:

=over

=item $converter->get_conf($option_string)

Returns the value of the Texinfo configuration option I<$option_string>.

=item $converter->set_conf($option_string, $value)

Set the Texinfo configuration option I<$option_string> to I<$value> if
not set as a converter option.

=item $converter->force_conf($option_string, $value)

Set the Texinfo configuration option I<$option_string> to I<$value>.
This should rarely be used, but the purpose of this method is to be able
to revert a configuration that is always wrong for a given output
format, like the splitting for example.

=item $result = $converter->convert_document_sections($root, $file_handler)

This method splits the I<$root> Texinfo tree at sections and 
calls C<convert_tree> on the elements.  If the optional I<$file_handler>
is given in argument, the result are output in I<$file_handler>, otherwise
the resulting string is returned.

=item $result = $converter->convert_accents($accent_command, \&format_accents, $in_upper_case)

I<$accent_command> is an accent command, which may have other accent
commands nested.  The function returns the accents formatted either
as encoded letters, or formatted using I<\&format_accents>.
If I<$in_upper_case> is set, the result should be upper cased.  

=back

Other C<Texinfo::Convert::Converter> methods target conversion to XML:

=over

=item $protected_text = $converter->xml_protect_text($text)

Protect special XML characters (&, E<lt>, E<gt>, ") of I<$text>.

=item $comment = $converter->xml_comment($text)

Returns an XML comment for I<$text>.

=item $result = xml_accent($text, $accent_command, $in_upper_case, $use_numeric_entities)

I<$text> is the text appearing within an accent command.  I<$accent_command>
should be a Texinfo tree element corresponding to an accent command taking
an argument.  I<$in_upper_case> is optional, and, if set, the text is put
in upper case.  The function returns the accented letter as XML entity 
if possible.  I<$use_numeric_entities> is also optional, and, if set, and
there is no XML entity, the numerical entity corresponding to unicode 
points is preferred to an ascii transliteration.

=item $result = $converter->xml_accents($accent_command, $in_upper_case)

I<$accent_command> is an accent command, which may have other accent
commands nested.  If I<$in_upper_case> is set, the result should be 
upper cased.  The function returns the accents formatted as XML.

=back

=head1 SEE ALSO

L<Texinfo::Common>, L<Texinfo::Convert::Unicode>, L<Texinfo::Report> 
and L<Texinfo::Parser>.  

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011, 2012, 2013 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at 
your option) any later version.

=cut
