# Info.pm: output tree as Info.
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

package Texinfo::Convert::Info;

use 5.00405;
use strict;

use Texinfo::Convert::Plaintext;
use Texinfo::Convert::Text;

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Texinfo::Convert::Plaintext);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Texinfo::Convert::Info ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
  convert
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '5.1.90';

my $STDIN_DOCU_NAME = 'stdin';

my %defaults = Texinfo::Convert::Plaintext::converter_defaults(undef, undef);
$defaults{'SHOW_MENU'} = 1;

sub converter_defaults($$)
{
  return %defaults;
}

sub output($)
{
  my $self = shift;
  my $root = shift;

  my $result;

  $self->_set_outfile();
  $self->{'input_basename'} = $STDIN_DOCU_NAME if ($self->{'input_basename'} eq '-');

  # no splitting when writing to the null device or to stdout
  if ($Texinfo::Common::null_device_file{$self->{'output_file'}} 
       or $self->{'output_file'} eq '-') {
    $self->force_conf('SPLIT_SIZE', undef);
  }

  push @{$self->{'count_context'}}, {'lines' => 0, 'bytes' => 0,
                                     'locations' => []};
  my $header = $self->_info_header();
  # header + text between setfilename and first node
  my $complete_header = $header;

  pop @{$self->{'count_context'}};
  return undef unless $self->_create_destination_directory();

  my $header_bytes = $self->count_bytes($header);
  my $complete_header_bytes = $header_bytes;
  my $elements = Texinfo::Structuring::split_by_node($root);

  my $fh;
  if (! $self->{'output_file'} eq '') {
    if ($self->get_conf('VERBOSE')) {
      print STDERR "Output file $self->{'output_file'}\n";
    }
    $fh = $self->Texinfo::Common::open_out($self->{'output_file'});
    if (!$fh) {
      $self->document_error(sprintf($self->__("could not open %s for writing: %s"),
                                    $self->{'output_file'}, $!));
      return undef;
    }
  }
  print STDERR "DOCUMENT\n" if ($self->get_conf('DEBUG'));
  my $out_file_nr = 0;
  my @indirect_files;
  if (!defined($elements) or $elements->[0]->{'extra'}->{'no_node'}) {
    $self->file_line_warn($self->__("document without nodes"), 
                          $self->{'info'}->{'input_file_name'});
    my $output = $header.$self->_convert($root);
    $self->_count_context_bug_message('no element ');

    my $footnotes = $self->_footnotes();
    $self->_count_context_bug_message('no element footnotes ');

    $output .= $footnotes;
    if ($fh) {
      print $fh $output;
    } else {
      $result = $output;
    }
  } else {
    unless ($self->{'structuring'} and $self->{'structuring'}->{'top_node'}
     and $self->{'structuring'}->{'top_node'}->{'extra'}->{'normalized'} eq 'Top') {
      $self->file_line_warn($self->__("document without Top node"),
                            $self->{'info'}->{'input_file_name'});
    }
    $out_file_nr = 1;
    my $first_node = 0;
    $self->{'count_context'}->[-1]->{'bytes'} += $header_bytes;
    my @nodes = @$elements;
    while (@nodes) {
      my $node = shift @nodes;
      my $node_text = $self->_convert_node($node);
      if (!$first_node) {
        $first_node = 1;
        if (defined($self->{'text_before_first_node'})) {
          $complete_header .= $self->{'text_before_first_node'};
          $complete_header_bytes += $self->count_bytes($self->{'text_before_first_node'});
        }
        # for the first node, header is prepended, not complete_header
        # as 'text_before_first_node' is already part of the node
        # text
        $node_text = $header . $node_text;
      }
      if ($fh) {
        print $fh $node_text;
      } else {
        $result .= $node_text;
      }
      if (defined($self->get_conf('SPLIT_SIZE')) 
          and $self->{'count_context'}->[-1]->{'bytes'} > 
                  $out_file_nr * $self->get_conf('SPLIT_SIZE') 
          and @nodes and $fh) {
        my $close_error;
        if (!close ($fh)) {
          $close_error = $!;
        }
        if ($out_file_nr == 1) {
          $self->register_close_file($self->{'output_file'});
          if (defined($close_error)) {
            $self->document_error(sprintf($self->__("error on closing %s: %s"),
                                  $self->{'output_file'}, $close_error));
            return undef;
          }
          if ($self->get_conf('VERBOSE')) {
            print STDERR "Renaming first output file as ".
                  $self->{'output_file'}.'-'.$out_file_nr."\n";
          }
          unless (rename($self->{'output_file'}, 
                         $self->{'output_file'}.'-'.$out_file_nr)) {
            $self->document_error(sprintf($self->__("rename %s failed: %s"),
                                         $self->{'output_file'}, $!));
            return undef;
          }
          # remove the main file from opened files since it was renamed
          # and add the file with a number.
          @{$self->{'opened_files'}} = grep {$_ ne $self->{'output_file'}}
               @{$self->{'opened_files'}};
          push @{$self->{'opened_files'}}, 
                   $self->{'output_file'}.'-'.$out_file_nr;
          push @indirect_files, [$self->{'output_filename'}.'-'.$out_file_nr,
                                 $complete_header_bytes];
          #print STDERR join(' --> ', @{$indirect_files[-1]}) ."\n";
        } else {
          $self->register_close_file($self->{'output_file'}.'-'.$out_file_nr);
          if (defined($close_error)) {
            $self->document_error(sprintf($self->__("error on closing %s: %s"),
                                  $self->{'output_file'}.'-'.$out_file_nr, 
                                  $close_error));
            return undef;
          }
        }
        $out_file_nr++;
        if ($self->get_conf('VERBOSE')) {
          print STDERR "New output file ".
                $self->{'output_file'}.'-'.$out_file_nr."\n";
        }
        $fh = $self->Texinfo::Common::open_out (
                               $self->{'output_file'}.'-'.$out_file_nr); 
        if (!$fh) {
           $self->document_error(sprintf(
                  $self->__("could not open %s for writing: %s"),
                  $self->{'output_file'}.'-'.$out_file_nr, $!));
           return undef;
        }
        print $fh $complete_header;
        $self->{'count_context'}->[-1]->{'bytes'} += $complete_header_bytes;
        push @indirect_files, [$self->{'output_filename'}.'-'.$out_file_nr,
                               $self->{'count_context'}->[-1]->{'bytes'}];
        #print STDERR join(' --> ', @{$indirect_files[-1]}) ."\n";
      }
    }
  }
  my $tag_text = '';
  if ($out_file_nr > 1) {
    $self->register_close_file($self->{'output_file'}.'-'.$out_file_nr);
    if (!close ($fh)) {
      $self->document_error(sprintf($self->__("error on closing %s: %s"),
                            $self->{'output_file'}.'-'.$out_file_nr, $!));
      return undef;
    }
    if ($self->get_conf('VERBOSE')) {
      print STDERR "Outputing the split manual file $self->{'output_file'}\n";
    }
    $fh = $self->Texinfo::Common::open_out($self->{'output_file'});
    if (!$fh) {
      $self->document_error(sprintf(
            $self->__("could not open %s for writing: %s"),
            $self->{'output_file'}, $!));
      return undef;
    }
    $tag_text = $complete_header;
    $tag_text .= "\x{1F}\nIndirect:";
    foreach my $indirect (@indirect_files) {
      $tag_text .= "\n$indirect->[0]: $indirect->[1]";
    }
  }

  $tag_text .= "\n\x{1F}\nTag Table:\n";
  if ($out_file_nr > 1) {
    $tag_text .=  "(Indirect)\n";
  }
  # This may happen for anchors in @insertcopying
  my %seen_anchors;
  foreach my $label (@{$self->{'count_context'}->[-1]->{'locations'}}) {
    next unless ($label->{'root'} and $label->{'root'}->{'extra'} 
                  and defined($label->{'root'}->{'extra'}->{'normalized'}));
    my $prefix;
    if ($label->{'root'}->{'cmdname'} eq 'node') {
      $prefix = 'Node';
    } else {
      if ($seen_anchors{$label->{'root'}->{'extra'}->{'normalized'}}) {
        $self->line_error(sprintf($self->__("\@%s output more than once: %s"),
                       $label->{'root'}->{'cmdname'},
                 Texinfo::Convert::Texinfo::convert({'contents' =>
                      $label->{'root'}->{'extra'}->{'node_content'}})),
                      $label->{'root'}->{'line_nr'});
        next;
      } else {
        $seen_anchors{$label->{'root'}->{'extra'}->{'normalized'}} = $label;
      }
      $prefix = 'Ref';
    }
    my ($label_text, $byte_count) = $self->_node_line($label->{'root'});
    $tag_text .=  "$prefix: $label_text\x{7F}$label->{'bytes'}\n";
  }
  $tag_text .=  "\x{1F}\nEnd Tag Table\n";
  my $coding = $self->get_conf('OUTPUT_ENCODING_NAME');
  if ($coding) {
    $tag_text .= "\n\x{1F}\nLocal Variables:\ncoding: $coding\nEnd:\n";
  }
  if ($fh) {
    print $fh $tag_text;
    # NOTE it should be possible to close STDOUT.  However this leads to
    # 'Filehandle STDOUT reopened as FH only for input' if there are files
    # reopened after closing STDOUT.  So closing STDOUT is handled by the
    # caller.
    unless ($self->{'output_file'} eq '-') {
      $self->register_close_file($self->{'output_file'});
      if (!close ($fh)) {
        $self->document_error(sprintf($self->__("error on closing %s: %s"),
                              $self->{'output_file'}, $!));
      }
    }
  } else {
    $result .= $tag_text;
  }
  return $result;
}

sub _info_header($)
{
  my $self = shift;

  $self->_set_global_multiple_commands();
  my $paragraph = Texinfo::Convert::Paragraph->new();
  my $result = $paragraph->add_text("This is ");
  # This ensures that spaces in file are kept.
  $result .= $paragraph->add_next($self->{'output_filename'});
  my $program = $self->get_conf('PROGRAM');
  my $version = $self->get_conf('PACKAGE_VERSION');
  if (defined($program) and $program ne '') {
    $result .= $paragraph->add_text(", produced by $program version $version from ");
  } else {
    $result .= $paragraph->add_text(", produced from ");
  }
  $result .= $paragraph->add_next($self->{'input_basename'});
  $result .= $paragraph->add_text('.');
  $result .= $paragraph->end();
  $result .= "\n";
  $self->{'empty_lines_count'} = 1;

  if ($self->{'extra'} and $self->{'extra'}->{'copying'}) {
    print STDERR "COPYING HEADER\n" if ($self->get_conf('DEBUG'));
    $self->{'in_copying_header'} = 1;
    my $copying = $self->_convert({'contents' => 
          $self->{'extra'}->{'copying'}->{'contents'}});
    $result .= $copying;
    $result .= $self->_footnotes();
    delete $self->{'in_copying_header'};
  }
  if ($self->{'info'}->{'dircategory_direntry'}) {
    $self->{'ignored_commands'}->{'direntry'} = 0;
    foreach my $command (@{$self->{'info'}->{'dircategory_direntry'}}) {
      if ($command->{'cmdname'} eq 'dircategory') {
        if ($command->{'extra'} 
            and defined($command->{'extra'}->{'misc_content'})) {
          my $dircategory = "INFO-DIR-SECTION ".$self->convert_line(
             {'contents' => $command->{'extra'}->{'misc_content'}});
          $result .= $self->ensure_end_of_line($dircategory);
        }
        $self->{'empty_lines_count'} = 0;
      } elsif ($command->{'cmdname'} eq 'direntry') {
        $result .= "START-INFO-DIR-ENTRY\n";
        my $direntry = $self->_convert($command);
        $result .= $direntry;
        $result .= "END-INFO-DIR-ENTRY\n\n";
        $self->{'empty_lines_count'} = 1;
      }
    }
    $self->{'ignored_commands'}->{'direntry'} = 1;
  }
  $self->_unset_global_multiple_commands();
  return $result;
}

sub _contents($$$)
{
  my $self = shift;
  my $section_root = shift;
  my $contents_or_shortcontents = shift;

  return ('', 0);
}

sub _printindex($$)
{
  my $self = shift;
  my $printindex = shift;
  return $self->_printindex_formatted($printindex, 1);
}

sub _error_outside_of_any_node($$)
{
  my $self = shift;
  my $root = shift;
  if (!$self->{'node'}) {
    $self->line_warn(sprintf($self->__("\@%s outside of any node"),
                     $root->{'cmdname'}), $root->{'line_nr'});
  }
}

my @directions = ('Next', 'Prev', 'Up');
sub _node($$)
{
  my $self = shift;
  my $node = shift;
  
  my $result = '';
  return '' if (!defined($node->{'extra'}->{'normalized'}));
  if (!$self->{'empty_lines_count'}) {
    $result .= "\n";
    $self->_add_text_count("\n");
    # if in the first node, complete the 'text_before_first_node' too.
    if (!$self->{'first_node_done'}) {
      $self->{'text_before_first_node'} .= "\n";
    }
  }
  if (!$self->{'first_node_done'}) {
    $self->{'first_node_done'} = 1;
  }

  # May happen when only converting a fragment
  my $output_filename = $self->{'output_filename'};
  if (defined($self->{'output_filename'})) {
    $output_filename = $self->{'output_filename'};
  } else {
    $output_filename = '';
  }

  $self->_add_location($node);
  my $node_begin = "\x{1F}\nFile: $output_filename,  Node: ";
  $result .= $node_begin;
  $self->_add_text_count($node_begin);
  my ($node_text, $byte_count) = $self->_node_line($node);
  if ($node_text =~ /,/ and $self->get_conf('INFO_SPECIAL_CHARS_WARNING')) {
    $self->line_warn(sprintf($self->__(
               "\@node name should not contain `,': %s"), $node_text),
                             $node->{'line_nr'});
  }
  $self->{'count_context'}->[-1]->{'bytes'} += $byte_count;
  $result .= $node_text;
  foreach my $direction(@directions) {
    if ($node->{'node_'.lc($direction)}) {
      my $node_direction = $node->{'node_'.lc($direction)};
      my $text = ",  $direction: ";
      $self->_add_text_count($text);
      $result .= $text;
      if ($node_direction->{'extra'}->{'manual_content'}) {
        $result .= $self->convert_line({'type' => '_code',
                          'contents' => [{'text' => '('},
                             @{$node_direction->{'extra'}->{'manual_content'}},
                                          {'text' => ')'}]});
      }
      if ($node_direction->{'extra'}->{'node_content'}) {
        my ($node_text, $byte_count) = $self->_node_line($node_direction);
        $self->{'count_context'}->[-1]->{'bytes'} += $byte_count;
        $result .= $node_text;
      }
    }
  }
  $result .="\n\n";
  $self->_add_text_count("\n\n");
  $self->{'count_context'}->[-1]->{'lines'} = 3;
  $self->{'empty_lines_count'} = 1;

  return $result;
}

my @image_files_extensions = ('.png', '.jpg');
sub _image($$)
{
  my $self = shift;
  my $root = shift;
  my @extensions = @image_files_extensions;

  my $lines_count = 0;

  if (defined($root->{'extra'}->{'brace_command_contents'}->[0])) {
    my $basefile = Texinfo::Convert::Text::convert(
      {'contents' => $root->{'extra'}->{'brace_command_contents'}->[0]},
      {'code' => 1, Texinfo::Common::_convert_text_options($self)});
    if (defined($root->{'extra'}->{'brace_command_contents'}->[4])) {
      my $extension = Texinfo::Convert::Text::convert(
        {'contents' => $root->{'extra'}->{'brace_command_contents'}->[4]},
        {'code' => 1, Texinfo::Common::_convert_text_options($self)});
      unshift @extensions, ".$extension";
      unshift @extensions, "$extension";
    }
    my $image_file;
    foreach my $extension (@extensions) {
      if ($self->Texinfo::Common::locate_include_file ($basefile.$extension)) {
        # use the basename and not the file found.  It is agreed that it is
        # better, since in any case the files are moved.
        $image_file = $basefile.$extension;
        last; 
      }
    }
    my ($text, $width) = $self->_image_text($root, $basefile);
    my $alt;
    if (defined($root->{'extra'}->{'brace_command_contents'}->[3])) {
     $alt = Texinfo::Convert::Text::convert(
       {'contents' => $root->{'extra'}->{'brace_command_contents'}->[3]},
       {Texinfo::Common::_convert_text_options($self)});
    }

    my $result;

    if (defined($image_file) or (defined($text) and defined($alt))) {
      $image_file =~ s/\\/\\\\/g;
      $image_file =~ s/\"/\\\"/g;
      $result = "\x{00}\x{08}[image src=\"$image_file\"";

      if (defined($root->{'extra'}->{'brace_command_contents'}->[3])) {
        $alt =~ s/\\/\\\\/g;
        $alt =~ s/\"/\\\"/g;
        $result .= " alt=\"$alt\"";
      }
      if (defined($text)) {
        $text =~ s/\\/\\\\/g;
        $text =~ s/\"/\\\"/g;
        $result .= " text=\"$text\"";
      }
      $result .= "\x{00}\x{08}]";
      if ($self->{'formatters'}->[-1]->{'_top_formatter'}) {
        $result .= "\n";
      }
      my $image_lines_count = ($result =~ tr/\n/\n/) +1;
      $self->_add_image($root, $image_lines_count, $width, 1);
    } else {
      $result = $self->_image_formatted_text($root, $basefile, $text);
      $lines_count = ($result =~ tr/\n/\n/);
      $self->_add_image($root, $lines_count+1, $width);
    }
    return ($result, $lines_count);
  }
  return ('', 0);
}

1;

__END__
# Automatically generated from maintain/template.pod

=head1 NAME

Texinfo::Convert::Info - Convert Texinfo tree to Info

=head1 SYNOPSIS

  my $converter 
    = Texinfo::Convert::Info->converter({'parser' => $parser});

  $converter->output($tree);

=head1 DESCRIPTION

Texinfo::Convert::Info converts a Texinfo tree to Info.

=head1 METHODS

=over

=item $converter = Texinfo::Convert::Info->converter($options)

Initialize an Info converter.  

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
