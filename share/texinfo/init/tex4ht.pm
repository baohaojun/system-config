# -*-perl-*-

#+##############################################################################
#
# tex4ht.pm: use tex4ht to convert tex to html
#
# Copyright 2005, 2007, 2009, 2011, 2012, 2013 Free Software Foundation, Inc.
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
# Originally written by Patrice Dumas.
#
#-##############################################################################
# To customize the command and the options, you could set
# $Texinfo::TeX4HT::STYLE_MATH to latex/tex
# $Texinfo::TeX4HT::STYLE_TEX to latex/texi
# and/or change
# $Texinfo::TeX4HT::tex4ht_command_math 
#    and $Texinfo::TeX4HT::tex4ht_options_math
# $Texinfo::TeX4HT::tex4ht_command_tex 
#    and $Texinfo::TeX4HT::tex4ht_options_tex

use strict;

my $global_cmds = get_conf('GLOBAL_COMMANDS');
if (!defined($global_cmds)) {
  set_from_init_file('GLOBAL_COMMANDS', []);
  $global_cmds = get_conf('GLOBAL_COMMANDS');
}
push @$global_cmds, ('math', 'tex');

texinfo_register_handler('structure', \&tex4ht_prepare);
texinfo_register_handler('init', \&tex4ht_convert);
texinfo_register_handler('finish', \&tex4ht_finish);

texinfo_register_command_formatting('math', \&tex4ht_do_tex);
texinfo_register_command_formatting('tex', \&tex4ht_do_tex);

{
use Cwd;

package Texinfo::TeX4HT;

use vars qw(
$STYLE_MATH
$STYLE_TEX
$tex4ht_command_math
$tex4ht_command_tex
$tex4ht_options_math
$tex4ht_options_tex
);

$STYLE_MATH = 'texi' if (!defined($STYLE_MATH));
$STYLE_TEX = 'tex' if (!defined($STYLE_TEX));

if (!defined($tex4ht_command_math)) {
  $tex4ht_command_math = 'httexi';
  $tex4ht_command_math = 'htlatex' if ($STYLE_MATH eq 'latex');
  $tex4ht_command_math = 'httex' if ($STYLE_MATH eq 'tex');
}
if (!defined($tex4ht_command_tex)) {
  $tex4ht_command_tex = 'httex';
  $tex4ht_command_tex = 'htlatex' if ($STYLE_TEX eq 'latex');
  $tex4ht_command_tex = 'httexi' if ($STYLE_TEX eq 'texi');
}

}

my %commands = ();

my $tex4ht_initial_dir;
my $tex4ht_out_dir;

sub tex4ht_prepare($)
{
  # set file names
  my $self = shift;

  return 1 if (defined($self->get_conf('OUTFILE'))
        and $Texinfo::Common::null_device_file{$self->get_conf('OUTFILE')});

  $tex4ht_initial_dir = Cwd::abs_path;
  $tex4ht_out_dir = $self->{'destination_directory'};
  $tex4ht_out_dir = File::Spec->curdir()
    if (!defined($tex4ht_out_dir) or $tex4ht_out_dir =~ /^\s*$/);

  my $document_name = $self->{'document_name'};
  my $tex4ht_basename = "${document_name}_tex4ht";

  # this initialization doesn't seems to be needed, but it is cleaner anyway
  %commands = ();
  $commands{'math'}->{'style'} = $Texinfo::TeX4HT::STYLE_MATH;
  $commands{'tex'}->{'style'} = $Texinfo::TeX4HT::STYLE_TEX;
  $commands{'math'}->{'exec'} = $Texinfo::TeX4HT::tex4ht_command_math;
  $commands{'tex'}->{'exec'} = $Texinfo::TeX4HT::tex4ht_command_tex;
  foreach my $command ('math', 'tex') {
    my $style = $commands{$command}->{'style'};
    $commands{$command}->{'basename'} = $tex4ht_basename . "_$command";
    my $suffix = '.tex';
    $suffix = '.texi' if ($style eq 'texi');
    $commands{$command}->{'basefile'} = $commands{$command}->{'basename'} . $suffix;
    $commands{$command}->{'html_file'} = $commands{$command}->{'basename'} . '.html';
    $commands{$command}->{'rfile'} = File::Spec->catfile($tex4ht_out_dir, 
                                          $commands{$command}->{'basefile'});
    my $rfile = $commands{$command}->{'rfile'};
    $commands{$command}->{'counter'} = 0;
    $commands{$command}->{'output_counter'} = 0;

    if ($self->{'extra'}->{$command}) {
      local *TEX4HT_TEXFILE;
      unless (open (*TEX4HT_TEXFILE, ">$rfile")) {
        $self->document_warn(sprintf($self->__("tex4ht.pm: could not open %s: %s"), 
                                      $rfile, $!));
        return 1;
      }
      $commands{$command}->{'handle'} = *TEX4HT_TEXFILE;

      my $style = $commands{$command}->{'style'};
      my $fh = $commands{$command}->{'handle'};
      my $comment = '@c';
      $comment = '%' if ($style ne 'texi');
      $comment .= " Automatically generated\n";
      if ($style eq 'texi') {
        print $fh "\\input texinfo
\@setfilename $commands{$command}->{'basename'}.info\n";
        print $fh "$comment";
      } else {
        print $fh "$comment";
        if ($style eq 'latex') {
          print $fh "\\documentstyle{article}\n\\begin{document}\n";
        } elsif ($style eq 'tex') {
          print $fh "\\csname tex4ht\\endcsname\n";
        }
      }
      foreach my $root (@{$self->{'extra'}->{$command}}) {
        $commands{$command}->{'counter'}++;
        my $counter = $commands{$command}->{'counter'};
        my $tree;
        if ($command eq 'math') {
          $tree = $root->{'args'}->[0];
        } else {
          $tree = {'contents' => [@{$root->{'contents'}}]};
          if ($tree->{'contents'}->[0] 
              and $tree->{'contents'}->[0]->{'type'}
              and $tree->{'contents'}->[0]->{'type'} eq 'empty_line_after_command') {
            shift @{$tree->{'contents'}};
          }
          if ($tree->{'contents'}->[-1]->{'cmdname'} 
              and $tree->{'contents'}->[-1]->{'cmdname'} eq 'end') {
            pop @{$tree->{'contents'}};
          }
        }
        my $text = Texinfo::Convert::Texinfo::convert($tree);
        $commands{$command}->{'commands'}->[$counter-1] = $root;

        # write to tex file
        my ($before_comment_open, $after_comment_open, $before_comment_close,
            $after_comment_close);

        if ($style eq 'texi') {
          $before_comment_open = "\@verbatim\n\n";
          $after_comment_open = "\n\@end verbatim\n";
          $before_comment_close = "\@verbatim\n";
          $after_comment_close = "\n\n\@end verbatim\n";
        } else {
          $before_comment_open = "\\HCode{\\Hnewline \\Hnewline ";
          $after_comment_open = "\\Hnewline}\n";
          $before_comment_close = "\\HCode{\\Hnewline ";
          $after_comment_close = "\\Hnewline \\Hnewline}\n";
        }
  
        my $begin_comment = "<!-- tex4ht_begin $commands{$command}->{'basename'} $command $counter -->";
        print $fh "$before_comment_open$begin_comment$after_comment_open";
        if ($command eq 'tex') {
          print $fh $text;
        } elsif ($command eq 'math') {
          if ($style eq 'texi') {
            print $fh '@math{' . $text . "}\n";
          } else {
            print $fh "\\IgnorePar \$" . $text . "\$";
          }
        }
        my $end_comment = "<!-- tex4ht_end $commands{$command}->{'basename'} $command $counter -->";
        print $fh "$before_comment_close$end_comment$after_comment_close";
      }
      # finish the tex file
      if ($style eq 'latex') {
        print $fh "\\end{document}\n";
      } elsif ($style eq 'tex') {
        print $fh "\n\\bye\n";
      } else {
        print $fh "\n\@bye\n";
      }
      close ($fh);
      # this has to be done during the 'process' phase, in 'output' it is 
      # too late.
      push @{$self->{'css_import_lines'}}, 
         "\@import \"$commands{$command}->{'basename'}.css\";\n";
    }
  }
  return 1;
}

sub tex4ht_convert($)
{
  my $self = shift;
  unless (chdir $tex4ht_out_dir) {
    $self->document_warn(sprintf($self->__("tex4ht.pm: chdir %s failed: %s"),
                         $tex4ht_out_dir, $!));
    return 0;
  }
  print STDERR "cwd($tex4ht_out_dir): " . Cwd::cwd() ."\n" 
    if ($self->get_conf('VERBOSE'));

  my $errors = 0;
  foreach my $command (keys(%commands)) {
    $errors += tex4ht_process_command($self, $command);
  }
  unless (chdir $tex4ht_initial_dir) {
    $self->document_warn(sprintf($self->__(
          "tex4ht.pm: unable to return to initial directory: %s"), $!));
    return 0;
  }
  return 1;
}

sub tex4ht_process_command($$) {
  my $self = shift;
  my $command = shift;
  
  return 0 unless ($commands{$command}->{'counter'});

  $self->document_warn(sprintf($self->__("tex4ht.pm: output file missing: %s"),
                               $commands{$command}->{'basefile'}))
    unless (-f $commands{$command}->{'basefile'});
  my $style = $commands{$command}->{'style'};
  # now run tex4ht
  my $options = '';
  if ($style eq 'math' and defined($Texinfo::TeX4HT::tex4ht_options_math)) {
    $options = $Texinfo::TeX4HT::tex4ht_options_math 
  } elsif ($style eq 'tex' and defined($Texinfo::TeX4HT::tex4ht_options_tex)) {
    $options = $Texinfo::TeX4HT::tex4ht_options_tex;
  }

  my $cmd = "$commands{$command}->{'exec'} $commands{$command}->{'basefile'} $options";
  print STDERR "tex4ht command: $cmd\n" if ($self->get_conf('VERBOSE'));
  if (system($cmd)) {
    $self->document_warn(sprintf($self->__(
                         "tex4ht.pm: command failed: %s"), $cmd));
    return 1;
  }

  # extract the html from the file created by tex4ht
  my $html_basefile = $commands{$command}->{'html_file'};
  unless (open (TEX4HT_HTMLFILE, $html_basefile)) {
    $self->document_warn(sprintf($self->__("tex4ht.pm: could not open %s: %s"), 
                                  $html_basefile, $!));
    return 1;
  }
  my $got_count = 0;
  my $line;
  while ($line = <TEX4HT_HTMLFILE>) {
    #print STDERR "$html_basefile: while $line";
    if ($line =~ /!-- tex4ht_begin $commands{$command}->{'basename'} (\w+) (\d+) --/) {
      my $command = $1;
      my $count = $2;
      my $text = '';
      my $end_found = 0;
      while ($line = <TEX4HT_HTMLFILE>) {
        #print STDERR "while search $command $count $line";
        if ($line =~ /!-- tex4ht_end $commands{$command}->{'basename'} $command $count --/) {
          $got_count++;
          chomp($text) if ($command eq 'math');
          $commands{$command}->{'results'}->{$commands{$command}->{'commands'}->[$count-1]} = $text;
          $end_found = 1;
          last;
        } else {
          $text .= $line;
        }
      }
      unless ($end_found) {
        $self->document_warn(sprintf($self->__(
                               "tex4ht.pm: end of \@%s item %d not found"), 
                                      $command, $count));
      }
    }
  }
  if ($got_count != $commands{$command}->{'counter'}) {
    $self->document_warn(sprintf($self->__(
       "tex4ht.pm: processing produced %d items in HTML; expected %d, the number of items found in the document for \@%s"), 
                                 $got_count, $commands{$command}->{'counter'},
                                 $command));
  }
  close (TEX4HT_HTMLFILE);
  return 0;
}

sub tex4ht_do_tex($$$$)
{
  my $self = shift;
  my $cmdname = shift;;
  my $command = shift;
  # return the resulting html 
  if (exists ($commands{$cmdname}->{'results'}->{$command})
      and defined($commands{$cmdname}->{'results'}->{$command})) {
    $commands{$cmdname}->{'output_counter'}++;
    return $commands{$cmdname}->{'results'}->{$command};
  } else {
    $self->document_warn(sprintf($self->__(
                       "tex4ht.pm: output has no HTML item for \@%s %s"),
                                  $cmdname, $command));
    return '';
  }
}

sub tex4ht_finish($)
{
  my $self = shift;
  # this is different from the warning in tex4ht_process_command as, here,
  # this is the number of retrieved fragment, not processed fragment.
  if ($self->get_conf('VERBOSE')) {
    foreach my $command (keys(%commands)) {
      if ($commands{$command}->{'output_counter'} != $commands{$command}->{'counter'}) {
        $self->document_warn(sprintf($self->__(
           "tex4ht.pm: processing retrieved %d items in HTML; expected %d, the number of items found in the document for \@%s"), 
                                  $commands{$command}->{'output_counter'}, 
                                  $commands{$command}->{'counter'}, $command));
      }
    }
  }
  return 1;
}

1;
