# -*-perl-*-
#+##############################################################################
#
# html32.pm: output HTML 3.2
#
#    Copyright (C) 2003, 2004, 2007, 2009, 2011 Free Software Foundation, Inc.
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

use strict;
use Texinfo::Convert::Converter qw(xml_protect_text);

use vars qw(%commands_formatting %style_commands_formatting);

set_from_init_file('COMPLEX_FORMAT_IN_TABLE', 1);

set_from_init_file('DOCTYPE', '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 3.2//EN" "http://www.w3.org/TR/html32/loose.dtd">');

set_from_init_file('FRAMESET_DOCTYPE', get_conf('DOCTYPE'));

set_from_init_file('BODYTEXT', 'bgcolor="#FFFFFF" text="#000000" link="#0000FF" vlink="#800080" alink="#FF0000"');

set_from_init_file('BEFORE_OVERVIEW', '');
set_from_init_file('AFTER_OVERVIEW', '');
set_from_init_file('BEFORE_TOC_LINES', '');
set_from_init_file('AFTER_TOC_LINES', '');

# html version for latex2html
set_from_init_file('L2H_HTML_VERSION', '3.2');

# no css, no class
set_from_init_file('NO_CSS', 1);

# no accesskey in html 3.2
set_from_init_file('USE_ACCESSKEY', 0);

set_from_init_file('MENU_SYMBOL', '*');

set_from_init_file('OPEN_QUOTE_SYMBOL', '`');
set_from_init_file('CLOSE_QUOTE_SYMBOL', "'");

foreach my $command ('euro', 'geq', 'leq',
   'bullet', 'equiv', 'expansion', 'point', 'result', 'arrow',
   'quotedblleft', 'quotedblright',
   'quoteleft', 'quoteright',
   'quotedblbase', 'quotesinglbase', 'guillemetleft', 'guillemetright',
   'guillemotleft', 'guillemotright', 'guilsinglleft', 'guilsinglright') {
  
  $commands_formatting{'normal'}->{$command}
    = xml_protect_text(undef,
             $Texinfo::Convert::Text::text_brace_no_arg_commands{$command});
}

$commands_formatting{'normal'}->{'oe'} = '&#156;';
$commands_formatting{'normal'}->{'OE'} = '&#140;';

foreach my $dots ('dots', 'enddots') {
  $commands_formatting{'normal'}->{$dots} = '<small>...</small>';
  $commands_formatting{'preformatted'}->{$dots} = '...';
}

foreach my $context ('preformatted', 'normal') {
  foreach my $command('sansserif', 'r') {
    $style_commands_formatting{$context}->{$command} = {};
  }
}

# &quot; is not in html 3.2
sub html32_protect_text($$)
{
  my $converter = shift;
  my $text = shift;
  $text =~ s/&/&amp;/g;
  $text =~ s/</&lt;/g;
  $text =~ s/>/&gt;/g;
  $text =~ s/\"/&#34;/g;
  $text =~ s/\f/&#12;/g;
  return $text;
}

texinfo_register_formatting_function('protect_text', \&html32_protect_text);

sub html32_convert_text($$$$)
{
  my $self = shift;
  my $type = shift;
  my $command = shift;
  my $text = shift;

  # do that first because in verb and verbatim, type is 'raw'
  if ($self->in_verbatim()) {
    return $self->protect_text($text);
  }
  return $text if ($self->in_raw());

  $text = uc($text) if ($self->in_upper_case());
  $text = $self->protect_text($text);

  if (!$self->in_code() and !$self->in_math()) {
    $text =~ s/``/&#34;/g;
    $text =~ s/''/&#34;/g;
    $text =~ s/---/\x{1F}/g;
    $text =~ s/--/-/g;
    $text =~ s/\x{1F}/--/g;
  }
  if (!$self->in_preformatted() and $self->in_space_protected()) {
    $text .= '&nbsp;' if (chomp($text));
    $text =~ s/ /&nbsp;/g;
  }
  return $text;
}
texinfo_register_type_formatting('text', \&html32_convert_text);


sub html32_convert_explained_command($$$$)
{
  my $self = shift;
  my $cmdname = shift;
  my $command = shift;
  my $args = shift;

  my $with_explanation;

  return '' if (!$args->[0] or !defined($args->[0]->{'normal'})
                or $args->[0]->{'normal'} !~ /\S/);

  if ($args->[1] and defined($args->[1]->{'string'})
                 and $args->[1]->{'string'} =~ /\S/) {
    $with_explanation = 1;
  }

  my $result;
  if ($with_explanation) {
    $result = $self->convert_tree ($self->gdt('{explained_string} ({explanation})',
          {'explained_string' => $args->[0]->{'tree'},
           'explanation' => $args->[1]->{'tree'} }));
  } else {
    $result = $args->[0]->{'normal'};
  }
  return $result;
}

foreach my $explained_command (keys(%Texinfo::Common::explained_commands)) {
  texinfo_register_command_formatting($explained_command,
                              \&html32_convert_explained_command);
}

# row in multitable. no thead in html 3.2
sub html32_convert_row_type($$$$) {
  my $self = shift;
  my $type = shift;
  my $command = shift;
  my $content = shift;

  return $content if ($self->in_string());
  if ($content =~ /\S/) {
    return '<tr>' . $content . '</tr>' . "\n";
  } else {
    return '';
  }
}
texinfo_register_type_formatting('row', \&html32_convert_row_type);

sub html32_convert_tab_command ($$$$)
{
  my $self = shift;
  my $cmdname = shift;
  my $command = shift;
  my $content = shift;

  my $row = $command->{'parent'};
  my $row_cmdname = $row->{'contents'}->[0]->{'cmdname'};

  # FIXME is it right?
  $content =~ s/^\s*//;
  $content =~ s/\s*$//;

  if ($self->in_string()) {
    return $content;
  }
  if ($row_cmdname eq 'headitem') {
    return "<th>" . $content . '</th>';
  } else {
    return "<td>" . $content . '</td>';
  }
}
texinfo_register_command_formatting('tab',
                            \&html32_convert_tab_command);

sub html32_convert_item_command($$$$)
{
  my $self = shift;
  my $cmdname = shift;
  my $command = shift;
  my $content = shift;

  if ($self->in_string()) {
    return $content;
  } elsif ($command->{'parent'}->{'type'}
           and $command->{'parent'}->{'type'} eq 'row') {
    return html32_convert_tab_command ($self, $cmdname, $command, $content);
  } else {
    return &{$self->default_commands_conversion($cmdname)}($self, $cmdname, $command, $content);
  }
}

texinfo_register_command_formatting('item',
                            \&html32_convert_item_command);
texinfo_register_command_formatting('headitem',
                            \&html32_convert_item_command);
1;
