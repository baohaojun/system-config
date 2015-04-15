# A style that tries to be analogous with a book, in HTML.
#
# This file is in the public domain. Thus it may easily be used as an 
# example for further customizations.
#
# Originally written by Patrice Dumas in 2004.
# Modified in 2007, 2008.
#
# This style is based on the scriptbasic style.

use strict;

use vars qw($element_file_name);

set_from_init_file('contents', 1);
set_from_init_file('INLINE_CONTENTS', 1);
set_from_init_file('USE_TITLEPAGE_FOR_TITLE', 1);

my @book_buttons = ('Back', 'Forward', ' ', 'Contents', 'Index', 'About');

foreach my $buttons ('SECTION_BUTTONS', 'CHAPTER_BUTTONS', 'TOP_BUTTONS') {
  set_from_init_file($buttons, \@book_buttons);
}

my @book_footer_buttons = ('Contents', 'Index', 'About');
foreach my $buttons ('MISC_BUTTONS', 'SECTION_FOOTER_BUTTONS') {
  set_from_init_file($buttons, \@book_footer_buttons);
}

set_from_init_file('NODE_FOOTER_BUTTONS', ['Back', 'Forward']);
set_from_init_file('LINKS_BUTTONS', 
    ['Top', 'Index', 'Contents', 'About', 'Up', 'NextFile', 'PrevFile']);

set_from_init_file('WORDS_IN_PAGE', undef);
set_from_init_file('SHOW_MENU', 0);
set_from_init_file('USE_NODES', undef);

set_from_init_file('BIG_RULE', '<hr>');

my ($book_previous_default_filename, $book_previous_file_name, 
    $book_unumbered_nr);

sub book_init($)
{
  my $converter = shift;

  $book_previous_default_filename = undef;
  $book_previous_file_name = undef;
  $book_unumbered_nr = 0;
  return 1;
}

texinfo_register_handler('init', \&book_init);

my $NO_BULLET_LIST_CLASS = 'no-bullet';

sub book_print_up_toc($$)
{
  my $converter = shift;
  my $command = shift;

  my $result = '';
  my $current_command = $command;
  my @up_commands;
  while (defined($current_command->{'section_up'}) 
           and ($current_command->{'section_up'} ne $current_command)
           and defined($current_command->{'section_up'}->{'cmdname'})) {
    unshift (@up_commands, $current_command->{'section_up'});
    $current_command = $current_command->{'section_up'};
  }
  # this happens for example for top element
  return '' if !(@up_commands);
  #print $fh "<ul>" . &$anchor('', $Texi2HTML::HREF{Contents}, '[' . $Texi2HTML::NAME{Contents} . ']') . " <br>\n";
  my $up = shift @up_commands;
#print STDERR "$up $up->{'cmdname'} ".Texinfo::Structuring::_print_root_command_texi($up)."\n";
  $result .= $converter->_attribute_class('ul', $NO_BULLET_LIST_CLASS)."><li>" 
  . "<a href=\"".$converter->command_href($up)."\">".$converter->command_text($up) 
   . "</a> </li>\n";
  foreach my $up (@up_commands) {
    $result .= '<li>'
    .$converter->_attribute_class('ul', $NO_BULLET_LIST_CLASS)."><li>"
    . "<a href=\"".$converter->command_href($up)."\">".$converter->command_text($up) 
   . "</a> </li>\n";
  }
  foreach my $up (@up_commands) {
    $result .= "</ul></li>\n";
  }
  $result .= "</ul>\n";
  return $result;
}

sub book_navigation_header($$$$)
{
  my $self = shift;
  my $buttons = shift;
  my $cmdname = shift;
  my $command = shift;

  my $element = $command->{'parent'};
  if ($element 
      and $element->{'extra'}->{'section'}
      and ($element->{'contents'}->[0] eq $command
          or (!$element->{'contents'}->[0]->{'cmdname'} 
              and $element->{'contents'}->[1] eq $command))
      and defined($element->{'filename'})
      and $self->{'counter_in_file'}->{$element->{'filename'}} == 1) {
    
    return book_print_up_toc($self, $element->{'extra'}->{'section'}) .
       &{$self->default_formatting_function('navigation_header')}($self,
                                 $buttons, $cmdname, $command);

  } else {
    return &{$self->default_formatting_function('navigation_header')}($self, 
             $buttons, $cmdname, $command);
  }
}

texinfo_register_formatting_function('navigation_header', 
                                     \&book_navigation_header);

sub book_print_sub_toc($$$);

sub book_print_sub_toc($$$)
{
  my $converter = shift;
  my $parent_command = shift;
  my $command = shift;

  my $result = '';
  my $content_href = $converter->command_href($command);
  my $heading = $converter->command_text($command);
  if ($content_href) {
    $result .= "<li> "."<a href=\"$content_href\">$heading</a>" . " </li>\n";
  }
#print STDERR "SUB_TOC $element->{'text'}\n"; #sleep 1;
  if ($command->{'section_childs'} and @{$command->{'section_childs'}}) {
#print STDERR "SUB_TOC child $element->{'child'}->{'text'}\n"; 
    $result .= '<li>'.$converter->_attribute_class('ul',$NO_BULLET_LIST_CLASS)
     .">\n". book_print_sub_toc($converter, $parent_command, 
                                $command->{'section_childs'}->[0]) 
     ."</ul></li>\n";
  }
  if (exists($command->{'section_next'})) {
#print STDERR "SUB_TOC next($element->{'text'}) $element->{'next'}->{'text'}\n"; 
    $result .= book_print_sub_toc($converter, $parent_command, 
                                  $command->{'section_next'});
  }
  return $result;
}

sub book_convert_heading_command($$$$$)
{
  my $self = shift;
  my $cmdname = shift;
  my $command = shift;
  my $args = shift;
  my $content = shift;

  my $result = '';

  # not clear that it may really happen
  if ($self->in_string) {
    $result .= $self->command_string($command) ."\n" if ($cmdname ne 'node');
    $result .= $content if (defined($content));
    return $result;
  }
  my $element_id = $self->command_id($command);
  $result .= "<a name=\"$element_id\"></a>\n"
    if (defined($element_id) and $element_id ne '');

  print STDERR "Process $command "
        .Texinfo::Structuring::_print_root_command_texi($command)."\n"
          if ($self->get_conf('DEBUG'));
  my $element;
  if ($Texinfo::Common::root_commands{$command->{'cmdname'}} 
      and $command->{'parent'}
      and $command->{'parent'}->{'type'}
      and $command->{'parent'}->{'type'} eq 'element') {
    $element = $command->{'parent'};
  }
  if ($element) {
    $result .= &{$self->{'format_element_header'}}($self, $cmdname,
                                            $command, $element);
  }

  my $heading_level;
  # FIXME this is done as in texi2html: node is used as heading if there 
  # is nothing else.  Is it right?
  if ($cmdname eq 'node') {
    if (!$element or (!$element->{'extra'}->{'section'}
                      and $element->{'extra'}->{'node'}
                      and $element->{'extra'}->{'node'} eq $command
                      # bogus node may not have been normalized
                      and defined($command->{'extra'}->{'normalized'}))) {
      if ($command->{'extra'}->{'normalized'} eq 'Top') {
        $heading_level = 0;
      } else {
        $heading_level = 3;
      }
    }
  } else {
    $heading_level = $command->{'level'};
  }

  my $heading = $self->command_text($command);
  # $heading not defined may happen if the command is a @node, for example
  # if there is an error in the node.
  if (defined($heading) and $heading ne '' and defined($heading_level)) {

    if ($self->get_conf('TOC_LINKS')
        and $Texinfo::Common::root_commands{$cmdname}
        and $Texinfo::Common::sectioning_commands{$cmdname}) {
      my $content_href = $self->command_contents_href($command, 'contents',
                                        $self->{'current_filename'});
      if ($content_href) {
        $heading = "<a href=\"$content_href\">$heading</a>";
      }
    }

    if ($self->in_preformatted()) {
      $result .= '<strong>'.$heading.'</strong>'."\n";
    } else {
      # if the level was changed, set the command name right
      if ($cmdname ne 'node'
          and $heading_level ne $Texinfo::Common::command_structuring_level{$cmdname}) {
        $cmdname
          = $Texinfo::Common::level_to_structuring_command{$cmdname}->[$heading_level];
      }
      $result .= &{$self->{'format_heading_text'}}($self, $cmdname, $heading,
                                            $heading_level, $command);
    }
  }
  if ($command->{'section_childs'} and @{$command->{'section_childs'}}
      and $cmdname ne 'top') {
    $result .= $self->_attribute_class('ul', $NO_BULLET_LIST_CLASS).">\n";
    $result .= book_print_sub_toc($self, $command, 
                                  $command->{'section_childs'}->[0]);
    $result .= "</ul>\n";
  }
  $result .= $content if (defined($content));
  return $result;
}

foreach my $command (keys(%Texinfo::Common::sectioning_commands), 'node') {
  texinfo_register_command_formatting($command, 
                                \&book_convert_heading_command);
}

sub book_element_file_name($$$)
{
  my $converter = shift;
  my $element = shift;
  my $filename = shift;

  return undef if ($converter->get_conf('NODE_FILENAMES') 
                   or !$converter->get_conf('SPLIT'));

  if (defined($book_previous_default_filename)
      and ($filename eq $book_previous_default_filename)) {
    return $book_previous_file_name;
  }

  my $prefix = $converter->{'document_name'};
  my $new_file_name;
  my $command = $element->{'extra'}->{'section'};
  return undef unless ($command);
  if ($converter->element_is_top($element)) {
    $new_file_name = "${prefix}_top.html";
  } elsif (defined($command->{'number'}) and ($command->{'number'} ne '')) {
    my $number = $command->{'number'};
    $number .= '.' unless ($number =~ /\.$/);
    $new_file_name = "${prefix}_$number" . 'html';
  } else {
    $book_unumbered_nr++;
    $new_file_name = "${prefix}_U." . $book_unumbered_nr . '.html';
  }
  $book_previous_default_filename = $filename;
  $book_previous_file_name = $new_file_name;
  return $new_file_name;
}

$element_file_name = \&book_element_file_name;

1;
