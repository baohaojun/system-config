# Structuring.pm: extract informations about a document structure based on the 
#                 document tree.
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
# Parts (also from Patrice Dumas) come from texi2html.pl.

package Texinfo::Structuring;

use 5.00405;
use strict;

# for debugging.  Also for index entries sorting.
use Texinfo::Convert::Text;
# for error messages 
use Texinfo::Convert::Texinfo;

use Carp qw(cluck);

require Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
@ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration       use Texinfo::Structuring ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
%EXPORT_TAGS = ( 'all' => [ qw(
  associate_internal_references
  complete_tree_nodes_menus
  elements_directions
  elements_file_directions
  insert_nodes_for_sectioning_commands
  merge_indices
  new_master_menu
  nodes_tree
  number_floats
  menu_to_simple_menu
  regenerate_master_menu
  sectioning_structure
  set_menus_to_simple_menu
  sort_indices
  sort_indices_by_letter
  split_by_node
  split_by_section
  split_pages
) ] );

@EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

@EXPORT = qw(
);

$VERSION = '5.1.90';


my %types_to_enter;
foreach my $type_to_enter ('brace_command_arg', 'misc_line_arg',
    'paragraph') {
  $types_to_enter{$type_to_enter} = 1;
}

# anchor may appear in @center? @item/x?
# going in node, sectioning 
# not going in: pagesizes listoffloats shorttitlepage 
#               settitle author subtitle title
# index entries
# footnote? section node
# float printindex contents shortcontents
# anchor

# Not used for now
sub _next_content($)
{
  my $current = shift;
  if ($current->{'contents'} and scalar(@{$current->{'contents'}})) {
    $current = $current->{'contents'}->[0];
  } elsif ($current->{'args'} and @{$current->{'args'}}
           and (!defined($current->{'args'}->[0]->{'type'})
               or ($current->{'args'}->[0]->{'type'} 
                  and $types_to_enter{$current->{'args'}->[0]->{'type'}}
                  and !($current->{'extra'} 
                        and $current->{'extra'}->{'misc_args'}))
               or $current->{'type'} and $current->{'type'} eq 'menu_entry')) {
    $current = $current->{'args'}->[0];
  } elsif ($current->{'next'}) {
    $current = $current->{'next'};
  } else {
    while ($current->{'parent'} and !$current->{'parent'}->{'next'}) {
      $current = $current->{'parent'};
    }
    if ($current->{'parent'} and $current->{'parent'}->{'next'}) {
      $current = $current->{'parent'}->{'next'}
    } else {
      $current = undef;
    }
  }
  return $current;
}

# Not used for now
# the tree is modified: 'next' pointers are added.
sub _collect_structure($)
{
  my $current = shift;

  while ($current) {
    if ($current->{'contents'} and scalar(@{$current->{'contents'}})) {
      for (my $i = 0; $i < scalar(@{$current->{'contents'}}) -1; $i++) {
        $current->{'contents'}->[$i]->{'next'} = $current->{'contents'}->[$i+1];
      }
    }
    if ($current->{'args'} and scalar(@{$current->{'args'}}) > 1
        and !($current->{'extra'} 
              and $current->{'extra'}->{'misc_args'})) {
      for (my $i = 0; $i < scalar(@{$current->{'args'}}) -1; $i++) {
        $current->{'args'}->[$i]->{'next'} = $current->{'args'}->[$i+1];
      }
    }
    print STDERR "".Texinfo::Parser::_print_current($current);
    $current = _next_content($current);
  }
}

my %command_structuring_level = %Texinfo::Common::command_structuring_level;

my %appendix_commands;
my %unnumbered_commands;
foreach my $command (keys(%command_structuring_level)) {
  if ($command =~ /appendix/) {
    $appendix_commands{$command} = 1;
  } elsif ($command =~ /unnumbered/) {
    $unnumbered_commands{$command} = 1;
  }
}
$unnumbered_commands{'top'} = 1;
$unnumbered_commands{'centerchap'} = 1;
$unnumbered_commands{'part'} = 1;

my $min_level = $command_structuring_level{'chapter'};
my $max_level = $command_structuring_level{'subsubsection'};

sub _section_level($)
{
  my $section = shift;
  my $level = $command_structuring_level{$section->{'cmdname'}};
  # correct level according to raise/lowersections
  if ($section->{'extra'} and $section->{'extra'}->{'sections_level'}) {
    $level -= $section->{'extra'}->{'sections_level'};
    if ($level < $min_level) {
      if ($command_structuring_level{$section->{'cmdname'}} < $min_level) {
        $level = $command_structuring_level{$section->{'cmdname'}};
      } else {
        $level = $min_level;
      }
    } elsif ($level > $max_level) {
      $level = $max_level;
    }
  }
  return $level;
}
# sets:
# 'level'
# 'number'
# 'section_childs'
# 'section_up'
# 'section_prev'
# 'section_next'
# 'toplevel_next'
# 'toplevel_prev'
# 'toplevel_up'
sub sectioning_structure($$)
{
  my $self = shift;
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'}) {
    return undef;
  }

  my $sec_root = {};
  my $previous_section;
  my $previous_toplevel;

  my $in_appendix = 0;
  # lowest level with a number.  This is the lowest level above 0.
  my $number_top_level;

  my $section_top;
  my @sections_list;
  
  # holds the current number for all the levels.  It is not possible to use
  # something like the last child index, because of @unnumbered.
  my @command_numbers;
  # keep track of the unnumbered
  my @command_unnumbered;
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'} and $content->{'cmdname'} ne 'node'
        and $content->{'cmdname'} ne 'bye') {
      push @sections_list, $content;
      if ($content->{'cmdname'} eq 'top') {
        if (! $section_top) {
          $section_top = $content;
        }
      }
      my $level = _section_level($content);
      $content->{'level'} = $level;

      if ($previous_section) {
        # new command is below
        if ($previous_section->{'level'} < $level) {
          if ($level - $previous_section->{'level'} > 1) {
            $self->line_error(sprintf($self->
                  __("raising the section level of \@%s which is too low"), 
                  $content->{'cmdname'}), $content->{'line_nr'});
            $content->{'level'} = $previous_section->{'level'} + 1;
          }
          $previous_section->{'section_childs'} = [$content];
          $content->{'section_up'} = $previous_section;

          # if the up is unnumbered, the number information has to be kept,
          # to avoid reusing an already used number.
          if (!$unnumbered_commands{$previous_section->{'cmdname'}}) {
            $command_numbers[$content->{'level'}] = undef;
          } elsif (!$unnumbered_commands{$content->{'cmdname'}}) {
            $command_numbers[$content->{'level'}]++;
          }
          if ($unnumbered_commands{$content->{'cmdname'}}) {
            $command_unnumbered[$content->{'level'}] = 1;
          } else {
            $command_unnumbered[$content->{'level'}] = 0;
          }
        } else {
          my $up = $previous_section->{'section_up'};
          my $new_upper_element;
          if ($previous_section->{'level'} != $level) {
            # means it is above the previous command, the up is to be found
            while ($up->{'section_up'} and $up->{'level'} >= $level) {
              $up = $up->{'section_up'};
            }
            if ($level <= $up->{'level'}) {
              if ($content->{'cmdname'} eq 'part') {
                $new_upper_element = 1;
                if ($level < $up->{'level'}) {
                  $self->line_warn(sprintf($self->__(
                    "no chapter-level command before \@%s"),
                          $content->{'cmdname'}), $content->{'line_nr'});
                }
              } else {
                $self->line_warn(sprintf($self->__(
         "lowering the section level of \@%s appearing after a lower element"), 
                  $content->{'cmdname'}), $content->{'line_nr'});
                $content->{'level'} = $up->{'level'} + 1;
              }
            }
          }
          if ($appendix_commands{$content->{'cmdname'}} and !$in_appendix
              and $content->{'level'} <= $number_top_level 
              and $up->{'cmdname'} and $up->{'cmdname'} eq 'part') {
            $up = $up->{'section_up'};
          }
          if ($new_upper_element) {
            # In that case the root has to be updated because the first 
            # 'part' just appeared
            $content->{'section_up'} = $sec_root;
            $sec_root->{'level'} = $level - 1;
            push @{$sec_root->{'section_childs'}}, $content;
            $number_top_level = $level;
            $number_top_level++ if (!$number_top_level);
          } else {
            push @{$up->{'section_childs'}}, $content;
            $content->{'section_up'} = $up;
            $content->{'section_prev'} = $up->{'section_childs'}->[-2];
            $content->{'section_prev'}->{'section_next'} = $content;
          }
          if (!$unnumbered_commands{$content->{'cmdname'}}) {
            $command_numbers[$content->{'level'}]++;
            $command_unnumbered[$content->{'level'}] = 0;
          } else {
            $command_unnumbered[$content->{'level'}] = 1;
          }
        }
      } else { # first section determines the level of the root.  It is 
               # typically -1 when there is a @top.
        $content->{'section_up'} = $sec_root;
        $sec_root->{'level'} = $level - 1;
        $sec_root->{'section_childs'} = [$content];
        $number_top_level = $level;
        # if $level of top sectioning element is 0, which means that
        # it is a @top, $number_top_level is 1 as it is associated to
        # the level of chapter/unnumbered...
        $number_top_level++ if (!$number_top_level);
        if ($content->{'cmdname'} ne 'top') {
          if (!$unnumbered_commands{$content->{'cmdname'}}) {
            $command_unnumbered[$content->{'level'}] = 0;
          } else {
            $command_unnumbered[$content->{'level'}] = 1;
          }
        }
      }
      if (!defined($command_numbers[$content->{'level'}])) {
        if ($unnumbered_commands{$content->{'cmdname'}}) {
          $command_numbers[$content->{'level'}] = 0;
        } else {
          $command_numbers[$content->{'level'}] = 1;
        }
      }
      if ($appendix_commands{$content->{'cmdname'}} and !$in_appendix) {
        $in_appendix = 1;
        $command_numbers[$content->{'level'}] = 'A';
      }
      if (!$unnumbered_commands{$content->{'cmdname'}}) {
        # construct the number, if not below an unnumbered
        if (!$command_unnumbered[$number_top_level]) {
          $content->{'number'} = $command_numbers[$number_top_level];
          for (my $i = $number_top_level+1; $i <= $content->{'level'}; $i++) {
            $content->{'number'} .= ".$command_numbers[$i]";
            # If there is an unnumbered above, then no number is added.
            if ($command_unnumbered[$i]) {
              delete $content->{'number'};
              last;
            }
          }
        }
      }
      $previous_section = $content;
      if ($content->{'cmdname'} ne 'part' 
          and $content->{'level'} <= $number_top_level) {
        if ($previous_toplevel) {
          $previous_toplevel->{'toplevel_next'} = $content;
          $content->{'toplevel_prev'} = $previous_toplevel;
        }
        $previous_toplevel = $content;
        if ($section_top and $content ne $section_top) {
          $content->{'toplevel_up'} = $section_top;
        }
      } elsif ($content->{'cmdname'} eq 'part' 
               and !$content->{'extra'}->{'part_associated_section'}) {
        $self->line_warn(sprintf($self->__(
            "no sectioning command associated with \@%s"),
                $content->{'cmdname'}), $content->{'line_nr'});
      }

      if ($self->{'DEBUG'}) {
        my $number = '';
        $number = $content->{'number'} if defined($content->{'number'});
        print STDERR "($content->{'level'}|$level|$command_structuring_level{$content->{'cmdname'}})[$command_numbers[$content->{'level'}]]($in_appendix) $number \@$content->{'cmdname'} ".Texinfo::Convert::Text::convert($content->{'args'}->[0])."\n";
      }
    }
  }
  $self->{'structuring'}->{'sectioning_root'} = $sec_root;
  $self->{'structuring'}->{'sections_list'} = \@sections_list;
  return $sec_root;
}

sub _print_sectioning_tree($);
sub _print_sectioning_tree($)
{
  my $current = shift;
  my $result = ' ' x $current->{'level'} . _print_root_command_texi($current)."\n";
  foreach my $child (@{$current->{'section_childs'}}) {
    $result .= _print_sectioning_tree($child);
  }
  return $result;
}


# Add raise/lowersections to be back at the normal level
sub _correct_level($$;$)
{
  my $section = shift;
  my $parent = shift;
  my $modifier = shift;
  $modifier = 1 if (!defined($modifier));

  my @result;
  if ($section->{'extra'} and $section->{'extra'}->{'sections_level'}) {
    my $level_to_remove = $modifier * $section->{'extra'}->{'sections_level'};
    my $command;
    if ($level_to_remove < 0) {
      $command = 'raisesection';
    } else {
      $command = 'lowersection';
    }
    my $remaining_level = abs($level_to_remove);
    while ($remaining_level) {
      push @result, {'cmdname' => $command,
                     'parent' => $parent};
      push @result, {'type' => 'empty_line', 'text' => "\n",
                     'parent' => $parent};
      $remaining_level--;
    }
  }
  return @result;
}

sub fill_gaps_in_sectioning($)
{
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'}) {
    return undef;
  }
  my @sections_list;
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'} and $content->{'cmdname'} ne 'node'
        and $content->{'cmdname'} ne 'bye') {
      push @sections_list, $content;
    }
  }

  return (undef, undef) if (!scalar(@sections_list));

  my @added_sections;
  my @contents;
  my $previous_section;
  foreach my $content(@{$root->{'contents'}}) {
    push @contents, $content;
    if (!@sections_list or $sections_list[0] ne $content) {
      next;
    }
    my $current_section = shift @sections_list;
    my $current_section_level = _section_level($current_section);
    my $next_section = $sections_list[0];
    
    if (defined($next_section)) {
      my $next_section_level = _section_level($next_section);
      if ($next_section_level - $current_section_level > 1) {
        my @correct_level_offset_commands = _correct_level($next_section,
                                                          $contents[-1]);
        if (@correct_level_offset_commands) {
          push @{$contents[-1]->{'contents'}}, @correct_level_offset_commands;
        }
        #print STDERR "* $current_section_level "._print_root_command_texi($current_section)."\n";
        #print STDERR "  $next_section_level "._print_root_command_texi($next_section)."\n";
        while ($next_section_level - $current_section_level > 1) {
          $current_section_level++;
          my $new_section = {'cmdname' =>
            $Texinfo::Common::level_to_structuring_command{'unnumbered'}->[$current_section_level],
            'parent' => $root,
          };
          $new_section->{'contents'} = [{'type' => 'empty_line', 
                                         'text' => "\n",
                                         'parent' => $new_section}];
          $new_section->{'args'} = [{'type' => 'misc_line_arg',
                                     'parent' => $new_section}];
          $new_section->{'args'}->[0]->{'contents'} = [
             {'type' => 'empty_spaces_after_command',
              'text' => " ",
              'extra' => {'command' => $new_section},
              'parent' => $new_section->{'args'}->[0]
             },
             {'cmdname' => 'asis',
              'parent' => $new_section->{'args'}->[0]
             },
             {'type' => 'spaces_at_end',
              'text' => "\n",
              'parent' => $new_section->{'args'}->[0]
             }];
          $new_section->{'args'}->[0]->{'contents'}->[1]->{'args'}
             = [{'type' => 'brace_command_arg',
                 'contents' => [],
                 'parent' => $new_section->{'args'}->[0]->{'contents'}->[1]}];
          my @misc_contents = @{$new_section->{'args'}->[0]->{'contents'}};
          Texinfo::Common::trim_spaces_comment_from_content(\@misc_contents);
          $new_section->{'extra'}->{'misc_content'} = \@misc_contents;
          push @contents, $new_section;
          push @added_sections, $new_section;
          #print STDERR "  -> "._print_root_command_texi($new_section)."\n";
        }
        my @set_level_offset_commands = _correct_level($next_section,
                                                       $contents[-1], -1);
        if (@set_level_offset_commands) {
          push @{$contents[-1]->{'contents'}}, @set_level_offset_commands;
        }
      }
    }
  }
  return (\@contents, \@added_sections);
}

sub _check_node_same_texinfo_code($$)
{
  my $reference_node = shift;
  my $node_extra = shift;

  my $reference_node_texi;
  if ($reference_node->{'extra'}->{'node_content'}) {
    $reference_node_texi = Texinfo::Convert::Texinfo::convert (
        {'contents' => $reference_node->{'extra'}->{'node_content'}});
    $reference_node_texi =~ s/\s+/ /g;
  } else {
    $reference_node_texi = '';
  }

  my $node_texi;
  if ($node_extra and $node_extra->{'node_content'}) {
    my @contents_node = @{$node_extra->{'node_content'}};
    pop @contents_node if ($contents_node[-1]->{'type'} 
               and $contents_node[-1]->{'type'} eq 'space_at_end_menu_node');
    $node_texi = Texinfo::Convert::Texinfo::convert (
      {'contents' => \@contents_node});
    $node_texi =~ s/\s+/ /g;
  } else {
    $node_texi = '';
  }
  return ($reference_node_texi eq $node_texi);
}


my @node_directions = ('next', 'prev', 'up');
# No translation of those special Info keywords.
my %direction_texts = (
 'prev' => 'Prev',
 'next' => 'Next',
 'up' => 'Up'
);

sub _check_menu_entry($$$$)
{
  my $self = shift;
  my $command = shift;
  my $menu_content = shift;
  my $check_menu_entries = shift;

  my $menu_node;

  if (!$self->{'labels'}->{$menu_content->{'extra'}->{'menu_entry_node'}->{'normalized'}}) {
    if ($check_menu_entries) {
      $self->line_error(sprintf($self->
       __("\@%s reference to nonexistent node `%s'"), 
         $command,
         Texinfo::Parser::_node_extra_to_texi(
                 $menu_content->{'extra'}->{'menu_entry_node'})), 
        $menu_content->{'line_nr'});
    }
  } else {
    my $normalized_menu_node
      = $menu_content->{'extra'}->{'menu_entry_node'}->{'normalized'};
    $menu_node = $self->{'labels'}->{$normalized_menu_node};
    if ($check_menu_entries and ! _check_node_same_texinfo_code($menu_node, 
        $menu_content->{'extra'}->{'menu_entry_node'})) {
      $self->line_warn(sprintf($self->
       __("\@%s entry node name `%s' different from %s name `%s'"), 
         $command,
         Texinfo::Parser::_node_extra_to_texi(
                 $menu_content->{'extra'}->{'menu_entry_node'}),
         $menu_node->{'cmdname'},
         Texinfo::Parser::_node_extra_to_texi($menu_node->{'extra'})),
        $menu_content->{'line_nr'});
    }
  }
  return $menu_node;
}

# first go through all the menu and set menu_up, menu_next, menu_prev
# and warn for unknown nodes.
# then go through all the nodes and set directions
sub nodes_tree($)
{
  my $self = shift;
  return undef unless ($self->{'nodes'} and @{$self->{'nodes'}});
  my $top_node;
  my $top_node_up;

  my $check_menu_entries = (!$self->{'novalidate'} and $self->{'SHOW_MENU'});

  foreach my $node (@{$self->{'nodes'}}) {
    if ($node->{'extra'}->{'normalized'} eq 'Top') {
      $top_node = $node;
      my $top_node_up_content_tree = Texinfo::Parser::parse_texi_line($self, 
                                                    $self->{'TOP_NODE_UP'});
      $top_node_up
        = {'type' => 'top_node_up',
           'extra' => Texinfo::Parser::_parse_node_manual(
                {'contents' => $top_node_up_content_tree->{'contents'}})};
    }
    if ($node->{'menus'}) {
      if ($self->{'SHOW_MENU'} and @{$node->{'menus'}} > 1) {
        foreach my $menu (@{$node->{'menus'}}[1 .. $#{$node->{'menus'}}]) {
          $self->line_warn(sprintf($self->__("multiple \@%s"), 
                        $menu->{'cmdname'}), $menu->{'line_nr'});
        }
      }
      # Remark: since the @menu are only checked if they are in @node, 
      # menu entry before the first node may be treated slightly differently.
      # at least, there are no error messages for them

      foreach my $menu (@{$node->{'menus'}}) {
        my $previous_node;
        foreach my $menu_content (@{$menu->{'contents'}}) {
          if ($menu_content->{'extra'}
             and $menu_content->{'extra'}->{'menu_entry_node'}) {
            my $menu_node;
            my $external_node;
            if (!$menu_content->{'extra'}->{'menu_entry_node'}->{'manual_content'}) {
              $menu_node = _check_menu_entry($self, 'menu', $menu_content, 
                                                    $check_menu_entries);
              # this may happen more than once for a given node if the node 
              # is in more than one menu.  Therefore all the menu up node 
              # are kept in $menu_node->{'menu_up_hash'}
              if ($menu_node) {
                $menu_node->{'menu_up'} = $node;
                $menu_node->{'menu_up_hash'}->{$node->{'extra'}->{'normalized'}} = 1;
              }
            } else {
              $external_node = 1;
              $menu_node = {'extra' => $menu_content->{'extra'}->{'menu_entry_node'}};
            }
            if ($menu_node) {
              if ($previous_node) {
                if (!$external_node) {
                  $menu_node->{'menu_prev'} = $previous_node;
                }
                if (!$previous_node->{'extra'}->{'manual_content'}) {
                  $previous_node->{'menu_next'} = $menu_node;
                }
              } else {
                $node->{'menu_child'} = $menu_node;
              }
              $previous_node = $menu_node;
            }
          }
        } # end menu
      }
    }
  }
  if ($check_menu_entries) {
    my $global_commands = $self->global_commands_information();
    if ($global_commands->{'detailmenu'}) {
      foreach my $detailmenu (@{$global_commands->{'detailmenu'}}) {
        foreach my $menu_content (@{$detailmenu->{'contents'}}) {
          if ($menu_content->{'extra'}
             and $menu_content->{'extra'}->{'menu_entry_node'}) {
            if (!$menu_content->{'extra'}->{'menu_entry_node'}->{'manual_content'}) {
              _check_menu_entry($self, 'detailmenu', $menu_content, 
                                $check_menu_entries);
            }
          }
        }
      }
    }
  }

  $top_node = $self->{'nodes'}->[0] if (!$top_node);
  foreach my $node (@{$self->{'nodes'}}) {
    # warn if node is not top node and doesn't appear in menu
    if ($self->{'SHOW_MENU'} and $node ne $top_node and !$node->{'menu_up'}) {
      $self->line_warn(sprintf($self->__("unreferenced node `%s'"), 
                    Texinfo::Parser::_node_extra_to_texi($node->{'extra'})), 
                       $node->{'line_nr'});
    }
    #print STDERR "Processing "
    #  .Texinfo::Parser::_node_extra_to_texi($node->{'extra'})."\n";
   
    my $automatic_directions = 
      (scalar(@{$node->{'extra'}->{'nodes_manuals'}}) == 1);

    if ($automatic_directions) {
      if ($node->{'extra'}->{'normalized'} ne 'Top') {
        foreach my $direction (@node_directions) {
          # prev already defined for the node first Top node menu entry
          if ($direction eq 'prev' and $node->{'node_'.$direction}
              and $node->{'node_'.$direction}->{'extra'}
              and $node->{'node_'.$direction}->{'extra'}->{'normalized'}
              and $node->{'node_'.$direction}->{'extra'}->{'normalized'} eq 'Top') {
            next;
          }
          if ($node->{'extra'}->{'associated_section'}) {
            my $section = $node->{'extra'}->{'associated_section'};
            # prefer the section associated to the part for node directions.
            if ($section->{'extra'}->{'part_associated_section'}) {
              $section = $section->{'extra'}->{'part_associated_section'};
            }
            # use toplevel to go through parts.  But not for @top as prev
            # or next.
            foreach my $direction_base ('section', 'toplevel') {
              if ($section->{$direction_base.'_'.$direction}
                 and $section->{$direction_base.'_'.$direction}->{'extra'}
                 and ($direction_base ne 'toplevel'
                      or $direction eq 'up'
                      or $section->{$direction_base.'_'.$direction}->{'cmdname'} ne 'top')
                 and $section->{$direction_base.'_'.$direction}->{'extra'}->{'associated_node'}) {
                $node->{'node_'.$direction} 
                  = $section->{$direction_base.'_'.$direction}->{'extra'}->{'associated_node'};
                last;
              }
            }
            # if set, a direction was found using sections.  Check consistency
            # with menu before going to the next direction
            if ($node->{'node_'.$direction}) {
              if ($self->{'SHOW_MENU'}) {
                if (!$node->{'menu_'.$direction}) {
                  $self->line_warn(sprintf($self->
                    __("node `%s' is %s for `%s' in sectioning but not in menu"), 
                  Texinfo::Parser::_node_extra_to_texi($node->{'node_'.$direction}->{'extra'}), 
                  $direction,
                  Texinfo::Parser::_node_extra_to_texi($node->{'extra'})),
                  $node->{'line_nr'});
                } elsif ($node->{'menu_'.$direction} ne $node->{'node_'.$direction}) {
                  $self->line_warn(sprintf($self->
                    __("node %s `%s' in menu `%s' and in sectioning `%s' differ"), 
                    $direction,
                    Texinfo::Parser::_node_extra_to_texi($node->{'extra'}),
                    Texinfo::Parser::_node_extra_to_texi($node->{'menu_'.$direction}->{'extra'}), 
                    Texinfo::Parser::_node_extra_to_texi($node->{'node_'.$direction}->{'extra'})),
                    $node->{'line_nr'});
                }
              }
              next;
            }
          }
          # no direction was found using sections, use menus.  This allows
          # using only automatic direction for manuals without sectioning
          # commands.
          if ($node->{'menu_'.$direction} 
              and !$node->{'menu_'.$direction}->{'extra'}->{'manual_content'}) {
            if ($self->{'SHOW_MENU'} and $node->{'extra'}->{'associated_section'}) {
              $self->line_warn(sprintf($self->
                  __("node `%s' is %s for `%s' in menu but not in sectioning"), 
                Texinfo::Parser::_node_extra_to_texi($node->{'menu_'.$direction}->{'extra'}),
                $direction,
                Texinfo::Parser::_node_extra_to_texi($node->{'extra'}), 
                  ), 
                $node->{'line_nr'});
            }
            $node->{'node_'.$direction} = $node->{'menu_'.$direction};
          }
        }
      } else {
        # Special case for Top node.
        $node->{'node_up'} = $top_node_up;
        $node->{'node_up'}->{'extra'}->{'top_node_up'} = $node;
        if ($node->{'menu_child'}) {
          $node->{'node_next'} = $node->{'menu_child'};
          if (!$node->{'menu_child'}->{'extra'}->{'manual_content'}) {
            $node->{'menu_child'}->{'node_prev'} = $node;
          }
        }
      }
    } else {
      my @directions = @{$node->{'extra'}->{'nodes_manuals'}};
      shift @directions;
      foreach my $direction (@node_directions) {
        my $node_direction = shift @directions;
        next if (!defined($node_direction));
        # external node
        if ($node_direction->{'manual_content'}) {
          $node->{'node_'.$direction} = { 'extra' => $node_direction };
          # set top_node_up for up of Top if it is the same as top_node_up
          if ($node->{'extra'}->{'normalized'} eq 'Top'
              and $direction eq 'up'
              and $top_node_up->{'extra'}->{'manual_content'}
              and ((!defined($node_direction->{'normalized'})
                    and !defined($top_node_up->{'extra'}->{'normalized'}))
                   or (defined($node_direction->{'normalized'}) 
                       and defined($top_node_up->{'extra'}->{'normalized'})
                       and $node_direction->{'normalized'} eq $top_node_up->{'extra'}->{'normalized'}))
              and (Texinfo::Convert::NodeNameNormalization::normalize_node(
                     {'contents' => $node_direction->{'manual_content'}})
                  eq Texinfo::Convert::NodeNameNormalization::normalize_node(
                     {'contents' => $top_node_up->{'extra'}->{'manual_content'}}))) {
            $node->{'node_'.$direction}->{'extra'}->{'top_node_up'} = $node;
          }
        } else {
          if ($self->{'labels'}->{$node_direction->{'normalized'}}) {
            my $node_target 
               = $self->{'labels'}->{$node_direction->{'normalized'}};
            $node->{'node_'.$direction} = $node_target;

            if (! $self->{'novalidate'} and ! _check_node_same_texinfo_code(
                $node_target, $node_direction)) {
              $self->line_warn(sprintf($self->
                __("%s pointer `%s' (for node `%s') different from %s name `%s'"),
                  $direction_texts{$direction},
                  Texinfo::Parser::_node_extra_to_texi(
                        $node_direction),
                  Texinfo::Parser::_node_extra_to_texi(
                        $node->{'extra'}),
                  $node_target->{'cmdname'},
                  Texinfo::Parser::_node_extra_to_texi(
                        $node_target->{'extra'})),
                $node->{'line_nr'});
            }
          } else {
            if ($self->{'novalidate'}) {
              $node->{'node_'.$direction} = { 'extra' => $node_direction };
            # special case of up for top an internal node and the same
            # as TOP_NODE_UP.  This is not the default case, since in the
            # default case TOP_NODE_UP is an external node.
            } elsif ($node->{'extra'}->{'normalized'} eq 'Top' 
                     and $direction eq 'up'
                     and !$top_node_up->{'extra'}->{'manual_content'}
                     and $node_direction->{'normalized'} eq $top_node_up->{'extra'}->{'normalized'}) {
              $node->{'node_'.$direction} = { 'type' => 'top_node_up',
                                              'extra' => $node_direction };
              $node->{'node_'.$direction}->{'extra'}->{'top_node_up'} 
                = $node;
            } else {
              $self->line_error(sprintf($self->
                                  __("%s reference to nonexistent `%s'"),
                    $direction_texts{$direction},
                    Texinfo::Parser::_node_extra_to_texi($node_direction)), 
                    $node->{'line_nr'});
            }
          }
        }
      }
    }
    # it may happen (rarely) that the node_up is a manual entry
    # and therefore $node->{'node_up'}->{'extra'}->{'manual_content'}.
    # The node_up should always be different from the menu_up, therefore
    # if in a menu, the second condition/error message applies.
    if ($node->{'node_up'} and ($node->{'node_up'}->{'extra'}->{'manual_content'}
         or !$node->{'menu_up_hash'}
         or !$node->{'menu_up_hash'}->{$node->{'node_up'}->{'extra'}->{'normalized'}})) {
      if (!$node->{'node_up'}->{'extra'}->{'manual_content'}) {
      # up node is a real node but has no menu entry
        $self->line_error(sprintf($self->
           __("node `%s' lacks menu item for `%s' despite being its Up target"), 
           Texinfo::Parser::_node_extra_to_texi($node->{'node_up'}->{'extra'}), 
           Texinfo::Parser::_node_extra_to_texi($node->{'extra'})),
           $node->{'node_up'}->{'line_nr'});
      # This leads to an error when there is an external nodes as up, and 
      # not in Top node.
      } elsif ($node->{'menu_up'}) {
        $self->line_warn(sprintf($self->
           __("for `%s', up in menu `%s' and up `%s' don't match"), 
          Texinfo::Parser::_node_extra_to_texi($node->{'extra'}),
          Texinfo::Parser::_node_extra_to_texi($node->{'menu_up'}->{'extra'}), 
          Texinfo::Parser::_node_extra_to_texi($node->{'node_up'}->{'extra'})),
                        $node->{'line_nr'});
      }
    }
  }
  $self->{'structuring'}->{'top_node'} = $top_node;
  return $top_node;
}

sub split_by_node($)
{
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'} or !@{$root->{'contents'}}) {
    return undef;
  }
  my $elements;
  my $current = { 'type' => 'element', 'extra' => {'no_node' => 1}};
  push @$elements, $current; 
  my @pending_parts = ();
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'} and $content->{'cmdname'} eq 'part'
        and $content->{'extra'}->{'part_associated_section'}) {
      push @pending_parts, $content;
      next;
    }
    if ($content->{'cmdname'} and $content->{'cmdname'} eq 'node') {
      if ($current->{'extra'}->{'no_node'}) {
        delete $current->{'extra'}->{'no_node'};
        $current->{'extra'}->{'node'} = $content;
      } else {
        $current = { 'type' => 'element', 'extra' => {'node' => $content}};
        $current->{'element_prev'} = $elements->[-1];
        $elements->[-1]->{'element_next'} = $current;
        push @$elements, $current;
      }
      $elements->[-1]->{'extra'}->{'element_command'} = $content;
      if ($content->{'extra'}->{'associated_section'}) {
        $elements->[-1]->{'extra'}->{'section'} 
          = $content->{'extra'}->{'associated_section'};
      }
    }
    if (@pending_parts) {
      foreach my $part (@pending_parts) {
        push @{$current->{'contents'}}, $part;
        $part->{'parent'} = $current;
      }
      @pending_parts = ();
    }
    push @{$current->{'contents'}}, $content;
    $content->{'parent'} = $current;
  }
  return $elements;
}

sub split_by_section($)
{
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'} or !@{$root->{'contents'}}) {
    return undef;
  }
  my $elements;
  my $current = { 'type' => 'element', 'extra' => {'no_section' => 1}};
  push @$elements, $current; 
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'}
        and (($content->{'cmdname'} eq 'node' 
              and $content->{'extra'}->{'associated_section'})
             or ($content->{'cmdname'} eq 'part'
                 and $content->{'extra'}->{'part_associated_section'}))) {
      my $new_section;
      if ($content->{'cmdname'} eq 'node') {
        $new_section = $content->{'extra'}->{'associated_section'};
      } else {
        $new_section = $content->{'extra'}->{'part_associated_section'};
      }
      if (! $current->{'extra'}->{'section'}
        or $new_section ne $current->{'extra'}->{'section'}) {
        if ($current->{'extra'}->{'no_section'}) {
          delete $current->{'extra'}->{'no_section'};
          $current->{'extra'}->{'section'}
            = $new_section;
        } else {
          $current = { 'type' => 'element', 
                       'extra' => {'section' => $new_section}};
          $current->{'element_prev'} = $elements->[-1];
          $elements->[-1]->{'element_next'} = $current;
          push @$elements, $current;
        }
        $elements->[-1]->{'extra'}->{'element_command'} 
          = $new_section;
      }
    } elsif ($content->{'cmdname'} and $content->{'cmdname'} ne 'node' 
                                   and $content->{'cmdname'} ne 'bye') {
      if ($current->{'extra'}->{'no_section'}) {
        delete $current->{'extra'}->{'no_section'};
        $current->{'extra'}->{'section'} = $content;
        $current->{'extra'}->{'element_command'} = $content;
      } elsif ($current->{'extra'}->{'section'} ne $content) {
        $current = { 'type' => 'element', 'extra' => {'section' => $content,
                                              'element_command' => $content}};
        $current->{'element_prev'} = $elements->[-1];
        $elements->[-1]->{'element_next'} = $current;
        push @$elements, $current;
      }
    }
    if ($content->{'cmdname'} and $content->{'cmdname'} eq 'node' 
        and $content->{'extra'}->{'associated_section'}) {
      $current->{'extra'}->{'node'} = $content;
    }
    push @{$current->{'contents'}}, $content;
    $content->{'parent'} = $current;
  }
  return $elements;
}

# associate elements to pages according to the splitting specification.
sub split_pages ($$)
{
  my $elements = shift;
  my $split = shift;

  return undef if (!$elements or !@$elements);

  my $split_level;
  if (!$split) {
    foreach my $element (@$elements) {
      $element->{'extra'}->{'first_in_page'} = $elements->[0];
    }
    return;
  } elsif ($split eq 'chapter') {
    $split_level = 1;
  } elsif ($split eq 'section') {
    $split_level = 2;
  } elsif ($split ne 'node') {
    warn "Unknown split specification: $split\n";
  }

  my $current_first_in_page;
  foreach my $element (@$elements) {
    my $level;
    if ($element->{'extra'}->{'section'}) {
      $level = $element->{'extra'}->{'section'}->{'level'};
    } elsif ($element->{'extra'}->{'node'} 
             and $element->{'extra'}->{'node'}->{'associated_section'}) {
      $level = $element->{'extra'}->{'node'}->{'associated_section'}->{'level'};
    }
    #print STDERR "level($split_level) $level "._print_element_command_texi($element)."\n";
    if (!defined($split_level) or (defined($level) and $split_level >= $level)
        or !$current_first_in_page) {
      $current_first_in_page = $element;
    }
    $element->{'extra'}->{'first_in_page'} = $current_first_in_page;
  }
}

# undef in argument should be an error.  Thus only node existing should be
# passed to this function.  Even if not existing the value returned should
# be undef.
sub _node_element($)
{
  my $node = shift;
  if ($node->{'extra'} and $node->{'extra'}->{'manual_content'}) {
    my $external_node = { 'type' => 'external_node',
      'extra' => {'manual_content' => $node->{'extra'}->{'manual_content'}}};
  
    if ($node->{'extra'}->{'node_content'}) {
      $external_node->{'extra'}->{'node_content'} 
        = $node->{'extra'}->{'node_content'};
      $external_node->{'extra'}->{'normalized'} = 
        Texinfo::Convert::NodeNameNormalization::normalize_node(
          {'contents' => $node->{'extra'}->{'node_content'}}); 
    }
    $external_node->{'extra'}->{'top_node_up'} 
      = $node->{'extra'}->{'top_node_up'}
      if (exists($node->{'extra'}->{'top_node_up'}));
    return $external_node;
  } elsif ($node->{'cmdname'} and $node->{'cmdname'} eq 'node') {
    return $node->{'parent'};
  } elsif ($node->{'type'} and $node->{'type'} eq 'top_node_up') {
    return $node;
  } else {
    # case of a @float or an @anchor
    return undef;
  }
}

# Do element directions (like in texi2html) and store them 
# in 'extra'->'directions'.
sub elements_directions($$)
{
  my $self = shift;
  my $elements = shift;
  return if (!$elements or !@$elements);

  my $node_top = $self->{'labels'}->{'Top'};
  foreach my $element (@$elements) {
    my $directions;
    $directions->{'This'} = $element;
    $directions->{'Forward'} = $element->{'element_next'}
      if ($element->{'element_next'}
          and (($element->{'extra'}->{'special_element'}
                and $element->{'element_next'}->{'extra'}->{'special_element'})
               or (!$element->{'extra'}->{'special_element'}
                and !$element->{'element_next'}->{'extra'}->{'special_element'})));
    $directions->{'Back'} = $element->{'element_prev'}
      if ($element->{'element_prev'}
          and (($element->{'extra'}->{'special_element'}
                and $element->{'element_prev'}->{'extra'}->{'special_element'})
               or (!$element->{'extra'}->{'special_element'}
                and !$element->{'element_prev'}->{'extra'}->{'special_element'})));
    if ($element->{'extra'}->{'node'}) {
      my $node = $element->{'extra'}->{'node'};
      foreach my $direction(['NodeUp', 'node_up'], ['NodeNext', 'node_next'],
                            ['NodePrev', 'node_prev']) {
        $directions->{$direction->[0]} = _node_element($node->{$direction->[1]})
            if ($node->{$direction->[1]});
      }
      # Now do NodeForward which is something like the following node.
      my $automatic_directions = 
        (scalar(@{$node->{'extra'}->{'nodes_manuals'}}) == 1);
      if ($node->{'menu_child'}) {
        $directions->{'NodeForward'} = _node_element($node->{'menu_child'});
      } elsif ($automatic_directions and $node->{'associated_section'}
               and $node->{'associated_section'}->{'section_childs'}
               and $node->{'associated_section'}->{'section_childs'}->[0]) {
        $directions->{'NodeForward'} 
          = $node->{'associated_section'}->{'section_childs'}->[0]->{'parent'};
      } elsif ($node->{'node_next'}) {
        $directions->{'NodeForward'} = _node_element($node->{'node_next'});
      } else {
        my $up = $node->{'node_up'};
        my @up_list = ($node);
        # the condition with the up_list avoids infinite loops
        # the last condition stops when the Top node is reached.
        while (defined($up) 
               and not (grep {$up eq $_} @up_list  
                        or ($node_top and $up eq $node_top))) {
          if (defined($up->{'node_next'})) {
            $directions->{'NodeForward'} = _node_element($up->{'node_next'});
            last;
          }
          push @up_list, $up;
          $up = $up->{'node_up'};
        }
      }
      
      $directions->{'NodeForward'}->{'extra'}->{'directions'}->{'NodeBack'} = $element
        if ($directions->{'NodeForward'}
            and $directions->{'NodeForward'}->{'type'} eq 'element'
            and !$directions->{'NodeForward'}->{'extra'}->{'directions'}->{'NodeBack'});
    }

    if (!$element->{'extra'}->{'section'}) {
      # If there is no associated section, find the previous element section.
      # Use the FastForward of this element.
      # Use it as FastBack if the section is top level, or use the FastBack.
      my $section_element;
      my $current = $element;
      while ($current->{'element_prev'}) {
        $current = $current->{'element_prev'};
        if ($current->{'extra'}->{'section'}) {
          $section_element = $current;
          last;
        }
      }
      if ($section_element) {
        if ($section_element->{'extra'}->{'directions'}->{'FastForward'}) {
          $directions->{'FastForward'} 
            = $section_element->{'extra'}->{'directions'}->{'FastForward'};
        }
        if ($section_element->{'extra'}->{'section'}->{'level'} <= 1) {
          $directions->{'FastBack'} = $section_element;
        } elsif ($section_element->{'extra'}->{'directions'}->{'Fastback'}) {
          $directions->{'FastBack'} 
            = $section_element->{'extra'}->{'directions'}->{'Fastback'};
        }
      }
    } else {
      my $section = $element->{'extra'}->{'section'};
      foreach my $direction(['Up', 'section_up'], ['Next', 'section_next'],
                            ['Prev', 'section_prev']) {
        # in most cases $section->{$direction->[1]}->{'parent'} is defined
        # but it may not be the case for the up of @top.
        # The section may be its own up in cases like
        #  @part part
        #  @chapter chapter
        # in that cas the direction is not set up
        $directions->{$direction->[0]} = $section->{$direction->[1]}->{'parent'}
          if ($section->{$direction->[1]} 
              and $section->{$direction->[1]}->{'parent'}
              and $section->{$direction->[1]}->{'parent'} ne $section->{'parent'});
      }

      my $up = $section;
      while ($up->{'level'} > 1 and $up->{'section_up'}) {
        $up = $up->{'section_up'};
      }

      # fastforward is the next element on same level than the upper parent
      # element.
      if ($up->{'level'} < 1 and $up->{'cmdname'} and $up->{'cmdname'} eq 'top'
          and $up->{'section_childs'} and @{$up->{'section_childs'}}) {
        $directions->{'FastForward'} = $up->{'section_childs'}->[0]->{'parent'};
      } elsif ($up->{'toplevel_next'}) {
        $directions->{'FastForward'} = $up->{'toplevel_next'}->{'parent'};
      } elsif ($up->{'section_next'}) {
        $directions->{'FastForward'} = $up->{'section_next'}->{'parent'};
      }
      # if the element isn't at the highest level, fastback is the 
      # highest parent element
      if ($up and $up ne $section) {
        $directions->{'FastBack'} = $up->{'parent'};
      } elsif ($section->{'level'} <= 1) {
        # the element is a top level element, we adjust the next
        # toplevel element fastback
        $directions->{'FastForward'}->{'extra'}->{'directions'}->{'FastBack'}  
          = $element if ($directions->{'FastForward'});
      }
    }
    # Use node up for Up if there is no section up.
    # Not done in the default case.
    if ($self->get_conf('USE_UP_NODE_FOR_ELEMENT_UP')
        and !$directions->{'Up'} and $element->{'extra'}->{'node'}
        and $element->{'extra'}->{'node'}->{'node_up'} 
        and (!$node_top or ($element->{'extra'}->{'node'} ne $node_top))) {
      #print STDERR "Using node for up "._print_element_command_texi($element)."\n";
      my $up_node_element = _node_element($element->{'extra'}->{'node'}->{'node_up'});
      $directions->{'Up'} = $up_node_element if ($up_node_element);
    }
    if ($element->{'extra'}->{'directions'}) {
      %{$element->{'extra'}->{'directions'}} = (%{$element->{'extra'}->{'directions'}}, 
                                                %$directions)
    } else {
      $element->{'extra'}->{'directions'} = $directions;
    }
  }
  if ($self->get_conf('DEBUG')) {
    foreach my $element (@$elements) {
      print STDERR "Directions($element): "
         .Texinfo::Structuring::_print_directions($element)."\n";
    }
  }
}

sub elements_file_directions($$)
{
  my $self = shift;
  my $elements = shift;
  return if (!$elements or !@$elements);

  foreach my $element (@$elements) {
    my $directions;
    my $filename;
    if (defined($element->{'filename'})) {
      $filename = $element->{'filename'};
      my $current_element = $element;
      while ($current_element->{'element_prev'}) {
        $current_element = $current_element->{'element_prev'};
        if (defined($current_element->{'filename'})) {
          if ($current_element->{'filename'} ne $filename) {
            $element->{'extra'}->{'directions'}->{'PrevFile'} = $current_element;
            last;
          }
        } else {
          last;
        }
      }
      $current_element = $element;
      while ($current_element->{'element_next'}) {
        $current_element = $current_element->{'element_next'};
        if (defined($current_element->{'filename'})) {
          if ($current_element->{'filename'} ne $filename) {
            $element->{'extra'}->{'directions'}->{'NextFile'} = $current_element;
            last;
          }
        } else {
          last;
        }
      }
    }
  }
}

my %sectioning_commands = %Texinfo::Common::sectioning_commands;
# for debugging
sub _print_root_command_texi($)
{
  my $command = shift;
  my $tree;
  if ($command->{'cmdname'}) {
    if ($command->{'cmdname'} eq 'node') {
      $tree = $command->{'extra'}->{'node_content'};
    } elsif ($sectioning_commands{$command->{'cmdname'}}) {
      $tree = $command->{'extra'}->{'misc_content'};
    }
  } else {
    return "Not a root command";
  }
  return '@'.$command->{'cmdname'}. ' '
       .Texinfo::Convert::Texinfo::convert ({'contents' => $tree})
          if ($tree);
  return 'UNDEF @'.$command->{'cmdname'};
}

# for debugging
sub _print_element_command_texi($)
{
  my $element = shift;
  if (!$element) {
    return "UNDEF ELEMENT";
  }
  if (!$element->{'type'}) {
    return "element $element without type: ".
       Texinfo::Parser::_print_current_keys($element);
  }

  if ($element->{'type'} eq 'external_node') {
    my $command = {'contents' => [{'text' => '('}, 
                        @{$element->{'extra'}->{'manual_content'}},
                               {'text' => ')'}]};
    if ($element->{'extra'}->{'node_content'}) {
      unshift @{$command->{'contents'}}, @{$element->{'extra'}->{'node_content'}};
    }
    return Texinfo::Convert::Texinfo::convert($command);
  }
  
  my $command = $element->{'extra'}->{'element_command'};
  if (!defined($command)) {
    # happens when there are only nodes and sections are used as elements
    my $result = "No associated command ";
    $result .= "(type $element->{'type'})" if (defined($element->{'type'}));
    return $result;
  }
  return _print_root_command_texi($command);
}

# for debugging
sub _print_directions($)
{
  my $element = shift;
  my $result = 'element: '._print_element_command_texi($element)."\n";

  if ($element->{'extra'} and $element->{'extra'}->{'directions'}) {
    foreach my $direction (sort(keys(%{$element->{'extra'}->{'directions'}}))) {
      $result .= "  $direction: ".
       _print_element_command_texi($element->{'extra'}->{'directions'}->{$direction})."\n";
    }
  } else {
    $result .= "  NO DIRECTION";
  }
  return $result;
}

# this is used in the test suite, but not likely to be useful in real life.
sub _unsplit($)
{
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'}) {
    return $root;
  }
  foreach my $content (@{$root->{'contents'}}) {
    $content->{'parent'} = $root;
  }
  return $root;
}

# associate internal reference commands like @*ref to labels
sub associate_internal_references($;$$)
{
  my $self = shift;
  my $labels = shift;
  my $refs = shift;
  $labels = $self->labels_information() if (!defined($labels));
  $refs = $self->internal_references_information() if (!defined($refs));
  return if (!defined($refs));
  foreach my $ref (@$refs) {
    if (!defined($labels->{$ref->{'extra'}->{'node_argument'}->{'normalized'}})
         and !$self->{'novalidate'}) {
      $self->line_error(sprintf($self->__("\@%s reference to nonexistent node `%s'"),
                           $ref->{'cmdname'}, 
                           Texinfo::Parser::_node_extra_to_texi(
                                $ref->{'extra'}->{'node_argument'})), 
                        $ref->{'line_nr'})
    } else {
      my $node_target 
        = $labels->{$ref->{'extra'}->{'node_argument'}->{'normalized'}};
      $ref->{'extra'}->{'label'} = $node_target;
      if (! $self->{'novalidate'} and ! _check_node_same_texinfo_code(
          $node_target, $ref->{'extra'}->{'node_argument'})) {
        $self->line_warn(sprintf($self->
           __("\@%s to `%s', different from %s name `%s'"), 
           $ref->{'cmdname'},
           Texinfo::Parser::_node_extra_to_texi(
              $ref->{'extra'}->{'node_argument'}),
           $node_target->{'cmdname'},
           Texinfo::Parser::_node_extra_to_texi(
                        $node_target->{'extra'})),
          $ref->{'line_nr'});
      }
    }
  }
}

sub number_floats($)
{
  my $floats = shift;
  return if (!defined($floats));
  foreach my $style (keys(%$floats)) {
    my %nr_in_chapter;
    my $float_index = 0;
    foreach my $float (@{$floats->{$style}}) {
      next if (!$float->{'extra'} 
               or !defined($float->{'extra'}->{'normalized'}));
      $float_index++;
      my $number;
      if ($float->{'float_section'}) {
        my $up = $float->{'float_section'};
        while ($up->{'section_up'} 
               #and $command_structuring_level{$up->{'cmdname'}} 
               and defined($up->{'section_up'}->{'cmdname'})
               and $command_structuring_level{$up->{'section_up'}->{'cmdname'}}) {
          $up = $up->{'section_up'};
        }
        if (!$unnumbered_commands{$up->{'cmdname'}}) {
          $nr_in_chapter{$up->{'number'}}++;
          $number = $up->{'number'} . '.' . $nr_in_chapter{$up->{'number'}};
        }
      }
      $number = $float_index if (!defined($number));
      $float->{'number'} = $number;
    }
  }
}

sub _reference_to_arg($$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;

  if ($current->{'cmdname'} and 
      $Texinfo::Common::ref_commands{$current->{'cmdname'}}
      and $current->{'extra'} 
      and $current->{'extra'}->{'brace_command_contents'}) {
    my @args_try_order;
    if ($current->{'cmdname'} eq 'inforef') {
      @args_try_order = (0, 1, 2);
    } else {
      @args_try_order = (0, 1, 2, 4, 3);
    }
    foreach my $index (@args_try_order) {
      if (defined($current->{'args'}->[$index]) 
          and defined($current->{'extra'}->{'brace_command_contents'}->[$index])) {
        # This a double checking that there is some content.
        # Not sure that it is useful.
        my $text = Texinfo::Convert::Text::convert($current->{'args'}->[$index]);
        if (defined($text) and $text =~ /\S/) {
          my $result = {'contents' => 
                $current->{'extra'}->{'brace_command_contents'}->[$index],
                        'parent' => $current->{'parent'}};
          return ($result);
        }
      }
    }
    return {'text' => '', 'parent' => $current->{'parent'}};
  } else {
    return ($current);
  }
}

sub reference_to_arg_in_tree($$)
{
  my $self = shift;
  my $tree = shift;
  return Texinfo::Common::modify_tree($self, $tree, \&_reference_to_arg);
}

# prepare a new node and register it
sub _new_node($$)
{
  my $self = shift;
  my $node_tree = shift;

  $node_tree = Texinfo::Common::protect_comma_in_tree($node_tree);
  $node_tree->{'contents'} 
     = Texinfo::Common::protect_first_parenthesis($node_tree->{'contents'});
  $node_tree = reference_to_arg_in_tree($self, $node_tree);

  my $empty_node = 0;
  if (!$node_tree->{'contents'} 
      or !scalar(@{$node_tree->{'contents'}})) {
    $node_tree->{'contents'} = [{'text' => ''}];
    $empty_node = 1;
  }

  unless (($node_tree->{'contents'}->[-1]->{'cmdname'}
       and ($node_tree->{'contents'}->[-1]->{'cmdname'} eq 'c'
            or $node_tree->{'contents'}->[-1]->{'cmdname'} eq 'comment'))
      or (defined($node_tree->{'contents'}->[-1]->{'text'})
          and $node_tree->{'contents'}->[-1]->{'text'} =~ /\n/)) {
    push @{$node_tree->{'contents'}}, 
           {'type' => 'spaces_at_end', 'text' => "\n"};
  }

  my $appended_number = 0 +$empty_node;
  my ($node, $parsed_node);

  while (!defined($node) 
         or ($self->{'labels'} 
            and $self->{'labels'}->{$parsed_node->{'normalized'}})) {
    $node = {'cmdname' => 'node', 'args' => [{}]};
    my $node_arg = $node->{'args'}->[0];
    $node_arg->{'parent'} = $node;
    @{$node_arg->{'contents'}} = (
       {'extra' => {'command' => $node},
        'text' => ' ',
        'type' => 'empty_spaces_after_command'},
        @{$node_tree->{'contents'}});
    if ($appended_number) {
      splice (@{$node_arg->{'contents'}}, -1, 0,
                  {'text' => " $appended_number"});
    }
    foreach my $content (@{$node_arg->{'contents'}}) {
      $content->{'parent'} = $node_arg;
    }
    $parsed_node = Texinfo::Parser::_parse_node_manual($node_arg);
    if (!defined($parsed_node) or !$parsed_node->{'node_content'}
        or $parsed_node->{'normalized'} !~ /[^-]/) {
      if ($appended_number) {
        return undef;
      } else {
        $node = undef;
      }
    }
    $appended_number++;
  }

  push @{$node->{'extra'}->{'nodes_manuals'}}, $parsed_node;
  if (!Texinfo::Parser::_register_label($self, $node, $parsed_node, undef)) {
    print STDERR "BUG: node unique, register failed:  $parsed_node->{'normalized'}\n";
  }
  push @{$self->{'nodes'}}, $node;
  return $node;
}

# reassociate a tree element to the new node, from previous node
sub _reassociate_to_node($$$$)
{
  my $self = shift;
  my $type = shift;
  my $current = shift;
  my $nodes = shift;
  my ($new_node, $previous_node) = @{$nodes};

  if ($current->{'cmdname'} and $current->{'cmdname'} eq 'menu') {
    if ($previous_node) {
      if (!$previous_node->{'menus'} or !@{$previous_node->{'menus'}}
           or !grep {$current eq $_} @{$previous_node->{'menus'}}) {
        print STDERR "Bug: menu $current not in previous node $previous_node\n";
      } else {
        @{$previous_node->{'menus'}} = grep {$_ ne $current} @{$previous_node->{'menus'}};
        delete $previous_node->{'menus'} if !(@{$previous_node->{'menus'}});
      }
    } else {
      my $info = $self->global_informations();
      if (!$info or !$info->{'unassociated_menus'} 
          or !@{$info->{'unassociated_menus'}}
          or !grep {$current eq $_} @{$info->{'unassociated_menus'}}) {
        print STDERR "Bug: menu $current not in unassociated menus\n";
      } else {
        @{$info->{'unassociated_menus'}} 
          = grep {$_ ne $current} @{$info->{'unassociated_menus'}};
        delete $info->{'unassociated_menus'} if !(@{$info->{'unassociated_menus'}});
      }
    }
    push @{$new_node->{'menus'}}, $current;
  } elsif ($current->{'extra'} and $current->{'extra'}->{'index_entry'}) {
    if ($previous_node 
        and (!$current->{'extra'}->{'index_entry'}->{'node'}
             or $current->{'extra'}->{'index_entry'}->{'node'} ne $previous_node)) {
      print STDERR "Bug: index entry $current (".
        Texinfo::Convert::Texinfo::convert ({'contents' => $current->{'extra'}->{'index_entry'}->{'content'}})
         .") not in previous node $previous_node\n";
      print STDERR "  previous node: "._print_root_command_texi($previous_node)."\n";
      if ($current->{'extra'}->{'index_entry'}->{'node'}) {
        print STDERR "  current node: ".
         _print_root_command_texi($current->{'extra'}->{'index_entry'}->{'node'})."\n";
      } else {
        print STDERR "  current node not set\n";
      }
    }
    $current->{'extra'}->{'index_entry'}->{'node'} = $new_node;
  }
  return ($current);
}

sub insert_nodes_for_sectioning_commands($$)
{
  my $self = shift;
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'}) {
    return (undef, undef);
  }
  my @added_nodes;
  my @contents;
  my $previous_node;
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'} and $content->{'cmdname'} ne 'node'
        and $content->{'cmdname'} ne 'bye'
        and $content->{'cmdname'} ne 'part'
        and not ($content->{'extra'} 
                 and $content->{'extra'}->{'associated_node'})) {
      my $new_node_tree;
      if ($content->{'cmdname'} eq 'top') {
        $new_node_tree = {'contents' => [{'text' => 'Top'}]};
      } else {
        $new_node_tree = Texinfo::Common::copy_tree({'contents' 
          => $content->{'extra'}->{'misc_content'}});
      }
      my $new_node = _new_node($self, $new_node_tree);
      if (defined($new_node)) {
        push @contents, $new_node;
        push @added_nodes, $new_node;
        $new_node->{'extra'}->{'associated_section'} = $content;
        $content->{'extra'}->{'associated_node'} = $new_node;
        $new_node->{'parent'} = $content->{'parent'};
        # reassociate index entries and menus
        Texinfo::Common::modify_tree($self, $content, \&_reassociate_to_node,
                                     [$new_node, $previous_node]);
      }
    }
    # check normalized to avoid erroneous nodes, such as duplicates
    $previous_node = $content 
      if ($content->{'cmdname'} 
          and $content->{'cmdname'} eq 'node'
          and $content->{'extra'}->{'normalized'});
    push @contents, $content;
  }
  return (\@contents, \@added_nodes);
}

sub _copy_contents($)
{
  my $contents = shift;
  if (ref($contents) ne 'ARRAY') {
    cluck "$contents not an array";
    return undef;
  }
  my $copy = Texinfo::Common::copy_tree({'contents' => $contents});
  return $copy->{'contents'};
}

sub _new_node_menu_entry($$)
{
  my $self = shift;
  my $node_contents = shift;

  my $entry = {'type' => 'menu_entry'};

  my $menu_entry_node = {'type' => 'menu_entry_node'};
  $menu_entry_node->{'contents'}
    = _copy_contents ($node_contents);

  foreach my $content (@{$menu_entry_node->{'contents'}}) {
    $content->{'parent'} = $menu_entry_node;
  }
  Texinfo::Common::protect_colon_in_tree($menu_entry_node);

  my $description = {'type' => 'menu_entry_description'};
  $description->{'contents'}->[0] = {'type' => 'preformatted',
                                     'parent' => $description};
  $description->{'contents'}->[0]->{'contents'}->[0] = {'text' =>"\n",
         'parent' => $description->{'contents'}->[0]};
  $entry->{'args'} 
   = [{'text' => '* ', 'type' => 'menu_entry_leading_text'},
     $menu_entry_node, 
     {'text' => '::', 'type' => 'menu_entry_separator'},
     $description];
  foreach my $arg(@{$entry->{'args'}}) {
    $arg->{'parent'} = $entry;
  }
  $self->Texinfo::Parser::_register_extra_menu_entry_information($entry);
  return $entry;
}

sub _new_block_command($$$)
{
  my $block_contents = shift;
  my $parent = shift;
  my $command_name = shift;

  my $end = {'cmdname' => 'end', 'extra' => 
                 {'command_argument' => $command_name,
                  'text_arg' => $command_name}};
  push @{$end->{'args'}},
    {'type' => 'misc_line_arg', 'parent' => $end};
  push @{$end->{'args'}->[0]->{'contents'}},
          ({'type' => 'empty_spaces_after_command',
           'text' => ' ',
           'extra' => {'command' => $end},
           'parent' => $end->{'args'}->[0]},
          {'text' => $command_name, 'parent' => $end->{'args'}->[0]},
          {'type' => 'spaces_at_end', 'text' => "\n", 
           'parent' => $end->{'args'}->[0]});
  my $new_block = {'cmdname' => $command_name, 'parent' => $parent,
                  'extra'=>{'end_command' => $end}};
  $end->{'extra'}->{'command'} = $new_block;
  $new_block->{'contents'} = [{'extra' => 
                                     {'command' => $new_block},
                              'type' => 'empty_line_after_command',
                              'text' => "\n"},
                              @$block_contents, $end];
  foreach my $content (@{$new_block->{'contents'}}) {
    $content->{'parent'} = $new_block;
  }
  return $new_block;
}

sub complete_node_menu($$)
{
  my $self = shift;
  my $node = shift;

  my @node_childs;
  if ($node->{'extra'}->{'associated_section'}->{'section_childs'}) {
    foreach my $child (@{$node->{'extra'}->{'associated_section'}->{'section_childs'}}) {
      if ($child->{'extra'} and $child->{'extra'}->{'associated_node'}) {
        push @node_childs, $child->{'extra'}->{'associated_node'};
      }
    }
  }
  # Special case for @top.  Gather all the children of the @part following
  # @top.
  if ($node->{'extra'}->{'associated_section'}->{'cmdname'} eq 'top') {
    my $current = $node->{'extra'}->{'associated_section'};
    while ($current->{'section_next'}) {
      $current = $current->{'section_next'};
      if ($current->{'cmdname'} and $current->{'cmdname'} eq 'part'
          and $current->{'section_childs'}) {
        foreach my $child (@{$current->{'section_childs'}}) {
          if ($child->{'extra'} and $child->{'extra'}->{'associated_node'}) {
            push @node_childs, $child->{'extra'}->{'associated_node'};
          }
        }
      } elsif ($current->{'extra'}->{'associated_node'}) {
        # for @appendix, and what follows, as it stops a @part, but is 
        # not below @top
        push @node_childs, $current->{'extra'}->{'associated_node'};
      }
    }
  }
  if (scalar(@node_childs)) {
    my %existing_entries;
    if ($node->{'menus'} and @{$node->{'menus'}}) {
      foreach my $menu (@{$node->{'menus'}}) {
        foreach my $entry (@{$menu->{'contents'}}) {
          if ($entry->{'type'} and $entry->{'type'} eq 'menu_entry') {
            my $entry_node = $entry->{'extra'}->{'menu_entry_node'};
            if (! $entry_node->{'manual_content'}
                and defined($entry_node->{'normalized'})) {
              $existing_entries{$entry_node->{'normalized'}} 
                = [$menu, $entry];
            }
          }
        }
      }
    }
    #print STDERR join('|', keys(%existing_entries))."\n";
    my @pending;
    my $current_menu;
    foreach my $node_entry (@node_childs) {
      if ($existing_entries{$node_entry->{'extra'}->{'normalized'}}) {
        my $entry;
        ($current_menu, $entry) 
         = @{$existing_entries{$node_entry->{'extra'}->{'normalized'}}};
        if (@pending) {
          my $index;
          for ($index = 0; $index < scalar(@{$current_menu->{'contents'}}); $index++) {
            #print STDERR "$index, ".scalar(@{$current_menu->{'contents'}})."\n";
            last if ($current_menu->{'contents'}->[$index] eq $entry);
          }
          splice (@{$current_menu->{'contents'}}, $index, 0, @pending);
          foreach my $entry (@pending) {
            $entry->{'parent'} = $current_menu;
          }
          @pending = ();
        }
      } else {
        my $entry = _new_node_menu_entry($self, 
                              $node_entry->{'extra'}->{'node_content'});
        push @pending, $entry;
      }
    }
    if (scalar(@pending)) {
      if (!$current_menu) {
        my $section = $node->{'extra'}->{'associated_section'};
        $current_menu = _new_block_command (\@pending, $section, 'menu');
        push @{$section->{'contents'}}, $current_menu;
        push @{$section->{'contents'}}, {'type' => 'empty_line',
                                         'text' => "\n", 
                                         'parent' => $section};
        push @{$node->{'menus'}}, $current_menu;
      } else {
        foreach my $entry (@pending) {
          $entry->{'parent'} = $current_menu;
        }
        my $end;
        if ($current_menu->{'contents'}->[-1]->{'cmdname'}
            and $current_menu->{'contents'}->[-1]->{'cmdname'} eq 'end') {
          $end = pop @{$current_menu->{'contents'}};
        }
        push @{$current_menu->{'contents'}}, @pending;
        push @{$current_menu->{'contents'}}, $end if ($end);
      }
    }
  }
}

# This should be called after sectioning_structure
sub complete_tree_nodes_menus($$)
{
  my $self = shift;
  my $root = shift;
  if (!$root->{'type'} or $root->{'type'} ne 'document_root'
      or !$root->{'contents'}) {
    return undef;
  }
  foreach my $content (@{$root->{'contents'}}) {
    if ($content->{'cmdname'} and $content->{'cmdname'} eq 'node'
        and (scalar(@{$content->{'extra'}->{'nodes_manuals'}}) == 1)
        and $content->{'extra'} 
        and $content->{'extra'}->{'associated_section'}) {
      complete_node_menu($self, $content);
    }
  }
}

sub _print_down_menus($$;$);
sub _print_down_menus($$;$)
{
  my $self = shift;
  my $node = shift;
  my $labels = shift;
  $labels = $self->labels_information() if (!defined($labels));

  my @master_menu_contents;

  if ($node->{'menus'} and scalar(@{$node->{'menus'}})) {
    my @node_children;
    foreach my $menu (@{$node->{'menus'}}) {
      foreach my $entry (@{$menu->{'contents'}}) {
        if ($entry->{'type'} and $entry->{'type'} eq 'menu_entry') {
          push @master_menu_contents, Texinfo::Common::copy_tree($entry);
          # gather node children to recusrsively print their menus
          my $entry_node = $entry->{'extra'}->{'menu_entry_node'};
          if (! $entry_node->{'manual_content'}
              and defined($entry_node->{'normalized'})) {
            my $node = $labels->{$entry_node->{'normalized'}};
            if (defined($node) and $node->{'extra'}) {
              push @node_children, $node;
            }
          }
        }
      }
    }
    if (scalar(@master_menu_contents)) {
      # Prepend node title
      my $node_title_contents;
      if ($node->{'extra'}->{'associated_section'}
          and $node->{'extra'}->{'associated_section'}->{'extra'}
          and $node->{'extra'}->{'associated_section'}->{'extra'}->{'misc_content'}) {
        $node_title_contents
          = _copy_contents($node->{'extra'}->{'associated_section'}->{'extra'}->{'misc_content'});
      } else {
        $node_title_contents = _copy_contents($node->{'extra'}->{'node_content'});
      }
      my $menu_comment = {'type' => 'menu_comment'};
      $menu_comment->{'contents'}->[0] = {'type' => 'preformatted',
                                          'parent' => $menu_comment};
    
      $menu_comment->{'contents'}->[0]->{'contents'}
        = [{'text' => "\n", 'type' => 'empty_line'}, @$node_title_contents,
           {'text' => "\n", 'type' => 'empty_line'},
           {'text' => "\n", 'type' => 'empty_line'}];
      foreach my $content (@{$menu_comment->{'contents'}->[0]->{'contents'}}) {
        $content->{'parent'} = $menu_comment->{'contents'}->[0];
      }
      unshift @master_menu_contents, $menu_comment;

      # now recurse in the children
      foreach my $child (@node_children) {
        push @master_menu_contents, _print_down_menus($self, $child, $labels);
      }
    }
  }
  return @master_menu_contents;
}

sub new_master_menu($;$)
{
  my $self = shift;
  my $labels = shift;
  $labels = $self->labels_information() if (!defined($labels));
  my $node = $labels->{'Top'};
  return undef if (!defined($node));

  my @master_menu_contents;
  if ($node->{'menus'} and scalar(@{$node->{'menus'}})) {
    foreach my $menu (@{$node->{'menus'}}) {
      foreach my $entry (@{$menu->{'contents'}}) {
        if ($entry->{'type'} and $entry->{'type'} eq 'menu_entry') {
          my $entry_node = $entry->{'extra'}->{'menu_entry_node'};
          if (! $entry_node->{'manual_content'}
              and defined($entry_node->{'normalized'})) {
            my $node = $labels->{$entry_node->{'normalized'}};
            if (defined($node) and $node->{'extra'}) {
              push @master_menu_contents, _print_down_menus($self, 
                                                            $node, $labels);
            }
          }
        }
      }
    }
  }
  if (scalar(@master_menu_contents)) {
    my $first_preformatted = $master_menu_contents[0]->{'contents'}->[0];
    my $master_menu_title = $self->gdt(' --- The Detailed Node Listing ---');
    my @master_menu_title_contents;
    foreach my $content (@{$master_menu_title->{'contents'}}, {'text' => "\n"}) {
      $content->{'parent'} = $first_preformatted;
      push @master_menu_title_contents, $content;
    }
    unshift @{$first_preformatted->{'contents'}}, @master_menu_title_contents;
    return _new_block_command(\@master_menu_contents, undef, 'detailmenu');
  } else {
    return undef;
  }
}

sub regenerate_master_menu($;$)
{
  my $self = shift;
  my $labels = shift;
  $labels = $self->labels_information() if (!defined($labels));
  my $top_node = $labels->{'Top'};
  return undef if (!defined($top_node));

  my $new_master_menu = new_master_menu($self, $labels);
  return undef if (!defined($new_master_menu) or !$top_node->{'menus'}
                   or !scalar(@{$top_node->{'menus'}}));

  foreach my $menu (@{$top_node->{'menus'}}) {
    my $detailmenu_index = 0;
    foreach my $entry (@{$menu->{'contents'}}) {
      if ($entry->{'cmdname'} and $entry->{'cmdname'} eq 'detailmenu') {
        # replace existing detailmenu by the master menu
        $new_master_menu->{'parent'} = $menu;
        splice (@{$menu->{'contents'}}, $detailmenu_index, 1, 
                $new_master_menu);
        return 1;
      }
      $detailmenu_index++;
    }
  }

  my $last_menu = $top_node->{'menus'}->[-1];
  my $index = scalar(@{$last_menu->{'contents'}});
  if ($index
      and $last_menu->{'contents'}->[$index-1]->{'cmdname'}
      and $last_menu->{'contents'}->[$index-1]->{'cmdname'} eq 'end') {
    $index --;
  }
  $new_master_menu->{'parent'} = $last_menu;
  if ($index
      and $last_menu->{'contents'}->[$index-1]->{'type'}
      and $last_menu->{'contents'}->[$index-1]->{'type'} eq 'menu_comment'
      and $last_menu->{'contents'}->[$index-1]->{'contents'}->[-1]->{'type'}
      and $last_menu->{'contents'}->[$index-1]->{'contents'}->[-1]->{'type'}
             eq 'preformatted') {
    my $empty_line = {'type' => 'empty_line', 'text' => "\n", 'parent' =>
               $last_menu->{'contents'}->[$index-1]->{'contents'}->[-1]};
    push @{$last_menu->{'contents'}->[$index-1]->{'contents'}}, $empty_line;
  } elsif ($index
           and $last_menu->{'contents'}->[$index-1]->{'type'}
           and $last_menu->{'contents'}->[$index-1]->{'type'} eq 'menu_entry') {
    my $menu_comment = {'type' => 'menu_comment', 'parent' => $last_menu};
    splice (@{$last_menu->{'contents'}}, $index, 0, $menu_comment);
    $index++;
    my $preformatted = {'type' => 'preformatted', 'parent' => $menu_comment};
    push @{$menu_comment->{'contents'}}, $preformatted;
    my $empty_line = {'type' => 'after_description_line', 'text' => "\n",
                      'parent' => $preformatted};
    push @{$preformatted->{'contents'}}, $empty_line;
  }
  splice (@{$last_menu->{'contents'}}, $index, 0, $new_master_menu);

  return 1;
}

sub _sort_string($$)
{
  my $a = shift;
  my $b = shift;
  return (($a =~ /^[[:alpha:]]/ and $b =~ /^[[:alpha:]]/)
              or ($a !~ /^[[:alpha:]]/ and $b !~ /^[[:alpha:]]/))
              ? ($a cmp $b)
                : (($a =~ /^[[:alpha:]]/ && 1) || -1);
}

sub _sort_index_entries($$)
{
  my $key1 = shift;
  my $key2 = shift;
  my $a = uc($key1->{'key'});
  my $b = uc($key2->{'key'});
  my $res = _sort_string($a, $b);
  if ($res == 0) {
    $res = ($key1->{'number'} <=> $key2->{'number'});
  }
  return $res;
}

sub _sort_index_entries_in_letter($$)
{
  my $key1 = shift;
  my $key2 = shift;
  my $a = uc($key1->{'key'});
  my $b = uc($key2->{'key'});
  my $res = ($a cmp $b);
  if ($res == 0) {
    $res = ($key1->{'number'} <=> $key2->{'number'});
  }
  return $res;
}

sub _do_index_keys($$$)
{
  my $self = shift;
  my $index_entries = shift;
  my $index_names = shift;

  my $options = {'sort_string' => 1};
  if ($self->get_conf('ENABLE_ENCODING') 
      and $self->get_conf('INPUT_ENCODING_NAME')) {
    $options->{'enabled_encoding'} = $self->get_conf('INPUT_ENCODING_NAME');
  }
  my %convert_text_options = Texinfo::Common::_convert_text_options($self);
  $options = {%$options, %convert_text_options};
  foreach my $index_name (keys(%$index_entries)) {
    foreach my $entry (@{$index_entries->{$index_name}}) {
      $entry->{'in_code'} = $index_names->{$entry->{'index_name'}}->{'in_code'};
      $options->{'code'} = $entry->{'in_code'};
      $entry->{'key'} = Texinfo::Convert::Text::convert(
                              {'contents' => $entry->{'content'}}, $options);
      if ($entry->{'key'} !~ /\S/) {
        $self->line_warn(sprintf($self->__("empty index key in \@%s"), 
                                 $entry->{'index_at_command'}),
                        $entry->{'command'}->{'line_nr'});
      }
    }
  }
}

sub sort_indices($$$)
{
  my $self = shift;
  my $index_entries = shift;
  my $index_names = shift;
  my $sorted_index_entries;
  _do_index_keys($self, $index_entries, $index_names);
  foreach my $index_name (keys(%$index_entries)) {
    @{$sorted_index_entries->{$index_name}} = 
        sort _sort_index_entries 
            grep {$_->{'key'} =~ /\S/} @{$index_entries->{$index_name}};
  }
  return $sorted_index_entries;
}

sub sort_indices_by_letter($$$)
{
  my $self = shift;
  my $index_entries = shift;
  my $index_names = shift;
  my $indices_sorted_by_letters;
  _do_index_keys($self, $index_entries, $index_names);
  foreach my $index_name (keys(%$index_entries)) {
    my $index_letter_hash;
    foreach my $index_entry (@{$index_entries->{$index_name}}) {
      next if ($index_entry->{'key'} !~ /\S/);
      my $letter = uc(substr($index_entry->{'key'}, 0, 1));
      push @{$index_letter_hash->{$letter}}, $index_entry;
    }
    foreach my $letter (sort _sort_string (keys %$index_letter_hash)) {
      my @sorted_letter_entries 
         = sort _sort_index_entries_in_letter @{$index_letter_hash->{$letter}};
      push @{$indices_sorted_by_letters->{$index_name}},
        { 'letter' => $letter, 'entries' => \@sorted_letter_entries }; 
    }
  }
  return $indices_sorted_by_letters;
}

sub merge_indices($)
{
  my $index_names = shift;

  my $merged_index_entries;
  foreach my $index_name (keys(%$index_names)) {
    my $index_info = $index_names->{$index_name};
    #print STDERR "MERGE_INDICES: $index_name\n";
    next if ($index_info->{'merged_in'});
    foreach my $contained_index (keys (%{$index_info->{'contained_indices'}})) {
      #print STDERR "MERGE_INDICES: $index_name, prefix $index_prefix\n";
      if ($index_names->{$contained_index}->{'index_entries'}) {
        #print STDERR "MERGE_INDICES: final $index_name <- $index_prefix\n";
        push @{$merged_index_entries->{$index_name}},
          @{$index_names->{$contained_index}->{'index_entries'}};
      }
    }
  }
  return $merged_index_entries;
}

# modify the menu tree to put description and menu comment content
# together directly in the menu.  Put the menu_entry in a preformatted.
# last merge preformatted.
sub menu_to_simple_menu($);

sub menu_to_simple_menu($)
{
  my $menu = shift;
  
  my @contents;
  foreach my $content (@{$menu->{'contents'}}) {
    if ($content->{'type'} and $content->{'type'} eq 'menu_comment') {
      push @contents, @{$content->{'contents'}};
    } elsif ($content->{'type'} and $content->{'type'} eq 'menu_entry') {
      my $preformatted = {'type' => 'preformatted', 'contents' => [$content]};
      push @contents, $preformatted;
      $content->{'parent'} = $preformatted;

      my $in_description;
      my @args = @{$content->{'args'}};
      @{$content->{'args'}} = ();
      while (@args) {
        if ($args[0]->{'type'} and $args[0]->{'type'} eq 'menu_entry_description') {
          my $description = shift @args;
          push @contents, @{$description->{'contents'}};
          push @contents, @args;
          last;
        } else {
          my $arg = shift @args;
          push @{$content->{'args'}}, $arg;
        }
      }
    } elsif ($content->{'cmdname'}
             and $Texinfo::Common::menu_commands{$content->{'cmdname'}}) {
      menu_to_simple_menu($content);
      push @contents, $content;
    } else {
      push @contents, $content;
    }
  }
  
  # reset parent, put in menu and merge preformatted.
  @{$menu->{'contents'}} = ();
  my $current_preformatted;
  foreach my $content (@contents) {
    $content->{'parent'} = $menu;
    if ($content->{'type'} and $content->{'type'} eq 'preformatted') {
      if (!defined($current_preformatted)) {
        $current_preformatted = $content;
        push @{$menu->{'contents'}}, $content;
      } else {
        foreach my $preformatted_content (@{$content->{'contents'}}) {
          push @{$current_preformatted->{'contents'}}, $preformatted_content;
          $preformatted_content->{'parent'} = $current_preformatted;
        }
      }
    } else {
      $current_preformatted = undef;
      push @{$menu->{'contents'}}, $content;
    }
  }
}

sub set_menus_to_simple_menu($)
{
  my $self = shift;

  if ($self->{'info'} and $self->{'info'}->{'unassociated_menus'}) {
    foreach my $menu (@{$self->{'info'}->{'unassociated_menus'}}) {
      menu_to_simple_menu($menu);
    }
  }
  if ($self->{'nodes'} and @{$self->{'nodes'}}) {
    foreach my $node (@{$self->{'nodes'}}) {
      if ($node->{'menus'}) {
        foreach my $menu (@{$node->{'menus'}}) {
          menu_to_simple_menu($menu);
        }
      }
    }
  }
}

1;

__END__

#Last,
#C<output_internal_links> may be used to output element and
#index entries references, mostly for HTML output.

=head1 NAME

Texinfo::Structuring - informations and transformations in Texinfo::Parser tree

=head1 SYNOPSIS

  use Texinfo::Structuring qw(sectioning_structure nodes_tree number_floats
    associate_internal_references split_by_node split_by_section split_pages
    merge_indices sort_indices_by_letter sort_indices elements_directions
    elements_file_directions);
  # $tree is a Texinfo document tree.  $parser is a Texinfo::Parser object.
  my $sections_root = sectioning_structure ($parser, $tree);
  my $top_node = nodes_tree($parser);
  number_floats($parser->floats_information());
  associate_internal_references($parser);
  my $elements;
  if ($split_at_nodes) {
    $elements = split_by_node($tree);
  } else {
    $elements = split_by_section($tree);
  }
  split_pages($elements, $split);
  elements_directions($parser, $elements);
  elements_file_directions($parser, $elements);

  my ($index_names, $merged_indices) 
     = $parser->indices_information();
  my $merged_index_entries
     = merge_indices($index_names);
  my $index_entries_sorted;
  if ($sort_by_letter) {
    $index_entries_sorted = sort_indices_by_letter($parser,
                                       $merged_index_entries, $index_names);
  } else {
    $index_entries_sorted = sort_indices($parser, $merged_index_entries,
                                         $index_names);
  }
  
  
=head1 DESCRIPTION

Texinfo::Structuring first allows to collect informations on a Texinfo tree.
In most case, it also requires a parser object to do that job.  Thanks to
C<sectioning_structure> the hierarchy of sectioning commands is determined.
The node and menus tree is analysed with C<nodes_tree>.  Floats get their 
standard numbering with C<number_floats> and internal references are matched
up with nodes, floats or anchors with C<associate_internal_references>.

It is also possible to group the top-level contents of the tree, which consist
in nodes and sectioning commands into elements that group together a node and
the next sectioning element.  With C<split_by_node> nodes are considered
to be the main sectionning elements, while with C<split_by_section> the 
sectioning command elements are the main elements.  The first mode is typical
of Info format, while the second correspond to a traditional book.
The elements may be further split in I<pages>, which are not pages as
in book pages, but more like web pages, and hold series of elements.

The elements may have directions to other elements prepared 
by C<elements_directions>.  C<elements_file_directions> should also
set direction related to files, provided files are associated with 
elements by the user.

C<merge_indices> may be used to merge indices, which may be sorted
with C<sort_indices> or C<sort_indices_by_letter> to sort by letters.

Other miscellaneous methods include C<set_menus_to_simple_menu> and
C<menu_to_simple_menu> to change the menu texinfo tree, as well
as C<insert_nodes_for_sectioning_commands> that adds nodes for 
sectioning commands without nodes and C<complete_tree_nodes_menus>
that completes the node menus based on the sectioning tree.



=head1 METHODS

No method is exported in the default case.

Most of those function references takes a Texinfo::Parser object
as argument, see L<Texinfo::Parser>.

=over

=item $sections_root = sectioning_structure ($parser, $tree)

This function goes through the tree and gather information on
the document structure for sectioning commands.  It returns the 
root of the sectioning commands tree.

For section elements, it sets:

=over

=item level

The level in the sectioning tree hierarchy.  0 is for C<@top> or 
C<@part>, 1 for C<@chapter>, C<@appendix>...  This level is corrected
by C<@raisesections> and C<@lowersections>.

=item number

The sectioning element number.

=item section_childs

An array holding sectioning elements children of the element.

=item section_up

=item section_prev

=item section_next

The up, previous and next sectioning elements.

=item toplevel_next

=item toplevel_prev

=item toplevel_up

The next and previous and up sectioning elements of toplevel sectioning
elements (like C<@top>, C<@chapter>, C<@appendix>), not taking into 
account C<@part> elements.

=back

=item my $top_node = nodes_tree($parser)

Goes through menu and nodes and set directions.  Returns the top
node.

This functions sets:

=over

=item menu_child

The first child in the menu of the node.

=item menu_up

=item menu_next

=item menu_prev

Up, next and previous directions as set in menus.

=item node_up

=item node_prev

=item node_next

Up, next and previous directions for the node.

=back

=item number_floats($float_information)

Number the floats as described in the Texinfo manual.  Sets
the I<number> key of the float tree elements.

=item associate_internal_references($parser)

Verify that internal references (C<@ref> and similar without
fourth of fifth argument) have an associated node, anchor or float.
Set the I<label> key in the I<extra> hash of the reference tree
element to the associated labeled tree element.

=item $elements = split_by_node($tree)

Returns a reference array of elements where a node is associated to
the following sectioning commands.  Sectioning commands without nodes
are also with the previous node, while nodes without sectioning commands
are alone in their elements.

Elements are regular tree items with type I<element>, the
associated nodes and sectioning tree items are in the array associated
with the I<contents> key.  They have directions, namely I<element_next>
and I<element_prev> pointing to the previous and the next element.

In the I<extra> hash they have

=over

=item no_node

A special case, if there are no nodes in the document, the value is set.

=item node

=item element_command

The node command associated with the element.

=item section

The sectioning command associated with the element node.

=back

=item $elements = split_by_section($tree) 

Similarly with C<split_by_node>, returns an array of elements.  This time,
lone nodes are associated with the previous sections and lone sections
makes up an element.

The extra hash keys set are the same, except that I<element_command> is 
the sectioning command associated with the element, and I<no_node> is 
replaced by I<no_section>.

=item $pages = split_pages($elements, $split)

The elements from the array reference argument have an extra I<first_in_page>
value set to the first element on the unit, and based on the
value of I<$split>.  The possible values for I<$split> are

=over

=item chapter

The elements are split at chapter or other toplevel sectioning elements.

=item node

Each element has its own page.

=item section

The elements are split at sectioning commands below chapter.

=item value evaluating to false

No splitting, only one page is returned, holding all the elements.

=back

=item elements_directions($parser, $elements)

Directions are set up for the elements in the array reference given in 
argument.  The corresponding hash reference is in 
C<< {'extra'}->{'directions'} >>
and keys correspond to directions while values are elements.

The following directions are set up:

=over

=item This

The element itself.

=item Forward

Element next.

=item Back

Previous element.

=item NodeForward

Following node element in reading order.  It is the next node, or the 
first in menu or the next of the up node.

=item NodeBack

Preceding node element.

=item NodeUp

=item NodeNext

=item NodePrev

The up, next and previous node elements.

=item Up

=item Next

=item Prev

The up, next and previous section elements.

=item FastForward

The next top level section element.

=item FastBack

For top level elements, the previous top level element.  For other elements
the up top level element.  For example, for a chapter element it is the 
previous chapter, for a subsection element it is the chapter element 
that contains the subsection.

=item FastForward

The next top level element.

=back

=item elements_file_directions($parser, $elements)

In the directions reference described above for C<elements_directions>, sets
the I<PrevFile> and C<NextFile> directions to the elements in previous and
following files.  

The API for association of pages/elements to files is not defined yet.

=item $merged_entries = merge_indices($index_names)

Using informations returned by L<Texinfo::Parser/indices_information>,
a structure holding all the index entries by index name is returned, 
with all the entries of merged indices merged with those of the indice 
merged into.

The I<$merged_entries> returned is a hash reference whose
keys are the index names and values arrays of index entry structures
described in details in L<Texinfo::Parser/index_entries>.

=item $index_entries_sorted = sort_indices_by_letter($parser, $merged_index_entries, $index_names)

=item $index_entries_sorted = sort_indices($parser, $merged_index_entries, $index_names)

These functions first sets a plain text key for each index entry, used for 
sorting.  In both cases, a hash reference with index names as keys is returned.

When sorting by letter, an array reference of letter hash references is 
associated with each index name.  Each letter hash reference has two 
keys, a I<letter> key with the letter, and an I<entries> key with an array
reference of sorted index entries beginning with the letter.

When simply sorting, the array of the sorted indes entries is associated
with the index name.

=item ($root_content, $added_sections) = fill_gaps_in_sectioning ($root)

This function adds empty C<@unnumbered> and similar commands in a tree
to fill gaps in sectioning.  This may be used, for example, when converting 
from a format that can handle gaps in sectioning.  I<$root> is the tree
root.  An array reference is returned, containing the root contents
with added sectioning commands, as well as an array reference containing 
the added sectioning commands.

If the sectioning commands are lowered or raised (with C<@raisesections>,
C<@lowersection>) the tree may be modified with C<@raisesections> or
C<@lowersection> added to some tree elements.

=item menu_to_simple_menu ($menu)

=item set_menus_to_simple_menu ($parser)

C<menu_to_simple_menu> transforms the tree of a menu tree element.  
C<set_menus_to_simple_menu> calls C<menu_to_simple_menu> for all the
menus of the document.

A simple menu has no I<menu_comment>, I<menu_entry> or I<menu_entry_description>
container anymore, their content are merged directly in the menu in 
I<preformatted> container.

=item ($root_content, $added_nodes) = insert_nodes_for_sectioning_commands ($parser, $tree)

Insert nodes for sectioning commands without node in C<$tree>.
An array reference is returned, containing the root contents
with added nodes, as well as an array reference containing the 
added nodes.

=item complete_tree_nodes_menus ($parser, $tree)

Add menu entries or whole menus for nodes associated with sections,
based on the sectioning tree.  This function should therefore be
called after L<sectioning_structure>.

=item $detailmenu = new_master_menu ($parser)

Returns a detailmenu tree element formatted as a master node.

=item regenerate_master_menu ($parser)

Regenerate the Top node master menu, replacing the first detailmenu
in Top node menus or appending at the end of the Top node menu.

=back

=head1 SEE ALSO

L<Texinfo manual|http://www.gnu.org/s/texinfo/manual/texinfo/>, 
L<Texinfo::Parser>.

=head1 AUTHOR

Patrice Dumas, E<lt>pertusus@free.frE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright 2010, 2011, 2012 Free Software Foundation, Inc.

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License,
or (at your option) any later version.

=cut
