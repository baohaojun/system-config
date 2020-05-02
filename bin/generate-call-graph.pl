#!/usr/bin/env perl

use strict;
use String::ShellQuote;

chomp(my $globalrc = qx(lookup-file .globalrc));
if (not -e $globalrc) {
    $globalrc = glob("~/.globalrc");
}

my $langmap = shell_quote(qx(lang-map-for-ctags $globalrc));
open(my $ctags_pipe, "-|", "mkcscope_files | ctags-ajoke --langmap=$langmap -xu --filter --filter-terminator='###terminator###\n' --format=2")
    or die "Can not create ctags program";

my ($last_ctags_output, $last_ctags_def, $last_ctags_type, $last_ctags_linum, $last_ctags_file,
    $this_ctags_output, $this_ctags_def, $this_ctags_type, $this_ctags_linum, $this_ctags_file,
    $current_file_handle, $current_file_line, %def_contains_map,
    );

sub debug(@) {
    print STDERR "@_\n";
}

my $lang_token_regexp = qr/[_0-9a-z]+/i;

sub def_contains($) {
    my ($word) = @_;
    $def_contains_map{$last_ctags_def} = {}
    unless exists $def_contains_map{$last_ctags_def};

    $def_contains_map{$last_ctags_def}{"$last_ctags_file:$last_ctags_linum:$last_ctags_type"} = {}
    unless exists $def_contains_map{$last_ctags_def}{"$last_ctags_file:$last_ctags_linum:$last_ctags_type"};

    $def_contains_map{$last_ctags_def}{"$last_ctags_file:$last_ctags_linum:$last_ctags_type"}{$current_file_line} = {}
    unless exists $def_contains_map{$last_ctags_def}{"$last_ctags_file:$last_ctags_linum:$last_ctags_type"}{$current_file_line};

    $def_contains_map{$last_ctags_def}{"$last_ctags_file:$last_ctags_linum:$last_ctags_type"}{$current_file_line}{$word} = 1;
                 }

open(my $call_graph, ">", "call_graph.dot") or
    die "Can not open call_graph.dot";
sub write_graph(@) {
    print $call_graph @_;
}

write_graph("digraph {\ngraph [ ratio=.5 ];\n");

while (<$ctags_pipe>) {
    chomp;
    if (m/^###terminator###$/) {
        while ($current_file_handle and my $line = <$current_file_handle>) {
            $current_file_line++;
            while ($line =~ m/$lang_token_regexp/g) {
                def_contains($&);
            }
        }
        close($current_file_handle) if $current_file_handle;
        undef $current_file_handle;
        undef $last_ctags_def;
        next;
    }

    ($this_ctags_def, $this_ctags_type, $this_ctags_linum, $this_ctags_file,) = split;
    unless ($this_ctags_linum =~ m/^\d+$/ and -e $this_ctags_file and length($this_ctags_def) > 1 and $this_ctags_def =~ m/$lang_token_regexp/) {
        debug "file $this_ctags_file, line $this_ctags_linum";
        next;
    }
    my $type = $last_ctags_type;
    my $do_record = 0;
    if ($type eq "function" or $type eq "method" or $type eq "class") {
        $do_record = 1;
    }
    unless ($current_file_handle) {
        open($current_file_handle, "<", $this_ctags_file);
        $current_file_line = 0;
    }
    if ($last_ctags_def) {
        while($current_file_line + 1 < $this_ctags_linum and my $line = <$current_file_handle>) {
            $current_file_line++;
            next unless $do_record;
            chomp($line);
            while ($line =~ m/$lang_token_regexp/g) {
                def_contains($&);
            }
        }
    }

    ($last_ctags_def, $last_ctags_type, $last_ctags_linum, $last_ctags_file,) =
        ($this_ctags_def, $this_ctags_type, $this_ctags_linum, $this_ctags_file,);

}

my %graph_map;
my (%calls_map, %called_by_map, %label_map);
foreach my $def (sort keys %def_contains_map) {
    foreach my $def_info (sort keys %{$def_contains_map{$def}}) {
        $def_info =~ m/:\d+:/ or die "def_info format incorrect";
        my ($file, $line, $type) = ($`, $&, $'); #');

        my $label = "\"$def\\n$type on $file$line\"";
        unless (exists $label_map{$label}) {
            $label_map{$label} = "l" . ((keys %label_map) + 0);
            # write_graph("$label_map{$label} [label=$label]\n");
        }

        #write_graph("\"$def\" -> $label_map{$label}\n");

        foreach my $line (sort keys %{$def_contains_map{$def}{$def_info}}) {
            foreach my $word (sort keys %{$def_contains_map{$def}{$def_info}{$line}}) {
                if (exists $def_contains_map{$word} and $word ne $def) {
                    unless (exists $called_by_map{$word}) {
                        $called_by_map{$word} = {};
                    }
                    $called_by_map{$word}{"$file:$line"} = "$def: $def_info";

                    $graph_map{$def} = {} unless exists $graph_map{$def};
                    unless (exists $graph_map{$def}{$word}) {
                        $graph_map{$def}{$word} = 1;
                        foreach my $info (keys %{$def_contains_map{$word}}) {
                            if ($info =~ m/method|function/) {
                                write_graph("\"$def\" -> \"$word\"\n");
                                last;
                            }
                        }
                    }



                    $calls_map{def} = {} unless exists $calls_map{$def};
                    $calls_map{$def}{$def_info} = {} unless exists $calls_map{$def}{$def_info};
                    $calls_map{$def}{$def_info}{$line} = {} unless exists $calls_map{$def}{$def_info}{$line};
                    $calls_map{$def}{$def_info}{$line}{$word} = 1;
                }
            }
        }
    }
}

foreach my $def (sort keys %def_contains_map) {

    print "* $def\n\n";
    foreach my $def_info (sort keys %{$def_contains_map{$def}}) {
        print "** is defined on\n";
        print "*** $def_info\n";
        $def_info =~ m/:\d+:/;
        my $file = $`;
        foreach my $line (sort keys %{$calls_map{$def}{$def_info}}) {
            foreach my $word (sort keys %{$calls_map{$def}{$def_info}{$line}}) {
                print "- calls $word on $file:$line\n";
            }
        }
    }

    print "\n** is called by\n";
    foreach my $file_lino (sort keys %{$called_by_map{$def}}) {
        print "- on $file_lino by $called_by_map{$def}{$file_lino}\n";
    }
    print "\n";
}

write_graph("}\n");
