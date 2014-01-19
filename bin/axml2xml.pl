#!/usr/bin/env perl
###############################################################################
##
## Copyright (C) 2008 Josh Guilfoyle <jasta@devtcg.org>
##
## Quick hack to reverse engineer Android's binary XML file format.  It is
## quite crude and much data is discarded in the format because I did not
## understand it's meaning.  It seems to correctly parse all of the XML
## files distributed with the Android SDK however.
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
###############################################################################

use strict;
use Unicode::String qw(utf16le);
use Data::Dumper;

###############################################################################

# Tweak this if you want to see all the annoying debug output.  Writes to
# STDERR so you can use this in combination with piped XML output.
my $DEBUG = 0;

my $TAG_OPEN = 0x10;
my $TAG_SUPPORTS_CHILDREN = 0x100000;
my $TAG_TEXT = 0x08;

###############################################################################

local $/;
my $data = <>;
my $doc = { data => $data, pos => 0 };

###############################################################################

# Some header, seems to be 3000 8000 always.
my @magic = unpack('vv', read_doc($doc, 4));

# Total file length.
my $length = unpack('V', read_doc($doc, 4));
debug("length=$length\n");

# Unknown, always 0100 1c00
my @unknown1 = unpack('vv', read_doc($doc, 4));

# Seems to be related to the total length of the string table.
my $tlen = unpack('V', read_doc($doc, 4));

# Number of items in the string table, plus some header non-sense?
my $strings = unpack('V', read_doc($doc, 4));
debug("strings=$strings\n");

# Seems to always be 0.
my $unknown2 = unpack('V', read_doc($doc, 4));

# Seems to always be 1.
my $unknown3 = unpack('V', read_doc($doc, 4));

# No clue, relates to the size of the string table?
my $unknown4 = unpack('V', read_doc($doc, 4));

# Seems to always be 0.
my $unknown5 = unpack('V', read_doc($doc, 4));

# Offset in string table of each string.
my @stroffs;

for (my $i = 0; $i < $strings; $i++)
{
        push @stroffs, unpack('V', read_doc($doc, 4));
}

debug(Dumper map { sprintf('%02x', $_) } @stroffs);

my $strings;
my $curroffs = 0;

# The string table looks to have been serialized from a hash table, since
# the positions are not sorted :)
foreach my $offs (sort { $a <=> $b } @stroffs)
{
        die unless $offs == $curroffs;

        my $len = unpack('v', read_doc($doc, 2));

        my $str = read_doc($doc, ($len) * 2);
        debug("str=$str\n");

        # Read the NUL, we're not interested in storing it.
        read_doc($doc, 2);

        $strings->{$offs} = $str;

        $curroffs += (($len + 1) * 2) + 2;
}

my @strings = map { $strings->{$_} } @stroffs;

debugf("curroffs=%d (0x%x)\n", $curroffs, $curroffs);

for (my $i = 0; $i < @strings; $i++)
{
        debugf("0x%02x. %s\n", $i, $strings[$i]);
}

#
# OPEN TAG:
# V=tagS 0x1400 1400 V=1 V=0 {ATTR? V=7 V=attrS V=valS V=attrS|0x3<<24 V=valS 0x0301 1000 V=0x18 V=? } V=~0 V=~0
# V=1    0x0800 0000 V=0x19 0x0201 1000 V=0x38 V=7 V=~0 V=~0
#
# OPEN TAG (normal, child, 3 attributes):
# V=tagS 0x1400 1400 V=3 V=0 V=xmlns V=attrS V=valS 0x0800 0010 V=~0 V=xmlns V=attrS V=valS V=0x0800 0010 V=~0 V=xmlns V=attrS V=valS 0x0800 0003 V=valS 0x0301 1000 V=0x18? V=0x0b? V=~0 V=~0
#
# OPEN TAG (outer tag, no attributes):
# V=tagS 0x1400 1400 V=0    V=0         0x0401 1000 V=0x1c V=0    V=~0
# V=1    0x0800 0000 V=0x20 0x0201 1000 V=0x38      V=0x4  V=~0   V=~0
#
# OPEN TAG (normal, child, NO ATTRIBUTES):
# V=tagS 0x1400 1400 V=0    V=0         0x0301 1000 V=0x18 V=0x0b V=~0 V=~0
#
# CLOSE TAG (normal, child):
# V=tagS 0x0401 1000 V=0x1c V=0         V=~0
#
# CLOSE TAG (outer tag):
# V=tagS 0x0101 1000 V=0x18 V=0x0c      V=~0

# Looks like the string table is word-aligned.
$doc->{pos} += ($doc->{pos} % 4);
debugf("pos=0x%x\n", $doc->{pos});

#my $no_clue1 = read_doc($doc, 48);

my $no_clue2 = read_doc_past_sentinel($doc);

#my $nstag = unpack('V', read_doc($doc, 4));
#my $nsurl = unpack('V', read_doc($doc, 4));
#
#my $nsmap = { $nsurl => $nstag };
#my $nstags = { reverse %$nsmap };
#
#my $nsdummy = read_doc($doc, 20);

my $nsmap = {};

debugf("pos=0x%x\n", $doc->{pos});

my $parsed = read_meat($doc);

debug("All done, DUMPING XML:\n");
print "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n";
print_tree($parsed, 0);

my $nsend = read_doc($doc, 8);

###############################################################################

sub print_tree
{
        my ($node, $depth) = @_;

        print "\t" x $depth;

        if (($node->{flags} & $TAG_TEXT) != 0)
        {
                print $node->{name}, "\n";
                return;
        }

        print '<';

        if (($node->{flags} & $TAG_OPEN) == 0)
        {
                print '/';
        }

        print $node->{name};

        foreach my $attr (@{$node->{attrs}})
        {
                if (scalar(@{$node->{attrs}}) == 1)
                {
                        print ' ';
                }
                else
                {
                        print "\n", "\t" x ($depth + 1);
                }

                $attr->{ns} and
                  printf '%s:', $attr->{ns};

                printf '%s="%s"', @$attr{qw/name value/};
        }

        scalar(@{$node->{children}}) == 0 and
          print ' /';

        print ">\n";

        if (scalar(@{$node->{children}}) > 0)
        {
                foreach my $child (@{$node->{children}})
                {
                        print_tree($child, $depth + 1);
                }

                print "\t" x $depth, "</$node->{name}>\n";
        }
}

###############################################################################

sub read_meat
{
        my $tag = read_tag($doc);
        die unless $tag;

        $tag->{children} = [ read_children($doc, $tag->{name}) ];

        return $tag;
}

sub read_children
{
        my ($doc, $stoptag) = @_;
        my @tags;

        while ((my $tag = read_tag($doc)))
        {
                # Whitespace leaks into this, but we don't support parsing it
                # correctly.
#               next unless $tag->{name} =~ m/[a-z]/i;

                if (($tag->{flags} & $TAG_SUPPORTS_CHILDREN) != 0)
                {
                        if (($tag->{flags} & $TAG_OPEN) != 0)
                        {
                                $tag->{children} = [ read_children($doc, $tag->{name}) ];
                        }
                        elsif ($tag->{name} eq $stoptag)
                        {
                                last;
                        }
                }

                push @tags, $tag;
        }

        return @tags;
}

sub read_tag
{
        my ($doc, $stoptag) = @_;
        my $tag;
        my @xmlns;

# Hack to support the strange xmlns attribute encoding without disrupting our
# processor.
READ_AGAIN:
        my $name = unpack('V', read_doc($doc, 4));
        debugf("tag=%s (%d) @ 0x%x\n", $strings[$name], $name, $doc->{pos});

        my $flags = unpack('V', read_doc($doc, 4));
        debugf("        flags=0x%08x (%d, open=%d, children=%d, text=%d)\n", $flags, $flags, $flags & $TAG_OPEN, $flags & $TAG_SUPPORTS_CHILDREN, $flags & $TAG_TEXT);

        # Strange way to specify xmlns attribute.
        if ($strings[$name] && $strings[$flags])
        {
                my $ns = utf16le($strings[$name])->utf8;
                my $url = utf16le($strings[$flags])->utf8;

                # TODO: How do we expect this?
                if ($ns =~ m/[a-z]/i && $url =~ m/^http:\/\//)
                {
                        debug("new map: $flags => $name\n");
                        $nsmap->{$flags} = $name;
                        push @xmlns, { name => "xmlns:$ns", value => $url };
                        read_doc_past_sentinel($doc);
                        goto READ_AGAIN;
                }
        }

        if (($flags & $TAG_SUPPORTS_CHILDREN) != 0 && ($flags & $TAG_OPEN) != 0)
        {
                $tag->{attrs} = [ @xmlns ];

                my $attrs = unpack('V', read_doc($doc, 4));
                debugf("        attrs=%d\n", $attrs);

                my $unknown = unpack('V', read_doc($doc, 4));

                while ($attrs-- > 0)
                {
                        my $ns = unpack('V', read_doc($doc, 4));

                        $ns != 0xffffffff and
                          debugf("              namespace=%s\n", $strings[$ns]);

                        my $attr = unpack('V', read_doc($doc, 4));
                        debugf("                attr=%s\n", $strings[$attr]);

                        # TODO: Escaping?
                        my $value = unpack('V', read_doc($doc, 4));
                        debugf("                value=%s\n", $strings[$value]);

                        my $attrflags = unpack('V', read_doc($doc, 4));

                        my $attr = {
                                name => utf16le($strings[$attr])->utf8,
                                value => utf16le($strings[$value])->utf8,
                                flags => $attrflags,
                        };

                        $ns != 0xffffffff and
                          $attr->{ns} = utf16le($strings[$nsmap->{$ns}])->utf8;

                        push @{$tag->{attrs}}, $attr;

                        my $padding = unpack('V', read_doc($doc, 4));
#                       read_doc_past_sentinel($doc, 1);
                }

                read_doc_past_sentinel($doc);
        }
        else
        {
                # There is strong evidence here that what I originally thought
                # to be a sentinel is not ;)
                my $whatever = unpack('V', read_doc($doc, 4));
                my $huh = unpack('V', read_doc($doc, 4));

                read_doc_past_sentinel($doc);
        }

        $tag->{name} = utf16le($strings[$name])->utf8;
        $tag->{flags} = $flags;

        return $tag;
}

###############################################################################

sub read_doc
{
        my ($doc, $n) = @_;

        (length($doc->{data}) - $doc->{pos}) < $n and
          die "Not enough data to read $n bytes at $doc->{pos}.\n";

        my $data = substr($doc->{data}, $doc->{pos}, $n);
        $doc->{pos} += $n;

        return $data;
}

sub peek_doc
{
        my ($doc, $n) = @_;

        my $data = read_doc($doc, $n);
        $doc->{pos} -= $n;

        return $data;
}

sub read_doc_past_sentinel
{
        my ($doc, $count) = @_;

        my $pos = $doc->{pos};

        # Read to sentinel.
        while ((my $word = read_doc($doc, 4)))
        {
                last if unpack('V', $word) == 0xffffffff;
        }

        my $n = 1;

        # Read past it.
        if (!defined($count) || $count < $n)
        {
                while ((my $word = peek_doc($doc, 4)))
                {
                        last unless unpack('V', $word) == 0xffffffff;

                        read_doc($doc, 4);
                        $n++;

                        last if (defined($count) && $count >= $n);
                }
        }

        debugf("[skipped %d sentinels, %d bytes]\n", $n, $doc->{pos} - $pos);
}

###############################################################################

sub debug($;@)
{
        print STDERR @_ if $DEBUG;
}

sub debugf($;@)
{
        printf STDERR @_ if $DEBUG;
}
