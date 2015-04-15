#
# Gtags.pl --- Global facility for Nvi-1.81
#
# Copyright (c) 2001, 2002, 2008, 2010 Tama Communications Corporation
#
# This file is part of GNU GLOBAL.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# usage:
#
#	nvi editor			command line
#	-----------------------------------------------------------
#	:perl tag qw(main)		global -x main
#	:perl tag qw(-T main)		global -Tx main
#	:perl tag qw(-r main)		global -rx main
#	:perl tag qw(-sl main)		global -slx main
#	:perl tag qw(-gi main)		global -gix main
#	:perl tag ('-go', 'int argc')	global -go 'int argc'
#	:perl tag qw(-I main)		global -Ix main
#	:perl tag qw(-P file)		global -Px file
#	:perl tag qw(-f %)		global -fx <current file>
#	:perl gozilla			gozilla +<current line> <current file>
#
# Suggested .nexrc: (If you have gtags.pl in ${HOME}/perl.)
#
# perl use lib "$ENV{HOME}/perl"
# perl require 'gtags.pl'
# map ^P :tagprev^M
# map ^N :tagnext^M
# map ^] :perl tag^M
# map ^G :perl gozilla^M
# ab gtag perl tag qw(
# ab gta perl tag qw(
# ab gt perl tag qw(
# ab gozill perl gozilla
# ab gozil perl gozilla
# ab gozi perl gozilla
# ab goz perl gozilla

#
# You need this version of GLOBAL.
#
my($supported_version) = '5.7';
#
# Quote characters
#
my($sq) = "'";
my($dq) = '"';
my($single_quote_char) = $sq . $dq . $sq . $dq . $sq;
#
# command existent check.
#
$w32 = ($^O =~ /^(ms)?(dos|win(32|nt))/i) ? 1 : 0;
$pathsep = ($w32) ? ';' : ':';
$command = '';
foreach (split(/$pathsep/, $ENV{'PATH'})) {
    if (-x "$_/global") {
        $command = "$_/global";
        last;
    }
}
#
# version check of global(1).
#
$error = '';
if ($command) {
    open(TAGS, "$command --version |");
    $_ = <TAGS>;
    chop($_);
    $version = $_;
    close(TAGS);
    my $result = version_supported($version, $supported_version);
    if ($result < 0) {
        $error = 'Global(1) not found.';
    } elsif ($result == 0) {
        $error = 'Your global(1) seems to be older version.';
    }
}
if ($error) {
    $error .= " Required GLOBAL-${supported_version} or later.";
}
sub main::tag {
    my $tagq, $tag;
    my $flag = '';
    my $end = 0;

    if ($error) {
        $curscr->Msg($error);
	return;
    }
    while ($_[0] =~ /^-/ && $end == 0) {
	my($option) = $_[0];
	if ($option !~ /^--/) {			# ignore long option
	    my($length) = length($option);
	    my($offset) = 0;
	    while ($offset < $length) {
	        my($c) = substr($option, $offset, 1);
                if ($c !~ /[-cenpquv]/) {
                    $flag .= $c;
	        }
		if ($c eq 'e') {
		    $end = 1;
		}
		$offset++;
            }
	}
        shift;
    }
    if ($_[0]) {
        $tag = $_[0];
	#
	# replace '%' with current file name.
	#
	if ($tag =~ /%/) {
            $path = $curscr->GetFileName();
	    $tag =~ s/%/$path/;
	}
    } else {
        #
        # get current position.
        #
        my($lineno, $column) = $curscr->GetCursor();
        my($line) = $curscr->GetLine($lineno);
	my($length) = length($line);
        #
        # extract the first word as a tag.
        #
	my($offset) = $column;
	while ($offset > 0 && substr($line, $offset, 1) =~ /^\w/) {
	    $offset--;
	}
	while ($offset < $length && substr($line, $offset, 1) !~ /^\w/) {
	    $offset++;
	}
	my($subline) = substr($line, $offset);
        ($tag) = $subline =~ /(\w+)/;
        if (!$tag) {
            $curscr->Msg("tag not found in current position.");
            return;
        }
	#
	# make global(1) locate tags based on the context.
	#
        $flag = ' --from-here="' . $lineno . ':' . $curscr->GetFileName() . '"';
    }
    #
    # quote tag.
    #
    my $quoted_tag;
    $length = length($tag);
    $offset = 0;
    while ($offset < $length) {
        my($c) = substr($tag, $offset, 1);
        if ($c eq "'") {
    	$quoted_tag .= $single_quote_char;
        } else {
    	$quoted_tag .= $c;
        }
        $offset++;
    }
    if ($flag !~ /f/) {
	$flag .= 'e';
    }
    $cmd = "$command --encode-path=\" \t\" -xq$flag '$quoted_tag'";
    open(TAGS, "$cmd |");
    $tagq = undef;
    while(<TAGS>) {
        my ($name, $lno, $path, $rest);
        if ((($name, $lno, $path, $rest) = /^([^ \t]+)[ \t]+(\d+)[ \t]+([^ \t]+)(.*)$/) >= 2) {
	    if (!$tagq) {
		$tagq = $curscr->TagQ($tag);
	    }
	    $path =~ s/%([0-9a-f][0-9a-f])/pack("C", hex($1))/eg;
            $tagq->Add($path, $lno, '');
        }
    } 
    close(TAGS);
    $status = $?;
    $status = $status / 256;
    if ($status == 0) {
        if (!$tagq) {
	    if ($flag =~ /f/) {
		$curscr->Msg("Tag not found in $tag");
	    } elsif ($flag =~ /P/) {
		$curscr->Msg("Path which matches to $tag not found.");
	    } elsif ($flag =~ /g/) {
		$curscr->Msg("Line which matches to $tag not found.");
	    } else {
		$curscr->Msg("Tag which matches to $tag not found.");
	    }
        } else {
	    $tagq->Push();
        }
    } elsif ($status == 2) {
        $curscr->Msg("invalid arguments. command line: $cmd");
    } elsif ($status == 3) {
        $curscr->Msg("GTAGS not found.");
    } else {
        $curscr->Msg("Global(1) failed. command line: $cmd");
    }
}
sub main::gozilla {
    if ($error) {
        $curscr->Msg($error);
	return;
    }
    my($filename) = $curscr->GetFileName();
    my($lineno, $column) = $curscr->GetCursor();
    system("gozilla +$lineno $filename");
}
#
# parse_version: parse a version and return sub-strings as an array.
#
#	i)	version number
#	r)	number array
#
sub parse_version {
    my($v) = @_;
    my($v1, $v2, $v3);

    if ($v =~ /(\d+)\.(\d+)\.(\d+)$/) {
	$v1 = $1;
	$v2 = $2;
	$v3 = $3;
    } elsif ($v =~ /(\d+)\.(\d+)$/) {
	$v1 = $1;
	$v2 = $2;
	$v3 = 0;
    } elsif ($v =~ /(\d+)$/) {
	$v1 = $1;
	$v2 = 0;
	$v3 = 0;
    } else {
	return ();	
    }
    return ($v1, $v2, $v3);
}
#
# version_supported: check whether supported version or not.
#
#	i)	$v	version
#	i)	$sv	supported version
#	r)		-1: illegal
#			1: supported
#			0: not supported
#
sub version_supported {
    my($v, $sv) = @_;
    my(@v) = parse_version($v);
    my(@s) = parse_version($sv);

    return (@v < 3) ? -1 :
	($v[0] * 1000000 + $v[1] * 1000 + $v[2] >= $s[0] * 1000000 + $s[1] * 1000 + $s[2]) ? 1 : 0;
}

1;
