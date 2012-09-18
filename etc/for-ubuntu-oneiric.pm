#! /usr/bin/perl -w

# Copyright Bill Allombert <ballombe@debian.org> 2001.
# Modifications copyright 2002 Julian Gilbey <jdg@debian.org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

package Devscripts::Packages;

use Carp;

BEGIN{
  use Exporter   ();
  use vars       qw(@EXPORT @ISA %EXPORT_TAGS);
  @EXPORT=qw(PackagesToFiles FilesToPackages PackagesMatch InstalledPackages);
  @ISA=qw(Exporter);
  %EXPORT_TAGS=();
}

=head1 NAME

Devscript::Packages - Interface to the dpkg package database

=head1 SYNOPSIS

use Devscript::Packages;

@files=PackagesToFiles(@packages);

@packages=FilesToPackages(@files);

@packages=PackagesMatch($regexp);

$packages_hashref=InstalledPackages($sources);

=head1 DESCRIPTION


PackagesToFiles: Return a list of files contained in a list of packages.

FilesToPackages: Return a list of packages containing at least
one file in a list of files, taking care to handle diversions correctly.

PackagesMatch: list of packages whose status match regexp.

InstalledPackages: ref to hash with keys being installed packages
(status = install ok installed).  If $sources is true, then include
the corresponding source packages as well in the list.

=cut

# input: a list of packages names.
# output: list of files they contain.

sub PackagesToFiles (@)
{
    return () if @_ == 0;

    my %files=();

    # We fork and use an exec, so that we don't have to worry how long an
    # input string the shell can handle.

    my $pid;
    my $sleep_count=0;
    do {
	$pid = open(DPKG, "-|");
	unless (defined $pid) {
	    carp("cannot fork: $!");
	    croak("bailing out") if $sleep_count++ > 6;
	    sleep 10;
	}
    } until defined $pid;

    if ($pid) {   # parent
	while (<DPKG>) {
	    chomp;
	    next if /^package diverts others to: / or -d $_;
	    $files{$_} = 1;
	}
	close DPKG or croak("dpkg -L failed: $!");
    } else {      # child
	# We must use C locale, else diversion messages may be translated.
	$ENV{'LC_ALL'}='C';
	exec('dpkg', '-L', @_)
	    or croak("can't exec dpkg -L: $!");
    }

    return keys %files;
}


# This basically runs a dpkg -S with a few bells and whistles
#
# input:  a list of files.
# output: list of packages they belong to.

sub FilesToPackages (@)
{
    return () if @_ == 0;

    # We fork and use an exec, so that we don't have to worry how long an
    # input string the shell can handle.

    my @dpkg_out;
    my $pid;
    my $sleep_count=0;
    do {
	$pid = open(DPKG, "-|");
	unless (defined $pid) {
	    carp("cannot fork: $!");
	    croak("bailing out") if $sleep_count++ > 6;
	    sleep 10;
	}
    } until defined $pid;

    if ($pid) {   # parent
	while (<DPKG>) {
	    # We'll process it later
	    chomp;
	    push @dpkg_out, $_;
	}
	if (! close DPKG) {
	    # exit status of 1 just indicates unrecognised files
	    if ($? & 0xff || $? >> 8 != 1) {
		carp("warning: dpkg -S exited with signal " . ($? & 0xff) . " and status " . ($? >> 8));
	    }
	}
    } else {      # child
	# We must use C locale, else diversion messages may be translated.
	$ENV{'LC_ALL'}='C';
	open STDERR, '>& STDOUT';  # Capture STDERR as well
	exec('dpkg', '-S', @_)
	    or croak("can't exec dpkg -S: $!");
    }


    my %packages=();
    my ($curfile, $pkgfrom);
    undef $pkgfrom;
    $curfile = shift;

    foreach (@dpkg_out) {
	# We want to handle diversions nicely.
	# Ignore local diversions
	if (/^local diversion from: /) {
	    # Do nothing
	}
	elsif (/^local diversion to: (.+)$/) {
	    if ($curfile eq $1) {
		$curfile = shift;
	    }
	}
	elsif (/^diversion by (\S+) from: (.+)$/) {
	    if ($curfile eq $2) {
		# So the file we're looking has been diverted
		$pkgfrom=$1;
	    }
	}
	elsif (/^diversion by (\S+) to: (.+)$/) {
	    if ($curfile eq $2) {
		# So the file we're looking is a diverted file
		# We shouldn't see it again
		$packages{$1} = 1;
		$curfile = shift;
	    }
	}
	elsif (/^dpkg: \Q$curfile\E not found\.$/) {
	    $curfile = shift;
	}
	elsif (/^dpkg-query: no path found matching pattern \Q$curfile\E\.$/) {
	    $curfile = shift;
	}
	elsif (/^(.*): \Q$curfile\E$/) {
	    my @pkgs = split /, /, $1;
	    if (@pkgs==1) { $packages{$pkgs[0]} = 1; }
	    else {
		# We've got a file which has been diverted by some package
		# and so is listed in two packages.  The *diverting* package
		# is the one with the file that was actually used.
		my $found=0;
		foreach my $pkg (@pkgs) {
		    if ($pkg eq $pkgfrom) {
			$packages{$pkgfrom} = 1;
			$found=1;
			last;
		    }
		}
		if (! $found) {
		    carp("Something wicked happened to the output of dpkg -S $curfile");
		}
	    }
	    # Prepare for the next round
	    $curfile = shift;
	    undef $pkgfrom;
	}

    }

    return keys %packages;
}


# Return a list of packages whose status entries match a given pattern

sub PackagesMatch ($)
{
    my $package;
    my $match=$_[0];
    my @matches=();

    open STATUS, '/var/lib/dpkg/status'
	or croak("Can't read /var/lib/dpkg/status: $!");

    while(<STATUS>) {
	chomp;
	s/\s+$//;
	if (/^Package: (.+)$/) { $package=$1; next; }
	/$match/ or next;
	push @matches, $package if $package;
	# So we only pick up each package at most once
	undef $package;
    }

    close STATUS or croak("Problem reading /var/lib/dpkg/status: $!");
    return @matches;
}


# Which packages are installed (Package and Source)?
# This uses internal knowledge about the /var/lib/dpkg/status file
# for efficiency - it runs 3 times faster than if it didn't use this
# info....  And calling a shell script is faster still: thanks to
# Arthur Korn <arthur@korn.ch> for this one ;-)
# For the correct settings of -B# -A#, keep up-to-date with
# the dpkg source, defn of fieldinfos[] in lib/parse.c
# (and should match wnpp-alert.sh)

sub InstalledPackages ($)
{
    my $grep_pattern = $_[0] ? '^\(Package\|Source\):' : '^Package:';

    open (PKG, qq[grep -B2 -A7 'Status: install ok installed' /var/lib/dpkg/status | grep '$grep_pattern' | cut -f2 -d' ' |])
	or croak("Problem opening grep pipe: $!");

    my %matches = map { chomp; $_ => 1 } <PKG>;

    close PKG or croak("Problem reading grep pipe: $!");

    return \%matches;
}

1;

=head1 AUTHOR

Bill Allombert <ballombe@debian.org>

=head1 COPYING

Copyright 2001 Bill Allombert <ballombe@debian.org>
Modifications copyright 2002 Julian Gilbey <jdg@debian.org>
dpkg-depcheck is free software, covered by the GNU General Public License, and
you are welcome to change it and/or distribute copies of it under
certain conditions.  There is absolutely no warranty for dpkg-depcheck.

=cut
