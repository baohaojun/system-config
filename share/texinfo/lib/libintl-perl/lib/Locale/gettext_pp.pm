#! /bin/false

# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: gettext_pp.pm,v 1.1 2011-10-12 23:51:27 pertusus Exp $

# Pure Perl implementation of Uniforum message translation.
# Copyright (C) 2002-2009 Guido Flohr <guido@imperia.net>,
# all rights reserved.

# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU Library General Public License as published
# by the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.

# You should have received a copy of the GNU Library General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.

package Locale::gettext_pp;

use strict;

require 5.004;

use vars qw ($__gettext_pp_default_dir 
			 $__gettext_pp_textdomain
			 $__gettext_pp_domain_bindings
			 $__gettext_pp_domain_codeset_bindings
			 $__gettext_pp_domains
			 $__gettext_pp_recoders
			 $__gettext_pp_unavailable_dirs
			 $__gettext_pp_domain_cache
			 $__gettext_pp_alias_cache
			 $__gettext_pp_context_glue);

use locale;

BEGIN {
	$__gettext_pp_textdomain = 'messages';
	$__gettext_pp_domain_bindings = {};
	$__gettext_pp_domain_codeset_bindings = {};
	$__gettext_pp_domains = {};
	$__gettext_pp_recoders = {};
	$__gettext_pp_unavailable_dirs = {};
	$__gettext_pp_domain_cache = {};
	$__gettext_pp_alias_cache = {};
	# The separator between msgctxt and msgid in a .mo file.  */
	$__gettext_pp_context_glue = "\004";
	
	$__gettext_pp_default_dir = '';
	
	for my $dir (qw (/usr/share/locale /usr/local/share/locale)) {
		if (-d $dir) {
			$__gettext_pp_default_dir = $dir;
			last;
		}
	}
}

BEGIN {
		require POSIX;
		require Exporter;
		use IO::Handle;
		require Locale::Recode;

		local $@;
		my ($has_messages, $five_ok);
		
		$has_messages = eval '&POSIX::LC_MESSAGES';

		unless (defined $has_messages && length $has_messages) {
				$five_ok = ! grep {my $x = eval "&POSIX::$_" || 0; $x eq '5';}
								qw (LC_CTYPE
								   LC_NUMERIC
								   LC_TIME
								   LC_COLLATE
								   LC_MONETARY
								   LC_ALL);
			if ($five_ok) {
				$five_ok = POSIX::setlocale (5, '');
			}
		}
		
		if (defined $has_messages && length $has_messages) {
eval <<'EOF';
sub LC_MESSAGES()
{
	local $!; # Do not clobber errno!
	
	return &POSIX::LC_MESSAGES;
}
EOF
		} elsif ($five_ok) {
eval <<'EOF';
sub LC_MESSAGES()
{
	local $!; # Do not clobber errno!

	# Hack: POSIX.pm deems LC_MESSAGES an invalid macro until
	# Perl 5.8.0.  However, on LC_MESSAGES should be 5 ...
	return 5;
}
EOF
		} else {
eval <<'EOF';
sub LC_MESSAGES()
{
	local $!; # Do not clobber errno!

	# This fallback value is widely used,
	# when LC_MESSAGES is not available.
	return 1729;
}
EOF
		}
}

use vars qw (%EXPORT_TAGS @EXPORT_OK @ISA $VERSION);

%EXPORT_TAGS = (locale_h => [ qw (
								  gettext
								  dgettext
								  dcgettext
								  ngettext
								  dngettext
								  dcngettext
								  pgettext
								  dpgettext
								  dcpgettext
								  npgettext
								  dnpgettext
								  dcnpgettext
								  textdomain
								  bindtextdomain
								  bind_textdomain_codeset
								  )
							  ],
				libintl_h => [ qw (LC_CTYPE
								   LC_NUMERIC
								   LC_TIME
								   LC_COLLATE
								   LC_MONETARY
								   LC_MESSAGES
								   LC_ALL)
							   ],
				);

@EXPORT_OK = qw (gettext
				 dgettext
				 dcgettext
				 ngettext
				 dngettext
				 dcngettext
				 pgettext
				 dpgettext
				 dcpgettext
				 npgettext
				 dnpgettext
				 dcnpgettext
				 textdomain
				 bindtextdomain
				 bind_textdomain_codeset
                 nl_putenv
				 LC_CTYPE
				 LC_NUMERIC
				 LC_TIME
				 LC_COLLATE
				 LC_MONETARY
				 LC_MESSAGES
				 LC_ALL);
@ISA = qw (Exporter);

my $has_nl_langinfo;

sub __load_catalog;
sub __load_domain;
sub __locale_category;

sub LC_NUMERIC()
{
	&POSIX::LC_NUMERIC;
}

sub LC_CTYPE()
{
	&POSIX::LC_CTYPE;
}

sub LC_TIME()
{
	&POSIX::LC_TIME;
}

sub LC_COLLATE()
{
	&POSIX::LC_COLLATE;
}

sub LC_MONETARY()
{
	&POSIX::LC_MONETARY;
}

sub LC_ALL()
{
	&POSIX::LC_ALL;
}

sub textdomain(;$)
{
	my $new_domain = shift;
	
	$__gettext_pp_textdomain = $new_domain if defined $new_domain && 
		length $new_domain;
	
	return $__gettext_pp_textdomain;
}

sub bindtextdomain($;$)
{
	my ($domain, $directory) = @_;

	my $retval;	
	if (defined $domain && length $domain) {
		if (defined $directory && length $directory) {
			$retval = $__gettext_pp_domain_bindings->{$domain} 
				= $directory;
		} elsif (exists $__gettext_pp_domain_bindings->{$domain}) {
			$retval = $__gettext_pp_domain_bindings->{$domain};
		} else {
			$retval = $__gettext_pp_default_dir;
		}
		$retval = '/usr/share/locale' unless defined $retval && 
			length $retval;
		return $retval;
	} else {
		return;
	}
}

sub bind_textdomain_codeset($;$)
{
	my ($domain, $codeset) = @_;
	
	if (defined $domain && length $domain) {
		if (defined $codeset && length $codeset) {
			return $__gettext_pp_domain_codeset_bindings->{$domain} = $codeset;
		} elsif (exists $__gettext_pp_domain_codeset_bindings->{$domain}) {
			return $__gettext_pp_domain_codeset_bindings->{$domain};
		}
	}
	
	return;
}

sub gettext($)
{
	my ($msgid) = @_;

	return dcnpgettext ('', undef, $msgid, undef, undef, undef);
}

sub dgettext($$)
{
	my ($domainname, $msgid) = @_;

	return dcnpgettext ($domainname, undef, $msgid, undef, undef, undef);
}

sub dcgettext($$$)
{
	my ($domainname, $msgid, $category) = @_;

	return dcnpgettext ($domainname, undef, $msgid, undef, undef, undef);
}

sub ngettext($$$)
{
	my ($msgid, $msgid_plural, $n) = @_;

	return dcnpgettext ('', undef, $msgid, $msgid_plural, $n, undef);
}

sub dngettext($$$$)
{
	my ($domainname, $msgid, $msgid_plural, $n) = @_;

	return dcnpgettext ($domainname, undef, $msgid, $msgid_plural, $n, undef);
}

sub dcngettext($$$$$)
{
	my ($domainname, $msgid, $msgid_plural, $n, $category) = @_;

	return dcnpgettext ($domainname, undef, $msgid, $msgid_plural, $n, , $category);
}


sub pgettext($$)
{
	my ($msgctxt, $msgid) = @_;

	return dcnpgettext ('', $msgctxt, $msgid, undef, undef, undef);
}

sub dpgettext($$$)
{
	my ($domainname, $msgctxt, $msgid) = @_;

	return dcnpgettext ($domainname, $msgctxt, $msgid, undef, undef, undef);
}

sub dcpgettext($$$$)
{
	my ($domainname, $msgctxt, $msgid, $category) = @_;

	return dcnpgettext ($domainname, $msgctxt, $msgid, undef, undef, undef);
}

sub npgettext($$$$)
{
	my ($msgctxt, $msgid, $msgid_plural, $n) = @_;

	return dcnpgettext ('', $msgctxt, $msgid, $msgid_plural, $n, undef);
}

sub dnpgettext($$$$$)
{
	my ($domainname, $msgctxt, $msgid, $msgid_plural, $n) = @_;

	return dcnpgettext ($domainname, $msgctxt, $msgid, $msgid_plural, $n, undef);
}

sub dcnpgettext($$$$$$)
{
	my ($domainname, $msgctxt, $msgid, $msgid_plural, $n, $category) = @_;

	return unless defined $msgid;

	my $plural = defined $msgid_plural;
	my $msg_ctxt_id = defined $msgctxt ? join($__gettext_pp_context_glue, ($msgctxt, $msgid)) : $msgid;
	
	local $!; # Do not clobber errno!
	
	# This is also done in __load_domain but we need a proper value.
	$domainname = 	$__gettext_pp_textdomain
		unless defined $domainname && length $domainname;
	
	# Category is always LC_MESSAGES (other categories are ignored).
	my $category_name = 'LC_MESSAGES';
	$category = LC_MESSAGES;
	
	my $domains = __load_domain ($domainname, $category, $category_name);
	
	my @trans = ();
	my $domain;
	my $found;
	foreach my $this_domain (@$domains) {
		if ($this_domain && defined $this_domain->{messages}->{$msg_ctxt_id}) {
			@trans = @{$this_domain->{messages}->{$msg_ctxt_id}};
			shift @trans;
			$domain = $this_domain;
			$found = 1;
			last;
		}
	}
	@trans = ($msgid, $msgid_plural) unless @trans;
	
	my $trans = $trans[0];
	if ($plural) {
		if ($domain) {
			my $nplurals = 0;
			($nplurals, $plural) = &{$domain->{plural_func}} ($n);
			$plural = 0 unless defined $plural;
			$nplurals = 0 unless defined $nplurals;
			$plural = 0 if $nplurals <= $plural;
		} else {
			$plural = $n != 1 || 0;
		}
		
		$trans = $trans[$plural] if defined $trans[$plural];
	}
	
	if ($found && defined $domain->{po_header}->{charset}) {
		my $input_codeset = $domain->{po_header}->{charset};
		# Convert into output charset.
		my $output_codeset = $__gettext_pp_domain_codeset_bindings->{$domainname};

		$output_codeset = $ENV{OUTPUT_CHARSET} unless defined $output_codeset;
		$output_codeset = __get_codeset ($category, $category_name,
										 $domain->{locale_id})
			unless defined $output_codeset;
		
		unless (defined $output_codeset) {
			# Still no point.
			my $lc_ctype = __locale_category (POSIX::LC_CTYPE(), 
										   'LC_CTYPE');
			$output_codeset = $1
				if $lc_ctype =~ /^[a-z]{2}(?:_[A-Z]{2})?\.([^@]+)/;
		}

		# No point. :-(
		$output_codeset = $domain->{po_header}->{charset}
			unless defined $output_codeset;
	
		if (exists $__gettext_pp_domain_cache->{$output_codeset}) {
			$output_codeset = $__gettext_pp_domain_cache->{$output_codeset};
		} else {
			$output_codeset = 'utf-8' if lc $output_codeset eq 'utf8';
			$output_codeset = 
				$__gettext_pp_domain_cache->{$output_codeset} =
				Locale::Recode->resolveAlias ($output_codeset);
		}
		
		if (defined $output_codeset &&
			$output_codeset ne $domain->{po_header}->{charset}) {
			# We have to convert.
			my $recoder;
			
			if (exists 
				$__gettext_pp_recoders->{$input_codeset}->{$output_codeset}) {
				$recoder = $__gettext_pp_recoders->{$input_codeset}->{$output_codeset};
			} else {
				$recoder = 
					$__gettext_pp_recoders->{$input_codeset}->{$output_codeset} =
					Locale::Recode->new (from => $input_codeset,
										 to => $output_codeset,
										 );
			}
			
			$recoder->recode ($trans);
		}
	}
	
	return $trans;
}

sub nl_putenv ($)
{
    my ($envspec) = @_;
    return unless defined $envspec;
    return unless length $envspec;
    return if substr ($envspec, 0, 1) eq '=';
    
    my ($var, $value) = split /=/, $envspec, 2;

    # In Perl we *could* set empty environment variables even under
    # MS-DOS, but for compatibility reasons, we implement the
    # brain-damaged behavior of the Microsoft putenv().
    if ($^O eq 'MSWin32') {
        $value = '' unless defined $value;
        if (length $value) {
            $ENV{$var} = $value;
        } else {
            delete $ENV{$var};
        }
    } else {
        if (defined $value) {
            $ENV{$var} = $value;
        } else {
            delete $ENV{$var};
        }
    }

    return 1;
}

sub __load_domain
{
	my ($domainname, $category, $category_name) = @_;
	
	$domainname = $__gettext_pp_textdomain
		unless defined $domainname && length $domainname;

	my $dir = bindtextdomain ($domainname, '');
	$dir = $__gettext_pp_default_dir unless defined $dir && length $dir;
	return [] unless defined $dir && length $dir;

	my @locales;
	my $cache_key;

	if (defined $ENV{LANGUAGE} && length $ENV{LANGUAGE}) {
		@locales = split /:/, $ENV{LANGUAGE};
		$cache_key = $ENV{LANGUAGE};
	} else {
		@locales = $cache_key = __locale_category ($category, $category_name);
	}

	# Have we looked that one up already?
	my $domains = $__gettext_pp_domain_cache->{$dir}->{$cache_key}->{$category_name}->{$domainname};
	
	if (@locales && !defined $domains) {
		my @dirs = ($dir);
		my @tries = (@locales);
		my %locale_lookup = map { $_ => $_ } @tries;

		foreach my $locale (@locales) {
			if ($locale =~ /^([a-z][a-z])
				(?:(_[A-Z][A-Z])?
				 (\.[-_A-Za-z0-9]+)?
				 )?
				(\@[-_A-Za-z0-9]+)?$/x) {
				
				if (defined $3) {
					defined $2 ?
						push @tries, $1 . $2 . $3 : push @tries, $1 . $3;
				}
				if (defined $2) {
					push @tries, $1 . $2;
					$locale_lookup{$1 . $2} = $locale;
				}
				if (defined $1) {
					push @tries, $1 if defined $1;
					$locale_lookup{$1} = $locale;
				}
			}
		}

		push @dirs, $__gettext_pp_default_dir
			if $__gettext_pp_default_dir && $dir ne $__gettext_pp_default_dir;
		
		my %seen = ();
		foreach my $basedir (@dirs) {
			foreach my $try (@tries) {
				my $fulldir = "$basedir/$try/$category_name";
				
				next if $seen{$fulldir}++;

				# If the cache for unavailable directories is removed,
				# the three lines below should be replaced by:
				# 'next unless -d $fulldir;'
				next if $__gettext_pp_unavailable_dirs->{$fulldir};
				++$__gettext_pp_unavailable_dirs->{$fulldir} and next
						unless -d $fulldir;

				my $domain = __load_catalog $fulldir, $domainname;
				next unless $domain;
				
				unless (defined $domain->{po_header}->{charset} &&
						length $domain->{po_header}->{charset} &&
						$try =~ /^(?:[a-z][a-z])
						(?:(?:_[A-Z][A-Z])?
						 (\.[-_A-Za-z0-9]+)?
						 )?
						(?:\@[-_A-Za-z0-9]+)?$/x) {
					$domain->{po_header}->{charset} = $1;
				}
				
				if (defined $domain->{po_header}->{charset}) {
					$domain->{po_header}->{charset} = 
						Locale::Recode->resolveAlias ($domain->{po_header}->{charset});
				}
				$domain->{locale_id} = $locale_lookup{$try};
				push @$domains, $domain;
			}
		}
		$__gettext_pp_domain_cache->{$dir}->{$cache_key}->{$category_name}->{$domainname} = $domains;
	}

	$domains = [] unless defined $domains;
	return $domains;
}

sub __load_catalog
{
	my ($directory, $domainname) = @_;
	
	my $filename = "$directory/$domainname.mo";
	
	# Alternatively we could check the filename for evil characters ...
	# (Important for CGIs).
	return unless -f $filename && -r $filename;
	
	local $/;
	local *HANDLE;
	
	open HANDLE, "<$filename"
		or return;
	binmode HANDLE;
	my $raw = <HANDLE>;
	close HANDLE;
	
	# Corrupted?
	return if ! defined $raw || length $raw < 28;
	
	my $filesize = length $raw;
	
	# Read the magic number in order to determine the byte order.
	my $domain = {};
	my $unpack = 'N';
	$domain->{potter} = unpack $unpack, substr $raw, 0, 4;
	
	if ($domain->{potter} == 0xde120495) {
		$unpack = 'V';
	} elsif ($domain->{potter} != 0x950412de) {
		return;
	}
	my $domain_unpack = $unpack x 6;
	
	my ($revision, $num_strings, $msgids_off, $msgstrs_off,
		$hash_size, $hash_off) = 
			unpack (($unpack x 6), substr $raw, 4, 24);
	
	return unless $revision == 0; # Invalid revision number.
	
	$domain->{revision} = $revision;
	$domain->{num_strings} = $num_strings;
	$domain->{msgids_off} = $msgids_off;
	$domain->{msgstrs_off} = $msgstrs_off;
	$domain->{hash_size} = $hash_size;
	$domain->{hash_off} = $hash_off;
	
	return if $msgids_off + 4 * $num_strings > $filesize;
	return if $msgstrs_off + 4 * $num_strings > $filesize;
	
	my @orig_tab = unpack (($unpack x (2 * $num_strings)), 
						   substr $raw, $msgids_off, 8 * $num_strings);
	my @trans_tab = unpack (($unpack x (2 * $num_strings)), 
							substr $raw, $msgstrs_off, 8 * $num_strings);
	
	my $messages = {};
	
	for (my $count = 0; $count < 2 * $num_strings; $count += 2) {
		my $orig_length = $orig_tab[$count];
		my $orig_offset = $orig_tab[$count + 1];
		my $trans_length = $trans_tab[$count];
		my $trans_offset = $trans_tab[$count + 1];
		
		return if $orig_offset + $orig_length > $filesize;
		return if $trans_offset + $trans_length > $filesize;
		
		my @origs = split /\000/, substr $raw, $orig_offset, $orig_length;
		my @trans = split /\000/, substr $raw, $trans_offset, $trans_length;
		
		# The singular is the key, the plural plus all translations is the
		# value.
		my $msgid = $origs[0];
		$msgid = '' unless defined $msgid && length $msgid;
		my $msgstr = [ $origs[1], @trans ];
		$messages->{$msgid} = $msgstr;
	}
	
	$domain->{messages} = $messages;
	
	# Try to find po header information.
	my $po_header = {};
	my $null_entry = $messages->{''}->[1];
	if ($null_entry) {
		my @lines = split /\n/, $null_entry;
		foreach my $line (@lines) {
			my ($key, $value) = split /:/, $line, 2;
			$key =~ s/-/_/g;
			$po_header->{lc $key} = $value;
		}
	}
	$domain->{po_header} = $po_header;
	
	if (exists $domain->{po_header}->{content_type}) {
		my $content_type = $domain->{po_header}->{content_type};
		if ($content_type =~ s/.*=//) {
			$domain->{po_header}->{charset} = $content_type;
		}
	}
	
	my $code = $domain->{po_header}->{plural_forms} || '';
	
	# Whitespace, locale-independent.
	my $s = '[ \t\r\n\013\014]';
	
	# Untaint the plural header.
	# Keep line breaks as is (Perl 5_005 compatibility).
	if ($code =~ m{^($s*
					 nplurals$s*=$s*[0-9]+
					 $s*;$s*
					 plural$s*=$s*(?:$s|[-\?\|\&=!<>+*/\%:;a-zA-Z0-9_\(\)])+
					 )}xms) {
		$domain->{po_header}->{plural_forms} = $1;
	} else {
		$domain->{po_header}->{plural_forms} = '';
	}
	
	# Determine plural rules.
	# The leading and trailing space is necessary to be able to match
	# against word boundaries.
	my $plural_func;
	
	if ($domain->{po_header}->{plural_forms}) {
		my $code = ' ' . $domain->{po_header}->{plural_forms} . ' ';
		$code =~ 
			s/([^_a-zA-Z0-9]|\A)([_a-z][_A-Za-z0-9]*)([^_a-zA-Z0-9])/$1\$$2$3/g;
		
		$code = "sub { my \$n = shift; 
				   my (\$plural, \$nplurals); 
				   $code; 
				   return (\$nplurals, \$plural ? \$plural : 0); }";
		
		# Now try to evaluate the code.	 There is no need to run the code in
		# a Safe compartment.  The above substitutions should have destroyed
		# all evil code.  Corrections are welcome!
		$plural_func = eval $code;
		undef $plural_func if $@;
	}
	
	# Default is Germanic plural (which is incorrect for French).
	$plural_func = eval "sub { (2, 1 != shift || 0) }" unless $plural_func;
	
	$domain->{plural_func} = $plural_func;
	
	return $domain;
}

sub __locale_category
{
	my ($category, $category_name) = @_;
	
	local $@;
	my $value = eval {POSIX::setlocale ($category)};
	
	# We support only XPG syntax, i. e.
	# language[_territory[.codeset]][@modifier].
	undef $value unless (defined $value && 
						 length $value &&
						 $value =~ /^[a-z][a-z]
						 (?:_[A-Z][A-Z]
						  (?:\.[-_A-Za-z0-9]+)?
						  )?
						 (?:\@[-_A-Za-z0-9]+)?$/x);
	
	unless ($value) {
		$value = $ENV{LC_ALL};
		$value = $ENV{$category_name} unless defined $value && length $value;
		$value = $ENV{LANG} unless defined $value && length $value;
		return 'C' unless defined $value && length $value;
	}
	
	return $value if $value ne 'C' && $value ne 'POSIX';
}

sub __get_codeset
{
	my ($category, $category_name, $locale_id) = @_;

	local $@;
	unless (defined $has_nl_langinfo) {
		eval {
			require I18N::Langinfo;
		};
		$has_nl_langinfo = !$@;
	}

	if ($has_nl_langinfo) {
		# Try to set the locale via the specified id.
		my $saved_locale = eval { POSIX::setlocale (LC_ALL) };
		my $saved_lc_all = $ENV{LC_ALL};

		# Now try to set the locale via the environment.  There is no
		# point in calling the langinfo routines if this fails.
		$ENV{LC_ALL} = $locale_id;
		my $codeset;
		my $lc_all = eval { POSIX::setlocale (LC_ALL, $locale_id); };
		$codeset = I18N::Langinfo::langinfo (I18N::Langinfo::CODESET())
			if defined $lc_all;

		if ($saved_locale) {
			eval { POSIX::setlocale (LC_ALL, $saved_locale); }
		}
		return $codeset;
	}

	return;
}
	
1;

__END__

=head1 NAME

Locale::gettext_pp - Pure Perl Implementation of Uniforum Message Translation

=head1 SYNOPSIS

 use gettext_pp (:locale_h :libintl_h);

 gettext $msgid;
 dgettext $domainname, $msgid;
 dcgettext $domainname, $msgid, LC_MESSAGES;
 ngettext $msgid, $msgid_plural, $count;
 dngettext $domainname, $msgid, $msgid_plural, $count;
 dcngettext $domainname, $msgid, $msgid_plural, $count, LC_MESSAGES;
 pgettext $msgctxt, $msgid;
 dpgettext $domainname, $msgctxt, $msgid;
 dcpgettext $domainname, $msgctxt, $msgid, LC_MESSAGES;
 npgettext $msgctxt, $msgid, $msgid_plural, $count;
 dnpgettext $domainname, $msgctxt, $msgid, $msgid_plural, $count;
 dcnpgettext $domainname, $msgctxt, $msgid, $msgid_plural, $count, LC_MESSAGES;
 textdomain $domainname;
 bindtextdomain $domainname, $directory;
 bind_textdomain_codeset $domainname, $encoding;
 my $category = LC_CTYPE;
 my $category = LC_NUMERIC;
 my $category = LC_TIME;
 my $category = LC_COLLATE;
 my $category = LC_MONETARY;
 my $category = LC_MESSAGES;
 my $category = LC_ALL;

=head1 DESCRIPTION

The module B<Locale::gettext_pp> is the low-level interface to 
message translation according to the Uniforum approach that is
for example used in GNU gettext and Sun's Solaris.

Normally you should not use this module directly, but the high
level interface Locale::TextDomain(3) that provides a much simpler
interface.	This description is therefore deliberately kept
brief.	Please refer to the GNU gettext documentation available at
L<http://www.gnu.org/manual/gettext/> for in-depth and background 
information on the topic.

=head1 FUNCTIONS

The module exports by default nothing.	Every function has to be
imported explicitely or via an export tag (L<"EXPORT TAGS">).

=over 4

=item B<gettext MSGID>

See L<Locale::Messages/FUNCTIONS>.

=item B<dgettext TEXTDOMAIN, MSGID>

See L<Locale::Messages/FUNCTIONS>.

=item B<dcgettext TEXTDOMAIN, MSGID, CATEGORY>

See L<Locale::Messages/FUNCTIONS>.

=item B<ngettext MSGID, MSGID_PLURAL, COUNT>

See L<Locale::Messages/FUNCTIONS>.

=item B<dngettext TEXTDOMAIN, MSGID, MSGID_PLURAL, COUNT>

See L<Locale::Messages/FUNCTIONS>.

=item B<dcngettext TEXTDOMAIN, MSGID, MSGID_PLURAL, COUNT, CATEGORY>

See L<Locale::Messages/FUNCTIONS>.

=item B<pgettext MSGCTXT, MSGID>

See L<Locale::Messages/FUNCTIONS>.

=item B<dpgettext TEXTDOMAIN, MSGCTXT, MSGID>

See L<Locale::Messages/FUNCTIONS>.

=item B<dcpgettext TEXTDOMAIN, MSGCTXT, MSGID, CATEGORY>

See L<Locale::Messages/FUNCTIONS>.

=item B<npgettext MSGCTXT, MSGID, MSGID_PLURAL, COUNT>

See L<Locale::Messages/FUNCTIONS>.

=item B<dnpgettext TEXTDOMAIN, MSGCTXT, MSGID, MSGID_PLURAL, COUNT>

See L<Locale::Messages/FUNCTIONS>.

=item B<dcnpgettext TEXTDOMAIN, MSGCTXT, MSGID, MSGID_PLURAL, COUNT, CATEGORY>

See L<Locale::Messages/FUNCTIONS>.

=item B<textdomain TEXTDOMAIN>

See L<Locale::Messages/FUNCTIONS>.

=item B<bindtextdomain TEXTDOMAIN, DIRECTORY>

See L<Locale::Messages/FUNCTIONS>.

=item B<bind_textdomain_codeset TEXTDOMAIN, ENCODING>

=item B<nl_putenv ENVSPEC>

See L<Locale::Messages/FUNCTIONS>.

=back

=head1 CONSTANTS

You can (maybe) get the same constants from POSIX(3); see there for
a detailed description

=over 4

=item B<LC_CTYPE>

=item B<LC_NUMERIC>

=item B<LC_TIME>

=item B<LC_COLLATE>

=item B<LC_MONETARY>

=item B<LC_MESSAGES>

=item B<LC_ALL>

See L<Locale::Messages/CONSTANTS> for more information.

=back

=head1 EXPORT TAGS

This module does not export anything unless explicitely requested.
You can import groups of functions via two tags:

=over 4

=item B<use Locale::gettext_pp (':locale_h')>

Imports the functions that are normally defined in the C include
file F<locale.h>:

=over 8

=item B<gettext()>

=item B<dgettext()>

=item B<dcgettext()>

=item B<ngettext()>

=item B<dngettext()>

=item B<dcngettext()>

=item B<pgettext()>

Introduced with libintl-perl 1.17.

=item B<dpgettext()>

Introduced with libintl-perl 1.17.

=item B<dcpgettext()>

Introduced with libintl-perl 1.17.

=item B<npgettext()>

Introduced with libintl-perl 1.17.

=item B<dnpgettext()>

Introduced with libintl-perl 1.17.

=item B<dcnpgettext()>

Introduced with libintl-perl 1.17.

=item B<textdomain()>

=item B<bindtextdomain()>

=item B<bind_textdomain_codeset()>

=back

=item B<use Locale::gettext_pp (':libintl_h')>

Imports the locale category constants:

=over 8

=item B<LC_CTYPE>

=item B<LC_NUMERIC>

=item B<LC_TIME>

=item B<LC_COLLATE>

=item B<LC_MONETARY>

=item B<LC_MESSAGES>

=item B<LC_ALL>

=back

=back

=head1 AUTHOR

Copyright (C) 2002-2009, Guido Flohr E<lt>guido@imperia.netE<gt>, all
rights reserved.  See the source code for details.

This software is contributed to the Perl community by Imperia 
(L<http://www.imperia.net/>).

=head1 SEE ALSO

Locale::TextDomain(3pm), Locale::Messages(3pm), Encode(3pm),
perllocale(3pm), POSIX(3pm), perl(1), gettext(1), gettext(3)

=cut

Local Variables:
mode: perl
perl-indent-level: 4
perl-continued-statement-offset: 4
perl-continued-brace-offset: 0
perl-brace-offset: -4
perl-brace-imaginary-offset: 0
perl-label-offset: -4
tab-width: 4
End:

=cut
