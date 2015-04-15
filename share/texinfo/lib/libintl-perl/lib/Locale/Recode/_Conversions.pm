#! /bin/false
# vim: set autoindent shiftwidth=4 tabstop=4:
# $Id: _Conversions.pm,v 1.1 2011-10-12 23:51:27 pertusus Exp $

# List of internally known conversions.
# Copyright (C) 2002-2009 Guido Flohr <guido@imperia.net>,
# all rights reserved.
# This file is generated, do not edit!

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

package Locale::Recode::_Conversions;

use strict;
use integer;

use vars qw ($conversions $optional_conversions);

# These are the canonical names of the encodings always available.
$conversions = {
	'ASMO_449' => 'ASMO_449',
	'ATARI-ST-EURO' => 'ATARI_ST_EURO',
	'ATARI-ST' => 'ATARI_ST',
	'CP10007' => 'CP10007',
	'CSN_369103' => 'CSN_369103',
	'CWI' => 'CWI',
	'DEC-MCS' => 'DEC_MCS',
	'EBCDIC-AT-DE-A' => 'EBCDIC_AT_DE_A',
	'EBCDIC-AT-DE' => 'EBCDIC_AT_DE',
	'EBCDIC-CA-FR' => 'EBCDIC_CA_FR',
	'EBCDIC-DK-NO-A' => 'EBCDIC_DK_NO_A',
	'EBCDIC-DK-NO' => 'EBCDIC_DK_NO',
	'EBCDIC-ES-A' => 'EBCDIC_ES_A',
	'EBCDIC-ES-S' => 'EBCDIC_ES_S',
	'EBCDIC-ES' => 'EBCDIC_ES',
	'EBCDIC-FI-SE-A' => 'EBCDIC_FI_SE_A',
	'EBCDIC-FI-SE' => 'EBCDIC_FI_SE',
	'EBCDIC-FR' => 'EBCDIC_FR',
	'EBCDIC-IS-FRISS' => 'EBCDIC_IS_FRISS',
	'EBCDIC-IT' => 'EBCDIC_IT',
	'EBCDIC-PT' => 'EBCDIC_PT',
	'EBCDIC-UK' => 'EBCDIC_UK',
	'EBCDIC-US' => 'EBCDIC_US',
	'ECMA-CYRILLIC' => 'ECMA_CYRILLIC',
	'GEORGIAN-ACADEMY' => 'GEORGIAN_ACADEMY',
	'GEORGIAN-PS' => 'GEORGIAN_PS',
	'GOST_19768-74' => 'GOST_19768_74',
	'GREEK-CCITT' => 'GREEK_CCITT',
	'GREEK7-OLD' => 'GREEK7_OLD',
	'GREEK7' => 'GREEK7',
	'HP-ROMAN8' => 'HP_ROMAN8',
	'IBM037' => 'IBM037',
	'IBM038' => 'IBM038',
	'IBM1004' => 'IBM1004',
	'IBM1026' => 'IBM1026',
	'IBM1047' => 'IBM1047',
	'IBM256' => 'IBM256',
	'IBM273' => 'IBM273',
	'IBM274' => 'IBM274',
	'IBM275' => 'IBM275',
	'IBM277' => 'IBM277',
	'IBM278' => 'IBM278',
	'IBM280' => 'IBM280',
	'IBM281' => 'IBM281',
	'IBM284' => 'IBM284',
	'IBM285' => 'IBM285',
	'IBM290' => 'IBM290',
	'IBM297' => 'IBM297',
	'IBM420' => 'IBM420',
	'IBM423' => 'IBM423',
	'IBM424' => 'IBM424',
	'IBM437' => 'IBM437',
	'IBM500' => 'IBM500',
	'IBM850' => 'IBM850',
	'IBM851' => 'IBM851',
	'IBM852' => 'IBM852',
	'IBM855' => 'IBM855',
	'IBM857' => 'IBM857',
	'IBM860' => 'IBM860',
	'IBM861' => 'IBM861',
	'IBM862' => 'IBM862',
	'IBM863' => 'IBM863',
	'IBM864' => 'IBM864',
	'IBM865' => 'IBM865',
	'IBM866' => 'IBM866',
	'IBM868' => 'IBM868',
	'IBM869' => 'IBM869',
	'IBM870' => 'IBM870',
	'IBM871' => 'IBM871',
	'IBM874' => 'IBM874',
	'IBM875' => 'IBM875',
	'IBM880' => 'IBM880',
	'IBM891' => 'IBM891',
	'IBM903' => 'IBM903',
	'IBM904' => 'IBM904',
	'IBM905' => 'IBM905',
	'IBM918' => 'IBM918',
	'IEC_P27-1' => 'IEC_P27_1',
	'INIS-8' => 'INIS_8',
	'INIS-CYRILLIC' => 'INIS_CYRILLIC',
	'INIS' => 'INIS',
	'ISO-8859-1' => 'ISO_8859_1',
	'ISO-8859-10' => 'ISO_8859_10',
	'ISO-8859-11' => 'ISO_8859_11',
	'ISO-8859-13' => 'ISO_8859_13',
	'ISO-8859-14' => 'ISO_8859_14',
	'ISO-8859-15' => 'ISO_8859_15',
	'ISO-8859-16' => 'ISO_8859_16',
	'ISO-8859-2' => 'ISO_8859_2',
	'ISO-8859-3' => 'ISO_8859_3',
	'ISO-8859-4' => 'ISO_8859_4',
	'ISO-8859-5' => 'ISO_8859_5',
	'ISO-8859-6' => 'ISO_8859_6',
	'ISO-8859-7' => 'ISO_8859_7',
	'ISO-8859-8' => 'ISO_8859_8',
	'ISO-8859-9' => 'ISO_8859_9',
	'ISO_10367-BOX' => 'ISO_10367_BOX',
	'ISO_2033-1983' => 'ISO_2033_1983',
	'ISO_5427-EXT' => 'ISO_5427_EXT',
	'ISO_5427' => 'ISO_5427',
	'ISO_5428' => 'ISO_5428',
	'KOI-8' => 'KOI_8',
	'KOI8-R' => 'KOI8_R',
	'KOI8-RU' => 'KOI8_RU',
	'KOI8-T' => 'KOI8_T',
	'KOI8-U' => 'KOI8_U',
	'LATIN-GREEK-1' => 'LATIN_GREEK_1',
	'LATIN-GREEK' => 'LATIN_GREEK',
	'MACINTOSH' => 'MACINTOSH',
	'MACARABIC' => 'MACARABIC',
	'MACCYRILLIC' => 'MACCYRILLIC',
	'MACCROATIAN' => 'MACCROATIAN',
	'MACGREEK' => 'MACGREEK',
	'MACHEBREW' => 'MACHEBREW',
	'MACICELAND' => 'MACICELAND',
	'MACROMANIA' => 'MACROMANIA',
	'MACTHAI' => 'MACTHAI',
	'MACTURKISH' => 'MACTURKISH',
	'MACUKRAINE' => 'MACUKRAINE',
	'MAC-IS' => 'MAC_IS',
	'MAC-SAMI' => 'MAC_SAMI',
	'MAC-UK' => 'MAC_UK',
	'NATS-DANO' => 'NATS_DANO',
	'NATS-SEFI' => 'NATS_SEFI',
	'NEXTSTEP' => 'NEXTSTEP',
	'TIS-620' => 'TIS_620',
	'UTF-8' => 'UTF_8',
	'VISCII' => 'VISCII',
	'WIN-SAMI-2' => 'SAMI_WS2',
	'WINDOWS-1250' => 'CP1250',
	'WINDOWS-1251' => 'CP1251',
	'WINDOWS-1252' => 'CP1252',
	'WINDOWS-1253' => 'CP1253',
	'WINDOWS-1254' => 'CP1254',
	'WINDOWS-1256' => 'CP1256',
	'WINDOWS-1257' => 'CP1257',
	'US-ASCII' => 'US_ASCII',
};

# These encodings are maybe available via Encode(3pm).
$optional_conversions = {
	'BIG5' => undef,
	'BIG5-HKSCS' => undef,
	'CN-GB' => undef,
	'CN-GB-ISOIR165' => undef,
	'CP1006' => undef,
	'CP1026' => undef,
	'CP1047' => undef,
	'CP1361' => undef,
	'CP949' => undef,
    'CP37' => undef,
    'CP424' => undef,
    'CP500' => undef,
    'CP737' => undef,
    'CP775' => undef,
    'CP856' => undef,
    'CP874' => undef,
    'CP875' => undef,
    'CP932' => undef,
    'CP936' => undef,
    'CP950' => undef,
	'EUC-JP' => undef,
	'EUC-KR' => undef,
	'EUC-TW' => undef,
    # mapping from 0xef to 0xff missing.
	# 'HP-ROMAN8' => undef,
	'GB18030' => undef,
	'HZ' => undef,
	'IBM437' => undef,
	'IBM850' => undef,
	'IBM852' => undef,
	'IBM855' => undef,
	'IBM857' => undef,
	'IBM860' => undef,
	'IBM861' => undef,
	'IBM862' => undef,
	'IBM863' => undef,
	'IBM864' => undef,
	'IBM865' => undef,
	'IBM866' => undef,
	'IBM869' => undef,
	'ISO-10646-UCS-2' => undef,
	'ISO-10646-UCS-4' => undef,
	'ISO-2022-JP' => undef,
	'ISO-2022-JP-1' => undef,
	'ISO-2022-KR' => undef,
	'ISO-8859-1' => undef,
	'ISO-8859-10' => undef,
	# This is broken in some versions of Encode.
	# 'ISO-8859-11' => undef,
	'ISO-8859-13' => undef,
	'ISO-8859-14' => undef,
	'ISO-8859-15' => undef,
    # Errors at 0xa5 and 0xab.
	# 'ISO-8859-16' => undef,
	'ISO-8859-2' => undef,
	'ISO-8859-3' => undef,
	'ISO-8859-4' => undef,
	'ISO-8859-5' => undef,
    # Uses arabic digits in ascii range?!
	# 'ISO-8859-6' => undef,
    # 0xa1 and 0xa2 are incorrectly encoded.
	# 'ISO-8859-7' => undef,
    # 0xfd and 0xfe are missing.
	# 'ISO-8859-8' => undef,
	'ISO-8859-9' => undef,
	'ISO-IR-149' => undef,
	'KOI8-R' => undef,
    # 0x95 is BULLET, not BULLET OPERATOR.
	# 'KOI8-U' => undef,
    # Seems to be messed up in certain Encode versions.
	# 'MACINTOSH' => undef,
	# TODO: Check other Mac encodings for correctness.
	# Nextstep is completely broken in my version of Encode.
	# 'NEXTSTEP' => undef,
	'SHIFT_JIS' => undef,
	'UCS-2BE' => undef,
	'UCS-2LE' => undef,
	'UCS-4BE' => undef,
	'UCS-4LE' => undef,
	'US-ASCII' => undef,
	'UTF-16' => undef,
	'UTF-16BE' => undef,
	'UTF-16LE' => undef,
	'UTF-32' => undef,
	'UTF-32BE' => undef,
	'UTF-32LE' => undef,
	'UTF-8' => undef,
    # 0x86 is missing, 0xa6 is incorrectly encoded.
	# 'VISCII' => undef,
	'WINDOWS-1250' => undef,
	'WINDOWS-1251' => undef,
	'WINDOWS-1252' => undef,
	'WINDOWS-1253' => undef,
	'WINDOWS-1254' => undef,
	'WINDOWS-1255' => undef,
	'WINDOWS-1256' => undef,
	'WINDOWS-1257' => undef,
	'WINDOWS-1258' => undef,
};

my $has_encode;

sub resolveAlias
{
	my (undef, $encoding) = @_;

	$encoding = uc $encoding;
	
	return $encoding if exists $conversions->{$encoding};
	return $encoding if exists $optional_conversions->{$encoding};

	require Locale::Recode::_Aliases;

	my $resolved = Locale::Recode::_Aliases::ALIASES()->{$encoding};
	
	return $resolved if $resolved;

	return;
}

sub isSupported
{
	my ($class, $encoding) = @_;

	return unless defined $encoding && length $encoding;

	$encoding = uc $encoding;
	my $mimename = $class->resolveAlias ($encoding);

	return unless $mimename;
	
	# Determine the correct module.
	if (exists $optional_conversions->{$mimename}) {
		unless (defined $has_encode) {
			eval "require Encode";
			$has_encode = !$@;

			if ($has_encode) {
				require Encode::Alias;

				# Add missing real names.
				Encode::Alias::define_alias (MS_KANJI => 'ShiftJIS');
				Encode::Alias::define_alias ('CN-GB' => 'EUC-CN');
			}
		}

		if ($has_encode) {
			# Now check whether Encode really supports that encoding.
			eval "Encode::encode ('$mimename', 'x')";
		
			unless ($@) {
				$conversions->{$mimename} = '_Encode';
			}
			delete $optional_conversions->{$mimename};
		}
	}

	return $conversions->{$mimename} if exists $conversions->{$mimename};

	return;
}

sub listSupported
{
	my ($class) = @_;

	foreach my $opt (keys %$optional_conversions) {
		$class->isSupported ($opt);
	}

	my @list = keys %$conversions;
	return @list;
}

# Find a conversion path.
sub findPath
{
	my ($class, $from, $to) = @_;

	$from = 'INTERNAL' eq uc $from ? 'INTERNAL' : $class->resolveAlias ($from);
	$to = 'INTERNAL' eq uc $to ? 'INTERNAL' : $class->resolveAlias ($to);
	
	return unless $from && $to;
	
	return [] if $from eq $to;

	my $from_module = $class->isSupported ($from);
	my $to_module = $class->isSupported ($to);

	if (!$from_module) {
		if ('INTERNAL' eq $from) {
			$from_module = $to_module or return;
		} else {
			return;
		}
	}

	if (!$to_module) {
		if ('INTERNAL' eq $to) {
			$to_module = $from_module or return;
		} else {
			return;
		}
	}

	if ($from_module eq $to_module
		|| $to eq 'INTERNAL'
		|| $to eq 'UTF-8') {
		return [[ $from_module, $from, $to ]];
	} elsif ($from eq 'INTERNAL') {
		return [[ $to_module, $from, $to ]];
	} else {
		return [[ $from_module, $from, 'INTERNAL' ],
				[ $to_module, 'INTERNAL', $to ]];
	}
}

# TODO: check for
# 7bit-jis
# AdobeStandardEncoding
# AdobeSymbol
# AdobeZdingbat
# ascii-ctrl
# big5ext
# big5plus
# cccii
# cns11643-1
# cns11643-2
# cns11643-3
# cns11643-4
# cns11643-5
# cns11643-6
# cns11643-7
# cns11643-f
# dingbats
# gb12345-raw
# gb2312-raw
# gsm0338
# jis0201-raw
# jis0208-raw
# jis0212-raw
# koi8-f
# MIME-B
# MIME-Header
# MIME-Q
# posix-bc
# symbol
# unisys

1;

__END__

=head1 NAME

Locale::Recode::_Conversions - Internal Table of Known Conversions

=head1 SYNOPSIS

use Locale::Recode::_Conversions

This module is internal to libintl.  Do not use it directly!

=head1 AUTHOR

Copyright (C) 2002-2009, Guido Flohr E<lt>guido@imperia.netE<gt>, all
rights reserved.  See the source code for details.

This software is contributed to the Perl community by Imperia 
(L<http://www.imperia.net/>).

=head1 SEE ALSO

Locale::Recode(3), perl(1)

=cut
Local Variables:
mode: perl
perl-indent-level: 4
perl-continued-statement-offset: 4
perl-continued-brace-offset: 0
perl-brace-offset: -4
perl-brace-imaginary-offset: 0
perl-label-offset: -4
cperl-indent-level: 4
cperl-continued-statement-offset: 2
tab-width: 4
End:
=cut
