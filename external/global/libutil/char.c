/*
 * Copyright (c) 2003 Tama Communications Corporation
 *
 * This file is part of GNU GLOBAL.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <ctype.h>

#include "char.h"
#include "strbuf.h"

#define B	BINARYCHAR
#define R	REGEXCHAR
#define U	URLCHAR
#define RU	REGEXCHAR | URLCHAR
const unsigned char chartype[256] = {
#if '\n' == 0x0a && ' ' == 0x20 && '0' == 0x30 \
  && 'A' == 0x41 && 'a' == 0x61 && '!' == 0x21
	/* ASCII */
	B, B, B, B, B, B, B, B, 0, 0, 0, 0, 0, 0, B, B,
	B, B, B, B, B, B, B, B, B, B, B, 0, B, B, B, B,
	0, U, 0, 0, R, 0, 0, U,RU,RU,RU, R, 0, U,RU, U,	/*  !"#$%&'()*+,-./ */
	U, U, U, U, U, U, U, U, U, U, 0, 0, 0, 0, 0, R,	/* 0123456789:;<=>? */
	0, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U,	/* @ABCDEFGHIJKLMNO */
	U, U, U, U, U, U, U, U, U, U, U, R, R, R, R, U,	/* PQRSTUVWXYZ[\]^_ */
	0, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U,	/* `abcdefghijklmno */
	U, U, U, U, U, U, U, U, U, U, U, R, 0, R, U,	/* pqrstuvwxyz{|}~ */
#else
#error "Unsupported character encoding."
#endif
};
/*
 * isregex: test whether or not regular expression
 *
 *	i)	s	string
 *	r)		1: is regex, 0: not regex
 */
int
isregex(const char *s)
{
	int c;

	while ((c = *s++) != '\0')
		if (isregexchar(c))
			return 1;
	return 0;
}
/*
 * quote string.
 *
 *	'aaa' => \'\a\a\a\'
 */
const char *
quote_string(const char *s)
{
	STATIC_STRBUF(sb);

	strbuf_clear(sb);
	for (; *s; s++) {
		if (!isalnum((unsigned char)*s))
			strbuf_putc(sb, '\\');
		strbuf_putc(sb, *s);
	}
	return strbuf_value(sb);
}
