/*
 * Copyright (c) 1996, 1997, 1998, 1999, 2000, 2006
 *	Tama Communications Corporation
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
#include <stdio.h>

#include "die.h"
#include "tab.h"

/*
 * Puct and getc are very slow on some platforms including GNU/Linux.
 * Because GLOBAL does not have multi-threaded program,
 * they can be replaced with non thread safe version.
 */
#ifdef HAVE_PUTC_UNLOCKED
#undef putc
#define putc	putc_unlocked
#endif
#ifdef HAVE_GETC_UNLOCKED
#undef getc
#define getc	getc_unlocked
#endif

static int tabs = 8;

/*
 * settabs: set default tab stop
 *
 *	i)	n	tab stop
 */
void
settabs(int n)
{
	if (n < 1 || n > 32)
		return;
	tabs = n;
}
/*
 * Read file converting tabs into spaces.
 *
 *	o)	buf	
 *	i)	size	size of 'buf'
 *	i)	ip	input file
 *	o)	dest_saved	current column in 'buf'
 *	o)	spaces_saved	left spaces
 *	r)		size of data
 *
 * Dest_saved and spaces_saved are control variables.
 * You must initialize them with 0 when the input file is opened.
 */
#define PUTSPACES \
	do {									\
		int n = (spaces < size) ? spaces : size;			\
		dest += n;							\
		size -= n;							\
		spaces -= n;							\
		do {								\
			*p++ = ' ';						\
		} while (--n);							\
	} while (0)
size_t
read_file_detabing(char *buf, size_t size, FILE *ip, int *dest_saved, int *spaces_saved)
{
	char *p;
	int c, dest, spaces;

	if (size == 0)
		return 0;
	p = buf;
	dest = *dest_saved;
	spaces = *spaces_saved;
	if (spaces > 0)
		PUTSPACES;
	while (size > 0) {
		c = getc(ip);
		if (c == EOF) {
			if (ferror(ip))
				die("read error.");
			break;
		}
		if (c == '\t') {
			spaces = tabs - dest % tabs;
			PUTSPACES;
		} else {
			*p++ = c;
			dest++;
			if (c == '\n')
				dest = 0;
			size--;
		}
	}
	*dest_saved = dest;
	*spaces_saved = spaces;
	return p - buf;
}
/*
 * detab_replacing: convert tabs into spaces and print with replacing.
 *
 *	i)	op	FILE *
 *	i)	buf	string including tabs
 *	i)	replace	replacing function
 */
void
detab_replacing(FILE *op, const char *buf, const char *(*replace)(int c))
{
	int dst, spaces;
	int c;

	dst = 0;
	while ((c = *buf++) != '\0') {
		if (c == '\t') {
			spaces = tabs - dst % tabs;
			dst += spaces;
			do {
				putc(' ', op);
			} while (--spaces);
		} else {
			const char *s = replace(c);
			if (s)
				fputs(s, op);
			else
				putc(c, op);
			dst++;
		}
	}
	putc('\n', op);
}
