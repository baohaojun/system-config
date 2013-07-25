/*
 * Copyright (c) 1998, 1999, 2000, 2004, 2006, 2010
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
#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include "strbuf.h"
#include "strmake.h"

/*
 * strmake: make string from original string with limit character.
 *
 *	i)	p	original string.
 *	i)	lim	limitter
 *	r)		result string
 *
 * Usage:
 *	strmake("aaa:bbb", ":/=")	=> "aaa"
 *
 * Note: The result string area is function local. So, following call
 *	 to this function may destroy the area.
 */
const char *
strmake(const char *p, const char *lim)
{
	STATIC_STRBUF(sb);
	const char *c;

	strbuf_clear(sb);
	for (; *p; p++) {
		for (c = lim; *c; c++)
			if (*p == *c)
				goto end;
		strbuf_putc(sb,*p);
	}
end:
	return strbuf_value(sb);
}

/*
 * strtrim: make string from original string with deleting blanks.
 *
 *	i)	p	original string.
 *	i)	flag	TRIM_HEAD	from only head
 *			TRIM_TAIL	from only tail
 *			TRIM_BOTH	from head and tail
 *			TRIM_ALL	from all
 *	o)	len	length of result string
 *			if len == NULL then nothing returned.
 *	r)		result string
 *
 * Usage:
 *	strtrim(" # define ", TRIM_HEAD, NULL)	=> "# define "
 *	strtrim(" # define ", TRIM_TAIL, NULL)	=> " # define"
 *	strtrim(" # define ", TRIM_BOTH, NULL)	=> "# define"
 *	strtrim(" # define ", TRIM_ALL, NULL)	=> "#define"
 *
 * Note: The result string area is function local. So, following call
 *	 to this function may destroy the area.
 */
const char *
strtrim(const char *p, int flag, int *len)
{
	STATIC_STRBUF(sb);
	int cut_off = -1;

	strbuf_clear(sb);
	/*
	 * Delete blanks of the head.
	 */
	if (flag != TRIM_TAIL)
		SKIP_BLANKS(p);
	/*
	 * Copy string.
	 */
	for (; *p; p++) {
		if (isspace(*p)) {
			if (flag != TRIM_ALL) {
				if (cut_off == -1 && flag != TRIM_HEAD)
					cut_off = strbuf_getlen(sb);
				strbuf_putc(sb,*p);
			}
		} else {
			strbuf_putc(sb,*p);
			cut_off = -1;
		}
	}
	/*
	 * Delete blanks of the tail.
	 */
	if (cut_off != -1)
		strbuf_setlen(sb, cut_off);
	if (len)
		*len = strbuf_getlen(sb);
	return strbuf_value(sb);
}
/*
 * strcmp with terminate character.
 *
 *	i)	s1	string1
 *	i)	s2	string2
 *	i)	term	terminate character
 *	r)		==0: equal, !=0: not equal
 *
 * Usage:
 *	strcmp_withterm("aaa", "aaa", ':')		=> 0
 *	strcmp_withterm("aaa:bbb", "aaa", ':')		=> 0
 *	strcmp_withterm("aaa:bbb", "aaa:ccc", ':')	=> 0
 *	strcmp_withterm("aaa/bbb", "aaa/ccc", ':')	=> -1
 */
int
strcmp_withterm(const char *s1, const char *s2, int term)
{
	unsigned int c1, c2;

	do {
		c1 = *s1++;
		c2 = *s2++;
		/* replace terminate character with NULL */
		if (c1 == term)
			c1 = '\0';
		if (c2 == term)
			c2 = '\0';
	} while (c1 == c2 && c1 != '\0');

	return c1 - c2;
}
/*
 * strcpy with terminate character.
 *
 *	i)	b	buffer
 *	i)	s	string
 *	i)	size	buffer size
 *	i)	term	terminate character
 *	r)		terminator's position
 */
const char *
strcpy_withterm(char *b, const char *s, int size, int term)
{
	char *endp = b + size - 1;

	while (*s && *s != term)
		if (b < endp)
			*b++ = *s++;
	*b = '\0';

	return s;
}
