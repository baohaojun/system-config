/*
 * Copyright (c) 1997, 1998, 1999, 2000, 2004
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
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif
#ifdef DEBUG
#include <stdio.h>
extern int debug;
#endif

#include "locatestring.h"

/*

String locator: usage and memory status

        'v': result pointer
 
string = "ABC XYZ XYZ ABC"

pointer = locatestring(string, "XYZ", MATCH_FIRST);
             v
        "ABC XYZ XYZ ABC"

pointer = locatestring(string, "XYZ", MATCH_LAST);
                 v
        "ABC XYZ XYZ ABC"

pointer = locatestring(string, "XYZ", MATCH_AT_FIRST);

        "ABC XYZ XYZ ABC" (nothing pointed)

pointer = locatestring(string, "ABC", MATCH_AT_FIRST);
            v
        "ABC XYZ XYZ ABC" (point the following character)

pointer = locatestring(string, "ABC", MATCH_AT_LAST);
                     v
        "ABC XYZ XYZ ABC"

pointer = locatestring(string, "ABC XYZ XYZ ABC", MATCH_COMPLETE);
         v
        "ABC XYZ XYZ ABC"

pointer = locatestring(string, "xyZ", MATCH_FIRST|IGNORE_CASE);
             v
        "ABC XYZ XYZ ABC"

 */

/*
 * strincmp: strncmp with ignoring case.
 *
 *	Interface is same with strncmp.
 */
static int
strincmp(const char *string, const char *pattern, size_t len)
{
	unsigned char s, p;

	while (len--) {
		s = tolower((unsigned char)*string++);
		p = tolower((unsigned char)*pattern++);
		if (s != p)
			return s - p;
		if (s == 0)
			break;
	}
	return 0;
}

/*
 * locatestring: locate pattern from string
 *
 *	i)	string	string
 *	i)	pattern	pattern
 *	i)	flag	MATCH_FIRST:	match first
 *			MATCH_AT_FIRST: match only at first column
 *			MATCH_LAST:	match last
 *			MATCH_AT_LAST:	match only at last column
 *			MATCH_COMPLETE	match completely
 *			IGNORE_CASE:	Ignore case
 *	r)		pointer or NULL
 *			If the flag == MATCH_AT_FIRST then the pointer
 *			points the following character of the matched
 *			string, else points at the head of it.
 *
 * This function is made to avoid compatibility problems.
 */
char *
locatestring(const char *string, const char *pattern, int flag)
{
	int c = *pattern;
	int plen = strlen(pattern);
	const char *p = NULL;
	int slen;
	int (*cmpfunc) (const char *, const char*, size_t);
#ifdef DEBUG
	FILE *dbg = stderr;
	const char *pre = string;
#endif

	cmpfunc = (flag & IGNORE_CASE) ? strincmp : strncmp;
	flag &= ~IGNORE_CASE;

	if (flag == MATCH_COMPLETE) {
		if (strlen(string) == plen && !(*cmpfunc)(string, pattern, plen))
			return (char *)string;
		else
			return NULL;
	}
	if (flag == MATCH_AT_LAST && (slen = strlen(string)) > plen)
		string += (slen - plen);
	for (; *string; string++) {
		if (*string == c)
			if (!(*cmpfunc)(string, pattern, plen)) {
				p = string;
				if (flag == MATCH_FIRST)
					break;
			}
		if (flag == MATCH_AT_FIRST || flag == MATCH_AT_LAST)
			break;
	}
#ifdef DEBUG
	if (debug) {
		fprintf(dbg, "locatestring: ");
		if (p == NULL)
			fprintf(dbg, "%s", pre);
		else {
			const char *pp = p;
			const char *post = pp + strlen(pattern);

			for (; *pre && pre < pp; pre++)
				fputc(*pre, dbg);
			fputc('[', dbg);
			for (; *pp && pp < post; pp++)
				fputc(*pp, dbg);
			fputc(']', dbg);
			for (; *post; post++)
				fputc(*post, dbg);
		}
		fputc('\n', dbg);
	}
#endif
	if (p && flag == MATCH_AT_FIRST)
		p += plen;
	return (char *)p;
}
